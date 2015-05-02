{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Network.Protocol.Snmp.AgentX
import Data.ByteString.Char8 (ByteString, pack, unpack)
import System.Directory
import System.FilePath
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import System.Process
import Control.Exception
import Control.Concurrent
import Data.Maybe (fromMaybe)
import Data.Time
import System.Exit
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Binary
import System.IO.Temp

import Prelude 

-- | Container for dictionary (script name ->  PVal) 
data Handle = Handle
  { exitCodeHandle :: ScriptName -> PVal
  , optionsHandle  :: ScriptName -> PVal
  , outputHandle   :: ScriptName -> PVal
  , errorsHandle   :: ScriptName -> PVal
  , statusHandle   :: ScriptName -> PVal
  }

-- | options for scripts
type Options = [String]

type ScriptName = String

data ScriptValues = ScriptValues
  { exitCode :: Value -- ^ returned value, int
  , output   :: Value -- ^ output, string
  , errors   :: Value -- ^ errors, string
  , options  :: Value -- ^ options, string
  , status   :: Value -- ^ 0 - disabled, 1 - enabled
  , lastExec :: UTCTime -- ^ last execute time
  } 

-- | path to folder with users scripts
scriptsPath :: FilePath
scriptsPath = "scripts"

-- | path to folder with users scripts
scriptsAliasesPath :: FilePath
scriptsAliasesPath = "scripts_aliases"

-- | path to folder with nagios plugins
nagiosPluginsPath :: FilePath
nagiosPluginsPath = "nagios"

-- | path to nagios plugins aliases
nagiosAliasesPath :: FilePath
nagiosAliasesPath = "nagios_aliases"

-- | state path
statePath :: FilePath
statePath = "script-to-snmp-state"

-- | snmp agent
-- execute scripts and checks in scriptPath and nagiosScriptsPath
-- return result as SNMP
main :: IO ()
main = do
    (configVersion, st) <- handle emptyMap (decodeFile statePath)
    mv <- newMVar st
    cv <- newMVar configVersion
    exitStatus <- newEmptyMVar 
    handle (putMVar exitStatus) $ agent "/var/agentx/master" [1,3,6,1,4,1,44729] Nothing (mibs (mkHandle cv mv))
    _ <- readMVar exitStatus :: IO ExitCode
    putStrLn "save status"
    newst <- Map.filter (\x -> status x == enabled True || status x == enabled False || options x /= str "") <$> readMVar mv
    print newst
    newcv <- readMVar cv
    when (st /= newst) $ encodeFile statePath (newcv, newst)
    where
      emptyMap :: SomeException -> IO (Integer, Map ScriptName ScriptValues)
      emptyMap _ = return (1, Map.empty)

-- | build MIB tree
mibs :: Handle -> [MIB]
mibs h = [ mkObject 0 "Fixmon" "about" Nothing
           , mkObjectType 0 "about" "agent-name" Nothing (bstr "script-to-snmp")
           , mkObjectType 1 "about" "version" Nothing (bstr "0.1")
           , mkObjectType 2 "about" "config_version" Nothing save
         , mkObject 1 "Fixmon" "scripts" Nothing
           , mkObject 1 "scripts" "scripts_table" (Just (table h scriptsPath scriptsAliasesPath))
         -- , mkObject 2 "Fixmon" "nagios" (Just (scripts h nagiosScriptsPath))
         , mkObject 2 "Fixmon" "nagios" Nothing
           , mkObject 1 "nagios" "nagios_table" (Just (table h nagiosPluginsPath nagiosAliasesPath))
         ]

table :: Handle -> FilePath -> FilePath -> Update
table h fp afp = Update $ do
    files <- liftIO $ getDeepDirectoryContents fp
    aliases <- liftIO $ getDeepDirectoryContents afp
    return $ mkTable h "nagios_table" Nothing object (map (fp </>) files ++ map (afp </>) aliases) 


type Obj = [(String, Handle -> FilePath -> PVal)]

object :: Obj
object = [ ("name"    , \_ n -> bstr (pack (takeFileName n)))
         , ("status"  , \h n -> statusHandle h n)
         , ("opts"    , \h n -> optionsHandle h n)
         , ("exitCode", \h n -> exitCodeHandle h n)
         , ("stderr"  , \h n -> errorsHandle h n)
         , ("stdout"  , \h n -> outputHandle h n)
         ]


mkTable :: Handle -> String -> Maybe Context -> Obj -> [FilePath] -> [MIB]
mkTable h parent mc obj scripts' =
    let count = length scripts'
        size = length obj
        tableHead = [ mkObject 1 parent "table_size" Nothing
                    , mkObjectType 0 "table_size" "table_size" mc (rsValue (Gaude32 $ fromIntegral count))
                    , mkObject 2 parent "table_body" Nothing
                    , mkObject 1 "table_body" "table_rows" Nothing
                    ]
        indexes :: [MIB]
        indexes = mkObject 1 "table_rows" "indexes" Nothing :
          map (\x -> mkObjectType x "indexes" "" mc (rsValue (Integer $ fromIntegral x))) [1 .. (fromIntegral count)]
        row :: Integer -> (String, Handle -> String -> PVal) -> [MIB]
        row n (name, pv) = mkObject n "table_rows" name Nothing :
          (map (\(x, fp) -> mkObjectType x name "" mc (pv h fp)) 
            $ zip [1 .. (fromIntegral count)] scripts')
        rows = concatMap (\(i, x) -> row i x) (zip [2 .. (1 + fromIntegral size)] obj)
    in tableHead ++ indexes ++ rows

-- | create handle
mkHandle :: MVar Integer -> MVar (Map ScriptName ScriptValues) -> Handle
mkHandle cv mv = Handle 
    { exitCodeHandle = Read . vread exitCode
    , optionsHandle = \sn -> rwValue (vread options sn) (commitOpts sn) (testOpts sn) (undoOpts sn)
    , outputHandle = Read . vread output
    , errorsHandle = Read . vread errors
    , statusHandle = \sn -> rwValue (vread status sn) (commitStatus sn) (testStatus sn) (undoOpts sn)
    }
    where
    zeroTime = UTCTime (toEnum 0) (toEnum 0)
    -- execute script , make getter for ro result and reexecute 
    vread f sn = do
        -- execute script by script name, modify mv
        runScript sn  
        m <- readMVar mv 
        return . f $ m ! sn
    -- setter for Options
    commitOpts sn v = do
        runScript sn 
        updateVersion
        modifyMVar_ mv (return . Map.update (\x -> Just $ x { options = v, lastExec = zeroTime }) sn)
        return NoCommitError
    -- Options must be string
    testOpts _ (String _) = return NoTestError
    testOpts _ _ = return WrongValue
    -- Nothing here
    undoOpts _ _ = return NoUndoError
    testStatus _ (Integer 0) = return NoTestError
    testStatus _ (Integer 1) = return NoTestError
    testStatus _ (Integer 2) = return NoTestError
    testStatus sn (Integer 3) = do
        -- only aliases can be removed
        if (isNotAlias sn)
           then return NoAccess
           else return NoTestError
    testStatus _ (Integer _) = return BadValue
    testStatus _ _ = return WrongValue
    commitStatus sn (Integer 2) = do
        updateVersion
        -- add alias
        let f = takeFileName sn
        aliasDirectory <- createTempDirectory (getAliasDirectory sn) f 
        copyFile sn (aliasDirectory </> f)
        runScript (aliasDirectory </> f)
        return NoCommitError
    commitStatus sn (Integer 3) = do
        updateVersion
        -- remove alias
        let d = dropFileName sn
        if (isNotAlias sn)
           then return CommitFailed
           else do
               removeFile sn
               removeDirectory d
               modifyMVar_ mv (return . Map.delete sn)
               return NoCommitError
    commitStatus sn v = do
        updateVersion
        runScript sn
        let vv = if (isNotAlias sn) then v else addTen v
        modifyMVar_ mv (return . Map.update (\x -> Just x { status = vv, lastExec = zeroTime }) sn)
        return NoCommitError
    addTen (Integer x) = Integer $ x + 10
    addTen _ = undefined
    -- update verion
    updateVersion = modifyMVar_ cv (return . succ)

    -- If first run, execute script, save ScriptValues to Map
    -- if other run, check timeout after last execute, when > 5s execute, or return last result
    runScript :: ScriptName -> IO ()
    runScript sn = modifyMVar_ mv $
        \st -> maybe (runAndUpdate st Nothing Nothing) (checkAndReturn st) $ Map.lookup sn st
      where
        runAndUpdate st opts' status' = do
            let String options' = fromMaybe (String "") opts'
                status'' = fromMaybe (disabled $ isNotAlias sn) status'
            if status'' == enabled (isNotAlias sn)
               then do
                   (c, o, e)  <- catch (readProcessWithExitCode sn (words . unpack $ options') []) 
                                      (\(problem::SomeException) -> return (ExitFailure (-1), "", show problem))
                   now <- getCurrentTime
                   let val = ScriptValues (exitToValue c) (str o) (str e) (String options') (enabled $ isNotAlias sn) now
                   return $ Map.insert sn val st
               else do
                   now <- getCurrentTime
                   let val = ScriptValues (exitToValue $ ExitFailure (-1)) (str "") (str "") (String options') (disabled $ isNotAlias sn) now
                   return $ Map.insert sn val st
        checkAndReturn st val = do
            now <- getCurrentTime
            if diffUTCTime now (lastExec val) > 5
               then runAndUpdate st (Just $ options val) (Just $ status val)
               else return st

-- | helpers
bstr :: ByteString -> PVal
bstr x = rsValue (String x)

str :: String -> Value
str x = String (pack x)

enabled :: Bool -> Value
enabled True = Integer 1
enabled False = Integer 11

disabled :: Bool -> Value 
disabled True = Integer 0
disabled False = Integer 10

exitToValue :: ExitCode -> Value
exitToValue ExitSuccess = Integer 0
exitToValue (ExitFailure i) = Integer $ fromIntegral i

getDeepDirectoryContents :: FilePath -> IO [FilePath]
getDeepDirectoryContents fp = do
    files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents fp
    concat <$> mapM fun files
    where
    fun :: FilePath -> IO [FilePath]
    fun x = do
        isD <- doesDirectoryExist (fp </> x)
        if isD
           then do
               f <- filter (`notElem` [".", ".."]) <$> getDirectoryContents (fp </> x)
               concat <$> mapM (\y -> fun (x </> y)) f
           else return [x]

base :: FilePath -> FilePath
base = head . splitDirectories

isScripts :: FilePath -> Bool
isScripts fp = scriptsPath == base fp

isNagiosPlugin :: FilePath -> Bool
isNagiosPlugin fp = nagiosPluginsPath == base fp

getAliasDirectory :: FilePath -> FilePath
getAliasDirectory fp 
  | isScripts fp = scriptsAliasesPath
  | otherwise = nagiosAliasesPath
             

isNotAlias :: FilePath -> Bool
isNotAlias fp = nagiosPluginsPath == (base fp) || scriptsPath == (base fp)


-- | here must be state saver
save :: PVal
save = rwValue readV commit test undo
  where
    test _ = return NoTestError
    commit _ = return NoCommitError
    undo _ = return NoUndoError
    readV = return $ String "success"

instance Binary ScriptValues where
    put sv = do
        let String s = options sv
            Integer i = status sv
        put s >> put i
    get = do
        s <- get
        i <- get
        return $ ScriptValues (String "") (String "") (String "") (String s) (Integer i) (UTCTime (toEnum 0) (toEnum 0))

instance Eq ScriptValues where
    a == b = options a == options b && status a == status b

instance Show ScriptValues where
    show a = show (options a)
