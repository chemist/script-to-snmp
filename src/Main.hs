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

import Prelude 

-- | Container for dictionary (script name ->  PVal) 
data Handle = Handle
  { exitCodeHandle :: ScriptName -> PVal
  , optionsHandle  :: ScriptName -> PVal
  , outputHandle   :: ScriptName -> PVal
  , errorsHandle   :: ScriptName -> PVal
  }

-- | options for scripts
type Options = [String]

type ScriptName = String

data ScriptValues = ScriptValues
  { exitCode :: Value -- ^ returned value, string
  , output   :: Value -- ^ output, string
  , errors   :: Value -- ^ errors, string
  , options  :: Value -- ^ mutable store for options
  , lastExec :: UTCTime -- ^ last execute time
  }

-- | path to folder with users scripts
scriptsPath :: FilePath
scriptsPath = "scripts"

-- | path to folder with nagios plugins
nagiosScriptsPath :: FilePath
nagiosScriptsPath = "nagios"

-- | snmp agent
-- execute scripts and checks in scriptPath and nagiosScriptsPath
-- return result as SNMP
main :: IO ()
main = do
    mv <- newMVar Map.empty
    agent "/var/agentx/master" [1,3,6,1,4,1,44729] Nothing (mibs $ mkHandle mv)

-- | build MIB tree
mibs :: Handle -> [MIB]
mibs h = [ mkObject 0 "Fixmon" "about" Nothing
           , mkObjectType 0 "about" "agent-name" Nothing (bstr "script-to-snmp")
           , mkObjectType 1 "about" "version" Nothing (bstr "0.1")
           , mkObjectType 2 "about" "update" Nothing update
         , mkObject 1 "Fixmon" "scripts" (Just (scripts h scriptsPath))
         , mkObject 2 "Fixmon" "nagios" (Just (scripts h nagiosScriptsPath))
         ]

-- | recursively MIBs builder from directory structure
scripts :: Handle -> FilePath -> Update
scripts h fp = Update $ do
    files <- filter (`notElem` [".", ".."]) <$> (liftIO $ getDirectoryContents fp)
    concat <$> zipWithM toObjects files [0..]
    where
    toObjects n i = do
        isD <- liftIO $ doesDirectoryExist (fp </> n)
        if isD
           then return $ [mkObject i fp n (Just (scripts h (fp </> n)))]
           else return 
             [ mkObject i fp n Nothing
             , mkObjectType 0 n "status" Nothing $ exitCodeHandle h (fp </> n)
             , mkObjectType 1 n "name"   Nothing $ bstr    $ pack (fp </> n)
             , mkObjectType 2 n "opts"   Nothing $ optionsHandle h (fp </> n)
             , mkObjectType 3 n "stdout" Nothing $ outputHandle h (fp </> n)
             , mkObjectType 4 n "stderr" Nothing $ errorsHandle h (fp </> n)
             ]

-- | create handle
mkHandle :: MVar (Map ScriptName ScriptValues) -> Handle
mkHandle mv = Handle 
    { exitCodeHandle = Read . vread exitCode
    , optionsHandle = \sn -> rwValue (vread options sn) (commitOpts sn) (testOpts sn) (undoOpts sn)
    , outputHandle = Read . vread output
    , errorsHandle = Read . vread errors
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
        modifyMVar_ mv (return . Map.update (\x -> Just $ x { options = v, lastExec = zeroTime }) sn)
        return NoCommitError
    -- Options must be string
    testOpts _ (String _) = return NoTestError
    testOpts _ _ = return WrongValue
    -- Nothing here
    undoOpts _ _ = return NoUndoError

    -- If first run, execute script, save ScriptValues to Map
    -- if other run, check timeout after last execute, when > 5s execute, or return last result
    runScript :: ScriptName -> IO ()
    runScript sn = modifyMVar_ mv $
        \st -> maybe (runAndUpdate st Nothing) (checkAndReturn st) $ Map.lookup sn st
      where
        runAndUpdate st opts' = do
            let String options' = fromMaybe (String "") opts'
            (c, o, e)  <- catch (readProcessWithExitCode sn (words . unpack $ options') []) 
                               (\(problem::SomeException) -> return (ExitFailure 1, "", show problem))
            now <- getCurrentTime
            let val = ScriptValues (str (show c)) (str o) (str e) (String options') now
            return $ Map.insert sn val st
        checkAndReturn st val = do
            now <- getCurrentTime
            if diffUTCTime now (lastExec val) > 5
               then runAndUpdate st (Just $ options val)
               else return st

-- | helpers
bstr :: ByteString -> PVal
bstr x = rsValue (String x)

str :: String -> Value
str x = String (pack x)

-- | here must be state saver
update :: PVal
update = rwValue readV commit test undo
  where
    test _ = return NoTestError
    commit _ = return NoCommitError
    undo _ = return NoUndoError
    readV = return $ String "success"

