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
import Data.Maybe (isJust, fromJust)
import Data.Time
import System.Exit
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.IORef

import Prelude 

data Handle = Handle
  { status :: ScriptName -> IO Value
  , opts :: ScriptName -> PVal
  , out :: ScriptName -> IO Value
  , err :: ScriptName -> IO Value
  }

type Options = [String]

type ScriptName = String

data ScriptValues = ScriptValues
  { exitCode :: Value
  , output   :: Value
  , errors   :: Value
  , options  :: IORef Options
  , lastExec :: UTCTime
  }

scriptsPath :: FilePath
scriptsPath = "scripts"

nagiosScriptsPath :: FilePath
nagiosScriptsPath = "nagios"

main :: IO ()
main = do
    mv <- newMVar Map.empty
    agent "/var/agentx/master" [1,3,6,1,4,1,44729] Nothing (mibs $ mkHandle mv)

mibs :: Handle -> [MIB]
mibs h = [ mkObject 0 "Fixmon" "about" Nothing
           , mkObjectType 0 "about" "agent-name" Nothing (bstr "script-to-snmp")
           , mkObjectType 1 "about" "version" Nothing (bstr "0.1")
           , mkObjectType 2 "about" "update" Nothing update
         , mkObject 1 "Fixmon" "scripts" (Just (scripts h scriptsPath))
         , mkObject 2 "Fixmon" "nagios" (Just (scripts h nagiosScriptsPath))
         ]

scripts :: Handle -> FilePath -> Update
scripts h fp = Update $ do
    files <- filter (\x -> x /= "." && x /= "..") <$> (liftIO $ getDirectoryContents fp)
    result <- mapM toObjects $ zip files [0 .. fromIntegral (length files)]
    return $ concat result
    where
    toObjects (n, i) = do
        isD <- liftIO $ doesDirectoryExist (fp </> n)
        if isD
           then return $ [mkObject i fp n (Just (scripts h (fp </> n)))]
           else return 
             [ mkObject i fp n Nothing
             , mkObjectType 0 n "status" Nothing (rdValue (status h (fp </> n)))
             , mkObjectType 1 n "name" Nothing (bstr $ pack (fp </> n))
             , mkObjectType 2 n "opts" Nothing (opts h (fp </> n))
             , mkObjectType 3 n "stdout" Nothing (rdValue $ out h (fp </> n))
             , mkObjectType 4 n "stderr" Nothing (rdValue $ err h (fp </> n))
             ]

mkHandle :: MVar (Map ScriptName ScriptValues) -> Handle
mkHandle mv = Handle 
    { status = \sn -> do
        runScript sn  
        m <- readMVar mv 
        return . exitCode $ m ! sn
    , opts = \sn -> rwValue (readOpts sn) (commitOpts sn) (testOpts sn) (undoOpts sn)
    , out = \sn -> do
        runScript sn 
        m <- readMVar mv
        return . output $ m ! sn
    , err = \sn -> do
        runScript sn 
        m <- readMVar mv
        return . errors $ m ! sn
    }
    where
    readOpts sn = do
        createOpts sn 
        m <- readMVar mv
        v <- readIORef (options $ m ! sn)
        return . String . pack . unwords $ v
    commitOpts sn (String v) = do
        createOpts sn 
        m <- readMVar mv
        writeIORef (options $ m ! sn) (words $ unpack v)
        return NoCommitError
    commitOpts _ _ = error "bad commit"
    testOpts _ (String _) = return NoTestError
    testOpts _ _ = return WrongValue
    undoOpts _ _ = return NoUndoError

    createOpts :: String -> IO ()
    createOpts sn = modifyMVar_ mv $ \st -> maybe (createOpts' st) (const (return st)) $ Map.lookup sn st
      where
        createOpts' st = do
            i <- newIORef []
            let zero = str ""
            let val = ScriptValues zero zero zero i (UTCTime (toEnum 0) (toEnum 0))
            return $ Map.insert sn val st
    
    runScript :: String -> IO ()
    runScript sn = modifyMVar_ mv $ \st -> maybe (runAndUpdate st Nothing) (checkAndReturn st) $ Map.lookup sn st
      where
        runAndUpdate st opts' = do
            opts'' <- if isJust opts'
                        then return (fromJust opts')
                        else newIORef []
            options' <- readIORef opts''
            (c, o, e)  <- catch (readProcessWithExitCode sn options' []) (\(problem::SomeException) -> return (ExitFailure 1, "", show problem))
            now <- getCurrentTime
            let val = ScriptValues (str (show c)) (str o) (str e) opts'' now
            return $ Map.insert sn val st
        checkAndReturn st val = do
            now <- getCurrentTime
            if diffUTCTime now (lastExec val) > 5
               then runAndUpdate st (Just $ options val)
               else return st

bstr :: ByteString -> PVal
bstr x = rsValue (String x)

str :: String -> Value
str x = String (pack x)

update :: PVal
update = rwValue readV commit test undo
  where
    test _ = return NoTestError
    commit _ = return NoCommitError
    undo _ = return NoUndoError
    readV = return $ String "success"



