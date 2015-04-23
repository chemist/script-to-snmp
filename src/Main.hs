{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Network.Protocol.Snmp.AgentX
import Data.ByteString.Char8 (ByteString, pack)
import System.Directory
import System.FilePath
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import System.Process
import Control.Exception
import Control.Concurrent
import Data.Time
import System.Exit
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

scriptsPath :: FilePath
scriptsPath = "scripts"

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

mibs :: Handle -> [MIB]
mibs h = [ mkObject 0 "Fixmon" "about" Nothing
           , mkObjectType 0 "about" "agent-name" Nothing (bstr "script-to-snmp")
           , mkObjectType 1 "about" "version" Nothing (bstr "0.1")
           , mkObjectType 2 "about" "update" Nothing update
         , mkObject 1 "Fixmon" "scripts" (Just (scripts h scriptsPath))
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
             , mkObjectType 1 n "name" Nothing (bstr $! pack (fp </> n))
             , mkObjectType 2 n "stdout" Nothing (rdValue $! out h (fp </> n))
             , mkObjectType 3 n "stderr" Nothing (rdValue $! err h (fp </> n))
             ]

data Handle = Handle
  { status :: String -> IO Value
  , out :: String -> IO Value
  , err :: String -> IO Value
  }

executor :: MVar (Map String ((Value, Value, Value), UTCTime)) -> Handle
executor mv = Handle 
    { status = \scr -> do
        runScript scr mv 
        m <- readMVar mv 
        return . one . fst $ m ! scr
    , out = \scr -> do
        runScript scr mv
        m <- readMVar mv
        return . two . fst $ m ! scr
    , err = \scr -> do
        runScript scr mv
        m <- readMVar mv
        return . three . fst $ m ! scr
    }

one :: (a, b, c) -> a
one (x, _, _) = x

two :: (a, b, c) -> b
two (_, x, _) = x

three :: (a, b, c) -> c
three (_, _, x) = x

runScript :: String -> MVar (Map String ((Value, Value, Value), UTCTime)) -> IO ()
runScript scr mv = modifyMVar_ mv $ \st -> maybe (runAndUpdate st) (checkAndReturn st) $ Map.lookup scr st
  where
    runAndUpdate st = do
        (c, o, e)  <- catch (readProcessWithExitCode scr [] []) (\(problem::SomeException) -> return (ExitFailure 1, "", show problem))
        now <- getCurrentTime
        return $ Map.insert scr ((str (show c), str o, str e), now) st
    checkAndReturn st (_, t) = do
        now <- getCurrentTime
        if diffUTCTime now t > 5
           then runAndUpdate st
           else return st

main :: IO ()
main = do
    mv <- newMVar Map.empty
    agent "/var/agentx/master" [1,3,6,1,4,1,44729] Nothing (mibs $ executor mv)

