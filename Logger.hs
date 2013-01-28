{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Logger
    ( editLog
    , helpLog
    , newLog
    , removeLog
    , viewLog
    ) where

import Log
--import ParseLog

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Calendar
import Data.Time.Clock
import System.Console.Haskeline
import System.Directory
import System.IO
import System.IO.Error

--Utility Functions

today :: IO Day
today = liftM utctDay getCurrentTime

logsFolder :: String -> IO FilePath
logsFolder x = getHomeDirectory >>= \h -> return $ h ++ "/Dropbox/logs/" ++ x ++ ".log"

openLog :: String -> IOMode -> IO Handle
openLog name mode = logsFolder name >>= flip openFile mode

--Writing to log files

editLog :: [String] -> InputT IO ()
editLog []    = helpLog ["edit"]
editLog [_]   = undefined
editLog (n:e) = liftIO $ do
    eitherLog <- tryIOError $ openLog n AppendMode
    case eitherLog of
        Left er ->
            when (isDoesNotExistError er) $ newLog [n] >> today >>= appendLog n . makeEntry e
        Right h -> today >>= hAppendLog h . makeEntry e

appendLog :: String -> Entry -> IO ()
appendLog n e = openLog n AppendMode >>= flip hPrint e

hAppendLog :: Handle -> Entry -> IO ()
hAppendLog = hPrint

--Help using the logger

helpLog :: [String] -> InputT IO ()
helpLog _ = outputStrLn
    "Usage: \n\
    \log edit   <logs>       Edit the given log files\n\
    \log help   <command>    If no command is given, display this message. Otherwise, display specific help message for a command.\n\
    \log new    <names>      Create log files with the given names\n\
    \log delete <logs>       Delete the given log files"

--Creating logs

newLog :: [String] -> IO ()
newLog = foldr (\x -> (>>) $ openLog x ReadWriteMode >>= \h -> creationEntry x h >> hClose h) $ return ()

creationEntry :: String -> Handle -> IO ()
creationEntry name h = today >>= hPutStrLn h . (++) (name ++ "\nCreated on ") . showGregorian

--Removing logs

removeLog :: [String] -> IO ()
removeLog = foldr (\x -> (>>) $ logsFolder x >>= \f -> ensureRemoveFile f) $ return ()

ensureRemoveFile :: FilePath -> IO ()
ensureRemoveFile = removeFile

--Viewing Logs

viewLog :: [String] -> IO ()
viewLog = foldr (\x -> (>>) $ openFile x ReadMode >>= \h -> hGetContents h >>= putStrLn >> hClose h) $ return ()
