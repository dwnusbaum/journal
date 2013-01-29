{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Logger
    ( editLog
    , helpLog
    , newLog
    , removeLog
    , viewLog
    ) where

import Entry

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.LocalTime
import System.Console.Haskeline
import System.Directory
import System.IO
import System.IO.Error

--Utility Functions

logsFolder :: String -> IO FilePath
logsFolder x = getHomeDirectory >>= \h -> return $ h ++ "/Dropbox/logs/" ++ x ++ ".log"

openLog :: String -> IOMode -> IO Handle
openLog name mode = logsFolder name >>= flip openFile mode

--Writing to log files

editLog :: String -> InputT IO ()
editLog n = do
    eitherLog <- liftIO $ tryIOError $ openLog n AppendMode
    case eitherLog of
        Left err -> undefined --when (isDoesNotExistError err) $ newLog n >> getZonedTime >>= appendLog n . makeEntry e
        Right ha -> undefined --getZonedTime >>= hAppendLog h . makeEntry e

appendLog :: String -> Entry -> IO ()
appendLog n e = openLog n AppendMode >>= \h -> hPrint h e >> hClose h

hAppendLog :: Handle -> Entry -> IO ()
hAppendLog h e = hPrint h e >> hClose h

--Help using the logger

helpLog :: String -> IO ()
helpLog _ = putStrLn
    "Usage: \n\
    \log edit   <logs>       Edit the given log files\n\
    \log help   <command>    If no command is given, display this message. Otherwise, display specific help message for a command.\n\
    \log new    <names>      Create log files with the given names\n\
    \log delete <logs>       Delete the given log files"

--Creating logs

newLog :: String -> IO ()
newLog n = openLog n ReadWriteMode >>= \h -> hPutStrLn h n >> hClose h

--Removing logs

removeLog :: String -> IO ()
removeLog n = logsFolder n >>= ensureRemoveFile

ensureRemoveFile :: FilePath -> IO ()
ensureRemoveFile = removeFile

--Viewing Logs

viewLog :: String -> IO ()
viewLog n = logsFolder n >>= flip openFile ReadMode >>= \h -> hGetContents h >>= putStrLn >> hClose h
