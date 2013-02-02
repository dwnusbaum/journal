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
logsFolder x = liftM (\h -> h ++ "/Dropbox/logs/" ++ x ++ ".hlog") getHomeDirectory

openLog :: String -> IOMode -> IO Handle
openLog name mode = logsFolder name >>= flip openFile mode

--Writing to log files

editLog :: String -> InputT IO ()
editLog n = do
    eitherLog <- liftIO $ tryIOError $ openLog n AppendMode
    case eitherLog of
        Left  e -> when (isDoesNotExistError e) $ liftIO (newLog n) >> editLog n
        Right h -> getInputLine "<Log entry>: " >>= \i -> case i of
            Nothing -> return ()
            Just i' -> liftIO $ getZonedTime >>= hAppendLog h . flip Entry i'

hAppendLog :: Handle -> Entry -> IO ()
hAppendLog h e = hPrint h e >> hClose h

--Help using the logger

helpLog :: String -> IO ()
helpLog "edit" = putStrLn
    "Syntax: log edit <log name>\n\
    \Action: Opens the given log for editing via the terminal.\n\
    \        If the log file does not exist, it is created."
helpLog "help" = putStrLn
    "Syntax: log help <command>\n\
    \Action: Display specific help message for the given command."
helpLog "new" = putStrLn
    "Syntax: log new <name>\n\
    \Action: Creates a new log file with the given name."
helpLog "remove" = putStrLn
    "Syntax: log help <log name>\n\
    \Action: Removes any log file with the given name"
helpLog "view" = putStrLn
    "Syntax: log help <log name>\n\
    \Action: Displays any log file with the given name"
helpLog _ = putStrLn
    "Usage:\n\
    \log edit   <logs>    Edit the given log file\n\
    \log help   <command> If no command is given, display this message.\n\
    \                     Otherwise, display specific help message for a command.\n\
    \log new    <names>   Creates a log file with the given name\n\
    \log remove <logs>    Removes the given log file"

--Creating logs

newLog :: String -> IO ()
newLog n = openLog n ReadWriteMode >>= \h -> hPutStrLn h n >> hClose h

--Removing logs

removeLog :: String -> InputT IO ()
removeLog n = liftIO (logsFolder n) >>= ensureRemoveFile

ensureRemoveFile :: FilePath -> InputT IO ()
ensureRemoveFile f = do
    i <- getInputLine $ "Are you sure you want to remove \'" ++ f ++ "\'? Enter (Y)es or (N)o: "
    case i of
        Just "Yes" -> liftIO $ removeFile f
        Just "Y"   -> liftIO $ removeFile f
        _          -> return ()

--Viewing Logs

viewLog :: String -> IO ()
viewLog n = logsFolder n >>= flip openFile ReadMode >>= \h -> hGetContents h >>= putStrLn >> hClose h
