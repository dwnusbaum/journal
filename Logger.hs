{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Logger
    ( editLog
    , helpLog
    , newLog
    , removeLog
    , viewLog
    ) where

import Log
import ParseLog

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Calendar
import Data.Time.Clock
import System.Console.Haskeline
import System.Directory
import System.IO

--Utility Functions

logsFolder :: String -> IO FilePath
logsFolder x = getHomeDirectory >>= \h -> return $ h ++ "/Dropbox/logs/" ++ x ++ ".log"

today :: IO Day
today = liftM utctDay getCurrentTime

printLog :: Maybe Log -> IO ()
printLog Nothing  = return ()
printLog (Just x) = print x

openLog :: String -> IOMode -> IO Handle
openLog name mode = logsFolder name >>= flip openFile mode

readLog :: String -> IOMode -> IO (Maybe Log)
readLog name mode = openLog name mode >>= hGetContents >>= parse name

appendLog :: String -> Entry -> IO ()
appendLog name e = undefined

--Writing to log files

editLog :: [String] -> InputT IO ()
editLog []     = undefined
editLog [x]    = undefined
editLog (x:xs) = liftIO (readLog x AppendMode) >>= \l -> case l of
    Nothing -> return ()
    Just (Log name day es) -> liftIO (today >>= \d -> void $ return $ Log name day $ es ++ [Entry ((length es) + 1) d (concat xs)])

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
creationEntry name h = today >>= hPutStrLn h . (++) (name ++ "\n0. Created on ") . showGregorian

--Removing logs

removeLog :: [String] -> IO ()
removeLog = foldr (\x -> (>>) $ logsFolder x >>= \f -> ensureRemoveFile f) $ return ()

ensureRemoveFile :: FilePath -> IO ()
ensureRemoveFile = removeFile

--Viewing Logs

viewLog :: [String] -> IO ()
viewLog = foldr (\x -> (>>) $ readLog x ReadMode >>= printLog) $ return ()
