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

import Data.Time.Calendar
import Data.Time.Clock
import System.Console.Haskeline
import System.Directory
import System.IO

--Data Type

data Log = Log String Day [Entry]

instance Show Log where
    show (Log n d xs) = n ++ "\n" ++ show (length xs) ++ " entries. Created on: " ++ showLn d ++ concatMap showLn xs

data Entry = Entry Int Day String

instance Show Entry where
    show (Entry n d s) = show n ++ ". " ++ showLn d ++ s

showLn :: (Show a) => a -> String
showLn s = show s ++ "\n"

--Utility Functions

logsFolder :: String -> IO FilePath
logsFolder x = getHomeDirectory >>= \h -> return $ h ++ "/Dropbox/logs/" ++ x ++ ".log"

printLog :: Maybe Log -> IO ()
printLog Nothing  = return ()
printLog (Just x) = print x

openLog :: String -> IOMode -> IO Handle
openLog name mode = logsFolder name >>= flip openFile mode

readLog :: String -> IOMode -> IO (Maybe Log)
readLog name mode = openLog name mode >>= hGetContents >>= parse name

--Writing to log files

editLog :: [String] -> InputT IO ()
editLog = undefined

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
creationEntry name h = getCurrentTime >>= hPutStrLn h . (++) (name ++ "\n0. Created on ") . showGregorian . utctDay

--Removing logs

removeLog :: [String] -> IO ()
removeLog = foldr (\x -> (>>) $ logsFolder x >>= \f -> ensureRemoveFile f) $ return ()

ensureRemoveFile :: FilePath -> IO ()
ensureRemoveFile = removeFile

--Viewing Logs

viewLog :: [String] -> IO ()
viewLog = foldr (\x -> (>>) $ readLog x ReadMode >>= printLog) $ return ()
