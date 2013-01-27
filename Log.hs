{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Log
    ( editLog
    , helpLog
    , newLog
    , removeLog
    ) where

--import Control.Monad.IO.Class
import Data.Time.Calendar
import Data.Time.Clock
import System.Console.Haskeline
import System.Directory
import System.IO

logsFolder :: String -> IO FilePath
logsFolder x = getHomeDirectory >>= \h -> return $ h ++ "/Dropbox/logs/" ++ x

openLog :: String -> IOMode -> IO Handle
openLog name mode = logsFolder name >>= flip openFile mode

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
newLog = foldr (\x -> (>>) $ openLog x ReadWriteMode >>= \h -> creationEntry h >> hClose h) $ return ()

creationEntry :: Handle -> IO ()
creationEntry h = getCurrentTime >>= hPutStrLn h . (++) "Log created on " . show . toGregorian . utctDay

--Removing logs

removeLog :: [String] -> IO ()
removeLog = foldr (\x -> (>>) $ logsFolder x >>= \f -> removeFile f >> putStrLn ("Log " ++ x ++ " was removed")) $ return ()
