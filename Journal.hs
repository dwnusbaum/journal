module Journal
    ( appendEntry
    , newJournal
    , removeJournal
    , viewJournal
    ) where

import Entry

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.LocalTime
import System.Console.Haskeline
import System.Directory
import System.IO
import System.IO.Error

-- Utility Functions

journalFolder :: String -> IO FilePath
journalFolder x = liftM (\h -> h ++ "/journals/" ++ x ++ ".txt") getHomeDirectory

openJournal :: String -> IOMode -> IO Handle
openJournal n m = journalFolder n >>= flip openFile m

-- Writing to journal files

appendEntry :: String -> InputT IO ()
appendEntry n = do
    eitherJournal <- liftIO $ tryIOError $ openJournal n AppendMode
    case eitherJournal of
        Left  e -> when (isDoesNotExistError e) $ liftIO (newJournal n) >> appendEntry n
        Right h -> getInputLine "> " >>= \i -> case i of
            Nothing -> return ()
            Just i' -> liftIO $ getZonedTime >>= hAppendJournal h . flip Entry i'

hAppendJournal :: Handle -> Entry -> IO ()
hAppendJournal h e = hPrint h e >> hClose h

-- Creating journals

newJournal :: String -> IO ()
newJournal n = openJournal n ReadWriteMode >>= \h -> hPutStrLn h n >> hClose h

-- Removing journals

removeJournal :: String -> InputT IO ()
removeJournal n = do
    f      <- liftIO $ journalFolder n
    exists <- liftIO $ doesFileExist f
    if exists
        then do
            i <- getInputLine $ "Are you sure you want to remove the journal " ++ show n ++ "? Enter (Y)es or (N)o: "
            case i of
                Just "Yes" -> liftIO $ removeFile f
                Just "Y"   -> liftIO $ removeFile f
                _          -> return ()
        else outputStrLn $ "Journal " ++ n ++ " does not exist"

-- Viewing Logs

viewJournal :: String -> IO ()
viewJournal n = do
    f <- journalFolder n
    exists <- doesFileExist f
    if exists
        then openFile f ReadMode >>= \h -> hGetContents h >>= putStrLn >> hClose h
        else putStrLn $ "Journal " ++ n ++ " does not exist"
