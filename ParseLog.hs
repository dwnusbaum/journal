{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module ParseLog
    ( parse
    ) where

import Log

import Control.Monad
import Data.Time.Calendar
import Text.Parsec hiding (parse)

type Parser = Parsec String ()

parse :: String -> IO (Maybe Log)
parse input = validate $ runParser parseLog () "log" input

validate :: Either ParseError Log -> IO (Maybe Log)
validate (Left  err) = print err >> return Nothing
validate (Right val) = return $ Just val

parseLog :: Parser Log
parseLog = do
    name    <- many1 alphaNum
    spaces >> many1 digit
    day     <- string ". Created on " >> parseDay
    entries <- spaces >> parseEntry `sepBy` char '\n'
    return $ Log name day entries

parseEntry :: Parser Entry
parseEntry = do
    num <- liftM read $ many1 digit
    day <- string ". " >> parseDay
    entries <- many anyChar
    return $ Entry num day entries

parseDay :: Parser Day
parseDay = do
    year  <- liftM read $ many1 digit
    char '-'
    month <- liftM read $ many1 digit
    char '-'
    day   <- liftM read $ many1 digit
    return $ fromGregorian year month day
