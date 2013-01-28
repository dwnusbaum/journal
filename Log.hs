{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Log
    ( Log(..)
    , Entry(..)
    , makeEntry
    ) where

import Data.Time.Calendar
import Data.Time.Format
import System.Locale

data Log = Log String Day [Entry]

data Entry = Entry Day String

instance Show Log where
    show (Log n d xs) = n ++ " log" ++ "\n" ++ "Created on: " ++ showDay d ++ "\n" ++ unwords (map showLn xs)

instance Show Entry where
    show (Entry d s) = showDay d ++ "\n|> " ++ s

showLn :: (Show a) => a -> String
showLn s = show s ++ "\n"

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%a %d %Y - %I:%M%p"

makeEntry :: [String] -> Day -> Entry
makeEntry e d = Entry d $ unwords e


