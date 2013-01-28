{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Log
    ( Log(..)
    , Entry(..)
    , makeEntry
    ) where

import Data.Time.Calendar

data Log = Log String Day [Entry]

data Entry = Entry Day String

instance Show Log where
    show (Log n d xs) = n ++ "\n" ++ "Created on: " ++ showLn d ++ unwords (map showLn xs)

instance Show Entry where
    show (Entry d s) = "|> " ++ showLn d ++ s

showLn :: (Show a) => a -> String
showLn s = show s ++ "\n"

makeEntry :: [String] -> Day -> Entry
makeEntry e d = Entry d $ unwords e
