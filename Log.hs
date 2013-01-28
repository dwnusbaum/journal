{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Log
    ( Log(..)
    , Entry(..)
    ) where

import Data.Time.Calendar

data Log = Log String Day [Entry]

instance Show Log where
    show (Log n d xs) = n ++ "\n" ++ show (length xs) ++ " entries. Created on: " ++ showLn d ++ concatMap showLn xs

data Entry = Entry Int Day String

instance Show Entry where
    show (Entry n d s) = show n ++ ". " ++ showLn d ++ s

showLn :: (Show a) => a -> String
showLn s = show s ++ "\n"
