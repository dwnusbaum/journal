{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Main where

import Log

import Prelude hiding (log)
import System.Console.Haskeline
import System.Environment

data Option = Edit | Help | New | Delete

logger :: Option -> [String] -> InputT IO ()
logger Edit   = editLog
logger Help   = helpLog
logger New    = newLog
logger Delete = deleteLog

parseArgs :: [String] -> (Option, [String])
parseArgs ("--edit"   :xs) = (Edit, xs)
parseArgs ("-e"       :xs) = (Edit, xs)
parseArgs ("--help"   :xs) = (Help, xs)
parseArgs ("-h"       :xs) = (Help, xs)
parseArgs ("--new"    :xs) = (New, xs)
parseArgs ("-n"       :xs) = (New, xs)
parseArgs ("--delete" :xs) = (Delete, xs)
parseArgs ("-d"       :xs) = (Delete, xs)
parseArgs xs               = (Help, xs)

main :: IO ()
main = getArgs >>= runInputT defaultSettings . uncurry logger . parseArgs


