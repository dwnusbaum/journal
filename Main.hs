{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Main where

import Logger

import System.Console.Haskeline
import System.Environment

logger :: [String] -> IO ()
logger ("edit"  :xs) = runInputT defaultSettings $ editLog xs
logger ("e"     :xs) = runInputT defaultSettings $ editLog xs
logger ("help"  :xs) = helpLog xs
logger ("h"     :xs) = helpLog xs
logger ("new"   :xs) = newLog xs
logger ("n"     :xs) = newLog xs
logger ("remove":xs) = removeLog xs
logger ("r"     :xs) = removeLog xs
logger ("view"  :xs) = viewLog xs
logger ("v"     :xs) = viewLog xs
logger xs            = helpLog xs

main :: IO ()
main = getArgs >>= logger
