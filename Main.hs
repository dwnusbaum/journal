{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Main where

import Logger

import System.Console.Haskeline
import System.Environment

logger :: [String] -> IO ()
logger ["edit"  ,x] = runInputT defaultSettings $ editLog x
logger ["-e"    ,x] = runInputT defaultSettings $ editLog x
logger ["help"  ,x] = helpLog   x
logger ["-h"    ,x] = helpLog   x
logger ["new"   ,x] = newLog    x
logger ["-n"    ,x] = newLog    x
logger ["remove",x] = runInputT defaultSettings $ removeLog x
logger ["-r"    ,x] = runInputT defaultSettings $ removeLog x
logger ["view"  ,x] = viewLog   x
logger ["-v"    ,x] = viewLog   x
logger _            = helpLog   ""

main :: IO ()
main = getArgs >>= logger
