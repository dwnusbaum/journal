{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Main where

import Logger

import Control.Monad.Trans.Class
import System.Console.Haskeline
import System.Environment

logger :: [String] -> InputT IO ()
logger ("edit"  :xs) = editLog xs
logger ("e"     :xs) = editLog xs
logger ("help"  :xs) = helpLog xs
logger ("h"     :xs) = helpLog xs
logger ("new"   :xs) = lift $ newLog xs
logger ("n"     :xs) = lift $ newLog xs
logger ("remove":xs) = lift $ removeLog xs
logger ("r"     :xs) = lift $ removeLog xs
logger ("view"  :xs) = lift $ viewLog xs
logger ("v"     :xs) = lift $ viewLog xs
logger xs            = helpLog xs

main :: IO ()
main = getArgs >>= runInputT defaultSettings . logger


