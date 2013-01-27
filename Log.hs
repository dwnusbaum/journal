{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Log
    ( editLog
    , helpLog
    , newLog
    , deleteLog
    ) where

import Prelude hiding (log)
import System.Console.Haskeline

editLog :: [String] -> InputT IO ()
editLog = undefined

helpLog :: [String] -> InputT IO ()
helpLog _ = outputStrLn "Use: log (--edit OR --new OR --delete)"

newLog :: [String] -> InputT IO ()
newLog = undefined

deleteLog :: [String] -> InputT IO ()
deleteLog = undefined
