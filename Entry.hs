{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Entry
    ( Entry(..)
    ) where

import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

data Entry = Entry ZonedTime String

instance Show Entry where
    show (Entry d s) = showDay d ++ "\n|> " ++ s

showDay :: ZonedTime -> String
showDay = formatTime defaultTimeLocale "%a, %b %d, %Y - %I:%M %p"
