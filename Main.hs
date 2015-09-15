module Main where

import Journal

import System.Console.Haskeline
import System.Environment

journal :: String -> String -> IO ()
journal "add"    j = runInputT defaultSettings $ appendEntry j
journal "a"      j = runInputT defaultSettings $ appendEntry j
journal "help"   j = helpJournal j
journal "h"      j = helpJournal j
journal "new"    j = newJournal j
journal "n"      j = newJournal j
journal "remove" j = runInputT defaultSettings $ removeJournal j
journal "r"      j = runInputT defaultSettings $ removeJournal j
journal "view"   j = viewJournal j
journal "v"      j = viewJournal j
journal _        _ = helpJournal ""

--Help using the tool

helpJournal :: String -> IO ()
helpJournal "add" = putStrLn
    "Syntax: journal add <journal name>\n\
    \Action: Add an entry to the given journal.\n\
    \        If the journal does not exist, it is created."
helpJournal "help" = putStrLn
    "Syntax: journal help <command>\n\
    \Action: Display specific help message for the given command."
helpJournal "new" = putStrLn
    "Syntax: journal new <name>\n\
    \Action: Creates a new journal file with the given name."
helpJournal "remove" = putStrLn
    "Syntax: journal help <journal name>\n\
    \Action: Removes any journal file with the given name"
helpJournal "view" = putStrLn
    "Syntax: journal help <journal name>\n\
    \Action: Displays any journal file with the given name"
helpJournal _ = putStrLn
    "Usage:\n\
    \journal add    <journal> Add an entry to the given log file.\n\
    \journal help   <command> If no command is given, display this message.\n\
    \                         Otherwise, display specific help message for a command.\n\
    \journal new    <name>    Creates a log file with the given name.\n\
    \journal remove <journal> Removes the given log file.\n\
    \journal view   <journal> View the given log file."

main :: IO ()
main = do
    as <- getArgs
    case length as of
        0 -> helpJournal ""
        1 -> helpJournal ""
        2 -> journal (head as) (head $ tail as)
        _ -> helpJournal ""
