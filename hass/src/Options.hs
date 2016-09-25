{-|
Module      : Options
Description : \-\- Parse command line arguments.
Stability   : experimental

This module contains functionality to parse command line arguments with which __hass__ was invoked.
All options that can't be needed by the first command are stripped.
-}

module Options ( commands
               , args
               , inArgs
               , putUsage
               , usage          
               , commandUsage          
               ) where

import           Data.List
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import           Control.Monad
import           System.Environment
import           System.Console.GetOpt
import           System.Exit
import           System.IO
import           Types (Option(..))


options = [ Option ['R'] ["reencrypt"]       (NoArg Reencrypt)
              "init: prompts for another master password and, if a storage for user exists, reencrypts it."
          , Option ['c'] ["clip"]            (NoArg Clip)
             "show, generate: don't print pass to screen, copy it to the clipboard." -- ++ " Will clear in 45 seconds."
          , Option ['e'] ["echo"]            (NoArg Echo)
              "insert: do NOT disable keyboard echo when the password is entered and do NOT confirm the password by asking for it twice."
          , Option ['n'] ["no-symbols"]      (NoArg NoSymbols)
              "generate: do not use any non-alphanumeric characters in the generated password."
          , Option ['i'] ["in-place"]        (NoArg InPlace)
              "generate: do not interactively prompt and only replace the first string with a generated password."
          , Option ['m'] ["multiline"]       (NoArg MultiLine)
              "insert: lines will be read until EOF or Ctrl+D is reached."
          , Option ['f'] ["force"]           (NoArg Force)
              "insert, generate, rm: do not interactively prompt before removal/overwriting."
          , Option ['r'] ["recursive"]       (NoArg Recursive)
              "rm: delete pass-name recursively if it is a directory."
          , Option ['h'] ["help"]            (NoArg Help)
              "display help and exit immediately -- present for user-friendliness reasons."
          ]

-- |Flags available for each command. Command string is mapped to list of options available for that command.
commandOptions = M.fromList [("init", Reencrypt : Path : []),
                             ("ls", []),
                             ("grep", []),
                             ("find", []),
                             ("insert", MultiLine : Echo : Force : []),
                             ("generate", Clip : NoSymbols : InPlace : Force : []),
                             ("edit", []),
                             ("show", Clip : []),
                             ("rm", Recursive : Force : []),
                             ("mv", Force : []),
                             ("cp", Force : []),
                             ("git", [])
                            ]

-- |Parse command line arguments into a list of flags and list of commands. Exits after printing usage in case of an error.
parse :: [String] -> IO ([Option], [String])
parse argv = case getOpt' Permute options argv of
 
        (opts, nonopts, unrecopts, []) ->
            if Help `elem` opts
              then putUsage >> exitWith ExitSuccess
              else do
                let commands = if null nonopts then ["list", ""] else nonopts
                return (head commands `relevant` nub opts, (reduce . head) commands : tail commands)
 
        (_,_,_,errs) -> do
            hPutStrLn stderr (concat errs ++ usageInfo usage options)
            exitWith (ExitFailure 1)

-- |A string describing commands available and pattern for invoking __hass__.
usage :: String
usage = "Usage: ./Main [command [-Recnamifr] ...]\nCommands available: init, list/ls, show, grep, find, insert, edit, generate, git, rm/remove/delete, mv/rename, cp/copy"

-- |Returns a string showing usage pattern and available flags for a particular command.
commandUsage :: String -> String
commandUsage c = case c of

        "init"     -> "init [ --reencrypt, -r ] [path/to/subfolder]"
        "ls"       -> "ls [path/to/subfolder]"
        "grep"     -> "grep [path/to/subfolder] searchstring"
        "find"     -> "find passnames"
        "insert"   -> "insert [ --echo, -e | --multiline, -m ] [ --force, -f ] path/to/file"
        "generate" -> "generate [ --no-symbols, -a ] [ --clip, -c ] [ --in-place, -i | --force, -f ] path/to/file [pass-length]"
        "edit"     -> "edit path/to/file"
        "show"     -> "show [--clip, -c] path/to/file"
        "rm"       -> "rm [--force, -f] [--recursive, -r] path/to/file"
        "mv"       -> "mv [--force, -f] path/to/src path/to/dest"
        "cp"       -> "cp [--force, -f] path/to/src path/to/dest"
        "git"      -> "git git-command-args"
        _else      -> usage

-- |Print usage to /stderr/ stream.
putUsage :: IO ()
putUsage = hPutStrLn stderr $ usageInfo usage options

-- |Extract commands from command line.
commands :: IO [String]
commands = snd <$> (getArgs >>= parse)

-- |Extract flags from command line.
args :: IO [Option]
args = fst <$> (getArgs >>= parse)

-- |Checks if a flag was passed to __hass__ via command line.
inArgs :: Option -> IO Bool
inArgs opt = (opt `elem`) <$> args

-- |Extract only the passed flags that are relevant to the first passed command. Falls back to no flags in case of an unrecognised command.
relevant :: String -> [Option] -> [Option]
relevant command opts = intersect opts $ maybe [] id (reduce command `M.lookup` commandOptions)

-- |Introduce some command synonyms to simplify command processing.
reduce :: String -> String
reduce c = case c of

        "list"   -> "ls" 
        "add"    -> "insert"
        "search" -> "find"
        "remove" -> "rm"
        "delete" -> "rm"
        "rename" -> "mv"
        "move"   -> "mv"
        "copy"   -> "cp"
        _else    -> c    
