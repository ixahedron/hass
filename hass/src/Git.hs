{-|
Module      : Git
Description : \-\- Git helper functions.
Stability   : experimental

Helper functions to be used with @hass@ git command.

Currently git repository is initialised in core directory and it follows that only the whole store can be tracked.
-}

module Git where

import System.Process (callProcess)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import UtilFunctions

-- |Return path to @.git@ directory in __hass__ storage directory.
gitDir :: IO String
gitDir = (</> ".git") <$> hassDir

-- |If @git init@ is run, add all password files to the repository in an initial commit.
gitInit :: [String] -> IO ()
gitInit = \args -> callGit args >> gitAdd "." >> gitCommit "Added current contents of password store."

-- |Add a file/directory's contents to the repository and tree.
gitAdd :: FilePath -> IO ()
gitAdd file = callGit $ "add" : file : []

-- |Remove a file/directory's contents from the repository and tree.
gitRm :: FilePath -> IO ()
gitRm file = callGit $ "rm" : file : []

-- |Make a commit to the password-storage repository.
gitCommit :: String -> IO ()
gitCommit message = callGit $ "commit" : "-m" : message : []

-- |Combines @git-add@ and @git-commit@ to simplify tracking new or edited password files.
gitTrackNewPassword :: FilePath -> String -> IO ()
gitTrackNewPassword f m = gitAdd f >> gitCommit m

-- |Is password storage directory a git repository?
isGitRepo :: IO Bool
isGitRepo = gitDir >>= doesDirectoryExist

-- |Call git process with passed arguments
callGit :: [String] -> IO ()
callGit = callProcess "git"
