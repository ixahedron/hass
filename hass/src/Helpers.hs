{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

{-|
Module      : Helpers
Description : \-\- Helper functions to use in Main.
Stability   : experimental
Portability : POSIX

Helper functions to be used in the main module.
-}
module Helpers ( runPwgen
                 -- * Traverse functions
                 -- ** Functions to be used with 'traverseStore'
               , reencryptEntry
               , copyEntry
               , removeEntry
               , printEntryName
               , printWith
               , grepInPass
                 -- * Main actions on passwords
               , actionInsert
               , actionGenerate
               , actionEdit
               , actionShow
               , actionRemove
               , actionCopy
               , actionGit
               ) where

import           System.Process (readProcess, readProcessWithExitCode, callProcess)
import           System.IO (openTempFile, hPutStrLn, hClose, openBinaryFile, IOMode (..))
import           System.Directory 
import           System.FilePath ((</>))
import           System.Environment (lookupEnv)
import           Codec.Crypto.SimpleAES (newIV)
import           Control.Monad (when, unless)
import           Control.Exception (bracket)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.List.Utils (join, split)
import           Data.List (isInfixOf)
import           Data.Label
import           System.Hclip (setClipboard)
import           UtilFunctions
import           Types
import           Options
import           Crypto
import           Git


-- |Call pwgen and read the resulting generated password.
-- Correctly passes @--no-symbols@ flag.
runPwgen :: Integer -> Bool -> IO BL.ByteString
runPwgen lng alphanum = LC.pack . init <$> readProcess "pwgen" args ""
  where
      args = (alphanum ? "-s" $ "-sy") : (pure . show) lng


-- ******** TraverseFunctions ********

-- |Reencrypt entry.
-- To be used with 'traverseStore' function when __hass__ is invoked with @init --reencrypt@.
reencryptEntry :: TraverseFunction
reencryptEntry tr entry = withEntry (get cdir tr </> entry) reencrypt
  where withEntry :: FilePath -> (TraverseReqs -> BL.ByteString -> BL.ByteString) -> IO ()
        withEntry entry action = do
            ec <- LC.fromStrict <$> C.readFile entry
            let res = action tr ec
             in C.writeFile entry $ LC.toStrict res
            whenM isGitRepo $ gitAdd entry

-- |Copy file of directory to the location specified in 'TraverseReqs'.
-- To be used with 'traverseStore' function when __hass__ is invoked with @cp@ or @mv@.
copyEntry :: TraverseFunction
copyEntry tr entry = ifM ((||) <$> targetDirExists <*> targetFileExists) (prompt targetPath >>=
  \a -> when a (copy' entry)) $ createDirectoryIfMissing True (get target tr) >> copy' entry
  where isDir = doesDirectoryExist entryPath
        targetDirExists  = doesDirectoryExist targetPath
        targetFileExists = doesFileExist targetPath
        entryPath = get cdir tr </> entry
        targetPath = get target tr </> entry

        copy' entry = ifM isDir (ifM targetDirExists (removeDirectoryRecursive targetPath)
          (whenM targetFileExists $ removeFile targetPath) >> createDirectory targetPath) $ copyFile entryPath targetPath

-- |Remove file or directory.
-- To be used with 'traverseStore' function when __hass__ is invoked with @rm@.
removeEntry :: TraverseFunction
removeEntry tr entry = ifM isFile (removeFile entryPath) $ whenM emptyDir (removeDirectoryRecursive entryPath)
  where isFile = doesFileExist entryPath
        emptyDir = null <$> listRemovingDirectory entryPath
        entryPath = get cdir tr </> entry

-- |Pretty print entry name.
-- To be used with 'traverseStore' function when __hass__ is invoked with @list@.
printEntryName :: TraverseFunction
printEntryName tr entry = putStrLn $ spaces ++ entry
  where spaces = replicate n ' '
        n = fromIntegral $ get depth tr

-- |Apply a filter passed as string within TraverseReqs to the entry name before printing it.
-- To be used with 'traverseStore' function when __hass__ is invoked with @find@.
printWith :: TraverseFunction
printWith tr entry = when (get string tr `isInfixOf` entry) $ putStrLn =<< makeRelativeToHass ((get cdir tr) </> entry)

-- |Decrypt a password, grep inside it for search string, and display the line containing matched string along with filename.
-- To be used with 'traverseStore' function when __hass__ is invoked with @grep@.
grepInPass :: TraverseFunction
grepInPass tr entry = do
  ec <- BL.readFile (get cdir tr </> entry)
  (ec, so, se) <- let plain = decrypt (get oldCR tr) ec
                   in readProcessWithExitCode "grep" ("--color=always" : "-T" : (get string tr) : []) (LC.unpack plain)
  unless (null so) $ putStrLn $ entry ++ ": " ++ so

{- ******
   Main actions on passwords
-}
-- |Generates a password of given length using 'runPwgen'.
-- The function then writes the encrypted result to the corresponding file
-- (possibly only replacing the first line if @--in-place@ was specified)
-- and either outputs it to stdout stream, or copies it to the clipboard if @--clip@ was passed.
actionGenerate :: FilePath -> Integer -> CryptReqs -> IO ()
actionGenerate file lng cr = createPath file >>
                             (inArgs NoSymbols >>= runPwgen lng) >>= \generatedPass ->
                             ifM (inArgs InPlace) 
                               (C.readFile file >>= \contents -> (LC.writeFile file . replaceInEntry generatedPass $ contents))
                                (LC.writeFile file $ encrypt cr generatedPass) >>
                             ifM (inArgs Clip) (toClipboard generatedPass >> putStrLn "Password copied to clipboard. Will clear in 45 seconds.") 
                                               (LC.putStrLn . LC.append "Generated password: " $ generatedPass) >>
                                                whenM isGitRepo (gitTrackNewPassword file ("Generated password for " ++ file))
    where replaceFirstLine new = LC.unlines . (:) new . tail . LC.lines
          replaceInEntry   new = encrypt cr . replaceFirstLine new . decrypt cr . LC.fromStrict

-- |Asks for a password to be inserted, asking to reconfirm
-- unless @--echo@ OR @--multiline@ has been passed, and writes it to the corresponding file. 
-- If __hass__ was invoked with @--multiline@ flag, waits for @Ctrl+D@ to stop reading the input.
actionInsert :: FilePath -> CryptReqs ->  IO ()
actionInsert file cr = do
  m <- inArgs MultiLine
  putStrLn $ (m ? "Input will be read until you press Ctrl+D. " $ "")
               ++ "Enter the password to be inserted:"
  echo <- inArgs Echo
  try1 <- getPassword m
  unless (m || echo)  (putStrLn "\nConfirm your password:" >> getPassword m >>= \try2 ->
                      (try1 /= try2 ? die "\nEntered passwords do not match, try again." $ putChar '\n'))
  createPath file
  LC.writeFile file . encrypt cr . LC.fromStrict $ try1
  putStrLn $ "\nSuccessfully inserted password."
  whenM isGitRepo $ gitTrackNewPassword file ("Inserted password for " ++ file)
    where 
      getPassword True = withEcho True $ LC.toStrict <$> LC.getContents
      getPassword    _ = inArgs Echo >>= (flip withEcho) (C.getLine)

-- |Calls the editor of choice of the system, looking up the @$EDITOR@ environment variable.
-- Falls back to @vi@ as it should be pre-installed on every Linux machine.
-- Calls @shred@ on the temporary file when the editing is done.
actionEdit :: FilePath -> CryptReqs -> IO ()
actionEdit file cr = withTempFile $ \tmpfile h -> do
  decrypt cr <$> LC.readFile file >>= LC.hPutStrLn h >> hClose h
  mEditor <- lookupEnv "EDITOR"
  callProcess ( maybe "vi" id mEditor) $ {-"-n" : "-c set nobackup" :-} return tmpfile
  encrypt cr <$> LC.readFile tmpfile >>= LC.writeFile file
  callProcess "shred" $ "-f" : "-u" : "-z" : return tmpfile
  whenM isGitRepo $ gitTrackNewPassword file ("Edited password for " ++ file)

-- |Either outputs the decrypted password to /stdout/ stream,
-- or copies it to the clipboard if @--clip@ was passed.
actionShow :: FilePath -> CryptReqs -> IO ()
actionShow file cr = inArgs Clip >>= \cl ->
  decrypt cr <$> LC.readFile file >>= \decryptedPass ->
    not cl ? LC.putStrLn decryptedPass $ toClipboard (fstLine decryptedPass) >> putStrLn "Password copied to clipboard. Will clear in 45 seconds." 
  where fstLine p = head $ LC.lines p

-- |Removes the password file/directory after asking for confirmation defaulting to negative unless @--force@ was specified.
-- Does not remove non-empty directories unless invoked with @--recursive@ flag.
-- Directory is considered empty if it contains nothing but possibly a @.check@ file.
actionRemove :: FilePath -> IO ()
actionRemove file = inArgs Force >>= \f -> f ? confirmedRm $ do
  putStrLn "Are you sure you want to delete this password? [Y/n]"
  yn <- getLine
  parseConfirmationRequest True yn ? confirmedRm $ putStrLn "Exiting without deleting password."
    where 
      confirmedRm = do
        dirCheck <- doesDirectoryExist file
        not dirCheck ? removeFile file $ do
          ifM (inArgs Recursive) (removeDirectory file) $
            listRemovingDirectory file >>= \contents ->
              if null contents then removeDirectory file
                               else die "Directory not empty and --recursive flag not specified."
        putStrLn $ "Successfully deleted password" ++ (dirCheck ? " directory." $ ".")
        whenM isGitRepo $ gitRm file >> gitCommit ("Removed password for " ++ file)

      removeDirectory file = traverseStore treqs file

      treqs = set act removeEntry . set cdir file . set aod After . set onCrypt True $ defTR

        
-- |Copies the password file/directory to the specified folder.
-- Confirmation for overwriting existing targets defaults to negative and occurs unless @--force@ was specified.
-- After copy execution selectively reencrypts copied files after asking for the most suitable master password.
actionCopy :: FilePath -> FilePath -> Bool -> CryptReqs -> IO ()
actionCopy src dest move cr = do
  selfsustainable <- doesFileExist cfsrc
  absDest <- passPath dest
  ifM (doesFileExist src) (copy' src absDest) $ do
    traverseStore (set target (absDest </> name src) $ treqs) src
    when move $ removeDirectoryRecursive src
    putStrLn $ (move ? "Moved" $ "Copied") ++ " all requested files. Selectively reencrypting them now."
    unless selfsustainable $ reencryptMoved (absDest </> name src) cr
  where name = last . split "/"
        cfsrc = src </> ".check"
        treqs = (set act copyEntry) . (set aod Before) . (set cdir src) . (set aocf True) $ defTR

        copy' src dst = do
          ntF   <- not <$> doesFileExist dst
          ntDir <- not <$> doesDirectoryExist dst
          if | ntF && ntDir -> createPath dst >> copyFile src dst >> when move (removeFile src)
             |          ntF -> prompt dst >>= \a -> when a (removeDirectoryRecursive dst >> copyFile src dst >> when move (removeFile src))
             |    otherwise -> prompt dst >>= \a -> when a (copyFile src dst >> when move (removeFile src))

-- A helper for actionCopy to make the code less obstruct.
reencryptMoved :: FilePath -> CryptReqs -> IO ()
reencryptMoved absDest cr = do
    cfe <- runMaybeT (checkFileExists absDest)
    cfe ?.. (BL.writeFile (absDest </> ".check") $ encrypt cr dummyValues ) $ \cfdestdir -> do
      putStrLn $ "Enter your master password for " ++ cfdestdir ++ " folder."
      mpdest <- withEcho False getByteLine
      c <- checkMasterPass cfdestdir mpdest
      if c then newIV >>= \genIV -> traverseStore (set newCR (CryptReqs mpdest genIV) $ treqs) absDest
           else die "Incorrect master password."
  where treqs = set act reencryptEntry . set onCrypt True . set cdir absDest . set oldCR cr $ defTR

-- |Calls git with arguments specified in the command line upon invokation.
actionGit :: [String] -> IO ()
actionGit = \args -> hassDir >>= \hassdir ->
  withDirectory hassdir (args == ["init"] ? (gitInit args) $ callGit args)

