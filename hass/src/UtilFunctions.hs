{-|
Module      : UtilFunctions
Description : \-\- Utility functions for all modules.
Stability   : experimental
Portability : POSIX

Utility functions to be used in other __hass__ modules.
-}

module UtilFunctions where

import           System.Directory (getHomeDirectory, getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing, doesFileExist, doesDirectoryExist, getDirectoryContents, Permissions(..), getPermissions, getTemporaryDirectory, makeAbsolute)
import           System.Exit (exitWith, ExitCode(..))
import           Control.Exception.Base (finally, bracket, bracket_)
import           Control.Monad (when, unless)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LC
import           System.FilePath ((</>))
import           System.IO (stdin, stderr, hGetEcho, hSetEcho, hPutStrLn, hClose, openTempFile, Handle)
import           System.Hclip (setClipboard)
import           Data.List ((\\))
import           Data.List.Utils
import           Data.Label
import           Types
import           Options
import           Daemon


-- |Output an error message to /stderr/ stream and exit with exit code 1.
-- Should be used only at user-interaction level (such as prompting for master password),
-- not instead of exceptions.
die :: String -> IO ()
die err = hPutStrLn stderr err >> exitWith (ExitFailure 1)

-- |The equivalent of 'getLine' for 'ByteString's.
getByteLine :: IO ByteString
getByteLine = LC.pack <$> getLine

-- |Sets clipboard, starting a daemon to wait 45 seconds and then erase the clipboard.
toClipboard :: ByteString -> IO ()
toClipboard str = setClipboardAndDaemonize $ LC.unpack str

-- |The __hass__ directory containing password storages.
hassDir :: IO FilePath
hassDir = (</> ".hass") <$> getHomeDirectory

-- |Exports path to concrete password file or directory.
passPath :: String -> IO FilePath
passPath service = (</> service) <$> hassDir

-- |Extract only the part of the path relative to __hass__ password storage directory.
-- Does not resolve special entries like @..@.
makeRelativeToHass :: FilePath -> IO FilePath
makeRelativeToHass path = tail <$> ((\\) <$> makeAbsolute path <*> hassDir)

-- |Checks if a sneaky path was entered, such as containing @..@.
checkSneakyPaths :: FilePath -> Bool
checkSneakyPaths path = ".." `elem` split "/" path ?
  error "You've attempted to pass a sneaky path to hass. Go home." $ True

-- |List directory entries, skipping dot files. To be used in listing action.
listDirectory :: FilePath -> IO [FilePath]
listDirectory dir = (filter notADotFile) <$> getDirectoryContents dir
  where notADotFile file = head file /= '.'

-- |List all directory entries except @.@, @..@ and @.check@.
-- To be used during removal to correctly check if the directory is empty of needed files.
listRemovingDirectory :: FilePath -> IO [FilePath]
listRemovingDirectory dir = (\\ [".", "..", ".check"]) <$> getDirectoryContents dir

-- |Creates path to storage directory, creating missing parents directories on demand.
createStorageDir :: String -> IO ()
createStorageDir substorage = passPath substorage >>= createDirectoryIfMissing True

-- |Creates directories in a path to a file, extracting everything prior to the last slash.
createPath :: FilePath -> IO ()
createPath path = createDirectoryIfMissing True $ extractDir path
  where
    extractDir :: FilePath -> FilePath
    extractDir = join "/" . init . split "/"

-- |Traverse a directory, recursively executing the passed action on each entry.
-- Executes the action on each directory entry, too, if anything other that 'No' is passed as 'aod' field in 'TraverseReqs'.
traverseStore :: TraverseReqs -> FilePath -> IO ()
traverseStore treqs dir = listDirectory dir >>= mapM_ traverse_aux
  where
    trAct = get act treqs
    traverse_aux entry = ifM (doesDirectoryExist $ dir </> entry) traverseUnlessCFFound $ trAct treqs entry
      where traverseUnlessCFFound = when (before $ get aod treqs) (trAct treqs $ entry) >>
              (doesFileExist $ dir </> ".check") >>= \cf -> 
               when (cf && get aocf treqs) (trAct treqs ".check") >> (doesFileExist $ dir </> entry </> ".check") >>= \rcf ->
               unless (get onCrypt treqs && rcf) (traverseStore ((set cdir (dir </> entry)) .
               (modify target (</> entry)) . (modify depth (+1)) $ treqs) $ dir </> entry) >>
               when (after $ get aod treqs) (trAct treqs entry)

-- |Prompt for confirmation in the event of trying to overwriting a target.
prompt :: FilePath -> IO Bool
prompt dst = ifM (inArgs Force) (return True) $ makeRelativeToHass dst >>= \rel ->
  (putStrLn $ "Target " ++ rel ++ " exists already. Are you sure you want to overwrite it? [y/N]") >>
  getLine >>= \a -> return (parseConfirmationRequest False a)

-- |Parse confirmation request. Takes a default value and a string to be parsed.
-- \"y\", \"yes\", \"Y\" are recognised as confirmative answers,
-- \"n\", \"no\", \"N\" as negative, empty line and unrecognised options will be defaulted.
parseConfirmationRequest :: Bool -> String -> Bool
parseConfirmationRequest   _   "y" = True
parseConfirmationRequest   _ "yes" = True
parseConfirmationRequest   _   "Y" = True
parseConfirmationRequest   _   "n" = False
parseConfirmationRequest   _  "no" = False
parseConfirmationRequest   _   "N" = False
parseConfirmationRequest def    [] = def
parseConfirmationRequest def     _ = def

{- *
   Actions with temporary effects
-}

-- |Set echoing /stdin/, execute an action and then restore the echo setting to the old value.
-- Restores the old value in case of an exception during action's execution.
withEcho :: Bool -> IO a -> IO a
withEcho echo action = hGetEcho stdin >>= \old -> bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

-- |Execute an action using temporary file.
-- Tries writing to /\/dev\/shm/ in order to avoid writing to difficult-to-erase disk sectors.
-- Falls back to default @$TMPDIR@ or /\/tmp/ if that variable isn't set and asks for confirmation.
withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile action = do
  e <- doesDirectoryExist "/dev/shm"
  access <- e ? (writable <$> getPermissions "/dev/shm") $ return False 
  tmpdir <- (access ? return "/dev/shm" $
              putStrLn "Warning: /dev/shm/ not available, falling back to default temporary directory. Proceed? [Y/n]" >>
              getLine >>= \a -> parseConfirmationRequest True a ? getTemporaryDirectory $ error "Exiting without editing.")
  (tmpfile, h) <- openTempFile tmpdir ""
  finally (action tmpfile h) (hClose h)

-- | Change the working directory and restore it afterwards, even if the given action fails due to an exception.
withDirectory :: FilePath -> IO a -> IO a
withDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory dir
    action

{- *
   If-functions
-}

-- |If-function to avoid using the built-in @if-then-else@ construct.
infixr 1 ?
(?) :: Bool -> a -> a -> a
(?)  True x _ = x
(?) False _ y = y

-- |If-function with boolean argument passed last. To be used in monad bindings.
ifOn :: a -> a -> Bool -> a
ifOn = flip . flip (?)

-- |If-function operating on monadic values.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb a b = mb >>= ifOn a b

-- |'when'-function operating on monadic values.
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb a = mb >>= \b -> when b a

-- |The same as 'maybe', but with \"on Just\" option passed last as to increase readability.
infixr 1 ?..
(?..) :: Maybe a -> b -> (a -> b) -> b
(?..) = flip $ flip . flip (flip maybe)
