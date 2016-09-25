{-# LANGUAGE ViewPatterns, MultiWayIf #-}

module Main where

import           System.Environment (getArgs)
import           System.Exit ()
import           System.Directory (doesFileExist, doesDirectoryExist)
import           System.FilePath ((</>))
import           System.Hclip (setClipboard)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC
import           Codec.Crypto.SimpleAES (newIV)
import           Control.Monad (when, unless)
import           Data.Label
import           Helpers
import           Crypto
import           Types
import           Options
import           UtilFunctions


main :: IO ()
main = commands >>= processCLArgs
    where processCLArgs :: [String] -> IO ()
          processCLArgs ("help":_)                      = putUsage
          processCLArgs ("ls":[])                       = processCLArgs ["ls", ""]
          processCLArgs ("ls":user:_)                   = assertStoreExists $ when (checkSneakyPaths      user) $ listStorage user
          processCLArgs ("grep":user:searchstring:_)    = assertStoreExists $ when (checkSneakyPaths      user) $ grepStorage user searchstring
          processCLArgs ("grep":searchstring:_)         = assertStoreExists $                                     grepStorage   "" searchstring
          processCLArgs ("find":user:searchstring:_)    = assertStoreExists $ when (checkSneakyPaths      user) $ findPassNames user searchstring
          processCLArgs ("find":searchstring:_)         = assertStoreExists $                                     findPassNames "" searchstring
          processCLArgs ("init":subfolder:_)            =                     when (checkSneakyPaths subfolder) $ initStorage subfolder
          processCLArgs ("init":[])                     =                                                         initStorage   ""
          processCLArgs ("insert":service:_)            = assertStoreExists $ when (checkSneakyPaths   service) $ insertPass service
          processCLArgs ("edit":service:_)              = assertStoreExists $ when (checkSneakyPaths   service) $ editPass service
          processCLArgs ("generate":service:length:_)   = assertStoreExists $ when (checkSneakyPaths   service) $ generatePass service $ readLength length
          processCLArgs ("generate":service:[])         = assertStoreExists $ when (checkSneakyPaths   service) $ generatePass service $ defPassLength
          processCLArgs ("show":service:_)              = assertStoreExists $ when (checkSneakyPaths   service) $ retrievePass service
          processCLArgs ("rm":service:_)                = assertStoreExists $ when (checkSneakyPaths   service) $ removePass service
          processCLArgs ("rm":[])                       = assertStoreExists $                                     removePass ""
          processCLArgs ("mv":service:dest:_)           = assertStoreExists $ when (checkSneakyPaths   service) $ movePass service dest
          processCLArgs ("cp":service:dest:_)           = assertStoreExists $ when (checkSneakyPaths   service) $ copyPass service dest
          processCLArgs ("git":_:_)                     = assertStoreExists $ tail <$> getArgs >>= actionGit
          processCLArgs (other:_)                       = putStrLn "Error in arguments." >> die (commandUsage other)

          readLength (reads -> [(n,"")]) = n
          readLength                   _ = defPassLength

          defPassLength = 20

          assertStoreExists action = ifM (hassDir >>= doesDirectoryExist) action $ die "Password store not yet initialised. Consider running init."


initStorage :: FilePath -> IO ()
initStorage subfolder = ifM (inArgs Reencrypt) (reencryptStorage subfolder) $ createStorage subfolder

reencryptStorage :: FilePath -> IO ()
reencryptStorage dir = runMaybeT (checkFileExists dir) >>= \cfe ->
                       cfe ?.. (createStorage dir) $
                               (\cfdir -> do
                                  putStrLn "Input your current master password:"
                                  omp <- withEcho False getByteLine
                                  e <- checkMasterPass cfdir omp
                                  not e ? die "Incorrect master password." $ do
                                      nmp <- getMasterPassword
                                      oiv <- newIV
                                      niv <- newIV
                                      dirpath <- passPath dir
                                      let treqs = (set act reencryptEntry . set onCrypt True . set cdir dirpath . set oldCR (CryptReqs omp oiv) . set newCR (CryptReqs nmp niv)) defTR
                                        in traverseStore treqs dirpath
                                      putStrLn $ "Reencrypted the password storage at " ++ dir ++ "."
                                      BL.writeFile (dirpath </> ".check") $ encrypt (CryptReqs nmp niv) dummyValues )
    where getMasterPassword = putStrLn "Input your new master password:" >> withEcho False getByteLine >>= \nmp ->
            BL.null nmp ? (ifM (not <$> onEmptyMP) getMasterPassword $
                          (putStrLn "Empty master password. Proceeding with weak encryption." >> return nmp)) $ return nmp

listStorage :: FilePath -> IO ()
listStorage subfolder = passPath subfolder >>= traverseStore (set act printEntryName . set aod Before $ defTR)

grepStorage :: FilePath -> String -> IO ()
grepStorage dir searchstring = runMaybeT (checkFileExists dir) >>= \cfe ->
                               cfe ?.. (die "Directory not initialised as a password storage.") $
                                       (\cfdir -> do
                                          putStrLn "Enter your master password:"
                                          mpass <- withEcho False getByteLine
                                          e <- checkMasterPass cfdir mpass
                                          not e ? putStrLn "Incorrect master password." $ do
                                            genIV <- newIV
                                            passPath dir >>= \absDir -> traverseStore (set cdir absDir . set oldCR (CryptReqs mpass genIV) $ treqs) absDir )
                                              where treqs = set act grepInPass . set onCrypt True . set string searchstring $ defTR

findPassNames :: FilePath -> String -> IO ()
findPassNames dir pattern = passPath dir >>= traverseStore (set act printWith . set cdir dir . set string pattern . set aod Before $ defTR)

insertPass :: FilePath -> IO ()
insertPass service = actionOnNonExistingPasses service $ set entity "insert" defAR

generatePass :: FilePath -> Integer -> IO ()
generatePass service length = actionOnNonExistingPasses service (set entity "generate" . set passlength length $ defAR)
                
editPass :: FilePath -> IO ()
editPass service = actionOnExistingPasses service $ set entity "edit" defAR

removePass :: FilePath -> IO ()
removePass service = actionOnExistingPasses service $ set entity "rm" defAR

movePass :: FilePath -> FilePath -> IO ()
movePass service destination = actionOnExistingPasses service (set entity "mv" . set dest destination $ defAR)

copyPass :: FilePath -> FilePath -> IO ()
copyPass service destination = actionOnExistingPasses service (set entity "cp" . set dest destination $ defAR)

retrievePass :: FilePath -> IO ()
retrievePass service = actionOnExistingPasses service $ set entity "show" defAR

actionOnNonExistingPasses :: FilePath -> ActionReqs -> IO ()
actionOnNonExistingPasses service actreqs = do
  file <- passPath service
  d <- doesDirectoryExist file
  when d $ die "A directory with this name exists already. Consider renaming this password or putting it in the found folder."
  e <- doesFileExist file
  e ? runForce file actreqs $ runMPCheck file actreqs

actionOnExistingPasses :: FilePath -> ActionReqs -> IO ()
actionOnExistingPasses service actreqs = do
  file <- passPath service
  d <- doesDirectoryExist file
  e <- doesFileExist file
  let ent = get entity actreqs

  if | e -> runMPCheck file actreqs
     | ent == "rm" || ent == "cp" -> if d then runMPCheck file actreqs else die "No file exists with this pass-name."
     | otherwise -> die $ "No password to " ++ ent ++ " at this location."

runMPCheck :: FilePath -> ActionReqs -> IO ()
runMPCheck file actreqs = runMaybeT (checkFileExists file) >>= \cfe ->
                          cfe ?.. (die "This directory is not a (subfolder of a) password storage. Consider runnung init.") $ \cfdir -> do    
            putStrLn "Enter your master password:"
            mpass <- withEcho False getByteLine
            c <- checkMasterPass cfdir mpass
            c ? (newIV >>= \genIV -> action file $ set cr (CryptReqs mpass genIV) actreqs) $ die "Incorrect master password." 

runForce :: FilePath -> ActionReqs -> IO ()
runForce file actreqs = ifM ((||) <$> inArgs Force <*> inArgs InPlace) (runMPCheck file actreqs) $ do
  putStrLn "Overwrite existing password? [y/N]"
  yn <- getLine
  parseConfirmationRequest False yn ? runMPCheck file actreqs $ putStrLn "Exiting without overwriting password."

action :: FilePath -> ActionReqs -> IO ()
action file areqs = case entity_ of

    "generate" -> actionGenerate file lng cr_
    "insert"   -> actionInsert file cr_
    "edit"     -> actionEdit file cr_
    "show"     -> actionShow file cr_
    "rm"       -> actionRemove file 
    "cp"       -> actionCopy file dest_ False cr_
    "mv"       -> actionCopy file dest_  True cr_
    _          -> die "Impossible case."
      
  where entity_ = get entity areqs
        lng = get passlength areqs
        cr_ = get cr areqs
        dest_ = get dest areqs
