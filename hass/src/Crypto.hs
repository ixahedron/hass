{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

{-|
Module      : Crypto
Description : \-\- The logic of a symmetric system used in __hass__.

This symmetric cryptosystem works as follows:

    1. AES-256 is used as a specification of choice.
    2. Initialisation vector (IV) is generated randomly for each encryption.
    3. AES key is made by taking a SHA-256 hash of the provided master password.
    4. A checkfile is created upon initialising a password storage directory.
       This checkfile contains 16 bytes of pre-defined noise (dummy values)
       which are encrypted using our AES key and a newly generated IV.
    5. Checking if the master password is correct consists of

           - decrypting the checkfile with master password's AES key
           - comparing the result with dummy values.

       The check is passed if the comparison yield equalness of both values.
-}

module Crypto (
               -- * Working with checkfiles
               checkMasterPass
             , checkFileExists
             , createStorage
             , createCheckFile
             , onEmptyMP
               -- * Encryption functions
             , reencrypt
             , encrypt
             , decrypt
               --
             , dummyValues
              ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.List.Utils (split, join)
import           Data.List (tails)
import           Data.Label
import           Control.Monad (mzero)
import           Control.Monad.Trans (lift)
import           Codec.Crypto.SimpleAES (Direction(..), Mode(CBC), IV, newIV, encryptMsg', decryptMsg)
import           Crypto.Hash.SHA256 (hash)
import           System.FilePath ((</>))
import           System.Directory (doesFileExist)
import           Types
import           UtilFunctions

-- |Checks if the master password entered by user is correct.
checkMasterPass :: FilePath -> BL.ByteString -> IO Bool
checkMasterPass path mP = do
   path <- (</> ".check") <$> passPath path
   contents <- BL.readFile path
   let plain = decrypt (set mp mP defCR) contents
    in return $ plain == dummyValues

-- |Tries to find the \"nearest\" checkfile by sequentially going up a directory
-- until __hass__ core directory is reached.
--
-- Returns a wrapped 'Nothing' if no checkfile was found,
-- or a wrapped 'Just' 'FilePath' in case of a success.
checkFileExists :: FilePath -> MaybeT IO FilePath
checkFileExists path = checkFileExists_aux possibleCheckFileDirs
  where cf dir = (</> ".check") <$> passPath dir
        possibleCheckFileDirs = map (join "/" . reverse) $ (tails . reverse . split "/" $ path)

        checkFileExists_aux                  [] = mzero
        checkFileExists_aux (possibledir : pds) = do
          e <- lift $ doesFileExist =<< cf possibledir
          e ? return possibledir $ checkFileExists_aux pds

-- | Create a checkfile in the specified directory using provided master password.
createCheckFile :: FilePath -> BL.ByteString -> IO () 
createCheckFile path mpass = do
  dir <- passPath path
  if | BL.null mpass  -> onEmptyMP >>= actionOnEmptyMP dir
     | otherwise      -> newIV >>= \genIV -> BL.writeFile (dir </> ".check") $ encrypt (CryptReqs mpass genIV) dummyValues

-- |Check if the directory is already initialised as a storage and, if not, create a checkfile.
createStorage :: FilePath -> IO ()
createStorage path = (runMaybeT $ checkFileExists path) >>= \mv ->
                     case mv of Just _ -> die "Master password is already set. Use init --reencrypt to set a new one."
                                Nothing -> do
                                  createStorageDir path
                                  putStrLn "Enter the master password which will be used for this store."
                                  mp <- withEcho False getByteLine
                                  createCheckFile path mp
                                  putStrLn $ "Password store initialised" ++ (null path ? "." $ " for " ++ path ++ ".")

-- |Reencrypt a 'ByteString' by decrypting it with old and encrypting with new data.
reencrypt :: TraverseReqs -> BL.ByteString -> BL.ByteString
reencrypt tr = enc . dec
  where enc = encrypt $ get newCR tr
        dec = decrypt $ get oldCR tr

-- |Encrypt a 'ByteString'.
encrypt :: CryptReqs -> BL.ByteString -> BL.ByteString
encrypt cr plain = encryptMsg' CBC key' iv' plain
  where iv' = get iv cr
        key' = aesKey $ get mp cr

-- |Decrypt a 'ByteString'.
decrypt :: CryptReqs -> BL.ByteString -> BL.ByteString
decrypt cr coded = decryptMsg  CBC key' coded
  where key' = aesKey $ get mp cr

-- Noise used to create checkfiles and check master passwords. See module decription for details.
dummyValues :: BL.ByteString
dummyValues = "\xd7~\xc2(\xbfs\x82\x1ft{@\xb7\xd88\xf2\t\xd7~\xc2(\xbfs\x82\x1ft{@\xb7\xd88\xf2\t"

-- |Take SHA-256 hash of the provided 'ByteString'.
aesKey :: BL.ByteString -> B.ByteString
aesKey mp = hash $ BL.toStrict mp

-- |Prompt for confirmation on entering empty master password and parse ('parseConfirmationRequest') it. Defaulting to negative.
onEmptyMP :: IO Bool
onEmptyMP = putStrLn "Empty master password. Are you sure you want to proceed? [y/N]" >> getLine >>= \a -> return (parseConfirmationRequest False a)

actionOnEmptyMP :: FilePath -> Bool -> IO ()
actionOnEmptyMP path False = putStrLn "Enter a new master password." >> getByteLine >>= createCheckFile path
actionOnEmptyMP path  True = putStrLn "Empty master password. Proceeding with weak encryption." >> newIV >>= \genIV -> BL.writeFile (path </> ".check") $ (encrypt $ CryptReqs "" genIV) dummyValues
