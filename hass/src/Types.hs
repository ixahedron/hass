{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

{-|
Module      : Types
Stability   : experimental
Portability : POSIX

/Types/ module contains all self-defined data types and instances, default representations for records, some type synonyms and lenses.
This makes this module necessary to be imported by almost all other __hass__ modules.
-}

module Types where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as B
import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Trans 
import qualified Control.Category as L
import           Data.Label

-- |Data type for parsing command line flags.
data Option = Reencrypt | Path | Clip | Echo | NoSymbols | InPlace | MultiLine | Force | Recursive | Help deriving (Eq,Ord,Show)

-- |Data type for ternary choices - action on directories in 'traverseStore' in this case.
data AoD = No | Before | After deriving (Eq, Ord, Show, Enum, Bounded)

-- |Used to check if 'aod' is 'Before'.
before Before = True
before      _ = False

-- |Used to check if 'aod' is 'After'.
after After = True
after     _ = False

-- |Record used for efficient passing of arguments to 'actionOnNonExistingPasses' and 'actionOnExistingPasses' functions.
data ActionReqs = ActionReqs {_entity     :: String      -- ^Which action to execute?
                            , _dest       :: FilePath    -- ^Destination file path needed by mv and cp
                            , _passlength :: Integer     -- ^Password length needed by actionGenerate
                            , _cr         :: CryptReqs   -- ^Attributes needed by encrypt-decrypt logic
                             }

-- |Default configuration of 'ActionReqs'.
defAR = ActionReqs "show" "" 0 defCR

-- |Record used for efficient passing of arguments to 'traverseStore' function.
-- If 'onCrypt' is specified, 'traverseStore' will skip the subfolders which have their own checkfiles.
data TraverseReqs = TraverseReqs {_aod     :: AoD                               -- ^Execute the action on directory entries?
                                , _aocf    :: Bool                              -- ^Execute the action on checkfile entries?
                                , _onCrypt :: Bool                              -- ^Does the action involve crypto shenanigans?
                                , _cdir    :: FilePath                          -- ^Current directory to pass to actions (will virtually change during 'traverseStore' run)
                                , _target  :: FilePath                          -- ^Target directory to pass to actions (will virtually change during 'traverseStore' run)
                                , _depth   :: Integer                           -- ^Depth of recursion
                                , _act     :: TraverseReqs -> FilePath -> IO () -- ^Action to be executed
                                , _string  :: String                            -- ^An additional string, can be used e.g. in grep or find actions
                                , _oldCR   :: CryptReqs                         -- ^Old master password
                                , _newCR   :: CryptReqs                         -- ^New master password
                                 }

-- |Default configuration of 'TraverseReqs'.
defTR = TraverseReqs No False False "" "" 0 undefined "" defCR defCR

-- |Record used for efficient passing of arguments to encrypt-decrypt functions.
data CryptReqs = CryptReqs {_mp :: BL.ByteString
                          , _iv :: B.ByteString
                           }

-- |Default implementation of 'CryptReqs'.
defCR = CryptReqs "" ""

-- |Type synonym for better readability.
type TraverseFunction = TraverseReqs -> FilePath -> IO ()


-- |A simple monad transformer.
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance (Functor m, Monad m) => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap
 
instance (Functor m, Monad m) => Alternative (MaybeT m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => Monad (MaybeT m) where
  fail _ = MaybeT $ return Nothing
  return = MaybeT . return . return
  x >>= f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

instance MonadTrans MaybeT where
  lift = \x ->  MaybeT (liftM Just x)

instance Monad m => MonadPlus (MaybeT m) where
  mzero     = MaybeT $ return Nothing
  mplus a b = MaybeT $ do mv <- runMaybeT a
                          case mv of
                               Nothing    -> runMaybeT b
                               Just _     -> return mv

-- * Labels for lenses
-- ** 'TraverseReqs', 'ActionReqs', 'CryptReqs'
mkLabels [''TraverseReqs, ''ActionReqs, ''CryptReqs]
