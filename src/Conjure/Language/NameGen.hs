{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Conjure.Language.NameGen
    ( NameGen
    , NameGenM
    , NameGenState
    , nextName
    , auxTableReset
    , auxTableLookup
    , auxTableInsert
    , exportNameGenState
    , importNameGenState
    , runNameGen
    ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Name

-- containers
import Data.Map.Strict as M
import Data.Set as S

-- pipes
import qualified Pipes


type NameGenState = ( M.Map NameKind Int        -- next int to use
                    , S.Set Name                -- set of names to avoid
                    , M.Map Int Name            -- the aux variables
                    )

type NameKind = Text

newtype NameGenM m a = NameGenM (StateT NameGenState m a)
    deriving ( Functor, Applicative, Monad
             , MonadFail, MonadUserError
             , MonadLog
             , MonadTrans
             , MonadState NameGenState
             , MonadIO
             )

class (Functor m, Applicative m, Monad m) => NameGen m where
    nextName :: NameKind -> m Name
    auxTableReset :: m ()
    auxTableLookup :: Int -> m (Maybe Name)
    auxTableInsert :: Int -> Name -> m ()
    exportNameGenState :: m ([(NameKind, Int)], [(Int, Name)])
    importNameGenState :: ([(NameKind, Int)], [(Int, Name)]) -> m ()

instance NameGen m => NameGen (StateT st m) where
    nextName = lift . nextName
    auxTableReset = lift auxTableReset
    auxTableLookup = lift . auxTableLookup
    auxTableInsert k v = lift (auxTableInsert k v)
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance (NameGen m, Monoid w) => NameGen (WriterT w m) where
    nextName = lift . nextName
    auxTableReset = lift auxTableReset
    auxTableLookup = lift . auxTableLookup
    auxTableInsert k v = lift (auxTableInsert k v)
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance NameGen m => NameGen (ReaderT r m) where
    nextName = lift . nextName
    auxTableReset = lift auxTableReset
    auxTableLookup = lift . auxTableLookup
    auxTableInsert k v = lift (auxTableInsert k v)
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance NameGen m => NameGen (IdentityT m) where
    nextName = lift . nextName
    auxTableReset = lift auxTableReset
    auxTableLookup = lift . auxTableLookup
    auxTableInsert k v = lift (auxTableInsert k v)
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance NameGen m => NameGen (ExceptT m) where
    nextName = lift . nextName
    auxTableReset = lift auxTableReset
    auxTableLookup = lift . auxTableLookup
    auxTableInsert k v = lift (auxTableInsert k v)
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance NameGen m => NameGen (MaybeT m) where
    nextName = lift . nextName
    auxTableReset = lift auxTableReset
    auxTableLookup = lift . auxTableLookup
    auxTableInsert k v = lift (auxTableInsert k v)
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance NameGen m => NameGen (Pipes.Proxy a b c d m) where
    nextName = lift . nextName
    auxTableReset = lift auxTableReset
    auxTableLookup = lift . auxTableLookup
    auxTableInsert k v = lift (auxTableInsert k v)
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance (Functor m, Monad m) => NameGen (NameGenM m) where
    nextName k = do
        mi <- gets (M.lookup k . fst3)
        out <- case mi of
                Nothing -> do
                    modify $ \ (st, avoid, auxs) -> (M.insert k 2 st, avoid, auxs)
                    return $ MachineName k 1 []
                Just !i -> do
                    modify $ \ (st, avoid, auxs) -> (M.insert k (i+1) st, avoid, auxs)
                    return $ MachineName k i []
        avoid <- gets snd3
        if out `S.member` avoid
            then nextName k
            else return out
    auxTableReset = modify $ \ (st, avoid, _) -> (st, avoid, M.empty)
    auxTableLookup k = gets (M.lookup k . thd3)
    auxTableInsert k v = modify $ \ (st, avoid, auxs) -> (st, avoid, M.insert k v auxs)
    exportNameGenState = gets $ \ (st, _, auxs) -> (M.toList st, M.toList auxs)
    importNameGenState (st, auxs) = modify $ \ (_, avoid, _) -> (M.fromList st, avoid, M.fromList auxs)

instance NameGen (Either Doc) where
    nextName _ = fail "nextName{Either Doc}"
    auxTableReset = fail "auxTableReset{Either Doc}"
    auxTableLookup _ = fail "auxTableLookup{Either Doc}"
    auxTableInsert _ _ = fail "auxTableInsert{Either Doc}"
    exportNameGenState = fail "exportNameGenState{Either Doc}"
    importNameGenState _ = fail "importNameGenState{Either Doc}"

instance NameGen Identity where
    nextName _ = fail "nextName{Identity}"
    auxTableReset = fail "auxTableReset{Identity}"
    auxTableLookup _ = fail "auxTableLookup{Identity}"
    auxTableInsert _ _ = fail "auxTableInsert{Identity}"
    exportNameGenState = fail "exportNameGenState{Identity}"
    importNameGenState _ = fail "importNameGenState{Identity}"

runNameGen :: (Monad m, Data x) => x -> NameGenM m a -> m a
runNameGen avoid (NameGenM comp) =
    let initState = (M.empty, S.fromList (universeBi avoid), M.empty)
    in  evalStateT comp initState
