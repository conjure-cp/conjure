{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Conjure.Language.NameGen
    ( NameGen
    , NameGenM
    , NameGenState
    , nextName
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
    exportNameGenState :: m [(NameKind, Int)]
    importNameGenState :: [(NameKind, Int)] -> m ()

instance NameGen m => NameGen (StateT st m) where
    nextName = lift . nextName
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance (NameGen m, Monoid w) => NameGen (WriterT w m) where
    nextName = lift . nextName
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance NameGen m => NameGen (ReaderT r m) where
    nextName = lift . nextName
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance NameGen m => NameGen (IdentityT m) where
    nextName = lift . nextName
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance NameGen m => NameGen (ExceptT m) where
    nextName = lift . nextName
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance NameGen m => NameGen (MaybeT m) where
    nextName = lift . nextName
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance NameGen m => NameGen (Pipes.Proxy a b c d m) where
    nextName = lift . nextName
    exportNameGenState = lift exportNameGenState
    importNameGenState = lift . importNameGenState

instance (Functor m, Monad m) => NameGen (NameGenM m) where
    nextName k = do
        mi <- gets (M.lookup k . fst)
        out <- case mi of
                Nothing -> do
                    modify $ \ (st, avoid) -> (M.insert k 2 st, avoid)
                    return $ MachineName k 1 []
                Just !i -> do
                    modify $ \ (st, avoid) -> (M.insert k (i+1) st, avoid)
                    return $ MachineName k i []
        avoid <- gets snd
        if out `S.member` avoid
            then nextName k
            else return out
    exportNameGenState = gets (M.toList . fst)
    importNameGenState st = modify $ \ (_, avoid) -> (M.fromList st, avoid)

instance NameGen (Either Doc) where
    nextName _ = fail "nextName{Either Doc}"
    exportNameGenState = fail "exportNameGenState{Either Doc}"
    importNameGenState _ = fail "importNameGenState{Either Doc}"

instance NameGen Identity where
    nextName _ = fail "nextName{Identity}"
    exportNameGenState = fail "exportNameGenState{Identity}"
    importNameGenState _ = fail "importNameGenState{Identity}"

runNameGen :: (Monad m, Data x) => x -> NameGenM m a -> m a
runNameGen avoid (NameGenM comp) = evalStateT comp initState
    where initState = (M.empty, S.fromList (universeBi avoid))
