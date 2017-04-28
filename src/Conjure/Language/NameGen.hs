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

-- pipes
import qualified Pipes


type NameGenState = M.Map NameKind Int        -- next int to use

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
        mi <- gets (M.lookup k)
        case mi of
            Nothing -> do
                modify $ M.insert k 2
                return $ MachineName k 1 []
            Just !i -> do
                modify $ M.insert k (i+1)
                return $ MachineName k i []
    exportNameGenState = gets M.toList
    importNameGenState = modify . const . M.fromList

instance NameGen (Either Doc) where
    nextName _ = fail "nextName{Either Doc}"
    exportNameGenState = fail "exportNameGenState{Either Doc}"
    importNameGenState _ = fail "importNameGenState{Either Doc}"

instance NameGen Identity where
    nextName _ = fail "nextName{Identity}"
    exportNameGenState = fail "exportNameGenState{Identity}"
    importNameGenState _ = fail "importNameGenState{Identity}"

runNameGen :: Monad m => NameGenM m a -> m a
runNameGen (NameGenM comp) = evalStateT comp initState
    where initState = M.empty
