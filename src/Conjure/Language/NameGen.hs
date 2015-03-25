{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Conjure.Language.NameGen where

-- conjure
import Conjure.Prelude
import Conjure.Language.Name

-- containers
import Data.Map as M

-- pipes
import qualified Pipes


type NameGenState = M.Map NameKind Int        -- next int to use

type NameKind = Text

newtype NameGenM m a = NameGenM (StateT NameGenState m a)
    deriving ( Functor, Applicative, Monad
             , MonadFail
             , MonadLog
             , MonadTrans
             , MonadState NameGenState
             )

class Monad m => NameGen m where
    nextName :: NameKind -> m Name
    updateNameGenState :: [(NameKind, Int)] -> m ()

instance NameGen m => NameGen (StateT st m) where
    nextName = lift . nextName
    updateNameGenState = lift . updateNameGenState

instance (NameGen m, Monoid w) => NameGen (WriterT w m) where
    nextName = lift . nextName
    updateNameGenState = lift . updateNameGenState

instance NameGen m => NameGen (Pipes.Proxy a b c d m) where
    nextName = lift . nextName
    updateNameGenState = lift . updateNameGenState

instance Monad m => NameGen (NameGenM m) where
    nextName k = do
        mi <- gets (M.lookup k)
        case mi of
            Nothing -> do
                modify $ M.insert k 2
                return $ MachineName k 1 []
            Just !i -> do
                modify $ M.insert k (i+1)
                return $ MachineName k i []
    updateNameGenState = modify . const . M.fromList

instance NameGen (Either Doc) where
    nextName _ = fail "nextName{Either Doc}"
    updateNameGenState _ = fail "updateNameGenState{Either Doc}"

runNameGen :: Monad m => NameGenM m a -> m a
runNameGen (NameGenM comp) = evalStateT comp initState
    where initState = M.empty
