module Conjure.UserError ( MonadUserError(..) ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Pretty

-- base
import System.Exit ( exitFailure )
import System.IO as X ( stderr, hPutStrLn )

-- pipes
import qualified Pipes


class Monad m => MonadUserError m where
    userErr :: [Doc] -> m a
    userErr1 :: Doc -> m a

instance MonadUserError (Either Doc) where
    userErr msgs = do
        let msgsOut = case msgs of
                []    -> bug "userErr []"
                [msg] -> [ "Error:" <++> msg ]
                _     -> [ "Error" <+> pretty (i :: Int) <> ":" <++> msg
                         | (i, msg) <- zip [1..] msgs
                         ]
        Left $ vcat
            $ "Conjure is exiting due to user errors."
            : msgsOut
    userErr1 = userErr . return

instance MonadUserError IO where
    userErr msgs =
        case userErr msgs of
            Left doc -> hPutStrLn stderr (renderNormal (doc :: Doc)) >> exitFailure
            Right x  -> return x
    userErr1 = userErr . return

instance MonadUserError m => MonadUserError (IdentityT m) where
    userErr = lift . userErr
    userErr1 = lift . userErr1

instance MonadUserError m => MonadUserError (MaybeT m) where
    userErr = lift . userErr
    userErr1 = lift . userErr1

instance MonadUserError m => MonadUserError (ExceptT m) where
    userErr = lift . userErr
    userErr1 = lift . userErr1

instance MonadUserError m => MonadUserError (StateT st m) where
    userErr = lift . userErr
    userErr1 = lift . userErr1

instance (MonadUserError m, Monoid w) => MonadUserError (WriterT w m) where
    userErr = lift . userErr
    userErr1 = lift . userErr1

instance MonadUserError m => MonadUserError (ReaderT r m) where
    userErr = lift . userErr
    userErr1 = lift . userErr1

instance MonadUserError m => MonadUserError (LoggerT m) where
    userErr = lift . userErr
    userErr1 = lift . userErr1

instance MonadUserError m => MonadUserError (Pipes.Proxy a b c d m) where
    userErr = lift . userErr
    userErr1 = lift . userErr1
