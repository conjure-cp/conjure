{-# LANGUAGE OverloadedStrings #-}

module Language.Core.MultiMatch ( multiMatch, matchSuccess, matchFail ) where

import Language.Core.Imports
import Language.Core.Definition ( CompT )

matchSuccess :: Monad m => CompT m a -> MaybeT (CompT m) a
matchSuccess = lift

matchFail :: Monad m => MaybeT (CompT m) a
matchFail = mzero

multiMatch :: Monad m => Doc -> [MaybeT (CompT m) a] -> CompT m a
multiMatch name as = do
    bs <- runMaybeT (msum as)
    case bs of
        Nothing -> throwErrorSingle $ "Non exhaustive patterns in " <+> name
        Just b  -> return b
