{-# LANGUAGE OverloadedStrings #-}

module Language.Core.MultiMatch ( multiMatch, matchSuccess, matchFail ) where

import Language.Core.Imports
import Language.Core.Definition ( CompT )


-- given a description, and a list of poccibly failing computations, 
-- returns the first computation which doesn't fail.
-- if all fails, throws an error with the given description.
multiMatch :: Monad m => Doc -> [MaybeT (CompT m) a] -> CompT m a
multiMatch name as = do
    bs <- runMaybeT (msum as)
    case bs of
        Nothing -> throwErrorSingle $ "Non exhaustive patterns in " <+> name
        Just b  -> return b

-- use this to indicate a code path where pattern matching succeeds.
matchSuccess :: Monad m => CompT m a -> MaybeT (CompT m) a
matchSuccess = lift

-- use this to indicate a code path where pattern matching fails.
matchFail :: Monad m => MaybeT (CompT m) a
matchFail = mzero
