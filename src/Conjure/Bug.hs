{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Conjure.Bug
    ( bug
    , bugFail, bugFailT
    ) where

import Conjure.Prelude
import Conjure.RepositoryVersion ( repositoryVersion )
import Conjure.Language.Pretty



-- call this function instead of "error"
bug :: HasCallStack => Doc -> a
bug message = error $ unlines
    [ "This should never happen, sorry!"
    , ""
    , "However, it did happen, so it must be a bug. Please report it to us!"
    , ""
    , "Conjure is actively maintained, we will get back to you as soon as possible."
    , "You can help us by providing a minimal failing example."
    , ""
    , "Also include the repository version for this build: " ++ repositoryVersion
    , ""
    , "Issue tracker: http://github.com/conjure-cp/conjure/issues"
    , "", "" , renderNormal message
    ]

bugFail :: Doc -> Either Doc a -> a
bugFail loc (Left err) = bug (vcat ["BUGFAIL at" <+> loc, err])
bugFail _   (Right x) = x

bugFailT :: Monad m => Doc -> ExceptT m a -> m a
bugFailT loc comp = do
    res <- runExceptT comp
    case res of
        Left err -> bug (vcat ["BUGFAIL at" <+> loc, err])
        Right x  -> return x

instance MonadFailDoc IO where
    failDoc msg = bug (vcat ["IO Error", msg])
