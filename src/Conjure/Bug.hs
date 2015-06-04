module Conjure.Bug
    ( bug
    , bugFail, bugFailT
    ) where

import Conjure.Prelude
import Conjure.RepositoryVersion ( repositoryVersion )
import Conjure.Language.Pretty


-- call this function instead of "error"
bug :: Doc -> a
bug message = error $ unlines
    [ "This should never happen, sorry!"
    , ""
    , "Please report a bug."
    , "Conjure is actively maintained, we will get back to you as soon as possible."
    , "You can help us by providing a minimal failing example."
    , "Also include repository version for this build: " ++ repositoryVersion
    , ""
    , "Issue tracker: http://bitbucket.org/stacs_cp/conjure-public/issues"
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
