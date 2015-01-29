module Conjure.Bug
    ( bug
    , bugFail
    , userErr
    ) where

import Conjure.Prelude
import Conjure.RepositoryVersion ( repositoryVersion )

import Conjure.Language.Pretty ( renderNormal )

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

bugFail :: Either Doc a -> a
bugFail (Left err) = bug ("BUGFAIL:" <+> err)
bugFail (Right x) = x

-- call this function instead of "error"
-- in case of a user error.
-- TODO: use fail
userErr :: Doc -> a
userErr = error . renderNormal

