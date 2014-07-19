module Bug
    ( bug
    , headNote
    , userErr
    ) where

import RepositoryVersion ( repositoryVersion )

import Stuff.Pretty ( Doc, renderNormal )

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

headNote :: Doc -> [a] -> a
headNote msg [] = bug msg
headNote _   (x:_) = x

-- call this function instead of "error"
-- in case of a user error.
userErr :: Doc -> a
userErr = error . renderNormal

