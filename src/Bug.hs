{-# LANGUAGE CPP #-}

module Bug
    ( bug
    , headNote
    , userErr
    ) where

import RepositoryVersion ( repositoryVersion )

import Stuff.Pretty ( Doc, renderNormal )

-- call this function instead of "error"
-- the String argument is only printed if compiled with --trace-logs
bug :: Doc -> a
bug _message = error $ unlines
    [ "This should never happen, sorry!"
    , ""
    , "Please report a bug."
    , "Conjure is actively maintained, we will get back to you as soon as possible."
    , "You can help us by providing a minimal failing example."
    , "Also include repository version for this build: " ++ repositoryVersion
    , ""
    , "Issue tracker: http://bitbucket.org/stacs_cp/conjure-public/issues"
    , "", "" , renderNormal _message
    ]

headNote :: Doc -> [a] -> a
headNote msg [] = bug msg
headNote _   (x:_) = x

-- call this function instead of "error"
-- in case of a user error.
-- parsing, type checking errors are of this kind.
userErr :: Doc -> a
userErr = error . renderNormal

