{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.CheckIfAllRefined where

import Language.E

-- see https://bitbucket.org/ozgurakgun/conjure/issue/6

checkIfAllRefined :: MonadConjure m => Spec -> m Spec
checkIfAllRefined spec@(Spec _ statements) = do
    let taggedIdentifiers = nub [ nm
                                | [xMatch| [Prim (S nm)] := reference |] <- universe statements
                                , '#' `elem` nm
                                ]
    void $ recordSpec spec
    let msg = "Some identifiers are not refined:" <+> prettyList id "," taggedIdentifiers
    if null taggedIdentifiers
        then return spec
        else err ErrFatal msg

