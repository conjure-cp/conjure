{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.CheckIfAllRefined where

import Language.E

-- see https://bitbucket.org/ozgurakgun/conjure/issue/6

checkIfAllRefined :: Monad m => Spec -> CompE m Spec
checkIfAllRefined spec@(Spec _ statements) = do
    let taggedIdentifiers = nub [ nm
                                | statement <- statements
                                , [xMatch| [Prim (S nm)] := reference |] <- universe statement
                                , '#' `elem` nm
                                ]
    void $ recordSpec spec
    let msg = "Some identifiers are not refined:" <+> prettyList id "," taggedIdentifiers
    if null taggedIdentifiers
        then return spec
        else err ErrFatal msg

