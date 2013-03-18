{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.CheckIfAllRefined
    ( checkIfAllRefined
    , removeRefinedDecls
    ) where

import Language.E

import qualified Data.Text as T


-- see https://bitbucket.org/ozgurakgun/conjure/issue/6

checkIfAllRefined :: MonadConjure m => Spec -> m Spec
checkIfAllRefined spec@(Spec _ statements) = do
    let taggedIdentifiers = nub [ nm
                                | [xMatch| [Prim (S nm)] := reference |]
                                    <- universe statements
                                , '#' `elem` T.unpack nm
                                ]
    let msg = "Some identifiers are not refined:" <+> prettyList id "," taggedIdentifiers
    if null taggedIdentifiers
        then return spec
        else err ErrFatal msg


removeRefinedDecls :: MonadConjure m => Spec -> m Spec
removeRefinedDecls (Spec s x) = do
    reprLog <- gets representationLog
    modify $ \ st -> st { representationLog = [] }
    let refinedDecls = sortNub $ map (\ (_,_,i,_) -> i ) reprLog
    let xs = statementAsList x
    let ys = xs \\ refinedDecls
    let y  = listAsStatement ys
    mapM_ (mkLog "removeRefinedDecl" . pretty) refinedDecls
    return $ Spec s y

