{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.RemoveDuplicateCons
    ( removeDuplicateCons
    , renameQuantifiedVarsTopLevel
    ) where

import Language.E
import qualified Data.Text as T

removeDuplicateCons :: MonadConjure m => Spec -> m Spec
removeDuplicateCons (Spec v x) = do
    let statements  = statementAsList x
    let statements' = nubKeepOrder $ map renameQuantifiedVarsTopLevel statements
    return $ Spec v $ listAsStatement statements'

newQs :: [Text]
newQs = [ "w__" `mappend` T.pack i'
        | i :: Int <- [0..]
        , let i' = show i
        ]

w2v :: E -> E
w2v = transform f
    where
        f p@(Prim (S old)) = case T.stripPrefix "w__" old of
            Nothing -> p
            Just x  -> Prim $ S $ "v__" `mappend` x
        f p = p

renameQuantifiedVarsTopLevel :: E -> E
renameQuantifiedVarsTopLevel = w2v . renameQuantifiedVars newQs

renameQuantifiedVars :: [Text] -> E -> E
renameQuantifiedVars = go
    where
        go  names t@[xMatch| [Prim (S old)] := quantified.quanVar.structural.single.reference |]
            | "v__" `T.isPrefixOf` old =
                let
                    (nextName:restNames) = names
                    t' = replace (Prim (S old)) (Prim (S nextName)) t
                in
                    go restNames t'
        go  names (Tagged t xs) = Tagged t (map (go names) xs)
        go _names t             = t

