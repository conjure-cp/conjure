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
    let statements' = nubKeepOrder $ map (renameAuxVarsTopLevel . renameQuantifiedVarsTopLevel) statements
    return $ Spec v $ listAsStatement statements'

newQs :: [Text]
newQs = [ "w__" `mappend` T.pack i'
        | i :: Int <- [0..]
        , let i' = show i
        ]

renameQuantifiedVarsTopLevel :: E -> E
renameQuantifiedVarsTopLevel = w2v . renameQuantifiedVars newQs
    where
        w2v :: E -> E
        w2v = transform f
            where
                f p@(Prim (S old)) = case T.stripPrefix "w__" old of
                    Nothing -> p
                    Just x  -> Prim $ S $ "v__" `mappend` x
                f p = p

renameQuantifiedVars :: [Text] -> E -> E
renameQuantifiedVars = go
    where
        go  names t@[xMatch| [Prim (S old)] := quantified.quanVar.structural.single.reference |]
            | not ("w__" `T.isPrefixOf` old) =
                case names of
                    (nextName:restNames) ->
                        let t' = replace (Prim (S old)) (Prim (S nextName)) t
                        in  go restNames t'
                    [] -> bug $ vcat [ "well done, you just ran out of fresh names."
                                     , "and there were an infinite number of them!"
                                     ]
        go  names (Tagged t xs) = Tagged t (map (go names) xs)
        go _names t             = t

renameAuxVarsTopLevel :: E -> E
renameAuxVarsTopLevel = w2aux . renameAuxVars newQs
    where
        w2aux :: E -> E
        w2aux = transform f
            where
                f p@(Prim (S old)) = case T.stripPrefix "w__" old of
                    Nothing -> p
                    Just x  -> Prim $ S $ "aux__" `mappend` x
                f p = p

renameAuxVars :: [Text] -> E -> E
renameAuxVars = go
    where
        go  names [xMatch| [actual] := withLocals.actual
                         | locals   := withLocals.locals
                         |] =
            let
                findNames =
                    [ nm | [xMatch| [Prim (S nm)] := topLevel.declaration.find.name.reference |] <- locals
                    , "aux__" `T.isPrefixOf` nm
                    ]
            in
                trace (show $ vcat $ "renameAuxVars" : map pretty findNames) $
                if null findNames
                    then [xMake| withLocals.actual := [go names actual]
                               | withLocals.locals := (map (go names) locals)
                               |]
                    else
                        let
                            newNames  = trace "foo 1" $ take (length findNames) names
                            restNames = trace "foo 2" $ drop (length findNames) names
                            replacer  = trace "foo 3" $ replaceAll $ zip (map (Prim . S) findNames)
                                                         (map (Prim . S) newNames)
                            actual'   = trace "foo 4" $ replacer actual
                            locals'   = trace "foo 5" $ map replacer locals
                        in
                            go restNames [xMake| withLocals.actual := [actual']
                                               | withLocals.locals := locals'
                                               |]
        go  names (Tagged t xs) = Tagged t (map (go names) xs)
        go _names t             = t

