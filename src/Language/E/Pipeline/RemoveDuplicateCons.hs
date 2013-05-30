{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.RemoveDuplicateCons
    ( removeDuplicateCons
    , renameQuantifiedVarsTopLevel
    ) where

import Bug
import Language.E
import qualified Data.Text as T

removeDuplicateCons :: MonadConjure m => Spec -> m Spec
removeDuplicateCons (Spec v x) = do
    let statements  = filter (/= [xMake| topLevel.suchThat.value.literal := [Prim (B True)] |])
                    $ statementAsList x
    let statements' = nubKeepOrder $ map ( bubbleHasAtMostOneSuchThat
                                         . renameAuxVarsTopLevel
                                         . renameQuantifiedVarsTopLevel
                                         ) statements
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
                        let
                            (oldbase, _, _) = identifierSplit old
                            replacer p@(Prim (S candidate)) =
                                let
                                    (cbase, crepr, cregion) = identifierSplit candidate
                                in
                                    if cbase == oldbase
                                        then Prim $ S $ identifierConstruct nextName crepr cregion
                                        else p
                            replacer p = p

                            t' = transform replacer t
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
                    [ nm
                    | [xMatch| [Prim (S nm)] := topLevel.declaration.find.name.reference |] <- locals
                    , "aux__" `T.isPrefixOf` nm
                    ]
            in
                if null findNames
                    then [xMake| withLocals.actual := [go names actual]
                               | withLocals.locals := (map (go names) locals)
                               |]
                    else
                        let
                            newNames  = take (length findNames) names
                            restNames = drop (length findNames) names
                            lu        = [ (findNameBase, newbase)
                                        | (findName, newbase) <- zip findNames newNames
                                        , let (findNameBase, _, _) = identifierSplit findName
                                        ]

                            replacer p@(Prim (S candidate)) =
                                let
                                    (cbase, crepr, cregion) = identifierSplit candidate
                                in
                                    case lookup cbase lu of
                                        Just newbase -> Prim $ S $ identifierConstruct newbase crepr cregion
                                        Nothing      -> p
                            replacer p = p

                            actual'   = transform replacer actual
                            locals'   = map (transform replacer) locals
                        in
                            go restNames [xMake| withLocals.actual := [actual']
                                               | withLocals.locals := locals'
                                               |]
        go  names (Tagged t xs) = Tagged t (map (go names) xs)
        go _names t             = t

bubbleHasAtMostOneSuchThat :: E -> E
bubbleHasAtMostOneSuchThat = transform go
    where
        go [xMatch| [actual] := withLocals.actual
                  | locals   := withLocals.locals
                  |] =
            let
                fromSuchThat [xMatch| xs := topLevel.suchThat |] = Just xs
                fromSuchThat _ = Nothing
                isn'tSuchThat = not . isJust . fromSuchThat

                others = filter isn'tSuchThat locals
                suchThats = concat $ mapMaybe fromSuchThat locals
                singleSuchThat = [xMake| topLevel.suchThat := suchThats |]
            in
                [xMake| withLocals.actual := [actual]
                      | withLocals.locals := (others ++ [singleSuchThat])
                      |]
        go p = p

