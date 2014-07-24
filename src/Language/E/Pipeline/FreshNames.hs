{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.FreshNames ( freshNames ) where

import Conjure.Prelude
import Language.E.Definition
import Language.E.Pretty
import Language.E.CompE

import qualified Data.Text as T
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M


-- if a rewrite rule introduces new names, those names need to be unique.
freshNames :: MonadConjure m => E -> m E
freshNames param = do
    let
        quanVars :: [E]
        quanVars = [ s | [xMatch| [s] := quantified.quanVar |] <- universe param ]

        collectSingles :: [E] -> [Text]
        collectSingles i = [ r | [xMatch| [Prim (S r)] := structural.single.reference |] <- i ]

        collectTuples :: [E] -> [E]
        collectTuples  i = [ r | [xMatch| rs := structural.tuple |] <- i , r <- rs ]

        newvarsInQuanVar :: S.HashSet Text
        newvarsInQuanVar = S.fromList
                         $ collectSingles quanVars
                        ++ collectSingles (collectTuples quanVars)

    let
        newvarsInBubbles :: S.HashSet Text
        newvarsInBubbles = S.fromList $ concat
                                [ r | [xMatch| ls := withLocals.locals |] <- universe param
                                    , let nameOut [xMatch| [Prim (S s)] := topLevel.declaration.find.name.reference |] = Just s
                                          nameOut _ = Nothing
                                    , let r = mapMaybe nameOut ls
                                ]

    uniqueNamesInQuanVar <- forM (S.toList newvarsInQuanVar) $ \ a -> do
        b <- nextUniqueName
        mkLog "gensym" (pretty a <+> "~~" <+> pretty b)
        return (a, b)
    uniqueNamesInBubbles <- forM (S.toList newvarsInBubbles) $ \ a -> do
        b' <- nextUniqueName
        let b = "aux__" `T.append` (T.drop 3 b')
        mkLog "gensym" (pretty a <+> "~~" <+> pretty b)
        return (a, identifierConstruct b (Just "regionS") Nothing)
    let uniqueNamesMap = M.fromList (uniqueNamesInQuanVar ++ uniqueNamesInBubbles)

    let
        f p@[xMatch| [Prim (S r)] := reference |]
            = case M.lookup r uniqueNamesMap of
                Nothing -> p
                Just t  -> [xMake| reference := [Prim (S t)] |]
        f p@[xMatch| [Prim (S r)] := topLevel.declaration.find.name.reference
                   | [d]          := topLevel.declaration.find.domain
                   |] = case identifierSplit r of
                            (base, Just "regionS", mrepr) ->
                                let r' = identifierConstruct base Nothing mrepr
                                in  [xMake| topLevel.declaration.find.name.reference := [Prim (S r')]
                                          | topLevel.declaration.find.domain         := [d]
                                          |]
                            _ -> p
        f p = p

    return $ transform f param

