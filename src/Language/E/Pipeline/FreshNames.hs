{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.FreshNames ( freshNames ) where

import Language.E.Imports
import Language.E.Definition

import qualified Data.Set as S
import qualified Data.Map as M


-- if a rewrite rule introduces new names, those names need to be unique.
freshNames :: Monad m => E -> CompE m E
freshNames param = do
    let
        quanVars :: [E]
        quanVars = [ s | [xMatch| [s] := quantified.quanVar |] <- universe param ]

        collectSingles :: [E] -> [String]
        collectSingles i = [ r | [xMatch| [Prim (S r)] := structural.single.reference |] <- i ]

        collectTuples :: [E] -> [E]
        collectTuples  i = [ r | [xMatch| rs := structural.tuple |] <- i , r <- rs ]

        newvarsInQuanVar :: S.Set String
        newvarsInQuanVar = S.fromList
                         $ collectSingles quanVars
                        ++ collectSingles (collectTuples quanVars)

    let
        newvarsInBubbles :: S.Set String
        newvarsInBubbles = S.fromList $ concat
                                [ r | [xMatch| ls := withLocals.locals |] <- universe param
                                    , let nameOut [xMatch| [Prim (S s)] := topLevel.declaration.find .name.reference |] = Just s
                                          nameOut [xMatch| [Prim (S s)] := topLevel.declaration.given.name.reference |] = Just s
                                          nameOut _ = Nothing
                                    , let r = mapMaybe nameOut ls
                                ]
    let
        newvars :: S.Set String
        newvars = newvarsInQuanVar `S.union` newvarsInBubbles
    -- let
    --     newvars  = S.fromList [ r | [xMatch| [Prim (S r)] := reference |] <- universe param
    --                               , r `notElem` ["forAll","exists","sum"]           -- don't rename these
    --                               , '#' `notElem` r                                 -- and if it has a # in it
    --                               ]
    uniqueNames <- forM (S.toList newvars) $ \ a -> do b <- nextUniqueName; return (a, Prim (S b))
    let uniqueNamesMap = M.fromList uniqueNames
    let
        f p@[xMatch| [Prim (S r)] := reference |]
            = case M.lookup r uniqueNamesMap of
                Nothing -> p
                Just t  -> [xMake| reference := [t] |]
        f p = p
    return $ transform f param
