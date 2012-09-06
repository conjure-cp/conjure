{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.E.Pipeline.FreshNames ( freshNames ) where

import Language.E.Imports
import Language.E.Definition

import qualified Data.Set as S
import qualified Data.Map as M


-- if a rewrite rule introduces new names, those names need to be unique.
freshNames :: Monad m => E -> CompE m E
freshNames param = do
    let
        newvars  = S.fromList [ r | [xMatch| [Prim (S r)] := reference |] <- universe param
                                  , r `notElem` ["forAll","exists","sum"]           -- don't rename these
                                  , '#' `notElem` r                                 -- and if it has a # in it
                                  ]
    uniqueNames <- forM (S.toList newvars) $ \ a -> do b <- nextUniqueName; return (a, Prim (S b))
    let uniqueNamesMap = M.fromList uniqueNames
    let
        f p@[xMatch| [Prim (S r)] := reference |]
            = case M.lookup r uniqueNamesMap of
                Nothing -> p
                Just t  -> [xMake| reference := [t] |]
        f p = p
    return $ transform f param
