{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.InitialiseSpecState where

import Language.E
import qualified Data.Set as S


initialiseSpecState :: Monad m => Spec -> CompE m ()
initialiseSpecState (Spec _ statements) = do
    let names = [ nm
                | statement <- statements
                , [xMatch| [Prim (S nm)] := reference |] <- universe statement
                ]
    modifyGlobal $ \ st -> st { allNamesPreConjure = S.fromList names }
    mapM_ processStatement statements

