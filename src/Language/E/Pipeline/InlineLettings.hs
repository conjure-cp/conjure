{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.InlineLettings where

import Language.E

import qualified Data.Map as M


inlineLettings :: MonadConjure m => Spec -> m Spec
inlineLettings (Spec v statements) =
    let

        splitLettingOrNot [xMatch| [nm] := topLevel.letting.name 
                                 | [x]  := topLevel.letting.expr
                                 |] = ( [(nm,x)] , [] )
        splitLettingOrNot x         = ( []       , [x] )

        (lettings, others) = mconcat $ map splitLettingOrNot $ statementAsList statements

        lettingsMap = M.fromList lettings

        f x | Just y <- M.lookup x lettingsMap = f y
        f (Tagged t xs) = Tagged t (map f xs)
        f x = x

    in
        return $ Spec v (f $ listAsStatement others)


