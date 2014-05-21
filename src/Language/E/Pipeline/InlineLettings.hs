{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.InlineLettings where

import Language.E

import qualified Data.HashMap.Strict as M


inlineLettings :: MonadConjure m => Spec -> m Spec
inlineLettings (Spec v statements) =
    let

        splitLettingOrNot [xMatch| [nm] := topLevel.letting.name 
                                 | [x]  := topLevel.letting.expr
                                 |] = ( [(nm,x)] , [] )
        splitLettingOrNot [xMatch| [nm] := topLevel.letting.name 
                                 | [x]  := topLevel.letting.domain
                                 |] = ( [(nm,x)] , [] )
        splitLettingOrNot x         = ( []       , [x] )

        (lettings, others) = mconcat $ map splitLettingOrNot $ statementAsList statements

        lettingsMap = M.fromList lettings

        f x | Just y <- M.lookup x lettingsMap = transform f y
        f x = x

    in
        return $ Spec v (transform f $ listAsStatement others)


