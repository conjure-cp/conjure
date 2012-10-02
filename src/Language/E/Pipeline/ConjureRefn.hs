{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.ConjureRefn where

import Language.E
import Language.E.Pipeline.RuleRefnToFunction ( ruleRefnToFunction )
import Language.E.Pipeline.ApplyRefn ( applyRefn )
import Language.E.Pipeline.RemoveUnused ( removeUnused )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Language.E.Pipeline.NoTuples ( noTuplesSpec )


conjureRefn :: (Functor m, Monad m)
    => Spec
    -> [RuleRefn]
    -> CompE m Spec
conjureRefn spec rules =
    case ruleRefnToFunction rules of
        Left  es -> err ErrFatal $ vcat $ map snd es
        Right fs ->
            let pipeline =  applyRefn fs
                        >=> removeUnused
                        >=> makeIdempotent noTuplesSpec
                        >=> trySimplifySpec
                        >=> (return . atMostOneSuchThat)
            in  pipeline spec
