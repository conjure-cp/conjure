{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.ConjureRefn where

import Language.E
import Language.E.Pipeline.RuleRefnToFunction ( ruleRefnToFunction )
import Language.E.Pipeline.ApplyRefn ( applyRefn )
import Language.E.Pipeline.Groom ( groomSpec )
import Language.E.Pipeline.NoTuples ( noTuplesSpec )
import Language.E.Pipeline.RemoveUnused ( removeUnused )



conjureRefn :: (Functor m, Monad m)
    => Bool
    -> Spec
    -> [RuleRefn]
    -> CompE m Spec
conjureRefn isFinal spec rules =
    case ruleRefnToFunction rules of
        Left  es -> err ErrFatal $ vcat $ map snd es
        Right fs ->
            let pipeline =  applyRefn fs
                        >=> removeUnused
                        >=> makeIdempotent noTuplesSpec
                        >=> (if isFinal then groomSpec else return)
            in  pipeline spec
