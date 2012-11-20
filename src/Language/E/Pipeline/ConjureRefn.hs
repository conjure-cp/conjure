{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.ConjureRefn where

import Language.E
import Language.E.Pipeline.ApplyRefn ( applyRefn )
import Language.E.Pipeline.BubbleUp ( bubbleUpSpec )
import Language.E.Pipeline.CheckIfAllRefined ( checkIfAllRefined )
import Language.E.Pipeline.Groom ( groomSpec )
import Language.E.Pipeline.NoTuples ( noTuplesSpec )
import Language.E.Pipeline.RemoveUnused ( removeUnused )
import Language.E.Pipeline.RuleRefnToFunction ( ruleRefnToFunction )



conjureRefn
    :: forall m
    .  ( MonadConjure m
       , MonadList m
       )
    => Bool
    -> Spec
    -> [RuleRefn]
    -> m Spec
conjureRefn isFinal spec rules = {-# SCC "conjureRefn" #-} withBindingScope' $
    case ( ruleRefnToFunction rules :: Either [ConjureError]
                                              [E -> m (Maybe [(String, E)])]
         ) of
        Left  es -> err ErrFatal $ vcat $ map (prettyError "refn") es
        Right fs ->
            let pipeline =  recordSpec >=> applyRefn fs
                        >=> recordSpec >=> makeIdempotent noTuplesSpec
                        >=> recordSpec >=> removeUnused
                        >=> recordSpec >=> checkIfAllRefined
                        >=> recordSpec >=> (if isFinal then groomSpec else return)
                        >=> recordSpec >=> bubbleUpSpec
                        >=> recordSpec
            in  pipeline spec

