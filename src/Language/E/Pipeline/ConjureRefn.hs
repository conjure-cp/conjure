{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.ConjureRefn where

import Language.E
import Language.E.Pipeline.ApplyRefn ( applyRefn )
import Language.E.Pipeline.AbstractDomsInQuans ( abstractDomsInQuans, quanDomAndSubsetEq )
import Language.E.Pipeline.BubbleUp ( bubbleUpSpec )
import Language.E.Pipeline.CheckIfAllRefined ( checkIfAllRefined )
import Language.E.Pipeline.ExplodeStructuralVars ( explodeStructuralVars )
import Language.E.Pipeline.InlineLettings ( inlineLettings )
import Language.E.Pipeline.ImplicitWheres ( implicitWheres )
import Language.E.Pipeline.HandlingEnums ( handleEnums )
import Language.E.Pipeline.HandlingUnnameds ( handleUnnameds )
import Language.E.Pipeline.NoTuples ( conjureNoTuples )
import Language.E.Pipeline.RemoveUnused ( removeUnused )
import Language.E.Pipeline.RuleRefnToFunction ( ruleRefnToFunction )



conjureRefn
    :: forall m
    .  MonadConjureList m
    => [RuleRepr]
    -> [RuleRefn]
    -> Spec
    -> m Spec
conjureRefn reprs refns spec = withBindingScope' $
    case ( ruleRefnToFunction refns :: Either [ConjureError]
                                              [E -> m (Maybe [(Text, E)])]
         ) of
        Left  es -> err ErrFatal $ vcat $ map (prettyError "refn") es
        Right fs -> do
            let fs' = abstractDomsInQuans reprs
                    : quanDomAndSubsetEq
                    : fs
            initialiseSpecState spec
            let pipeline =  return
                        >=> recordSpec >=> implicitWheres
                        >=> recordSpec >=> explodeStructuralVars
                        >=> recordSpec >=> handleEnums
                        >=> recordSpec >=> handleUnnameds
                        >=> recordSpec >=> inlineLettings
                        >=> recordSpec >=> applyRefn fs'
                        >=> recordSpec >=> checkIfAllRefined
                        >=> recordSpec >=> conjureNoTuples
                        >=> recordSpec >=> removeUnused
                        >=> recordSpec >=> bubbleUpSpec
                        >=> recordSpec
            pipeline spec

