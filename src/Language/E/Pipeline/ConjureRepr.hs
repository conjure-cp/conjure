{-# LANGUAGE OverloadedStrings #-}

module Language.E.Pipeline.ConjureRepr where

import Language.E
import Language.E.Pipeline.ApplyRepr ( applyRepr )
import Language.E.Pipeline.ExplodeStructuralVars ( explodeStructuralVars )
import Language.E.Pipeline.InlineLettings ( inlineLettings )
import Language.E.Pipeline.IntroduceFakeConstraints
import Language.E.Pipeline.IntroduceRegions ( introduceRegions )
import Language.E.Pipeline.ImplicitWheres ( implicitWheres )
import Language.E.Pipeline.HandlingEnums ( handleEnums )
import Language.E.Pipeline.HandlingUnnameds ( handleUnnameds )
import Language.E.Pipeline.NoTuples ( noTuplesSpec )

import Data.HashSet as S


conjureRepr
    :: MonadConjureList m
    => [RuleRepr]
    -> Spec
    -> m Spec
conjureRepr reprs spec = withBindingScope' $ do
    flags <- getsGlobal conjureFlags
    let useChannelling = not $ S.member "--no-channelling" flags
    mkLog "useChannelling" $ pretty useChannelling 
    initialiseSpecState spec
    let pipeline =  recordSpec "entering conjureRepr"
                >=> implicitWheres                          >=> recordSpec "implicitWheres"
                >=> explodeStructuralVars                   >=> recordSpec "explodeStructuralVars"
                >=> handleEnums                             >=> recordSpec "handleEnums"
                >=> handleUnnameds                          >=> recordSpec "handleUnnameds"
                >=> inlineLettings                          >=> recordSpec "inlineLettings"
                -- following is to remove any unnecessary occurrences of variables
                >=> simplifySpec                            >=> recordSpec "simplifySpec"
                >=> noTuplesSpec                            >=> recordSpec "noTuplesSpec"
                >=> introduceFakeConstraints                >=> recordSpec "introduceFakeConstraints"
                >=> introduceRegions useChannelling         >=> recordSpec "introduceRegions"
                >=> applyRepr reprs                         >=> recordSpec "applyRepr"
                >=> return . removeFakeConstraints          >=> recordSpec "removeFakeConstraints"
    pipeline spec



conjureReprPure
    :: [RuleRepr] -> [RuleRefn] -> Spec
    -> [(Either Doc Spec, LogTree)]
conjureReprPure reprs _ = onlyOneError . go
    where

        onlyOneError [] = []
        onlyOneError (x:xs)
            | isLeft (fst x) = [x]
            | otherwise      = x : onlyOneError xs

        go :: Spec -> [(Either Doc Spec, LogTree)]
        go s = trace "Repr" $
            let
                mouts :: [(Either Doc Spec, LogTree)]
                mouts = runCompE "Repr" $ conjureRepr reprs s

            in  mouts

