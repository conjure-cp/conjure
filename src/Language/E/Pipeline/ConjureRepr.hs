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
import Language.E.Pipeline.NoTuples ( allNoTuplesSpec )
import Conjure.Mode ( ConjureModeWithFlags(..), ConjureMode(..), ConjureModeMultiple(..) )


conjureRepr
    :: MonadConjureList m
    => [RuleRepr]
    -> Spec
    -> m Spec
conjureRepr reprs spec = withBindingScope' $ do
    ConjureModeWithFlags mode _ _ _ _ <- getsGlobal conjureMode
    let useChannelling = case mode of
                            ModeMultipleOutput DFNoChannelling _ _ _ -> False
                            _ -> True
    mkLog "useChannelling" $ pretty useChannelling
    initialiseSpecState spec
    let pipeline =  recordSpec "entering conjureRepr"
                >=> introduceFakeConstraints                >=> recordSpec "introduceFakeConstraints"
                >=> introduceRegions useChannelling         >=> recordSpec "introduceRegions"
                >=> implicitWheres                          >=> recordSpec "implicitWheres"
                >=> explodeStructuralVars                   >=> recordSpec "explodeStructuralVars"
                >=> handleEnums                             >=> recordSpec "handleEnums"
                >=> handleUnnameds                          >=> recordSpec "handleUnnameds"
                >=> inlineLettings                          >=> recordSpec "inlineLettings"
                >=> explodeStructuralVars                   >=> recordSpec "explodeStructuralVars"
                -- following is to remove any unnecessary occurrences of variables
                >=> simplifySpec                            >=> recordSpec "simplifySpec"
                >=> allNoTuplesSpec                         >=> recordSpec "allNoTuplesSpec"
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

