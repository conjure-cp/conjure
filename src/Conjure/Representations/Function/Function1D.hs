{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Function.Function1D
    ( function1D
    , domainValues
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.DomainSizeOf
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal
import Conjure.Representations.Common


function1D :: forall m . MonadFail m => Representation m
function1D = Representation chck downD structuralCons downC up searchStrategy

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainFunction _
                    attrs@(FunctionAttr _ PartialityAttr_Total _)
                    innerDomainFr
                    innerDomainTo) | domainCanIndexMatrix innerDomainFr =
            DomainFunction "Function1D" attrs
                <$> f innerDomainFr
                <*> f innerDomainTo
        chck _ _ = []

        outName name = mconcat [name, "_", "Function1D"]

        downD :: TypeOf_DownD m
        downD (name, DomainFunction "Function1D"
                    (FunctionAttr _ PartialityAttr_Total _)
                    innerDomainFr
                    innerDomainTo) | domainCanIndexMatrix innerDomainFr = return $ Just
            [ ( outName name
              , DomainMatrix
                  (forgetRepr innerDomainFr)
                  innerDomainTo
              ) ]
        downD _ = na "{downD} Function1D"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1
            (DomainFunction "Function1D"
                (FunctionAttr sizeAttr PartialityAttr_Total jectivityAttr)
                innerDomainFr
                innerDomainTo) | domainCanIndexMatrix innerDomainFr = do

            let injectiveCons m = return $ [essence| allDiff(&m) |]

            let surjectiveCons fresh m = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)
                    in
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &m[&j] = &i
                        |]
            let jectivityCons fresh m = case jectivityAttr of
                    JectivityAttr_None       -> []
                    JectivityAttr_Injective  -> injectiveCons        m
                    JectivityAttr_Surjective -> surjectiveCons fresh m
                    JectivityAttr_Bijective  -> injectiveCons        m
                                       ++ surjectiveCons fresh m

            cardinality <- domainSizeOf innerDomainFr

            let innerStructuralCons fresh m = do
                    let (iPat, i) = quantifiedVar (headInf fresh)
                    let activeZone b = [essence| forAll &iPat : &innerDomainFr . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomainTo

                    let inLoop = [essence| &m[&i] |]
                    outs <- innerStructuralConsGen (tail fresh) inLoop
                    return (map activeZone outs)

            return $ \ fresh func -> do
                refs <- downX1 func
                case refs of
                    [m] -> do
                        isc <- innerStructuralCons fresh m
                        return $ concat
                            [ jectivityCons fresh m
                            , mkSizeCons sizeAttr cardinality
                            , isc
                            ]
                    _ -> na "{structuralCons} Function1D"

        structuralCons _ _ _ = na "{structuralCons} Function1D"

        downC :: TypeOf_DownC m
        downC ( name
              , DomainFunction "Function1D"
                    (FunctionAttr _ PartialityAttr_Total _)
                    innerDomainFr
                    innerDomainTo
              , ConstantAbstract (AbsLitFunction vals)
              ) | domainCanIndexMatrix innerDomainFr = do
            froms            <- domainValues innerDomainFr
            valsOut          <- sequence
                [ val
                | fr <- froms
                , let val = case lookup fr vals of
                                Nothing -> fail $ vcat [ "No value for " <+> pretty fr
                                                       , "In:" <+> pretty (AbsLitFunction vals)
                                                       ]
                                Just v  -> return v
                ]
            return $ Just
                [ ( outName name
                  , DomainMatrix (forgetRepr innerDomainFr) innerDomainTo
                  , ConstantAbstract $ AbsLitMatrix (forgetRepr innerDomainFr) valsOut
                  ) ]
        downC _ = na "{downC} Function1D"

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainFunction "Function1D"
                                (FunctionAttr _ PartialityAttr_Total _)
                                innerDomainFr _)) =
            case lookup (outName name) ctxt of
                Nothing -> fail $ vcat $
                    [ "No value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant ->
                    case constant of
                        ConstantAbstract (AbsLitMatrix _ vals) -> do
                            froms <- domainValues innerDomainFr
                            return ( name
                                   , ConstantAbstract $ AbsLitFunction $ zip froms vals
                                   )
                        _ -> fail $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (outName name)
                                , "But got:" <+> pretty constant
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
        up _ _ = na "{up} Function1D"

        searchStrategy :: TypeOf_SearchStrategy m
        searchStrategy p = map (BranchingOn . fst) . fromJustNote "searchStrategy" <$> downD p


domainValues :: (MonadFail m, Pretty r) => Domain r Constant -> m [Constant]
domainValues dom =
    case dom of
        DomainBool -> return [ConstantBool False, ConstantBool True]
        DomainInt rs -> map ConstantInt <$> valuesInIntDomain rs
        _ -> fail ("domainValues, not supported:" <+> pretty dom)
