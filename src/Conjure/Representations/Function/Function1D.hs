{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Function.Function1D
    ( function1D
    , domainValues
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Constant
import Conjure.Language.DomainSizeOf
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal
import Conjure.Representations.Common

-- unordered-containers
import qualified Data.HashMap.Strict as M


function1D :: forall m . (MonadFailDoc m,MonadFail m, NameGen m, ?typeCheckerMode :: TypeCheckerMode) => Representation m
function1D = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainFunction _
                    attrs@(FunctionAttr _ PartialityAttr_Total _)
                    innerDomainFr
                    innerDomainTo) | domainCanIndexMatrix innerDomainFr = do
            innerDomainFr' <- f innerDomainFr
            innerDomainTo' <- f innerDomainTo
            return [ DomainFunction Function_1D attrs fr to
                   | fr <- innerDomainFr'
                   , to <- innerDomainTo'
                   ]
        chck _ _ = return []

        outName :: Domain HasRepresentation x -> Name -> Name
        outName = mkOutName Nothing

        downD :: TypeOf_DownD m
        downD (name, domain@(DomainFunction Function_1D
                    (FunctionAttr _ PartialityAttr_Total _)
                    innerDomainFr
                    innerDomainTo)) | domainCanIndexMatrix innerDomainFr = return $ Just
            [ ( outName domain name
              , DomainMatrix
                  (forgetRepr innerDomainFr)
                  innerDomainTo
              ) ]
        downD _ = na "{downD} Function1D"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1
            (DomainFunction Function_1D
                (FunctionAttr sizeAttr PartialityAttr_Total jectivityAttr)
                innerDomainFr
                innerDomainTo) | domainCanIndexMatrix innerDomainFr = do

            let
                injectiveCons :: Expression -> m [Expression]
                injectiveCons m = do
                    tyTo <- typeOfDomain innerDomainTo
                    let canAllDiff = case tyTo of
                            TypeBool{} -> True
                            TypeInt{}  -> True
                            TypeEnum{} -> True
                            _          -> False
                    if canAllDiff
                        then
                            return $ return $ -- list
                                [essence| allDiff(&m) |]
                        else do
                            (iPat, i) <- quantifiedVar
                            (jPat, j) <- quantifiedVar
                            return $ return $ -- list
                                [essence|
                                    forAll &iPat, &jPat : &innerDomainFr .
                                        &i .< &j -> &m[&i] != &m[&j]
                                |]

                surjectiveCons :: Expression -> m [Expression]
                surjectiveCons m = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &m[&j] = &i
                        |]

                jectivityCons :: Expression -> m [Expression]
                jectivityCons m = case jectivityAttr of
                    JectivityAttr_None       -> return []
                    JectivityAttr_Injective  -> injectiveCons  m
                    JectivityAttr_Surjective -> surjectiveCons m
                    JectivityAttr_Bijective  -> (++) <$> injectiveCons m <*> surjectiveCons m

            cardinality <- domainSizeOf innerDomainFr

            let innerStructuralCons m = do
                    (iPat, i) <- quantifiedVarOverDomain (forgetRepr innerDomainFr)
                    let activeZone b = [essence| forAll &iPat : &innerDomainFr . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomainTo

                    let inLoop = [essence| &m[&i] |]
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ func -> do
                refs <- downX1 func
                case refs of
                    [m] ->
                        concat <$> sequence
                            [ jectivityCons m
                            , return (mkSizeCons sizeAttr cardinality)
                            , innerStructuralCons m
                            ]
                    _ -> na "{structuralCons} Function1D"

        structuralCons _ _ _ = na "{structuralCons} Function1D"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainFunction Function_1D
                    (FunctionAttr _ PartialityAttr_Total _)
                    innerDomainFr
                    innerDomainTo)
              , viewConstantFunction -> Just vals_
              ) | domainCanIndexMatrix innerDomainFr = do
            let vals = M.fromList vals_
            froms            <- domainValues innerDomainFr
            valsOut          <- sequence
                [ val
                | fr <- froms
                , let val = case M.lookup fr vals of
                                Nothing -> failDoc $ vcat [ "No value for" <+> pretty fr
                                                       , "In:" <+> pretty (AbsLitFunction vals_)
                                                       ]
                                Just v  -> return v
                ]
            return $ Just
                [ ( outName domain name
                  , DomainMatrix (forgetRepr innerDomainFr) innerDomainTo
                  , ConstantAbstract $ AbsLitMatrix (forgetRepr innerDomainFr) valsOut
                  ) ]
        downC _ = na "{downC} Function1D"

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainFunction Function_1D
                                (FunctionAttr _ PartialityAttr_Total _)
                                innerDomainFr _)) =
            case lookup (outName domain name) ctxt of
                Nothing -> failDoc $ vcat $
                    [ "(in Function1D up)"
                    , "No value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant ->
                    case viewConstantMatrix constant of
                        Just (_, vals) -> do
                            froms <- domainValues innerDomainFr
                            return ( name
                                   , ConstantAbstract $ AbsLitFunction $ zip froms vals
                                   )
                        _ -> failDoc $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (outName domain name)
                                , "But got:" <+> pretty constant
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
        up _ _ = na "{up} Function1D"

        symmetryOrdering ::TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            [inner] <- downX1 inp
            Just [(_, innerDomain)] <- downD ("SO", domain)
            innerSO downX1 inner innerDomain
            


domainValues :: (MonadFailDoc m, Pretty r) => Domain r Constant -> m [Constant]
domainValues dom =
    case dom of
        DomainBool -> return [ConstantBool False, ConstantBool True]
        DomainInt t rs -> map (ConstantInt t) <$> valuesInIntDomain rs
        _ -> failDoc ("domainValues, not supported:" <+> pretty dom)

