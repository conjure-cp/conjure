{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Function.FunctionND ( functionND ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.TH
import Conjure.Language.Lenses
import Conjure.Language.ZeroVal ( zeroVal )
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues )


functionND :: forall m . MonadFail m => Representation m
functionND = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainFunction _
                    attrs@(FunctionAttr _ PartialityAttr_Total _)
                    innerDomainFr@(DomainTuple innerDomainFrs)
                    innerDomainTo) | all domainCanIndexMatrix innerDomainFrs =
            DomainFunction "FunctionND" attrs
                <$> f innerDomainFr
                <*> f innerDomainTo
        chck _ _ = []

        nameValues name = mconcat [name, "_", "FunctionND"]

        downD :: TypeOf_DownD m
        downD (name, DomainFunction "FunctionND"
                    (FunctionAttr _ PartialityAttr_Total _)
                    (DomainTuple innerDomainFrs)
                    innerDomainTo) | all domainCanIndexMatrix innerDomainFrs = do
            let unroll is j = foldr DomainMatrix j is
            return $ Just
                [ ( nameValues name
                  , unroll (map forgetRepr innerDomainFrs) innerDomainTo
                  )
                ]
        downD _ = na "{downD} FunctionND"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1
            (DomainFunction "FunctionND"
                (FunctionAttr sizeAttr PartialityAttr_Total jectivityAttr)
                (DomainTuple innerDomainFrs)
                innerDomainTo) | all domainCanIndexMatrix innerDomainFrs = do

            let innerDomainFr =  DomainTuple innerDomainFrs

            let
                frArity = length innerDomainFrs

                index x m 1     = make opIndexing m                     (make opIndexing x 1)
                index x m arity = make opIndexing (index x m (arity-1)) (make opIndexing x (fromInt arity))

            let injectiveCons fresh values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        valuesIndexedI = index i values frArity
                    in
                        [essence|
                            allDiff([ &valuesIndexedI
                                    | &iPat : &innerDomainFr
                                    ])
                        |]

            let surjectiveCons fresh values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)

                        valuesIndexedJ = index j values frArity
                    in
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &valuesIndexedJ = &i
                        |]

            let jectivityCons fresh values = case jectivityAttr of
                    JectivityAttr_None       -> []
                    JectivityAttr_Injective  -> injectiveCons  fresh values
                    JectivityAttr_Surjective -> surjectiveCons fresh values
                    JectivityAttr_Bijective  -> injectiveCons  fresh values
                                       ++ surjectiveCons fresh values

            let cardinality fresh =
                    let
                        (iPat, _) = quantifiedVar (fresh `at` 0)
                    in
                        [essence| sum &iPat : &innerDomainFr . 1 |]

            let innerStructuralCons fresh values = do
                    let (iPat, i) = quantifiedVar (headInf fresh)
                        valuesIndexedI = index i values frArity
                    let activeZone b = [essence| forAll &iPat : &innerDomainFr . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomainTo

                    let inLoop = valuesIndexedI
                    outs <- innerStructuralConsGen (tail fresh) inLoop
                    return (map activeZone outs)

            return $ \ fresh func -> do
                refs <- downX1 func
                case refs of
                    [values] -> do
                        isc <- innerStructuralCons fresh values
                        return $ concat
                            [ jectivityCons fresh values
                            , mkSizeCons sizeAttr (cardinality fresh)
                            , isc
                            ]
                    _ -> na "{structuralCons} FunctionND"

        structuralCons _ _ _ = na "{structuralCons} FunctionND"

        downC :: TypeOf_DownC m
        downC ( name
              , DomainFunction "FunctionND"
                    (FunctionAttr _ PartialityAttr_Total _)
                    (DomainTuple innerDomainFrs)
                    innerDomainTo
              , ConstantAbstract (AbsLitFunction vals)
              ) | all domainCanIndexMatrix innerDomainFrs = do
            z <- zeroVal innerDomainTo
            let
                check :: [Constant] -> Maybe Constant
                check indices = listToMaybe [ v
                                            | (ConstantAbstract (AbsLitTuple k), v) <- vals
                                            , k == indices
                                            ]

            let
                unrollD :: [Domain () Constant] -> Domain r Constant -> Domain r Constant
                unrollD is j = foldr DomainMatrix j is

            let
                unrollC :: MonadFail m
                        => [Domain () Constant]
                        -> [Constant]               -- indices
                        -> m Constant
                unrollC [i] prevIndices = do
                    domVals <- domainValues i
                    let active val = check $ prevIndices ++ [val]
                    return $ ConstantAbstract $ AbsLitMatrix i
                                [ fromMaybe z (active val)
                                | val <- domVals ]
                unrollC (i:is) prevIndices = do
                    domVals <- domainValues i
                    matrixVals <- forM domVals $ \ val ->
                        unrollC is (prevIndices ++ [val])
                    return $ ConstantAbstract $ AbsLitMatrix i matrixVals
                unrollC is prevIndices = fail $ vcat [ "FunctionND.up.unrollC"
                                                     , "    is         :" <+> vcat (map pretty is)
                                                     , "    prevIndices:" <+> pretty (show prevIndices)
                                                     ]

            outValues <- unrollC (map forgetRepr innerDomainFrs) []
            return $ Just
                [ ( nameValues name
                  , unrollD (map forgetRepr innerDomainFrs) innerDomainTo
                  , outValues
                  )
                ]

        downC _ = na "{downC} FunctionND"

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainFunction "FunctionND"
                                (FunctionAttr _ PartialityAttr_Total _)
                                (DomainTuple innerDomainFrs) _)) =
            case lookup (nameValues name) ctxt of
                Just valuesMatrix -> do
                    let
                        allIndices :: (MonadFail m, Pretty r) => [Domain r Constant] -> m [[Constant]]
                        allIndices = fmap sequence . mapM domainValues

                        index :: MonadFail m => Constant -> [Constant] -> m Constant
                        index m [] = return m
                        index (ConstantAbstract (AbsLitMatrix indexDomain vals)) (i:is) = do
                            froms <- domainValues indexDomain
                            case lookup i (zip froms vals) of
                                Nothing -> fail "Value not found. FunctionND.up.index"
                                Just v  -> index v is
                        index m is = bug ("RelationAsMatrix.up.index" <+> pretty m <+> pretty (show is))

                    indices  <- allIndices innerDomainFrs
                    vals     <- forM indices $ \ these -> do
                        value <- index valuesMatrix these
                        return (ConstantAbstract (AbsLitTuple these), value)
                    return ( name
                           , ConstantAbstract $ AbsLitFunction vals
                           )
                Nothing -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameValues name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} FunctionND"
