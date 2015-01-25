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
import Conjure.Representations.Function.Function1D ( domainValues, toIntDomain )


functionND :: MonadFail m => Representation m
functionND = Representation chck downD structuralCons downC up

    where

        chck f (DomainFunction _
                    attrs@(FunctionAttr _ PartialityAttr_Total _)
                    innerDomainFr@(DomainTuple innerDomainFrs)
                    innerDomainTo) | all domainCanIndexMatrix innerDomainFrs =
            DomainFunction "FunctionND" attrs
                <$> f innerDomainFr
                <*> f innerDomainTo
        chck _ _ = []

        nameValues name = mconcat [name, "_", "FunctionND"]

        downD (name, DomainFunction "FunctionND"
                    (FunctionAttr _ PartialityAttr_Total _)
                    (DomainTuple innerDomainFrs')
                    innerDomainTo) | all domainCanIndexMatrix innerDomainFrs' = do
            innerDomainFrs <- mapM toIntDomain innerDomainFrs'
            let unroll is j = foldr DomainMatrix j is
            return $ Just
                [ ( nameValues name
                  , unroll (map (forgetRepr "Representation.FunctionND") innerDomainFrs) innerDomainTo
                  )
                ]
        downD _ = na "{downD} FunctionND"

        -- FIX: inner structural constraints
        structuralCons _ downX1
            (DomainFunction "FunctionND"
                (FunctionAttr sizeAttr PartialityAttr_Total jectivityAttr)
                (DomainTuple innerDomainFrs')
                innerDomainTo) | all domainCanIndexMatrix innerDomainFrs' = do
            innerDomainFrs    <- mapM toIntDomain innerDomainFrs'
            let innerDomainFr =  DomainTuple innerDomainFrs

            let
                frArity = length innerDomainFrs

                index x m 1     = make opIndexing m                     (make opIndexing x 1)
                index x m arity = make opIndexing (index x m (arity-1)) (make opIndexing x (fromInt arity))

            let injectiveCons fresh values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)

                        valuesIndexedI = index i values frArity
                        valuesIndexedJ = index j values frArity
                    in
                        [essence|
                            and([ &valuesIndexedI != &valuesIndexedJ
                                | &iPat : &innerDomainFr
                                , &jPat : &innerDomainFr
                                , &i != &j
                                ])
                        |]

            let surjectiveCons fresh values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)

                        valuesIndexed = index j values frArity
                    in
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &valuesIndexed = &i
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

            return $ \ fresh func -> do
                refs <- downX1 func
                case refs of
                    [values] -> return $ concat
                        [ jectivityCons fresh values
                        , mkSizeCons sizeAttr (cardinality fresh)
                        ]
                    _ -> na "{structuralCons} FunctionND"

        structuralCons _ _ _ = na "{structuralCons} FunctionND"

        downC ( name
              , DomainFunction "FunctionND"
                    (FunctionAttr _ PartialityAttr_Total _)
                    (DomainTuple innerDomainFrs')
                    innerDomainTo
              , ConstantAbstract (AbsLitFunction vals)
              ) | all domainCanIndexMatrix innerDomainFrs' = do
            z <- zeroVal innerDomainTo
            innerDomainFrs <- fmap (fmap e2c) <$> mapM toIntDomain (fmap (fmap Constant) innerDomainFrs')
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
                        => [ ( Domain () Constant       -- the int domain
                             , Domain () Constant       -- the actial domain
                             ) ]
                        -> [Constant]               -- indices
                        -> m Constant
                unrollC [(i,i')] prevIndices = do
                    domVals <- domainValues i'
                    let active val = check $ prevIndices ++ [val]
                    return $ ConstantAbstract $ AbsLitMatrix i
                                [ fromMaybe z (active val)
                                | val <- domVals ]
                unrollC ((i,i'):is) prevIndices = do
                    domVals <- domainValues i'
                    matrixVals <- forM domVals $ \ val ->
                        unrollC is (prevIndices ++ [val])
                    return $ ConstantAbstract $ AbsLitMatrix i matrixVals
                unrollC is prevIndices = fail $ vcat [ "FunctionND.up.unrollC"
                                                     , "    is         :" <+> vcat (map pretty is)
                                                     , "    prevIndices:" <+> pretty (show prevIndices)
                                                     ]

            outValues <- unrollC (zip (map (forgetRepr "Representation.Function1DPartial") innerDomainFrs)
                                      (map (forgetRepr "Representation.Function1DPartial") innerDomainFrs')) []
            return $ Just
                [ ( nameValues name
                  , unrollD (map (forgetRepr "Representation.Function1DPartial") innerDomainFrs) innerDomainTo
                  , outValues
                  )
                ]

        downC _ = na "{downC} FunctionND"

        up ctxt (name, domain@(DomainFunction "FunctionND"
                                (FunctionAttr _ PartialityAttr_Total _)
                                (DomainTuple innerDomainFrs') _)) = do

            innerDomainFrs <- fmap (fmap e2c) <$> mapM toIntDomain (fmap (fmap Constant) innerDomainFrs')

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

                    indices' <- allIndices innerDomainFrs'
                    indices  <- allIndices innerDomainFrs
                    vals     <- forM (zip indices indices') $ \ (these, these') -> do
                        value <- index valuesMatrix these
                        return (ConstantAbstract (AbsLitTuple these'), value)
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
