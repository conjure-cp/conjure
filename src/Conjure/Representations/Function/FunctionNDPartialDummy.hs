{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Function.FunctionNDPartialDummy ( functionNDPartialDummy ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
-- import Conjure.Language.DomainSizeOf
-- import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.ZeroVal ( EnumerateDomain )
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues )
import Conjure.Representations.Function.FunctionND ( viewAsDomainTupleS, mkLensAsDomainTupleS )


functionNDPartialDummy :: forall m .
    MonadFail m =>
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Representation m
functionNDPartialDummy = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainFunction _
                    attrs@(FunctionAttr _ PartialityAttr_Partial _)
                    innerDomainFr@(viewAsDomainTupleS -> Just innerDomainFrs)
                    innerDomainTo@DomainInt{}) | all domainCanIndexMatrix innerDomainFrs = do
            innerDomainFr' <- f innerDomainFr
            innerDomainTo' <- f innerDomainTo
            return [ DomainFunction Function_NDPartialDummy attrs fr to
                   | fr <- innerDomainFr'
                   , to <- innerDomainTo'
                   ]
        chck _ _ = return []

        outName :: Domain HasRepresentation x -> Name -> Name
        outName = mkOutName Nothing

        -- getMaxSize attrs innerDomain = case attrs of
        --     SizeAttr_MaxSize x -> return x
        --     SizeAttr_MinMaxSize _ x -> return x
        --     _ -> reTag TagInt <$> domainSizeOf innerDomain

        calcDummyDomain :: Pretty r => Domain r Expression -> Domain r Expression
        calcDummyDomain (DomainInt t [RangeBounded lb ub]) =
                DomainInt t [RangeBounded [essence| &lb - 1 |] ub]
        calcDummyDomain dom@(DomainInt t ranges) =
            let dummyElem = calcDummyElem dom
            in  DomainInt t (ranges ++ [RangeSingle dummyElem])
        calcDummyDomain dom = bug ("ExplicitVarSizeWithDummy.calcDummyDomain" <+> pretty dom)

        calcDummyElem :: Pretty r => Domain r Expression -> Expression
        calcDummyElem dom =
            let theMax = bugFail "calcDummyElem: minOfDomain" (minOfDomain dom)
            in  [essence| &theMax - 1 |]

        calcDummyElemC :: Pretty r => Domain r Constant -> Constant
        calcDummyElemC (DomainInt _ []) = bug "ExplicitVarSizeWithDummy.calcDummyElemC []"
        calcDummyElemC (DomainInt t rs) = ConstantInt t $
            minimum [ i
                    | r <- rs
                    , i <- case r of
                        RangeSingle (ConstantInt _ x) -> [x]
                        RangeBounded (ConstantInt _ x) (ConstantInt _ y) -> [x..y]
                        _ -> bug ("ExplicitVarSizeWithDummy.calcDummyElemC" <+> pretty r)
                    ] - 1
        calcDummyElemC d = bug ("ExplicitVarSizeWithDummy.calcDummyElemC" <+> pretty d)

        downD :: TypeOf_DownD m
        downD (name, domain@(DomainFunction Function_NDPartialDummy
                    (FunctionAttr _ PartialityAttr_Partial _)
                    (viewAsDomainTupleS -> Just innerDomainFrs)
                    innerDomainTo)) | all domainCanIndexMatrix innerDomainFrs = do
            let unroll is j = foldr DomainMatrix j is
            let domainWithDummy = calcDummyDomain innerDomainTo
            return $ Just
                [ ( outName domain name
                  , unroll (map forgetRepr innerDomainFrs) domainWithDummy
                  )
                ]
        downD _ = na "{downD} FunctionNDPartialDummy"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1
            (DomainFunction Function_NDPartialDummy
                (FunctionAttr sizeAttr PartialityAttr_Partial jectivityAttr)
                innerDomainFr@(viewAsDomainTupleS -> Just innerDomainFrs)
                innerDomainTo) | all domainCanIndexMatrix innerDomainFrs = do
            let
                kRange = case innerDomainFr of
                        DomainTuple ts  -> map fromInt [1 .. genericLength ts]
                        DomainRecord rs -> map (fromName . fst) rs
                        _ -> []
                toIndex x = if null kRange then [x] else [ [essence| &x[&k] |] | k <- kRange ]
                index x m = make opMatrixIndexing m (toIndex x)
                dummyElem = calcDummyElem innerDomainTo

            let
                injectiveCons :: Expression -> m [Expression]
                injectiveCons values = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    let valuesIndexedI = index i values
                    let valuesIndexedJ = index j values
                    return $ return $ -- list
                        [essence|
                            and([ &valuesIndexedI != &valuesIndexedJ
                                | &iPat : &innerDomainFr
                                , &jPat : &innerDomainFr
                                , &i .< &j
                                , &valuesIndexedI != &dummyElem
                                , &valuesIndexedJ != &dummyElem
                                ])
                        |]

                surjectiveCons :: Expression -> m [Expression]
                surjectiveCons values = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    let valuesIndexed = index j values
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &valuesIndexed = &i
                        |]

                jectivityCons :: Expression -> m [Expression]
                jectivityCons values = case jectivityAttr of
                    JectivityAttr_None       -> return []
                    JectivityAttr_Injective  -> injectiveCons  values
                    JectivityAttr_Surjective -> surjectiveCons values
                    JectivityAttr_Bijective  -> (++) <$> injectiveCons  values
                                                     <*> surjectiveCons values

                cardinality :: Expression -> m Expression
                cardinality values = do
                    (iPat, i) <- quantifiedVar
                    let valuesIndexed  = index i values
                    return [essence| sum &iPat : &innerDomainFr . toInt(&valuesIndexed != &dummyElem) |]

            let innerStructuralCons values = do
                    (iPat, i) <- quantifiedVarOverDomain (forgetRepr innerDomainFr)
                    let valuesIndexed = index i values
                    let activeZone b = [essence| forAll &iPat : &innerDomainFr . &valuesIndexed != &dummyElem -> &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomainTo

                    let inLoop = valuesIndexed
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ rel -> do
                refs <- downX1 rel
                case refs of
                    [values] ->
                        concat <$> sequence
                            [ jectivityCons values
                            , mkSizeCons sizeAttr <$> cardinality values
                            , innerStructuralCons values
                            ]
                    _ -> na "{structuralCons} FunctionNDPartialDummy"

        structuralCons _ _ _ = na "{structuralCons} FunctionNDPartialDummy"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainFunction Function_NDPartialDummy
                    (FunctionAttr _ PartialityAttr_Partial _)
                    innerDomainFr@(viewAsDomainTupleS -> Just innerDomainFrs)
                    innerDomainTo)
              , viewConstantFunction -> Just vals
              ) | all domainCanIndexMatrix innerDomainFrs
                , Just (_mk, inspect) <- mkLensAsDomainTupleS innerDomainFr = do
            let
                check :: [Constant] -> Maybe Constant
                check indices = listToMaybe [ v
                                            | (inspect -> Just k, v) <- vals
                                            , k == indices
                                            ]

            let dummyElem = calcDummyElemC innerDomainTo

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
                                [ case active val of
                                    Nothing -> dummyElem
                                    Just v  -> v
                                | val <- domVals ]
                unrollC (i:is) prevIndices = do
                    domVals <- domainValues i
                    matrixVals <- forM domVals $ \ val ->
                        unrollC is (prevIndices ++ [val])
                    return $ ConstantAbstract $ AbsLitMatrix i matrixVals
                unrollC is prevIndices = failDoc $ vcat [ "FunctionNDPartialDummy.up.unrollC"
                                                     , "    is         :" <+> vcat (map pretty is)
                                                     , "    prevIndices:" <+> pretty (show prevIndices)
                                                     ]

            outValues <- unrollC (map forgetRepr innerDomainFrs) []
            return $ Just
                [ ( outName domain name
                  , unrollD (map forgetRepr innerDomainFrs) innerDomainTo
                  , outValues
                  )
                ]

        downC _ = na "{downC} FunctionNDPartialDummy"

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainFunction Function_NDPartialDummy
                                (FunctionAttr _ PartialityAttr_Partial _)
                                innerDomainFr@(viewAsDomainTupleS -> Just innerDomainFrs) innerDomainTo))

            | Just (mk, _inspect) <- mkLensAsDomainTupleS innerDomainFr = do
            let dummyElem = calcDummyElemC innerDomainTo
            case lookup (outName domain name) ctxt of
                Just valuesMatrix -> do
                    let
                        allIndices :: (MonadFail m, Pretty r) => [Domain r Constant] -> m [[Constant]]
                        allIndices = fmap sequence . mapM domainValues

                        index :: MonadFail m => Constant -> [Constant] -> m Constant
                        index m [] = return m
                        index (ConstantAbstract (AbsLitMatrix indexDomain vals)) (i:is) = do
                            froms <- domainValues indexDomain
                            case lookup i (zip froms vals) of
                                Nothing -> failDoc "Value not found. FunctionND.up.index"
                                Just v  -> index v is
                        index m is = bug ("FunctionND.up.index" <+> pretty m <+> pretty (show is))

                    indices  <- allIndices innerDomainFrs
                    vals     <- forM indices $ \ these -> do
                        value <- index valuesMatrix these
                        return $ if value /= dummyElem
                                    then
                                        Just (mk these, value)
                                    else
                                        Nothing
                    return ( name
                           , ConstantAbstract $ AbsLitFunction (catMaybes vals)
                           )
                Nothing -> failDoc $ vcat $
                    [ "(in FunctionNDPartialDummy up)"
                    , "No value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} FunctionNDPartialDummy"

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            [values] <- downX1 inp
            Just [(_, DomainMatrix _innerDomainFr innerDomainTo)] <- downD ("SO", domain)
            innerSO downX1 values innerDomainTo
