{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Function.FunctionND
    ( functionND
    , viewAsDomainTuple
    , viewAsDomainTupleS
    , mkLensAsDomainTuple
    , mkLensAsDomainTupleS
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues )


functionND :: forall m . (MonadFail m,MonadFailDoc  m, NameGen m, ?typeCheckerMode :: TypeCheckerMode) => Representation m
functionND = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainFunction _
                    attrs@(FunctionAttr _ PartialityAttr_Total _)
                    innerDomainFr@(viewAsDomainTuple -> Just innerDomainFrs)
                    innerDomainTo) | all domainCanIndexMatrix innerDomainFrs = do
            innerDomainFr' <- f innerDomainFr
            innerDomainTo' <- f innerDomainTo
            return [ DomainFunction Function_ND attrs fr to
                   | fr <- innerDomainFr'
                   , to <- innerDomainTo'
                   ]
        chck _ _ = return []

        nameValues :: Domain HasRepresentation x -> Name -> Name
        nameValues = mkOutName Nothing

        downD :: TypeOf_DownD m
        downD (name, domain@(DomainFunction Function_ND
                    (FunctionAttr _ PartialityAttr_Total _)
                    (viewAsDomainTuple -> Just innerDomainFrs)
                    innerDomainTo)) | all domainCanIndexMatrix innerDomainFrs = do
            let unroll is j = foldr DomainMatrix j is
            return $ Just
                [ ( nameValues domain name
                  , unroll (map forgetRepr innerDomainFrs) innerDomainTo
                  )
                ]
        downD _ = na "{downD} FunctionND"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1
            (DomainFunction Function_ND
                (FunctionAttr sizeAttr PartialityAttr_Total jectivityAttr)
                innerDomainFr@(viewAsDomainTuple -> Just innerDomainFrs)
                innerDomainTo) | all domainCanIndexMatrix innerDomainFrs = do

            let
                kRange = case innerDomainFr of
                        DomainTuple ts  -> map fromInt [1 .. genericLength ts]
                        DomainRecord rs -> map (fromName . fst) rs
                        _ -> bug $ vcat [ "FunctionNDPartial.structuralCons"
                                        , "innerDomainFr:" <+> pretty innerDomainFr
                                        ]
                toIndex x = [ [essence| &x[&k] |] | k <- kRange ]
                index x m = make opMatrixIndexing m (toIndex x)

            let
                injectiveCons :: Expression -> m [Expression]
                injectiveCons values = do
                    tyTo <- typeOfDomain innerDomainTo
                    let canAllDiff = case tyTo of
                            TypeBool{} -> True
                            TypeInt{}  -> True
                            TypeEnum{} -> True
                            _          -> False
                    if canAllDiff
                        then do
                            (iPat, i) <- quantifiedVar
                            let valuesIndexedI = index i values
                            return $ return $ -- list
                                [essence|
                                    allDiff([ &valuesIndexedI
                                            | &iPat : &innerDomainFr
                                            ])
                                |]
                        else do
                            (iPat, i) <- quantifiedVar
                            (jPat, j) <- quantifiedVar
                            let valuesIndexedI = index i values
                            let valuesIndexedJ = index j values
                            return $ return $ -- list
                                [essence|
                                    forAll &iPat, &jPat : &innerDomainFr .
                                        &i .< &j -> &valuesIndexedI != &valuesIndexedJ
                                |]

                surjectiveCons :: Expression -> m [Expression]
                surjectiveCons values = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    let valuesIndexedJ = index j values
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &valuesIndexedJ = &i
                        |]

                jectivityCons :: Expression -> m [Expression]
                jectivityCons values = case jectivityAttr of
                    JectivityAttr_None       -> return []
                    JectivityAttr_Injective  -> injectiveCons  values
                    JectivityAttr_Surjective -> surjectiveCons values
                    JectivityAttr_Bijective  -> (++) <$> injectiveCons  values
                                                     <*> surjectiveCons values

                cardinality :: m Expression
                cardinality = do
                    (iPat, _) <- quantifiedVar
                    return [essence| sum &iPat : &innerDomainFr . 1 |]

            let innerStructuralCons values = do
                    (iPat, i) <- quantifiedVarOverDomain (forgetRepr innerDomainFr)
                    let valuesIndexedI = index i values
                    let activeZone b = [essence| forAll &iPat : &innerDomainFr . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomainTo

                    let inLoop = valuesIndexedI
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ func -> do
                refs <- downX1 func
                case refs of
                    [values] ->
                        concat <$> sequence
                            [ jectivityCons values
                            , mkSizeCons sizeAttr <$> cardinality
                            , innerStructuralCons values
                            ]
                    _ -> na "{structuralCons} FunctionND"

        structuralCons _ _ _ = na "{structuralCons} FunctionND"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainFunction Function_ND
                    (FunctionAttr _ PartialityAttr_Total _)
                    innerDomainFr@(viewAsDomainTuple -> Just innerDomainFrs)
                    innerDomainTo)
              , value@(viewConstantFunction -> Just vals)
              ) | all domainCanIndexMatrix innerDomainFrs
                , Just (_mk, inspect) <- mkLensAsDomainTuple innerDomainFr = do
            let
                check :: [Constant] -> Maybe Constant
                check indices = listToMaybe [ v
                                            | (inspect -> Just k, v) <- vals
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

                    missing <- concatForM domVals $ \ val ->
                        case active val of
                            Nothing -> return [ConstantAbstract $ AbsLitTuple $ prevIndices ++ [val]]
                            Just {} -> return []

                    unless (null missing) $
                        failDoc $ vcat [ "Some points are undefined on a total function:" <++> prettyList id "," missing
                                    , "    Function:" <+> pretty name
                                    , "    Domain:" <++> pretty domain
                                    , "    Value :" <++> pretty value
                                    ]

                    return $ ConstantAbstract $ AbsLitMatrix i
                                [ fromMaybe (bug $ "FunctionND downC" <+> pretty val) (active val)
                                | val <- domVals ]
                unrollC (i:is) prevIndices = do
                    domVals <- domainValues i
                    matrixVals <- forM domVals $ \ val ->
                        unrollC is (prevIndices ++ [val])
                    return $ ConstantAbstract $ AbsLitMatrix i matrixVals
                unrollC is prevIndices = failDoc $ vcat [ "FunctionND.up.unrollC"
                                                     , "    is         :" <+> vcat (map pretty is)
                                                     , "    prevIndices:" <+> pretty (show prevIndices)
                                                     ]

            outValues <- unrollC (map forgetRepr innerDomainFrs) []
            return $ Just
                [ ( nameValues domain name
                  , unrollD (map forgetRepr innerDomainFrs) innerDomainTo
                  , outValues
                  )
                ]

        downC _ = na "{downC} FunctionND"

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainFunction Function_ND
                                (FunctionAttr _ PartialityAttr_Total _)
                                innerDomainFr@(viewAsDomainTuple -> Just innerDomainFrs) _))

            | Just (mk, _inspect) <- mkLensAsDomainTuple innerDomainFr =

            case lookup (nameValues domain name) ctxt of
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
                        return (mk these, value)
                    return ( name
                           , ConstantAbstract $ AbsLitFunction vals
                           )
                Nothing -> failDoc $ vcat $
                    [ "(in FunctionND up)"
                    , "No value for:" <+> pretty (nameValues domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} FunctionND"

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            [inner] <- downX1 inp
            Just [(_, innerDomain)] <- downD ("SO", domain)
            innerSO downX1 inner innerDomain

viewAsDomainTuple :: Domain r x -> Maybe [Domain r x]
viewAsDomainTuple (DomainTuple doms) = Just doms
viewAsDomainTuple (DomainRecord doms) = Just (doms |> sortBy (comparing fst) |> map snd)
viewAsDomainTuple _ = Nothing

-- acts like viewAsDomainTuple, except single domains are returned as singleton tuples too
viewAsDomainTupleS :: Domain r x -> Maybe [Domain r x]
viewAsDomainTupleS (DomainTuple doms) = Just doms
viewAsDomainTupleS (DomainRecord doms) = Just (doms |> sortBy (comparing fst) |> map snd)
viewAsDomainTupleS d = Just [d]


mkLensAsDomainTuple :: Domain r x -> Maybe ( [Constant] -> Constant             -- how to make a literal
                                           , Constant -> Maybe [Constant]       -- how to inspect a literal
                                           )
mkLensAsDomainTuple (DomainTuple _) =
    Just
        ( \ vals -> ConstantAbstract (AbsLitTuple vals)
        , \ val -> case val of
                ConstantAbstract (AbsLitTuple vals) -> Just vals
                _ -> Nothing
        )
mkLensAsDomainTuple (DomainRecord doms) =
    let names = doms |> sortBy (comparing fst) |> map fst
    in  Just
        ( \ vals -> ConstantAbstract (AbsLitRecord (zip names vals))
        , \ val -> case val of
                ConstantAbstract (AbsLitRecord vals) -> Just (vals |> sortBy (comparing fst) |> map snd)
                _ -> Nothing
        )
mkLensAsDomainTuple _ = Nothing



-- acts like mkLensAsDomainTuple, except single domains are returned as singleton tuples too
mkLensAsDomainTupleS :: Domain r x -> Maybe ( [Constant] -> Constant             -- how to make a literal
                                            , Constant -> Maybe [Constant]       -- how to inspect a literal
                                            )
mkLensAsDomainTupleS (DomainTuple _) =
    Just
        ( \ vals -> ConstantAbstract (AbsLitTuple vals)
        , \ val -> case val of
                ConstantAbstract (AbsLitTuple vals) -> Just vals
                _ -> Nothing
        )
mkLensAsDomainTupleS (DomainRecord doms) =
    let names = doms |> sortBy (comparing fst) |> map fst
    in  Just
        ( \ vals -> ConstantAbstract (AbsLitRecord (zip names vals))
        , \ val -> case val of
                ConstantAbstract (AbsLitRecord vals) -> Just (vals |> sortBy (comparing fst) |> map snd)
                _ -> Nothing
        )
mkLensAsDomainTupleS _ =
    Just
        ( \ vals -> case vals of
                        [v] -> v
                        _ -> bug "mkLensAsDomainTupleS"
        , \ v -> Just [v]
        )
