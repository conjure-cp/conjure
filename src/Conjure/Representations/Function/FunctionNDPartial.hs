{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Representations.Function.FunctionNDPartial ( functionNDPartial ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Language.ZeroVal ( zeroVal, EnumerateDomain )
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues )
import Conjure.Representations.Function.FunctionND ( viewAsDomainTuple, mkLensAsDomainTuple )


functionNDPartial :: forall m .
    MonadFail m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Representation m
functionNDPartial = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainFunction _
                    attrs@(FunctionAttr _ PartialityAttr_Partial _)
                    innerDomainFr@(viewAsDomainTuple -> Just innerDomainFrs)
                    innerDomainTo) | all domainCanIndexMatrix innerDomainFrs = do
            innerDomainFr' <- f innerDomainFr
            innerDomainTo' <- f innerDomainTo
            return [ DomainFunction Function_NDPartial attrs fr to
                   | fr <- innerDomainFr'
                   , to <- innerDomainTo'
                   ]
        chck _ _ = return []

        nameFlags  = mkOutName (Just "Flags")
        nameValues = mkOutName (Just "Values")

        downD :: TypeOf_DownD m
        downD (name, domain@(DomainFunction Function_NDPartial
                    (FunctionAttr _ PartialityAttr_Partial _)
                    (viewAsDomainTuple -> Just innerDomainFrs)
                    innerDomainTo)) | all domainCanIndexMatrix innerDomainFrs = do
            let unroll is j = foldr DomainMatrix j is
            return $ Just
                [ ( nameFlags domain name
                  , unroll (map forgetRepr innerDomainFrs) DomainBool
                  )
                , ( nameValues domain name
                  , unroll (map forgetRepr innerDomainFrs) innerDomainTo
                  )
                ]
        downD _ = na "{downD} FunctionNDPartial"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1
            (DomainFunction Function_NDPartial
                (FunctionAttr sizeAttr PartialityAttr_Partial jectivityAttr)
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
                injectiveCons :: Expression -> Expression -> m [Expression]
                injectiveCons flags values = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    let flagsIndexedI  = index i flags
                    let valuesIndexedI = index i values
                    let flagsIndexedJ  = index j flags
                    let valuesIndexedJ = index j values
                    return $ return $ -- list
                        [essence|
                            and([ &valuesIndexedI != &valuesIndexedJ
                                | &iPat : &innerDomainFr
                                , &jPat : &innerDomainFr
                                , &i .< &j
                                , &flagsIndexedI
                                , &flagsIndexedJ
                                ])
                        |]

                surjectiveCons :: Expression -> Expression -> m [Expression]
                surjectiveCons flags values = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar

                    let flagsIndexed  = index j flags
                    let valuesIndexed = index j values
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &flagsIndexed /\ &valuesIndexed = &i
                        |]

                jectivityCons :: Expression -> Expression -> m [Expression]
                jectivityCons flags values = case jectivityAttr of
                    JectivityAttr_None       -> return []
                    JectivityAttr_Injective  -> injectiveCons  flags values
                    JectivityAttr_Surjective -> surjectiveCons flags values
                    JectivityAttr_Bijective  -> (++) <$> injectiveCons  flags values
                                                     <*> surjectiveCons flags values

                cardinality :: Expression -> m Expression
                cardinality flags = do
                    (iPat, i) <- quantifiedVar
                    let flagsIndexed  = index i flags
                    return [essence| sum &iPat : &innerDomainFr . toInt(&flagsIndexed) |]

                dontCareInactives :: Expression -> Expression -> m [Expression]
                dontCareInactives flags values = do
                    (iPat, i) <- quantifiedVar
                    let flagsIndexed  = index i flags
                    let valuesIndexed = index i values
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : &innerDomainFr . &flagsIndexed = false ->
                                dontCare(&valuesIndexed)
                        |]

            let innerStructuralCons flags values = do
                    (iPat, i) <- quantifiedVarOverDomain (forgetRepr innerDomainFr)
                    let flagsIndexed  = index i flags
                    let valuesIndexed = index i values
                    let activeZone b = [essence| forAll &iPat : &innerDomainFr . &flagsIndexed -> &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomainTo

                    let inLoop = valuesIndexed
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ rel -> do
                refs <- downX1 rel
                case refs of
                    [flags,values] ->
                        concat <$> sequence
                            [ jectivityCons flags values
                            , dontCareInactives flags values
                            , mkSizeCons sizeAttr <$> cardinality flags
                            , innerStructuralCons flags values
                            ]
                    _ -> na "{structuralCons} FunctionNDPartial"

        structuralCons _ _ _ = na "{structuralCons} FunctionNDPartial"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainFunction Function_NDPartial
                    (FunctionAttr _ PartialityAttr_Partial _)
                    innerDomainFr@(viewAsDomainTuple -> Just innerDomainFrs)
                    innerDomainTo)
              , ConstantAbstract (AbsLitFunction vals)
              ) | all domainCanIndexMatrix innerDomainFrs
                , Just (_mk, inspect) <- mkLensAsDomainTuple innerDomainFr = do
            z <- zeroVal innerDomainTo
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
                        -> m (Constant, Constant)
                unrollC [i] prevIndices = do
                    domVals <- domainValues i
                    let active val = check $ prevIndices ++ [val]
                    return ( ConstantAbstract $ AbsLitMatrix i
                                [ case active val of
                                    Nothing -> ConstantBool False
                                    Just{}  -> ConstantBool True
                                | val <- domVals ]
                           , ConstantAbstract $ AbsLitMatrix i
                                [ fromMaybe z (active val)
                                | val <- domVals ]
                           )
                unrollC (i:is) prevIndices = do
                    domVals <- domainValues i
                    (matrixFlags, matrixVals) <- fmap unzip $ forM domVals $ \ val ->
                        unrollC is (prevIndices ++ [val])
                    return ( ConstantAbstract $ AbsLitMatrix i matrixFlags
                           , ConstantAbstract $ AbsLitMatrix i matrixVals
                           )
                unrollC is prevIndices = fail $ vcat [ "FunctionNDPartial.up.unrollC"
                                                     , "    is         :" <+> vcat (map pretty is)
                                                     , "    prevIndices:" <+> pretty (show prevIndices)
                                                     ]

            (outFlags, outValues) <- unrollC (map forgetRepr innerDomainFrs) []
            return $ Just
                [ ( nameFlags domain name
                  , unrollD (map forgetRepr innerDomainFrs) DomainBool
                  , outFlags
                  )
                , ( nameValues domain name
                  , unrollD (map forgetRepr innerDomainFrs) innerDomainTo
                  , outValues
                  )
                ]

        downC _ = na "{downC} FunctionNDPartial"

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainFunction Function_NDPartial
                                (FunctionAttr _ PartialityAttr_Partial _)
                                innerDomainFr@(viewAsDomainTuple -> Just innerDomainFrs) _))

            | Just (mk, _inspect) <- mkLensAsDomainTuple innerDomainFr =

            case (lookup (nameFlags domain name) ctxt, lookup (nameValues domain name) ctxt) of
                (Just flagMatrix, Just valuesMatrix) -> do
                    let
                        allIndices :: (MonadFail m, Pretty r) => [Domain r Constant] -> m [[Constant]]
                        allIndices = fmap sequence . mapM domainValues

                        index :: MonadFail m => Constant -> [Constant] -> m Constant
                        index m [] = return m
                        index (ConstantAbstract (AbsLitMatrix indexDomain vals)) (i:is) = do
                            froms <- domainValues indexDomain
                            case lookup i (zip froms vals) of
                                Nothing -> fail "Value not found. FunctionNDPartial.up.index"
                                Just v  -> index v is
                        index m is = bug ("FunctionNDPartial.up.index" <+> pretty m <+> pretty (show is))

                    indices  <- allIndices innerDomainFrs
                    vals     <- forM indices $ \ these -> do
                        flag  <- index flagMatrix   these
                        value <- index valuesMatrix these
                        case viewConstantBool flag of
                            Just False -> return Nothing
                            Just True  -> return (Just (mk these, value))
                            _ -> fail $ vcat
                                [ "Expecting a boolean literal, but got:" <++> pretty flag
                                , "                           , and    :" <+> pretty value
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                    return ( name
                           , ConstantAbstract $ AbsLitFunction $ catMaybes vals
                           )

                (Nothing, _) -> fail $ vcat $
                    [ "(in FunctionNDPartial up 1)"
                    , "No value for:" <+> pretty (nameFlags domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> fail $ vcat $
                    [ "(in FunctionNDPartial up 2)"
                    , "No value for:" <+> pretty (nameValues domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} FunctionNDPartial"

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering _innerSO _downX1 inp _name _domain =
            return inp
