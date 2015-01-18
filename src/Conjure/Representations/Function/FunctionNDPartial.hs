{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Function.FunctionNDPartial ( functionNDPartial ) where

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


functionNDPartial :: MonadFail m => Representation m
functionNDPartial = Representation chck downD structuralCons downC up

    where

        chck f (DomainFunction _
                    attrs@(FunctionAttr _ PartialityAttr_Partial _)
                    innerDomainFr@(DomainTuple innerDomainFrs)
                    innerDomainTo) | all domainCanIndexMatrix innerDomainFrs =
            DomainFunction "FunctionNDPartial" attrs
                <$> f innerDomainFr
                <*> f innerDomainTo
        chck _ _ = []

        nameFlags  name = mconcat [name, "_", "FunctionNDPartial_Flags"]
        nameValues name = mconcat [name, "_", "FunctionNDPartial_Values"]

        downD (name, DomainFunction "FunctionNDPartial"
                    (FunctionAttr _ PartialityAttr_Partial _)
                    (DomainTuple innerDomainFrs')
                    innerDomainTo) | all domainCanIndexMatrix innerDomainFrs' = do
            innerDomainFrs <- mapM toIntDomain innerDomainFrs'
            let unroll is j = foldr DomainMatrix j is
            return $ Just
                [ ( nameFlags name
                  , unroll (map forgetRepr innerDomainFrs) DomainBool
                  )
                , ( nameValues name
                  , unroll (map forgetRepr innerDomainFrs) innerDomainTo
                  )
                ]
        downD _ = na "{downD} FunctionNDPartial"

        -- FIX: inner structural constraints
        structuralCons _ downX1
            (DomainFunction "FunctionNDPartial"
                (FunctionAttr sizeAttr PartialityAttr_Partial jectivityAttr)
                (DomainTuple innerDomainFrs')
                innerDomainTo) | all domainCanIndexMatrix innerDomainFrs' = do
            innerDomainFrs    <- mapM toIntDomain innerDomainFrs'
            let innerDomainFr =  DomainTuple innerDomainFrs

            let
                frArity = length innerDomainFrs

                index x m 1     = make opIndexing m                     (make opIndexing x 1)
                index x m arity = make opIndexing (index x m (arity-1)) (make opIndexing x (fromInt arity))

            let injectiveCons fresh flags values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)

                        flagsIndexedI  = index i flags  frArity
                        valuesIndexedI = index i values frArity
                        flagsIndexedJ  = index j flags  frArity
                        valuesIndexedJ = index j values frArity
                    in
                        [essence|
                            and([ &valuesIndexedI != &valuesIndexedJ
                                | &iPat : &innerDomainFr
                                , &jPat : &innerDomainFr
                                , &i != &j
                                , &flagsIndexedI
                                , &flagsIndexedJ
                                ])
                        |]

            let surjectiveCons fresh flags values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)

                        flagsIndexed  = index j flags  frArity
                        valuesIndexed = index j values frArity
                    in
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &flagsIndexed /\ &valuesIndexed = &i
                        |]

            let jectivityCons fresh flags values = case jectivityAttr of
                    JectivityAttr_None       -> []
                    JectivityAttr_Injective  -> injectiveCons  fresh flags values
                    JectivityAttr_Surjective -> surjectiveCons fresh flags values
                    JectivityAttr_Bijective  -> injectiveCons  fresh flags values
                                             ++ surjectiveCons fresh flags values

            let cardinality fresh flags =
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        flagsIndexed  = index i flags  frArity
                    in
                        [essence| sum &iPat : &innerDomainFr . toInt(&flagsIndexed) |]

            let dontCareInactives fresh flags values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)

                        flagsIndexed  = index i flags  frArity
                        valuesIndexed = index i values frArity
                    in
                        [essence|
                            forAll &iPat : &innerDomainFr . &flagsIndexed = false ->
                                dontCare(&valuesIndexed)
                        |]

            return $ \ fresh rel -> do
                refs <- downX1 rel
                case refs of
                    [flags,values] -> return $ concat [ jectivityCons fresh flags values
                                                      , dontCareInactives fresh flags values
                                                      , mkSizeCons sizeAttr (cardinality fresh flags)
                                                      ]
                    _ -> na "{structuralCons} FunctionNDPartial"

        structuralCons _ _ _ = na "{structuralCons} FunctionNDPartial"

        downC ( name
              , DomainFunction "FunctionNDPartial"
                    (FunctionAttr _ PartialityAttr_Partial _)
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
                        -> m (Constant, Constant)
                unrollC [(i,i')] prevIndices = do
                    domVals <- domainValues i'
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
                unrollC ((i,i'):is) prevIndices = do
                    domVals <- domainValues i'
                    (matrixFlags, matrixVals) <- liftM unzip $ forM domVals $ \ val ->
                        unrollC is (prevIndices ++ [val])
                    return ( ConstantAbstract $ AbsLitMatrix i matrixFlags
                           , ConstantAbstract $ AbsLitMatrix i matrixVals
                           )
                unrollC is prevIndices = fail $ vcat [ "FunctionNDPartial.up.unrollC"
                                                     , "    is         :" <+> vcat (map pretty is)
                                                     , "    prevIndices:" <+> pretty (show prevIndices)
                                                     ]

            (outFlags, outValues) <- unrollC (zip (map forgetRepr innerDomainFrs)
                                                  (map forgetRepr innerDomainFrs')) []
            return $ Just
                [ ( nameFlags name
                  , unrollD (map forgetRepr innerDomainFrs) DomainBool
                  , outFlags
                  )
                , ( nameValues name
                  , unrollD (map forgetRepr innerDomainFrs) innerDomainTo
                  , outValues
                  )
                ]

        downC _ = na "{downC} FunctionNDPartial"

        up ctxt (name, domain@(DomainFunction "FunctionNDPartial"
                                (FunctionAttr _ PartialityAttr_Partial _)
                                (DomainTuple innerDomainFrs') _)) = do

            innerDomainFrs <- fmap (fmap e2c) <$> mapM toIntDomain (fmap (fmap Constant) innerDomainFrs')

            case (lookup (nameFlags name) ctxt, lookup (nameValues name) ctxt) of
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
                        index m is = bug ("RelationAsMatrix.up.index" <+> pretty m <+> pretty (show is))

                    indices' <- allIndices innerDomainFrs'
                    indices  <- allIndices innerDomainFrs
                    vals     <- forM (zip indices indices') $ \ (these, these') -> do
                        flag  <- index flagMatrix   these
                        value <- index valuesMatrix these
                        case flag of
                            ConstantBool False -> return Nothing
                            ConstantBool True  -> return (Just (ConstantAbstract (AbsLitTuple these'), value))
                            _ -> fail $ vcat
                                [ "Expecting a boolean literal, but got:" <+> pretty flag
                                , "                           , and    :" <+> pretty value
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                    return ( name
                           , ConstantAbstract $ AbsLitFunction $ catMaybes vals
                           )

                (Nothing, _) -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameFlags name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameValues name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} FunctionNDPartial"

