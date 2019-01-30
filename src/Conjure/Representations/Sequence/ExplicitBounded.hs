{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Sequence.ExplicitBounded ( sequenceExplicitBounded ) where

-- conjure
import Conjure.Prelude
import Conjure.Language
import Conjure.Language.ZeroVal ( zeroVal, EnumerateDomain )
import Conjure.Language.TypeOf ( typeOf )
import Conjure.Representations.Internal
import Conjure.Representations.Common


sequenceExplicitBounded :: forall m .
    MonadFail m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Representation m
sequenceExplicitBounded = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainSequence _ attrs@(SequenceAttr sizeAttr _) innerDomain) | hasMaxSize sizeAttr =
            map (DomainSequence Sequence_ExplicitBounded attrs) <$> f innerDomain
        chck _ _ = return []

        nameMarker = mkOutName (Just "Length")
        nameValues = mkOutName (Just "Values")

        hasMaxSize SizeAttr_Size{} = True
        hasMaxSize SizeAttr_MaxSize{} = True
        hasMaxSize SizeAttr_MinMaxSize{} = True
        hasMaxSize _ = False

        getMaxSize (SizeAttr_MaxSize x) = return x
        getMaxSize (SizeAttr_MinMaxSize _ x) = return x
        getMaxSize _ = fail "Unknown maxSize"

        downD :: TypeOf_DownD m
        downD (name, domain@(DomainSequence
                        Sequence_ExplicitBounded
                        (SequenceAttr (SizeAttr_Size size) _)
                        innerDomain)) =
            return $ Just
                [ ( nameMarker domain name
                  , DomainInt TagInt [RangeBounded size size]
                  )
                , ( nameValues domain name
                  , DomainMatrix
                      (DomainInt TagInt [RangeBounded 1 size])
                      innerDomain
                  ) ]
        downD (name, domain@(DomainSequence
                        Sequence_ExplicitBounded
                        (SequenceAttr sizeAttr _)
                        innerDomain)) = do
            maxSize <- getMaxSize sizeAttr
            return $ Just
                [ ( nameMarker domain name
                  , DomainInt TagInt [RangeBounded 0 maxSize]
                  )
                , ( nameValues domain name
                  , DomainMatrix
                      (DomainInt TagInt [RangeBounded 1 maxSize])
                      innerDomain
                  ) ]
        downD _ = na "{downD} ExplicitBounded"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainSequence Sequence_ExplicitBounded
                                        (SequenceAttr (SizeAttr_Size size) jectivityAttr) innerDomain) = do
            let
                injectiveCons :: Expression -> m [Expression]
                injectiveCons values = do
                    innerType <- typeOf innerDomain
                    case innerType of
                      TypeInt _ -> do
                            return $ return $ -- list
                                [essence| allDiff(&values) |]

                      _ ->  do
                            (iPat, i) <- quantifiedVar
                            (jPat, j) <- quantifiedVar
                            return $ return $ -- list
                                [essence|
                                    and([ &values[&i] != &values[&j]
                                        | &iPat : int(1..&size)
                                        , &jPat : int(1..&size)
                                        , &i .< &j
                                        ])
                                |]

                surjectiveCons :: Expression -> m [Expression]
                surjectiveCons values = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : &innerDomain .
                                exists &jPat : int(1..&size) .
                                    &values[&j] = &i
                        |]

                jectivityCons :: Expression -> m [Expression]
                jectivityCons values = case jectivityAttr of
                    JectivityAttr_None       -> return []
                    JectivityAttr_Injective  -> injectiveCons  values
                    JectivityAttr_Surjective -> surjectiveCons values
                    JectivityAttr_Bijective  -> (++) <$> injectiveCons  values
                                                     <*> surjectiveCons values

            let innerStructuralCons values = do
                    (iPat, i) <- quantifiedVarOverDomain [essenceDomain| int(1..&size) |]
                    let activeZone b = [essence| forAll &iPat : int(1..&size) . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &values[&i] |]
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ sequ -> do
                refs <- downX1 sequ
                case refs of
                    [_marker,values] ->
                        concat <$> sequence
                            [ jectivityCons       values
                            , innerStructuralCons values
                            ]
                    _ -> na "{structuralCons} ExplicitBounded"
        structuralCons f downX1 (DomainSequence Sequence_ExplicitBounded (SequenceAttr sizeAttr jectivityAttr) innerDomain) = do
            maxSize <- getMaxSize sizeAttr
            let injectiveCons marker values = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            and([ &values[&i] != &values[&j]
                                | &iPat : int(1..&maxSize)
                                , &jPat : int(1..&maxSize)
                                , &i .< &j
                                , &i <= &marker
                                , &j <= &marker
                                ])
                        |]

            let surjectiveCons marker values = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : &innerDomain .
                                exists &jPat : int(1..&maxSize) .
                                    (&j <= &marker) /\ &values[&j] = &i
                        |]

            let jectivityCons marker values = case jectivityAttr of
                    JectivityAttr_None       -> return []
                    JectivityAttr_Injective  -> injectiveCons  marker values
                    JectivityAttr_Surjective -> surjectiveCons marker values
                    JectivityAttr_Bijective  -> (++) <$> injectiveCons  marker values
                                                     <*> surjectiveCons marker values

            let dontCareAfterMarker marker values = do
                    (iPat, i) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : int(1..&maxSize) . &i > &marker ->
                                dontCare(&values[&i])
                        |]

            let innerStructuralCons marker values = do
                    (iPat, i) <- quantifiedVarOverDomain [essenceDomain| int(1..&maxSize) |]
                    let activeZone b = [essence| forAll &iPat : int(1..&maxSize) . &i <= &marker -> &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &values[&i] |]
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ sequ -> do
                refs <- downX1 sequ
                case refs of
                    [marker,values] ->
                        concat <$> sequence
                            [ dontCareAfterMarker marker values
                            , return (mkSizeCons sizeAttr marker)
                            , jectivityCons       marker values
                            , innerStructuralCons marker values
                            ]
                    _ -> na "{structuralCons} ExplicitBounded"

        structuralCons _ _ _ = na "{structuralCons} ExplicitBounded"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainSequence _ (SequenceAttr (SizeAttr_Size size) _) innerDomain)
              , ConstantAbstract (AbsLitSequence constants)
              ) =
            return $ Just
                [ ( nameMarker domain name
                  , DomainInt TagInt [RangeBounded size size]
                  , ConstantInt TagInt (genericLength constants)
                  )
                , ( nameValues domain name
                  , DomainMatrix (DomainInt TagInt [RangeBounded 1 size]) innerDomain
                  , ConstantAbstract $ AbsLitMatrix (DomainInt TagInt [RangeBounded 1 size]) constants
                  )
                ]
        downC ( name
              , domain@(DomainSequence _ (SequenceAttr sizeAttr _) innerDomain)
              , ConstantAbstract (AbsLitSequence constants)
              ) = do
            maxSize <- getMaxSize sizeAttr
            let indexDomain i = mkDomainIntB (fromInt i) maxSize
            maxSizeInt <-
                case maxSize of
                    ConstantInt _ x -> return x
                    _ -> fail $ vcat
                            [ "Expecting an integer for the maxSize attribute."
                            , "But got:" <+> pretty maxSize
                            , "When working on:" <+> pretty name
                            , "With domain:" <+> pretty domain
                            ]
            z <- zeroVal innerDomain
            let zeroes = replicate (fromInteger (maxSizeInt - genericLength constants)) z
            return $ Just
                [ ( nameMarker domain name
                  , defRepr (indexDomain 0)
                  , ConstantInt TagInt (genericLength constants)
                  )
                , ( nameValues domain name
                  , DomainMatrix (indexDomain 1) innerDomain
                  , ConstantAbstract $ AbsLitMatrix (indexDomain 1) (constants ++ zeroes)
                  )
                ]
        downC (name, domain, constant) = na $ vcat [ "{downC} ExplicitBounded"
                                                   , "name    :" <+> pretty name
                                                   , "domain  :" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain) =
            case (lookup (nameMarker domain name) ctxt, lookup (nameValues domain name) ctxt) of
                (Just marker, Just constantMatrix) ->
                    case marker of
                        ConstantInt _ card ->
                            case viewConstantMatrix constantMatrix of
                                Just (_, vals) ->
                                    return (name, ConstantAbstract (AbsLitSequence (genericTake card vals)))
                                _ -> fail $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty (nameValues domain name)
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty domain
                                        ]
                        _ -> fail $ vcat
                                [ "Expecting an integer literal for:" <+> pretty (nameMarker domain name)
                                , "But got:" <+> pretty marker
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                (Nothing, _) -> fail $ vcat $
                    [ "(in Sequence ExplicitBounded up 1)"
                    , "No value for:" <+> pretty (nameMarker domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> fail $ vcat $
                    [ "(in Sequence ExplicitBounded up 2)"
                    , "No value for:" <+> pretty (nameValues domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering _innerSO _downX1 inp _name _domain =
            return inp
