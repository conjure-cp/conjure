module Conjure.Representations.Set.ExplicitVarSizeWithMarker
    ( setExplicitVarSizeWithMarker
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Lenses
import Conjure.Language.Pretty
import Conjure.Language.ZeroVal ( zeroVal )
import Conjure.Representations.Internal


setExplicitVarSizeWithMarker :: MonadFail m => Representation m
setExplicitVarSizeWithMarker = Representation chck setDown_ structuralCons setDown setUp

    where

        chck _ (DomainSet _ (SetAttrSize{}) _) = []
        chck f (DomainSet _ attrs innerDomain) = DomainSet "ExplicitVarSizeWithMarker" attrs <$> f innerDomain
        chck _ _ = []

        nameMarker name = mconcat [name, "_", "ExplicitVarSizeWithMarker", "_Marker"]
        nameValues name = mconcat [name, "_", "ExplicitVarSizeWithMarker", "_Values" ]

        getMaxSize attrs innerDomain = case attrs of
            SetAttrMaxSize x -> return x
            SetAttrMinMaxSize _ x -> return x
            _ -> bug $ "domainSize of:" <+> pretty innerDomain

        setDown_ (name, DomainSet _ attrs innerDomain) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 0) maxSize]
            return $ Just
                [ ( nameMarker name
                  , indexDomain
                  )
                , ( nameValues name
                  , DomainMatrix (forgetRepr indexDomain) innerDomain
                  )
                ]
        setDown_ _ = fail "N/A {setDown_}"

        structuralCons (name, DomainSet "ExplicitVarSizeWithMarker" attrs innerDomain@DomainInt{}) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 0) maxSize]
            return $ Just $ \ fresh ->
                let
                    marker = Reference (nameMarker name)
                                       (Just (DeclHasRepr
                                                  Find
                                                  (nameMarker name)
                                                  indexDomain))
                    values = Reference (nameValues name)
                                       (Just (DeclHasRepr
                                                  Find
                                                  (nameValues name)
                                                  (DomainMatrix (forgetRepr indexDomain) innerDomain)))

                    iName = headInf fresh

                    orderingUpToMarker =
                        make opAnd
                            [make opMapOverDomain
                                (mkLambda iName TypeInt $ \ i ->
                                    make opLt
                                        (make opIndexing values i)
                                        (make opIndexing values (make opPlus i (fromInt 1)))
                                )
                                (make opFilter
                                    (mkLambda iName TypeInt $ \ i ->
                                        make opLeq
                                            (make opPlus i (fromInt 1))
                                            marker
                                    )
                                    (Domain indexDomain)
                                )
                            ]

                    -- orderingUpToMarker i = [essence|
                    --     forAll &i : int(1..&maxSize)
                    --         , &i+1 <= &marker
                    --         . &values[i] .< refn[i+1]
                    -- |]

                    dontCareAfterMarker =
                        make opAnd
                            [make opMapOverDomain
                                (mkLambda iName TypeInt $ \ i ->
                                    make opDontCare (make opIndexing values i)
                                )
                                (make opFilter
                                    (mkLambda iName TypeInt $ \ i ->
                                        make opGt i marker
                                    )
                                    (Domain indexDomain)
                                )
                            ]

                    -- dontCareAfterMarker i = [essence|
                    --     forAll &i : int(1..&maxSize)
                    --         , &i > &marker
                    --         . dontCare(&values[&i])
                    -- |]

                in
                    [ orderingUpToMarker
                    , dontCareAfterMarker
                    ]
        structuralCons _ = fail "N/A {structuralCons}"



        setDown (name, domain@(DomainSet _ attrs innerDomain), ConstantSet constants) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 0) maxSize]
            maxSizeInt <-
                case maxSize of
                    ConstantInt x -> return x
                    _ -> fail $ vcat
                            [ "Expecting an integer for the maxSize attribute."
                            , "But got:" <+> pretty maxSize
                            , "When working on:" <+> pretty name
                            , "With domain:" <+> pretty domain
                            ]
            z <- zeroVal innerDomain
            let zeroes = replicate (maxSizeInt - length constants) z
            return $ Just
                [ ( nameMarker name
                  , indexDomain
                  , ConstantInt (length constants)
                  )
                , ( nameValues name
                  , DomainMatrix   (forgetRepr indexDomain) innerDomain
                  , ConstantMatrix (forgetRepr indexDomain) (constants ++ zeroes)
                  )
                ]
        setDown _ = fail "N/A {setDown}"

        setUp ctxt (name, domain) =
            case (lookup (nameMarker name) ctxt, lookup (nameValues name) ctxt) of
                (Just marker, Just constantMatrix) ->
                    case marker of
                        ConstantInt card ->
                            case constantMatrix of
                                ConstantMatrix _ vals ->
                                    return (name, ConstantSet (take card vals))
                                _ -> fail $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty (nameValues name)
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty domain
                                        ]
                        _ -> fail $ vcat
                                [ "Expecting an integer literal for:" <+> pretty (nameMarker name)
                                , "But got:" <+> pretty marker
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                (Nothing, _) -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameMarker name)
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

