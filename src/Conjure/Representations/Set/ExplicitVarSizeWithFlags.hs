module Conjure.Representations.Set.ExplicitVarSizeWithFlags
    ( setExplicitVarSizeWithFlags
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Lenses
import Conjure.Language.TypeOf
import Conjure.Language.Pretty
import Conjure.Language.ZeroVal ( zeroVal )
import Conjure.Representations.Internal



setExplicitVarSizeWithFlags :: MonadFail m => Representation m
setExplicitVarSizeWithFlags = Representation chck setDown_ structuralCons setDown setUp

    where

        chck _ (DomainSet _ (SetAttrSize{}) _) = []
        chck f (DomainSet _ attrs innerDomain) = DomainSet "ExplicitVarSizeWithFlags" attrs <$> f innerDomain
        chck _ _ = []

        nameFlag   name = mconcat [name, "_", "ExplicitVarSizeWithFlags", "_Flags"]
        nameValues name = mconcat [name, "_", "ExplicitVarSizeWithFlags", "_Values"]

        getMaxSize attrs innerDomain = case attrs of
            SetAttrMaxSize x -> return x
            SetAttrMinMaxSize _ x -> return x
            _ -> bug $ "domainSize of:" <+> pretty innerDomain


        setDown_ (name, DomainSet _ attrs innerDomain) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]
            return $ Just
                [ ( nameFlag name
                  , DomainMatrix (forgetRepr indexDomain) DomainBool
                  )
                , ( nameValues name
                  , DomainMatrix (forgetRepr indexDomain) innerDomain
                  )
                ]
        setDown_ _ = fail "N/A {setDown_}"

        structuralCons (name, DomainSet "ExplicitVarSizeWithFlags" attrs innerDomain) = do
            innerType <- typeOf innerDomain
            maxSize   <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]
            return $ Just $ \ fresh ->
                let
                    flags = Reference (nameFlag name)
                                       (Just (DeclHasRepr
                                                  Find
                                                  (nameFlag name)
                                                  (DomainMatrix (forgetRepr indexDomain) DomainBool)))
                    values = Reference (nameValues name)
                                       (Just (DeclHasRepr
                                                  Find
                                                  (nameValues name)
                                                  (DomainMatrix (forgetRepr indexDomain) innerDomain)))

                    iName = headInf fresh

                    -- forAll i : int(1..&mx-1) , flags[i+1] . values[i] .< values[i+1]
                    orderingWhenFlagged =
                        make opAnd
                            [make opMapOverDomain
                                (mkLambda iName innerType $ \ i ->
                                    make opLt
                                        (make opIndexing values i)
                                        (make opIndexing values (make opPlus i (fromInt 1)))
                                )
                                (make opFilter
                                    (mkLambda iName innerType $ \ i ->
                                        make opIndexing flags (make opPlus i (fromInt 1))
                                    )
                                    (Domain $ DomainInt [RangeBounded (fromInt 1) (make opMinus maxSize (fromInt 1))])
                                )
                            ]

                    -- forAll i : int(1..&mx ) , !flags[i] . dontCare(values[i])
                    dontCareWhenNotFlagged =
                        make opAnd
                            [make opMapOverDomain
                                (mkLambda iName innerType $ \ i ->
                                    make opDontCare (make opIndexing values i)
                                )
                                (make opFilter
                                    (mkLambda iName innerType $ \ i ->
                                        make opEq
                                            (make opIndexing flags i)
                                            (fromBool False)
                                    )
                                    (Domain indexDomain)
                                )
                            ]

                    -- forAll i : int(1..&mx-1) , flags[i+1] . flags[i]
                    flagsToTheLeft =
                        make opAnd
                            [make opMapOverDomain
                                (mkLambda iName innerType $ \ i ->
                                    make opIndexing flags i
                                )
                                (make opFilter
                                    (mkLambda iName innerType $ \ i ->
                                        make opIndexing flags (make opPlus i (fromInt 1))
                                    )
                                    (Domain $ DomainInt [RangeBounded (fromInt 1) (make opMinus maxSize (fromInt 1))])
                                )
                            ]

                in
                    [ orderingWhenFlagged
                    , dontCareWhenNotFlagged
                    , flagsToTheLeft
                    ]
        structuralCons _ = fail "N/A {structuralCons}"

        setDown (name, domain@(DomainSet _ attrs innerDomain), ConstantSet constants) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]

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

            let trues  = replicate (length constants)              (ConstantBool True)
            let falses = replicate (maxSizeInt - length constants) (ConstantBool False)

            return $ Just
                [ ( nameFlag name
                  , DomainMatrix   (forgetRepr indexDomain) DomainBool
                  , ConstantMatrix (forgetRepr indexDomain) (trues ++ falses)
                  )
                , ( nameValues name
                  , DomainMatrix   (forgetRepr indexDomain) innerDomain
                  , ConstantMatrix (forgetRepr indexDomain) (constants ++ zeroes)
                  )
                ]
        setDown _ = fail "N/A {setDown}"

        setUp ctxt (name, domain) =
            case (lookup (nameFlag name) ctxt, lookup (nameValues name) ctxt) of
                (Just flagMatrix, Just constantMatrix) ->
                    case flagMatrix of
                        -- TODO: check if indices match
                        ConstantMatrix _ flags ->
                            case constantMatrix of
                                ConstantMatrix _ vals ->
                                    return (name, ConstantSet [ v
                                                              | (i,v) <- zip flags vals
                                                              , i == ConstantBool True
                                                              ] )
                                _ -> fail $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty (nameValues name)
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty domain
                                        ]
                        _ -> fail $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (nameFlag name)
                                , "But got:" <+> pretty flagMatrix
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                (Nothing, _) -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameFlag name)
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

