{-# LANGUAGE FlexibleContexts #-}

module Conjure.Representations.Set.ExplicitVarSizeWithMarker
    ( setExplicitVarSizeWithMarker
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.ZeroVal ( zeroVal )
import Conjure.Representations.Internal


setExplicitVarSizeWithMarker :: (Applicative m, MonadError Doc m) => Representation m
setExplicitVarSizeWithMarker = Representation chck setDown_ setDown setUp

    where

        chck _ (DomainSet _ (SetAttrSize{}) _) = []
        chck f (DomainSet _ attrs innerDomain) = DomainSet "ExplicitVarSizeWithMarker" attrs <$> f innerDomain
        chck _ _ = []

        nameMarker name = mconcat [name, "_", "ExplicitVarSizeWithMarker", "_Marker"]
        nameMain   name = mconcat [name, "_", "ExplicitVarSizeWithMarker", "_Main"  ]

        getMaxSize attrs innerDomain = case attrs of
            SetAttrMaxSize x -> return x
            SetAttrMinMaxSize _ x -> return x
            _ -> bug $ "domainSize of:" <+> pretty innerDomain

        setDown_ (name, DomainSet _ attrs innerDomain) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]
            return $ Just
                [ ( nameMarker name
                  , indexDomain
                  )
                , ( nameMain name
                  , DomainMatrix (forgetRepr indexDomain) innerDomain
                  )
                ]
        setDown_ _ = throwError "N/A {setDown_}"

        setDown (name, domain@(DomainSet _ attrs innerDomain), ConstantSet constants) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]
            maxSizeInt <-
                case maxSize of
                    ConstantInt x -> return x
                    _ -> throwError $ vcat
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
                , ( nameMain name
                  , DomainMatrix   (forgetRepr indexDomain) innerDomain
                  , ConstantMatrix (forgetRepr indexDomain) (constants ++ zeroes)
                  )
                ]
        setDown _ = throwError "N/A {setDown}"

        setUp ctxt (name, domain) =
            case (lookup (nameMarker name) ctxt, lookup (nameMain name) ctxt) of
                (Just marker, Just constantMatrix) ->
                    case marker of
                        ConstantInt card ->
                            case constantMatrix of
                                ConstantMatrix _ vals ->
                                    return (name, ConstantSet (take card vals))
                                _ -> throwError $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty (nameMain name)
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty domain
                                        ]
                        _ -> throwError $ vcat
                                [ "Expecting an integer literal for:" <+> pretty (nameMarker name)
                                , "But got:" <+> pretty marker
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                (Nothing, _) -> throwError $ vcat $
                    [ "No value for:" <+> pretty (nameMarker name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> throwError $ vcat $
                    [ "No value for:" <+> pretty (nameMain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)

