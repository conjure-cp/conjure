{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Set.ExplicitVarSizeWithFlags ( setExplicitVarSizeWithFlags ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.TH
import Conjure.Language.DomainSize
import Conjure.Language.Pretty
import Conjure.Language.ZeroVal ( zeroVal )
import Conjure.Representations.Internal
import Conjure.Representations.Common


setExplicitVarSizeWithFlags :: MonadFail m => Representation m
setExplicitVarSizeWithFlags = Representation chck downD structuralCons downC up

    where

        chck _ (DomainSet _ (SetAttr SizeAttrSize{}) _) = []
        chck f (DomainSet _ attrs innerDomain) =
            DomainSet "ExplicitVarSizeWithFlags" attrs <$> f innerDomain
        chck _ _ = []

        nameFlag   name = mconcat [name, "_", "ExplicitVarSizeWithFlags", "_Flags"]
        nameValues name = mconcat [name, "_", "ExplicitVarSizeWithFlags", "_Values"]

        getMaxSize attrs innerDomain = case attrs of
            SizeAttrMaxSize x -> return x
            SizeAttrMinMaxSize _ x -> return x
            _ -> domainSizeOf innerDomain


        downD (name, DomainSet _ (SetAttr attrs) innerDomain) = do
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
        downD _ = fail "N/A {downD}"

        structuralCons (name, domain@(DomainSet "ExplicitVarSizeWithFlags" (SetAttr attrs) innerDomain)) = do
            [flags, values] <- rDownX setExplicitVarSizeWithFlags name domain
            maxSize         <- getMaxSize attrs innerDomain
            let
                orderingWhenFlagged fresh = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0) TypeInt
                    in
                        [essence|
                            forAll &iPat : int(1..&maxSize-1) , &flags[&i+1] . &values[&i] < &values[&i+1]
                        |]

                dontCareWhenNotFlagged fresh = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0) TypeInt
                    in
                        [essence|
                            forAll &iPat : int(1..&maxSize) , &flags[&i] = false . dontCare(&values[&i])
                        |]

                flagsToTheLeft fresh = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0) TypeInt
                    in
                        [essence|
                            forAll &iPat : int(1..&maxSize-1) , &flags[&i+1] . &flags[&i]
                        |]

                cardinality fresh =
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0) TypeInt
                    in
                        [essence| sum &iPat : int(1..&maxSize) . toInt(&flags[&i]) |]


            return $ Just $ \ fresh -> concat [ orderingWhenFlagged fresh
                                              , dontCareWhenNotFlagged fresh
                                              , flagsToTheLeft fresh
                                              , mkSizeCons attrs (cardinality fresh)
                                              ]

        structuralCons _ = fail "N/A {structuralCons}"

        downC (name, domain@(DomainSet _ (SetAttr attrs) innerDomain), ConstantSet constants) = do
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
        downC _ = fail "N/A {downC}"

        up ctxt (name, domain) =
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

