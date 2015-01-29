{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Set.ExplicitVarSizeWithMarker ( setExplicitVarSizeWithMarker ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.DomainSize
import Conjure.Language.Pretty
import Conjure.Language.ZeroVal ( zeroVal )
import Conjure.Representations.Internal
import Conjure.Representations.Common


setExplicitVarSizeWithMarker :: forall m . MonadFail m => Representation m
setExplicitVarSizeWithMarker = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck m
        chck _ (DomainSet _ (SetAttr SizeAttr_Size{}) _) = []
        chck f (DomainSet _ attrs innerDomain) = DomainSet "ExplicitVarSizeWithMarker" attrs <$> f innerDomain
        chck _ _ = []

        nameMarker name = mconcat [name, "_", "ExplicitVarSizeWithMarker", "_Marker"]
        nameValues name = mconcat [name, "_", "ExplicitVarSizeWithMarker", "_Values" ]

        getMaxSize attrs innerDomain = case attrs of
            SizeAttr_MaxSize x -> return x
            SizeAttr_MinMaxSize _ x -> return x
            _ -> domainSizeOf innerDomain

        downD :: TypeOf_DownD m
        downD (name, DomainSet _ (SetAttr attrs) innerDomain) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain i = mkDomainIntB (fromInt i) maxSize
            return $ Just
                [ ( nameMarker name
                  , defRepr "Set.ExplicitVarSizeWithMarker.downD" (indexDomain 0)
                  )
                , ( nameValues name
                  , DomainMatrix (indexDomain 1) innerDomain
                  )
                ]
        downD _ = na "{downD} ExplicitVarSizeWithMarker"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainSet "ExplicitVarSizeWithMarker" (SetAttr attrs) innerDomain) = do
            maxSize <- getMaxSize attrs innerDomain
            let
                orderingUpToMarker fresh marker values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                    in
                        [essence|
                            forAll &iPat : int(1..&maxSize-1) . &i + 1 <= &marker ->
                                &values[&i] < &values[&i+1]
                        |]

                dontCareAfterMarker fresh marker values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                    in
                        [essence|
                            forAll &iPat : int(1..&maxSize) . &i > &marker ->
                                dontCare(&values[&i])
                        |]

                innerStructuralCons fresh marker values = do
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    let activeZone b = [essence| forAll &iPat : int(1..&maxSize) . &i <= &marker -> &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &values[&i] |]
                    outs <- innerStructuralConsGen (tail fresh) inLoop
                    return (map activeZone outs)

            return $ \ fresh set -> do
                refs <- downX1 set
                case refs of
                    [marker,values] -> do
                        isc <- innerStructuralCons fresh marker values
                        return $ concat [ orderingUpToMarker  fresh marker values
                                        , dontCareAfterMarker fresh marker values
                                        , mkSizeCons attrs marker
                                        , isc
                                        ]
                    _ -> na "{structuralCons} ExplicitVarSizeWithMarker"

        structuralCons _ _ _ = na "{structuralCons} ExplicitVarSizeWithMarker"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainSet _ (SetAttr attrs) innerDomain)
              , ConstantAbstract (AbsLitSet constants)
              ) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain i = mkDomainIntB (fromInt i) maxSize
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
                  , defRepr "Set.ExplicitVarSizeWithMarker.downC" (indexDomain 0)
                  , ConstantInt (length constants)
                  )
                , ( nameValues name
                  , DomainMatrix (indexDomain 1) innerDomain
                  , ConstantAbstract $ AbsLitMatrix (indexDomain 1) (constants ++ zeroes)
                  )
                ]
        downC _ = na "{downC} ExplicitVarSizeWithMarker"

        up :: TypeOf_Up m
        up ctxt (name, domain) =
            case (lookup (nameMarker name) ctxt, lookup (nameValues name) ctxt) of
                (Just marker, Just constantMatrix) ->
                    case marker of
                        ConstantInt card ->
                            case constantMatrix of
                                ConstantAbstract (AbsLitMatrix _ vals) ->
                                    return (name, ConstantAbstract (AbsLitSet (take card vals)))
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

