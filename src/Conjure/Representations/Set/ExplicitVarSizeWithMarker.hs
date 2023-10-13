{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Set.ExplicitVarSizeWithMarker ( setExplicitVarSizeWithMarker ) where

-- conjure
import Conjure.Prelude
import Conjure.Language
import Conjure.Language.DomainSizeOf
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.ZeroVal ( zeroVal, EnumerateDomain )
import Conjure.Representations.Internal
import Conjure.Representations.Common


setExplicitVarSizeWithMarker :: forall m . (MonadFailDoc m, NameGen m, EnumerateDomain m) => Representation m
setExplicitVarSizeWithMarker = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck _ (DomainSet _ (SetAttr SizeAttr_Size{}) _) = return []
        chck f (DomainSet _ attrs innerDomain) = map (DomainSet Set_ExplicitVarSizeWithMarker attrs) <$> f innerDomain
        chck _ _ = return []

        nameMarker = mkOutName (Just "Marker")
        nameValues = mkOutName (Just "Values")

        getMaxSize attrs innerDomain = case attrs of
            SizeAttr_MaxSize x -> return x
            SizeAttr_MinMaxSize _ x -> return x
            _ -> reTag TagInt <$> domainSizeOf innerDomain

        downD :: TypeOf_DownD m
        downD (name, domain@(DomainSet _ (SetAttr attrs) innerDomain)) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain i = mkDomainIntB (fromInt i) maxSize
            return $ Just
                [ ( nameMarker domain name
                  , defRepr (indexDomain 0)
                  )
                , ( nameValues domain name
                  , DomainMatrix (indexDomain 1) innerDomain
                  )
                ]
        downD _ = na "{downD} ExplicitVarSizeWithMarker"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainSet Set_ExplicitVarSizeWithMarker (SetAttr attrs) innerDomain) = do
            maxSize <- getMaxSize attrs innerDomain
            let
                orderingUpToMarker marker values = do
                    (iPat, i) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : int(1..&maxSize-1) . &i + 1 <= &marker ->
                                &values[&i] .< &values[&i+1]
                        |]

                dontCareAfterMarker marker values = do
                    (iPat, i) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : int(1..&maxSize) . &i > &marker ->
                                dontCare(&values[&i])
                        |]

                innerStructuralCons marker values = do
                    let overDomain = [essenceDomain| int(1..&maxSize) |]
                    (iPat, i) <- quantifiedVarOverDomain overDomain
                    let activeZone b = [essence| forAll &iPat : &overDomain . &i <= &marker -> &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &values[&i] |]
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ set -> do
                refs <- downX1 set
                case refs of
                    [marker,values] ->
                        concat <$> sequence
                            [ orderingUpToMarker  marker values
                            , dontCareAfterMarker marker values
                            , return (mkSizeCons attrs marker)
                            , innerStructuralCons marker values
                            ]
                    _ -> na "{structuralCons} ExplicitVarSizeWithMarker"

        structuralCons _ _ _ = na "{structuralCons} ExplicitVarSizeWithMarker"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainSet _ (SetAttr attrs) innerDomain)
              , viewConstantSet -> Just constants
              ) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain i = mkDomainIntB (fromInt i) maxSize
            maxSizeInt <-
                case maxSize of
                    ConstantInt _ x -> return x
                    _ -> failDoc $ vcat
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
        downC _ = na "{downC} ExplicitVarSizeWithMarker"

        up :: TypeOf_Up m
        up ctxt (name, domain) =
            case (lookup (nameMarker domain name) ctxt, lookup (nameValues domain name) ctxt) of
                (Just marker, Just constantMatrix) ->
                    case marker of
                        ConstantInt _ card ->
                            case (viewConstantMatrix constantMatrix, constantMatrix) of
                                (Just (_, vals), _) ->
                                    return (name, ConstantAbstract (AbsLitSet (genericTake card vals)))
                                (_, ConstantUndefined msg ty) ->         -- undefined propagates
                                    return (name, ConstantUndefined ("Set-ExplicitVarSizeWithMarker " `mappend` msg) ty)
                                _ -> failDoc $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty (nameValues domain name)
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty domain
                                        ]
                        _ -> failDoc $ vcat
                                [ "Expecting an integer literal for:" <+> pretty (nameMarker domain name)
                                , "But got:" <+> pretty marker
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                (Nothing, _) -> failDoc $ vcat $
                    [ "(in Set ExplicitVarSizeWithMarker up 1)"
                    , "No value for:" <+> pretty (nameMarker domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> failDoc $ vcat $
                    [ "(in Set ExplicitVarSizeWithMarker up 2)"
                    , "No value for:" <+> pretty (nameValues domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            [marker, values] <- downX1 inp
            Just [_, (_, DomainMatrix index inner)] <- downD ("SO", domain)
            (iPat, i) <- quantifiedVar
            soValues <- innerSO downX1 [essence| &values[&i] |] inner
            return
                [essence|
                    ( &marker
                    ,[ &soValues
                     | &iPat : &index
                     ]
                    )
                |]

