{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Set.ExplicitVarSizeWithFlags ( setExplicitVarSizeWithFlags ) where

-- conjure
import Conjure.Prelude
import Conjure.Language
import Conjure.Language.DomainSizeOf
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.ZeroVal ( zeroVal, EnumerateDomain )
import Conjure.Representations.Internal
import Conjure.Representations.Common


setExplicitVarSizeWithFlags :: forall m . (MonadFail m, NameGen m, EnumerateDomain m) => Representation m
setExplicitVarSizeWithFlags = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck _ (DomainSet _ (SetAttr SizeAttr_Size{}) _) = return []
        chck f (DomainSet _ attrs innerDomain) =
            map (DomainSet Set_ExplicitVarSizeWithFlags attrs) <$> f innerDomain
        chck _ _ = return []

        nameFlag   = mkOutName (Just "Flags")
        nameValues = mkOutName (Just "Values")

        getMaxSize attrs innerDomain = case attrs of
            SizeAttr_MaxSize x -> return x
            SizeAttr_MinMaxSize _ x -> return x
            _ -> reTag TagInt <$> domainSizeOf innerDomain


        downD :: TypeOf_DownD m
        downD (name, domain@(DomainSet _ (SetAttr attrs) innerDomain)) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = mkDomainIntB 1 maxSize
            return $ Just
                [ ( nameFlag domain name
                  , DomainMatrix (forgetRepr indexDomain) DomainBool
                  )
                , ( nameValues domain name
                  , DomainMatrix (forgetRepr indexDomain) innerDomain
                  )
                ]
        downD _ = na "{downD} ExplicitVarSizeWithFlags"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainSet Set_ExplicitVarSizeWithFlags (SetAttr attrs) innerDomain) = do
            maxSize <- getMaxSize attrs innerDomain
            let
                orderingWhenFlagged flags values = do
                    (iPat, i) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : int(1..&maxSize-1) . &flags[&i+1] -> &values[&i] .< &values[&i+1]
                        |]

                dontCareWhenNotFlagged flags values = do
                    (iPat, i) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : int(1..&maxSize) . &flags[&i] = false -> dontCare(&values[&i])
                        |]

                flagsToTheLeft flags = do
                    (iPat, i) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : int(1..&maxSize-1) . &flags[&i+1] -> &flags[&i]
                        |]

                innerStructuralCons flags values = do
                    (iPat, i) <- quantifiedVarOverDomain [essenceDomain| int(1..&maxSize) |]
                    let activeZone b = [essence| forAll &iPat : int(1..&maxSize) . &flags[&i] -> &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &values[&i] |]
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ set -> do
                refs <- downX1 set
                case refs of
                    [flags, values] ->
                        concat <$> sequence
                            [ orderingWhenFlagged    flags values
                            , dontCareWhenNotFlagged flags values
                            , flagsToTheLeft         flags
                            , return $ mkSizeCons attrs $ make opTwoBars set False
                            , innerStructuralCons flags values
                            ]
                    _ -> na "{structuralCons} ExplicitVarSizeWithFlags"

        structuralCons _ _ _ = na "{structuralCons} ExplicitVarSizeWithFlags"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainSet _ (SetAttr attrs) innerDomain)
              , ConstantAbstract (AbsLitSet constants)
              ) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = mkDomainIntB 1 maxSize

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

            let trues  = replicate (length constants)                                   (ConstantBool True)
            let falses = replicate (fromInteger (maxSizeInt - genericLength constants)) (ConstantBool False)

            return $ Just
                [ ( nameFlag domain name
                  , DomainMatrix
                      (forgetRepr indexDomain)
                      DomainBool
                  , ConstantAbstract $ AbsLitMatrix
                      (forgetRepr indexDomain)
                      (trues ++ falses)
                  )
                , ( nameValues domain name
                  , DomainMatrix
                      (forgetRepr indexDomain)
                      innerDomain
                  , ConstantAbstract $ AbsLitMatrix
                      (forgetRepr indexDomain)
                      (constants ++ zeroes)
                  )
                ]
        downC _ = na "{downC} ExplicitVarSizeWithFlags"

        up :: TypeOf_Up m
        up ctxt (name, domain) =
            case (lookup (nameFlag domain name) ctxt, lookup (nameValues domain name) ctxt) of
                (Just flagMatrix, Just constantMatrix) ->
                    case viewConstantMatrix flagMatrix of
                        -- TODO: check if indices match
                        Just (_, flags) ->
                            case viewConstantMatrix constantMatrix of
                                Just (_, vals) ->
                                    return (name, ConstantAbstract $ AbsLitSet
                                                    [ v
                                                    | (i,v) <- zip flags vals
                                                    , viewConstantBool i == Just True
                                                    ] )
                                _ -> fail $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty (nameValues domain name)
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty domain
                                        ]
                        _ -> fail $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (nameFlag domain name)
                                , "But got:" <+> pretty flagMatrix
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                (Nothing, _) -> fail $ vcat $
                    [ "(in Set ExplicitVarSizeWithFlags up 1)"
                    , "No value for:" <+> pretty (nameFlag domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> fail $ vcat $
                    [ "(in Set ExplicitVarSizeWithFlags up 2)"
                    , "No value for:" <+> pretty (nameValues domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            [flags, values] <- downX1 inp
            Just [_, (_, DomainMatrix index inner)] <- downD ("SO", domain)
            (iPat, i) <- quantifiedVar
            soValues <- innerSO downX1 [essence| &values[&i] |] inner
            return
                [essence|
                    flatten([ flatten([ [-toInt(&flags[&i])]
                                      , &soValues
                                      ])
                            | &iPat : &index
                            ])
                |]
