{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.MSet.ExplicitVarSizeWithFlags ( msetExplicitVarSizeWithFlags ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Lenses
import Conjure.Language.TH
import Conjure.Language.DomainSize
import Conjure.Language.Pretty
import Conjure.Language.ZeroVal ( zeroVal )
import Conjure.Representations.Internal
import Conjure.Representations.Common


msetExplicitVarSizeWithFlags :: MonadFail m => Representation m
msetExplicitVarSizeWithFlags = Representation chck downD structuralCons downC up

    where

        chck f (DomainMSet _ attrs innerDomain) =
            DomainMSet "ExplicitVarSizeWithFlags" attrs <$> f innerDomain
        chck _ _ = []

        nameFlag   name = mconcat [name, "_", "ExplicitVarSizeWithFlags", "_Flags"]
        nameValues name = mconcat [name, "_", "ExplicitVarSizeWithFlags", "_Values"]

        getMaxSize attrs innerDomain = case attrs of
            MSetAttr (SizeAttr_Size x) _ -> return x
            MSetAttr (SizeAttr_MaxSize x) _ -> return x
            MSetAttr (SizeAttr_MinMaxSize _ x) _ -> return x
            MSetAttr _ (OccurAttr_MaxOccur x) -> do y <- domainSizeOf innerDomain ; return (x * y)
            MSetAttr _ (OccurAttr_MinMaxOccur _ x) -> do y <- domainSizeOf innerDomain ; return (x * y)
            _ -> fail ("getMaxSize, mset not supported. attributes:" <+> pretty attrs)

        getMinOccur attrs = case attrs of
            MSetAttr _ (OccurAttr_MinOccur x) -> Just x
            MSetAttr _ (OccurAttr_MinMaxOccur x _) -> Just x
            _ -> Nothing

        getMaxOccur attrs innerDomain = case attrs of
            MSetAttr _ (OccurAttr_MaxOccur x) -> return x
            MSetAttr _ (OccurAttr_MinMaxOccur _ x) -> return x
            MSetAttr (SizeAttr_Size x) _ -> do y <- domainSizeOf innerDomain ; return (make opMin [x,y])
            MSetAttr (SizeAttr_MaxSize x) _ -> do y <- domainSizeOf innerDomain ; return (make opMin [x,y])
            MSetAttr (SizeAttr_MinMaxSize _ x) _ -> do y <- domainSizeOf innerDomain ; return (make opMin [x,y])
            _ -> fail ("getMaxSize, mset not supported. attributes:" <+> pretty attrs)

        downD (name, DomainMSet _ attrs innerDomain) = do
            maxSize  <- getMaxSize attrs innerDomain
            maxOccur <- getMaxOccur attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]
            let flagDomain  = DomainInt [RangeBounded (fromInt 0) maxOccur]
            return $ Just
                [ ( nameFlag name
                  , DomainMatrix (forgetRepr indexDomain) flagDomain
                  )
                , ( nameValues name
                  , DomainMatrix (forgetRepr indexDomain) innerDomain
                  )
                ]
        downD _ = na "{downD} ExplicitVarSizeWithFlags"

        structuralCons f downX1 (DomainMSet "ExplicitVarSizeWithFlags" attrs@(MSetAttr sizeAttrs _) innerDomain) = do
            maxSize  <- getMaxSize attrs innerDomain
            let
                orderingWhenFlagged fresh flags values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                    in
                        [essence|
                            forAll &iPat : int(1..&maxSize-1) . &flags[&i+1] > 0 -> &values[&i] < &values[&i+1]
                        |]

                dontCareWhenNotFlagged fresh flags values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                    in
                        [essence|
                            forAll &iPat : int(1..&maxSize) . &flags[&i] = 0 -> dontCare(&values[&i])
                        |]

                flagsToTheLeft fresh flags = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                    in
                        [essence|
                            forAll &iPat : int(1..&maxSize-1) . &flags[&i+1] > 0 -> &flags[&i] > 0
                        |]

                cardinality fresh flags =
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                    in
                        [essence| sum &iPat : int(1..&maxSize) . &flags[&i] |]

                -- maxOccur is enforced by the domain of the flag
                minOccurrenceCons fresh flags =
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                    in
                        [ [essence| forAll &iPat : int(1..&maxSize) . &flags[&i] >= &minOccur |]
                        | Just minOccur <- [getMinOccur attrs]
                        ]

                innerStructuralCons fresh flags values = do
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    let activeZone b = [essence| forAll &iPat : int(1..&maxSize) . &flags[&i] > 0 -> &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &values[&i] |]
                    refs <- downX1 inLoop
                    outs <- innerStructuralConsGen (tail fresh) refs
                    return (map activeZone outs)

            return $ \ fresh refs ->
                case refs of
                    [flags, values] -> do
                        isc <- innerStructuralCons fresh flags values
                        return $ concat [ orderingWhenFlagged    fresh flags values
                                        , dontCareWhenNotFlagged fresh flags values
                                        , flagsToTheLeft         fresh flags
                                        , minOccurrenceCons      fresh flags
                                        , mkSizeCons sizeAttrs (cardinality fresh flags)
                                        , isc
                                        ]
                    _ -> na "{structuralCons} ExplicitVarSizeWithFlags"

        structuralCons _ _ _ = na "{structuralCons} ExplicitVarSizeWithFlags"

        downC ( name
              , domain@(DomainMSet _ attrs innerDomain)
              , ConstantAbstract (AbsLitMSet constants')
              ) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]

            let constants = histogram constants'

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

            let counts = map (ConstantInt . snd) constants
            let falses = replicate (maxSizeInt - length constants) (ConstantInt 0)

            return $ Just
                [ ( nameFlag name
                  , DomainMatrix (forgetRepr indexDomain) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (forgetRepr indexDomain) (counts ++ falses)
                  )
                , ( nameValues name
                  , DomainMatrix (forgetRepr indexDomain) innerDomain
                  , ConstantAbstract $ AbsLitMatrix (forgetRepr indexDomain) (map fst constants ++ zeroes)
                  )
                ]
        downC _ = na "{downC} ExplicitVarSizeWithFlags"

        up ctxt (name, domain) =
            case (lookup (nameFlag name) ctxt, lookup (nameValues name) ctxt) of
                (Just flagMatrix, Just constantMatrix) ->
                    case flagMatrix of
                        -- TODO: check if indices match
                        ConstantAbstract (AbsLitMatrix _ flags) ->
                            case constantMatrix of
                                ConstantAbstract (AbsLitMatrix _ vals) ->
                                    return (name, ConstantAbstract $ AbsLitMSet $ concat
                                                    [ replicate i v
                                                    | (ConstantInt i,v) <- zip flags vals
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

