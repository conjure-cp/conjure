{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Representations.Partition.PartitionAsSet
    ( partitionAsSet
    , partitionAsSetAllFlavours
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Constant
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal

-- text
import Data.Text as T ( stripPrefix )


partitionAsSet
    :: forall m . (MonadFail m, NameGen m)
    => (forall x . Pretty x => Domain HasRepresentation x -> Representation m)
    -> HasRepresentation
    -> Representation m
partitionAsSet dispatch (HasRepresentation (Name repr)) =
    case T.stripPrefix "PartitionAsSet" repr of
        Nothing -> bug ("(1) partitionAsSet called with:" <+> pretty repr)
        Just num_ ->
            case readMay (textToString num_) of
                Nothing -> bug ("(2) partitionAsSet called with:" <+> pretty repr)
                Just num ->
                    case [ b | (a,b) <- flavours, a == num ] of
                        [(repr1, repr2)] -> partitionAsSet_ dispatch repr1 repr2
                        _ -> bug ("(3) partitionAsSet called with:" <+> pretty repr)
partitionAsSet _ repr = bug ("(4) partitionAsSet called with:" <+> pretty (show repr))


partitionAsSetAllFlavours
    :: forall m . (MonadFail m, NameGen m)
    => Bool
    -> (forall x . Pretty x => Domain HasRepresentation x -> Representation m)
    -> [Representation m]
partitionAsSetAllFlavours useLevels dispatch =
    [ partitionAsSet_ dispatch repr1 repr2
    | (repr1, repr2) <- map snd flavours
    , not useLevels || repr1 /= "ExplicitVarSizeWithFlags"
    , not useLevels || repr2 /= "ExplicitVarSizeWithFlags"
    ]


flavours :: [(Int, (HasRepresentation, HasRepresentation))]
flavours = zip [1..] [ (a,b) | a <- opts1, b <- opts2 ]
    where
        opts1 = ["Explicit", "ExplicitVarSizeWithMarker", "ExplicitVarSizeWithFlags"]
        opts2 = ["Explicit", "ExplicitVarSizeWithMarker", "ExplicitVarSizeWithFlags", "Occurrence"]


partitionAsSet_
    :: forall m . (MonadFail m, NameGen m)
    => (forall x . Pretty x => Domain HasRepresentation x -> Representation m)
    -> HasRepresentation
    -> HasRepresentation
    -> Representation m
partitionAsSet_ dispatch repr1 repr2 = Representation chck downD structuralCons downC up

    where

        thisReprFlavour = case [ a | (a,b) <- flavours, b == (repr1, repr2) ] of
            [a] -> a
            _   -> bug "partitionAsSet.thisReprFlavour"

        thisReprName = "PartitionAsSet" `mappend` Name (stringToText (show thisReprFlavour))

        chck :: TypeOf_ReprCheck
        chck f (DomainPartition _ attrs innerDomain) =
            let
                repr1Fixed = case partsNum  attrs of SizeAttr_Size{} -> True ; _ -> False
                repr2Fixed = case partsSize attrs of SizeAttr_Size{} -> True ; _ -> False
                repr2Inty  = case innerDomain of
                                DomainInt{} -> True
                                _           -> False
                repr1CanBe = if repr1Fixed
                                then ["Explicit"]
                                else ["ExplicitVarSizeWithMarker", "ExplicitVarSizeWithFlags"]
                repr2CanBe = concat
                           [ if repr2Fixed
                                then ["Explicit"]
                                else ["ExplicitVarSizeWithMarker", "ExplicitVarSizeWithFlags"]
                           , if repr2Inty
                                then ["Occurrence"]
                                else []
                           ]
            in
                if repr1 `elem` repr1CanBe && repr2 `elem` repr2CanBe
                    then DomainPartition (HasRepresentation thisReprName) attrs <$> f innerDomain
                    else []
        chck _ _ = []

        outName name = mconcat [name, "_", thisReprName]

        outDomain (DomainPartition repr (PartitionAttr{..}) innerDomain)
            | repr == HasRepresentation thisReprName =
            return (DomainSet repr1 (SetAttr partsNum) (DomainSet repr2 (SetAttr partsSize) innerDomain))
        outDomain domain =
            na $ vcat [ "{outDomain} PartitionAsSet"
                      , "repr1:"  <+> pretty repr1
                      , "repr2:"  <+> pretty repr2
                      , "domain:" <+> pretty domain
                      ]

        downD :: TypeOf_DownD m
        downD (name, inDom) = do
            outDom <- outDomain inDom
            return $ Just [ ( outName name , outDom ) ]

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 inDom@(DomainPartition _ attrs innerDomain) = return $ \ inpRel -> do
            refs <- downX1 inpRel
            let
                exactlyOnce rel = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return $ -- for list
                        [essence|
                            forAll &iPat : &innerDomain .
                                1  = sum ([ 1
                                          | &jPat <- &rel
                                          , &i in &j
                                          ])
                                |]

                regular rel | isRegular attrs = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            and([ |&i| = |&j|
                                | &iPat <- &rel
                                , &jPat <- &rel
                                ])
                        |]
                regular _ = return []

                partsAren'tEmpty rel = do
                    (iPat, i) <- quantifiedVar
                    return $ return [essence| and([ |&i| >= 1 | &iPat <- &rel ]) |]

            case refs of
                [rel] -> do
                    outDom                 <- outDomain inDom
                    innerStructuralConsGen <- f outDom
                    concat <$> sequence
                        [ exactlyOnce rel
                        , regular rel
                        , partsAren'tEmpty rel
                        , innerStructuralConsGen rel
                        ]
                _ -> na $ vcat [ "{structuralCons} PartitionAsSet"
                               , pretty inDom
                               ]
        structuralCons _ _ domain = na $ vcat [ "{structuralCons} PartitionAsSet"
                                              , "domain:" <+> pretty domain
                                              ]

        downC :: TypeOf_DownC m
        downC ( name
              , inDom
              , ConstantAbstract (AbsLitPartition vals)
              ) = do
            outDom <- outDomain inDom
            rDownC
                (dispatch outDom)
                ( outName name
                , outDom
                , ConstantAbstract $ AbsLitSet $ map (ConstantAbstract . AbsLitSet) vals
                )
        downC (name, domain, constant) = na $ vcat [ "{downC} PartitionAsSet"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainPartition repr _ _)) | repr == HasRepresentation thisReprName =
            case lookup (outName name) ctxt of
                Nothing -> fail $ vcat $
                    [ "(in PartitionAsSet up)"
                    , "No value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just (viewConstantSet -> Just sets) -> do
                    let setOut (viewConstantSet -> Just xs) = return xs
                        setOut c = fail $ "Expecting a set, but got:" <+> pretty c
                    vals <- mapM setOut sets
                    return (name, ConstantAbstract (AbsLitPartition vals))
                Just (ConstantUndefined msg ty) ->        -- undefined propagates
                    return (name, ConstantUndefined ("PartitionAsSet " `mappend` msg) ty)
                Just constant -> fail $ vcat $
                    [ "Incompatible value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    , "Expected a set value, but got:" <+> pretty constant
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ (name, domain) = na $ vcat [ "{up} PartitionAsSet"
                                        , "name:" <+> pretty name
                                        , "domain:" <+> pretty domain
                                        ]
