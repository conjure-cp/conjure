{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Conjure.Language.Domain.AddAttributes
    ( allSupportedAttributes
    , addAttributesToDomain
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Name
import Conjure.Language.Domain
import Conjure.Language.Pretty

-- containers
import Data.Set as S ( singleton )


allSupportedAttributes :: [(Name, Int)]
allSupportedAttributes =
    map (,1) [ "size", "minSize", "maxSize"
             , "minOccur", "maxOccur"
             , "numParts", "minNumParts", "maxNumParts"
             , "partSize", "minPartSize", "maxPartSize"
             ] ++
    map (,0) [ "total"
             , "injective", "surjective", "bijective"
             , "regular"
             ] ++
    map (,0) [ "reflexive"
             , "irreflexive"
             , "coreflexive"
             , "symmetric"
             , "antiSymmetric"
             , "aSymmetric"
             , "transitive"
             , "total"
             , "connex"
             , "Euclidean"
             , "serial"
             , "equivalence"
             , "partialOrder"
             ]


addAttributesToDomain
    :: ( MonadFail m
       , Pretty r
       , Pretty x
       )
    => Domain r x
    -> [(AttrName, Maybe x)]
    -> m (Domain r x)
addAttributesToDomain domain [] = return domain
addAttributesToDomain domain ((attr, val) : rest) = do
    domain' <- addAttributeToDomain domain attr val
    addAttributesToDomain domain' rest


addAttributeToDomain
    :: ( MonadFail m
       , Pretty r
       , Pretty x
       )
    => Domain r x                                   -- the input domain
    -> AttrName                                     -- the name of the attribute
    -> Maybe x                                      -- the value for the attribute
    -> m (Domain r x)                               -- the modified domain

addAttributeToDomain d@DomainAny{}       = const $ const $ return d
addAttributeToDomain d@DomainBool{}      = const $ const $ return d
addAttributeToDomain d@DomainIntE{}      = const $ const $ return d
addAttributeToDomain d@DomainInt{}       = const $ const $ return d
addAttributeToDomain d@DomainEnum{}      = const $ const $ return d
addAttributeToDomain d@DomainUnnamed{}   = const $ const $ return d
addAttributeToDomain d@DomainTuple{}     = const $ const $ return d
addAttributeToDomain d@DomainRecord{}    = const $ const $ return d
addAttributeToDomain d@DomainVariant{}   = const $ const $ return d
addAttributeToDomain d@DomainMatrix{}    = const $ const $ return d
addAttributeToDomain d@DomainOp{}        = const $ const $ return d
addAttributeToDomain d@DomainReference{} = const $ const $ return d
addAttributeToDomain d@DomainMetaVar{}   = const $ const $ return d

addAttributeToDomain domain@(DomainSet r (SetAttr sizeAttr) inner) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainSet r (SetAttr (SizeAttr_Size val)) inner
        AttrName_minSize -> do
            let fails = fail $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainSet r (SetAttr (SizeAttr_MinSize val)) inner
                SizeAttr_MaxSize maxS -> return $ DomainSet r (SetAttr (SizeAttr_MinMaxSize val maxS)) inner
        AttrName_maxSize -> do
            let fails = fail $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainSet r (SetAttr (SizeAttr_MaxSize val)) inner
                SizeAttr_MinSize minS -> return $ DomainSet r (SetAttr (SizeAttr_MinMaxSize minS val)) inner
        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater attr _ =
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]

addAttributeToDomain domain@(DomainMSet r (MSetAttr sizeAttr occurAttr) inner) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainMSet r (MSetAttr (SizeAttr_Size val) occurAttr) inner
        AttrName_minSize -> do
            let fails = fail $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainMSet r
                                            (MSetAttr (SizeAttr_MinSize val)         occurAttr)
                                            inner
                SizeAttr_MaxSize maxS -> return $ DomainMSet r
                                            (MSetAttr (SizeAttr_MinMaxSize val maxS) occurAttr)
                                            inner
        AttrName_maxSize -> do
            let fails = fail $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainMSet r
                                            (MSetAttr (SizeAttr_MaxSize val)         occurAttr)
                                            inner
                SizeAttr_MinSize minS -> return $ DomainMSet r
                                            (MSetAttr (SizeAttr_MinMaxSize minS val) occurAttr)
                                            inner
        AttrName_minOccur -> do
            let fails = fail $ "Cannot add a minOccur attribute to this domain:" <++> pretty domain
            case occurAttr of
                OccurAttr_MinOccur{}    -> fails
                OccurAttr_MinMaxOccur{} -> fails
                OccurAttr_None          -> return $ DomainMSet r
                                            (MSetAttr sizeAttr (OccurAttr_MinOccur val))
                                            inner
                OccurAttr_MaxOccur maxO -> return $ DomainMSet r
                                            (MSetAttr sizeAttr (OccurAttr_MinMaxOccur val maxO))
                                            inner
        AttrName_maxOccur -> do
            let fails = fail $ "Cannot add a maxOccur attribute to this domain:" <++> pretty domain
            case occurAttr of
                OccurAttr_MaxOccur{}    -> fails
                OccurAttr_MinMaxOccur{} -> fails
                OccurAttr_None          -> return $ DomainMSet r
                                            (MSetAttr sizeAttr (OccurAttr_MaxOccur val))
                                            inner
                OccurAttr_MinOccur minO -> return $ DomainMSet r
                                            (MSetAttr sizeAttr (OccurAttr_MinMaxOccur minO val))
                                            inner
        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater attr _ =
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]

addAttributeToDomain domain@(DomainFunction r
                            (FunctionAttr sizeAttr partialityAttr jectivityAttr)
                            inF inT) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_Size val) partialityAttr jectivityAttr)
                                            inF inT
        AttrName_minSize -> do
            let fails = fail $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MinSize val) partialityAttr jectivityAttr)
                                            inF inT
                SizeAttr_MaxSize maxS -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MinMaxSize val maxS) partialityAttr jectivityAttr)
                                            inF inT
        AttrName_maxSize -> do
            let fails = fail $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MaxSize val) partialityAttr jectivityAttr)
                                            inF inT
                SizeAttr_MinSize minS -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MinMaxSize minS val) partialityAttr jectivityAttr)
                                            inF inT
        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater "total" Nothing = return $ DomainFunction r
                                            (FunctionAttr sizeAttr PartialityAttr_Total jectivityAttr)
                                            inF inT
    updater "injective" Nothing = return $
        case jectivityAttr of
            JectivityAttr_None       -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Injective )
                                            inF inT
            JectivityAttr_Injective  -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Injective )
                                            inF inT
            JectivityAttr_Surjective -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
            JectivityAttr_Bijective  -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
    updater "surjective" Nothing = return $
        case jectivityAttr of
            JectivityAttr_None          -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Surjective)
                                            inF inT
            JectivityAttr_Injective     -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
            JectivityAttr_Surjective    -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Surjective)
                                            inF inT
            JectivityAttr_Bijective     -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
    updater "bijective" Nothing = return $ DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective)
                                            inF inT
    updater attr _ =
        fail $ vcat [ "Unsupported attribute" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

addAttributeToDomain domain@(DomainSequence r
                            (SequenceAttr sizeAttr jectivityAttr)
                            inner) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainSequence r
                                            (SequenceAttr (SizeAttr_Size val) jectivityAttr)
                                            inner
        AttrName_minSize -> do
            let fails = fail $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainSequence r
                                            (SequenceAttr (SizeAttr_MinSize val) jectivityAttr)
                                            inner
                SizeAttr_MaxSize maxS -> return $ DomainSequence r
                                            (SequenceAttr (SizeAttr_MinMaxSize val maxS) jectivityAttr)
                                            inner
        AttrName_maxSize -> do
            let fails = fail $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainSequence r
                                            (SequenceAttr (SizeAttr_MaxSize val) jectivityAttr)
                                            inner
                SizeAttr_MinSize minS -> return $ DomainSequence r
                                            (SequenceAttr (SizeAttr_MinMaxSize minS val) jectivityAttr)
                                            inner
        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater "injective" Nothing = return $
        case jectivityAttr of
            JectivityAttr_None       -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Injective )
                                            inner
            JectivityAttr_Injective  -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Injective )
                                            inner
            JectivityAttr_Surjective -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Bijective )
                                            inner
            JectivityAttr_Bijective  -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Bijective )
                                            inner
    updater "surjective" Nothing = return $
        case jectivityAttr of
            JectivityAttr_None          -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Surjective)
                                            inner
            JectivityAttr_Injective     -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Bijective )
                                            inner
            JectivityAttr_Surjective    -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Surjective)
                                            inner
            JectivityAttr_Bijective     -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Bijective )
                                            inner
    updater "bijective" Nothing = return $ DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Bijective)
                                            inner
    updater attr _ =
        fail $ vcat [ "Unsupported attribute" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

addAttributeToDomain domain@(DomainRelation r
                            (RelationAttr sizeAttr binRelAttr)
                            inners) = updater where
    supportedBinRel :: [AttrName]
    supportedBinRel =
        [ "reflexive", "irreflexive", "coreflexive"
        , "symmetric", "antiSymmetric", "aSymmetric"
        , "transitive", "total", "connex", "Euclidean"
        , "serial", "equivalence", "partialOrder"
        ]
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainRelation r (RelationAttr (SizeAttr_Size val) binRelAttr) inners
        AttrName_minSize -> do
            let fails = fail $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MinSize val)         binRelAttr)
                                            inners
                SizeAttr_MaxSize maxS -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MinMaxSize val maxS) binRelAttr)
                                            inners
        AttrName_maxSize -> do
            let fails = fail $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MaxSize val)         binRelAttr)
                                            inners
                SizeAttr_MinSize minS -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MinMaxSize minS val) binRelAttr)
                                            inners
        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater attr Nothing | attr `elem` supportedBinRel = case readBinRel attr of
        Nothing ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
        Just a  -> return $ DomainRelation r
                                (RelationAttr sizeAttr (binRelAttr `mappend` BinaryRelationAttrs (S.singleton a)))
                                inners
    updater attr _ =
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]

addAttributeToDomain domain@(DomainPartition r partitionAttr inner) = updater where
    updater attr (Just val) = case attr of

        AttrName_numParts ->
            case partsNum partitionAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a numParts attribute to this domain:" <++> pretty domain
                _               -> return $ DomainPartition r (partitionAttr { partsNum = SizeAttr_Size val }) inner
        AttrName_minNumParts -> do
            let fails = fail $ "Cannot add a minNumParts attribute to this domain:" <++> pretty domain
            case partsNum partitionAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainPartition r
                                            (partitionAttr { partsNum = SizeAttr_MinSize val })
                                            inner
                SizeAttr_MaxSize maxS -> return $ DomainPartition r
                                            (partitionAttr { partsNum = SizeAttr_MinMaxSize val maxS })
                                            inner
        AttrName_maxNumParts -> do
            let fails = fail $ "Cannot add a maxNumParts attribute to this domain:" <++> pretty domain
            case partsNum partitionAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainPartition r
                                            (partitionAttr { partsNum = SizeAttr_MaxSize val })
                                            inner
                SizeAttr_MinSize minS -> return $ DomainPartition r
                                            (partitionAttr { partsNum = SizeAttr_MinMaxSize minS val })
                                            inner

        AttrName_partSize ->
            case partsSize partitionAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a partSize attribute to this domain:" <++> pretty domain
                _               -> return $ DomainPartition r (partitionAttr { partsSize = SizeAttr_Size val }) inner
        AttrName_minPartSize -> do
            let fails = fail $ "Cannot add a minPartSize attribute to this domain:" <++> pretty domain
            case partsSize partitionAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainPartition r
                                            (partitionAttr { partsSize = SizeAttr_MinSize val })
                                            inner
                SizeAttr_MaxSize maxS -> return $ DomainPartition r
                                            (partitionAttr { partsSize = SizeAttr_MinMaxSize val maxS })
                                            inner
        AttrName_maxPartSize -> do
            let fails = fail $ "Cannot add a maxPartSize attribute to this domain:" <++> pretty domain
            case partsSize partitionAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainPartition r
                                            (partitionAttr { partsSize = SizeAttr_MaxSize val })
                                            inner
                SizeAttr_MinSize minS -> return $ DomainPartition r
                                            (partitionAttr { partsSize = SizeAttr_MinMaxSize minS val })
                                            inner

        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater AttrName_regular Nothing =
            return $ DomainPartition r (partitionAttr { isRegular  = True }) inner
    updater attr _ =
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
