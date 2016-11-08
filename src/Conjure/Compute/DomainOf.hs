{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Compute.DomainOf ( DomainOf(..) ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug

import Conjure.Language

import Conjure.Language.TypeOf
import Conjure.Compute.DomainUnion

type Dom = Domain () Expression

class DomainOf a where

    -- | calculate the domain of `a`
    domainOf
        :: (MonadFail m, NameGen m)
        => a -> m Dom

    -- | calculate the index domains of `a`
    --   the index is the index of a matrix.
    --   returns [] for non-matrix inputs.
    --   has a default implementation in terms of domainOf, so doesn't need to be implemented specifically.
    --   but sometimes it is better to implement this directly.
    indexDomainsOf
        :: (MonadFail m, NameGen m, Pretty a)
        => a -> m [Dom]
    indexDomainsOf = defIndexDomainsOf

defIndexDomainsOf :: (MonadFail m, NameGen m, DomainOf a) => a -> m [Dom]
defIndexDomainsOf x = do
    dom <- domainOf x
    let
        collect (DomainMatrix index inner) = index : collect inner
        collect _ = []
    return (collect dom)

instance DomainOf ReferenceTo where
    domainOf (Alias x) = domainOf x
    -- domainOf (InComprehension (GenDomainNoRepr Single{} dom)) = return dom
    -- domainOf (InComprehension (GenDomainHasRepr _ dom)) = return (forgetRepr dom)
    -- domainOf (InComprehension (GenInExpr Single{} x)) = do
    --     domX <- domainOf x
    --     innX <- innerDomainOf domX
    --     return innX
    domainOf x@InComprehension{} = fail $ vcat [ "domainOf-ReferenceTo-InComprehension", pretty x, pretty (show x) ]
    domainOf (DeclNoRepr  _ _ dom _) = return dom
    domainOf (DeclHasRepr _ _ dom  ) = return (forgetRepr dom)
    domainOf RecordField{}  = fail "domainOf-ReferenceTo-RecordField"
    domainOf VariantField{} = fail "domainOf-ReferenceTo-VariantField"


instance DomainOf Expression where
    domainOf (Reference _ (Just refTo)) = domainOf refTo
    domainOf (Constant x) = domainOf x
    domainOf (AbstractLiteral x) = domainOf x
    domainOf (Op x) = domainOf x
    domainOf (WithLocals h _) = domainOf h
    domainOf x = fail ("domainOf{Expression}:" <+> pretty (show x))

    -- if an empty matrix literal has a type annotation
    indexDomainsOf (Typed lit ty) | emptyCollectionX lit =
        let
            tyToDom (TypeMatrix TypeInt t) = DomainInt [RangeBounded 1 0] : tyToDom t
            tyToDom _ = []
        in
            return (tyToDom ty)

    indexDomainsOf (Reference _ (Just refTo)) = indexDomainsOf refTo
    indexDomainsOf (Constant x) = indexDomainsOf x
    indexDomainsOf (AbstractLiteral x) = indexDomainsOf x
    indexDomainsOf (Op x) = indexDomainsOf x
    indexDomainsOf (WithLocals h _) = indexDomainsOf h
    indexDomainsOf x = fail ("indexDomainsOf{Expression}:" <+> pretty (show x))

-- this should be better implemented by some ghc-generics magic
instance (DomainOf x, TypeOf x, Pretty x, ExpressionLike x, Domain () x :< x, Dom :< x) => DomainOf (Op x) where
    domainOf (MkOpActive x) = domainOf x
    domainOf (MkOpAllDiff x) = domainOf x
    domainOf (MkOpAnd x) = domainOf x
    domainOf (MkOpApart x) = domainOf x
    domainOf (MkOpAttributeAsConstraint x) = domainOf x
    domainOf (MkOpDefined x) = domainOf x
    domainOf (MkOpDiv x) = domainOf x
    domainOf (MkOpDontCare x) = domainOf x
    domainOf (MkOpDotLeq x) = domainOf x
    domainOf (MkOpDotLt x) = domainOf x
    domainOf (MkOpEq x) = domainOf x
    domainOf (MkOpFactorial x) = domainOf x
    domainOf (MkOpFlatten x) = domainOf x
    domainOf (MkOpFreq x) = domainOf x
    domainOf (MkOpGeq x) = domainOf x
    domainOf (MkOpGt x) = domainOf x
    domainOf (MkOpHist x) = domainOf x
    domainOf (MkOpIff x) = domainOf x
    domainOf (MkOpImage x) = domainOf x
    domainOf (MkOpImageSet x) = domainOf x
    domainOf (MkOpImply x) = domainOf x
    domainOf (MkOpIn x) = domainOf x
    domainOf (MkOpIndexing x) = domainOf x
    domainOf (MkOpIntersect x) = domainOf x
    domainOf (MkOpInverse x) = domainOf x
    domainOf (MkOpLeq x) = domainOf x
    domainOf (MkOpLexLeq x) = domainOf x
    domainOf (MkOpLexLt x) = domainOf x
    domainOf (MkOpLt x) = domainOf x
    domainOf (MkOpMax x) = domainOf x
    domainOf (MkOpMin x) = domainOf x
    domainOf (MkOpMinus x) = domainOf x
    domainOf (MkOpMod x) = domainOf x
    domainOf (MkOpNegate x) = domainOf x
    domainOf (MkOpNeq x) = domainOf x
    domainOf (MkOpNot x) = domainOf x
    domainOf (MkOpOr x) = domainOf x
    domainOf (MkOpParticipants x) = domainOf x
    domainOf (MkOpParts x) = domainOf x
    domainOf (MkOpParty x) = domainOf x
    domainOf (MkOpPow x) = domainOf x
    domainOf (MkOpPowerSet x) = domainOf x
    domainOf (MkOpPred x) = domainOf x
    domainOf (MkOpPreImage x) = domainOf x
    domainOf (MkOpProduct x) = domainOf x
    domainOf (MkOpRange x) = domainOf x
    domainOf (MkOpRelationProj x) = domainOf x
    domainOf (MkOpRestrict x) = domainOf x
    domainOf (MkOpSlicing x) = domainOf x
    domainOf (MkOpSubsequence x) = domainOf x
    domainOf (MkOpSubset x) = domainOf x
    domainOf (MkOpSubsetEq x) = domainOf x
    domainOf (MkOpSubstring x) = domainOf x
    domainOf (MkOpSucc x) = domainOf x
    domainOf (MkOpSum x) = domainOf x
    domainOf (MkOpSupset x) = domainOf x
    domainOf (MkOpSupsetEq x) = domainOf x
    domainOf (MkOpTildeLeq x) = domainOf x
    domainOf (MkOpTildeLt x) = domainOf x
    domainOf (MkOpTogether x) = domainOf x
    domainOf (MkOpToInt x) = domainOf x
    domainOf (MkOpToMSet x) = domainOf x
    domainOf (MkOpToRelation x) = domainOf x
    domainOf (MkOpToSet x) = domainOf x
    domainOf (MkOpTrue x) = domainOf x
    domainOf (MkOpTwoBars x) = domainOf x
    domainOf (MkOpUnion x) = domainOf x
    domainOf (MkOpXor x) = domainOf x

    indexDomainsOf (MkOpActive x) = indexDomainsOf x
    indexDomainsOf (MkOpAllDiff x) = indexDomainsOf x
    indexDomainsOf (MkOpAnd x) = indexDomainsOf x
    indexDomainsOf (MkOpApart x) = indexDomainsOf x
    indexDomainsOf (MkOpAttributeAsConstraint x) = indexDomainsOf x
    indexDomainsOf (MkOpDefined x) = indexDomainsOf x
    indexDomainsOf (MkOpDiv x) = indexDomainsOf x
    indexDomainsOf (MkOpDontCare x) = indexDomainsOf x
    indexDomainsOf (MkOpDotLeq x) = indexDomainsOf x
    indexDomainsOf (MkOpDotLt x) = indexDomainsOf x
    indexDomainsOf (MkOpEq x) = indexDomainsOf x
    indexDomainsOf (MkOpFactorial x) = indexDomainsOf x
    indexDomainsOf (MkOpFlatten x) = indexDomainsOf x
    indexDomainsOf (MkOpFreq x) = indexDomainsOf x
    indexDomainsOf (MkOpGeq x) = indexDomainsOf x
    indexDomainsOf (MkOpGt x) = indexDomainsOf x
    indexDomainsOf (MkOpHist x) = indexDomainsOf x
    indexDomainsOf (MkOpIff x) = indexDomainsOf x
    indexDomainsOf (MkOpImage x) = indexDomainsOf x
    indexDomainsOf (MkOpImageSet x) = indexDomainsOf x
    indexDomainsOf (MkOpImply x) = indexDomainsOf x
    indexDomainsOf (MkOpIn x) = indexDomainsOf x
    indexDomainsOf (MkOpIndexing x) = indexDomainsOf x
    indexDomainsOf (MkOpIntersect x) = indexDomainsOf x
    indexDomainsOf (MkOpInverse x) = indexDomainsOf x
    indexDomainsOf (MkOpLeq x) = indexDomainsOf x
    indexDomainsOf (MkOpLexLeq x) = indexDomainsOf x
    indexDomainsOf (MkOpLexLt x) = indexDomainsOf x
    indexDomainsOf (MkOpLt x) = indexDomainsOf x
    indexDomainsOf (MkOpMax x) = indexDomainsOf x
    indexDomainsOf (MkOpMin x) = indexDomainsOf x
    indexDomainsOf (MkOpMinus x) = indexDomainsOf x
    indexDomainsOf (MkOpMod x) = indexDomainsOf x
    indexDomainsOf (MkOpNegate x) = indexDomainsOf x
    indexDomainsOf (MkOpNeq x) = indexDomainsOf x
    indexDomainsOf (MkOpNot x) = indexDomainsOf x
    indexDomainsOf (MkOpOr x) = indexDomainsOf x
    indexDomainsOf (MkOpParticipants x) = indexDomainsOf x
    indexDomainsOf (MkOpParts x) = indexDomainsOf x
    indexDomainsOf (MkOpParty x) = indexDomainsOf x
    indexDomainsOf (MkOpPow x) = indexDomainsOf x
    indexDomainsOf (MkOpPowerSet x) = indexDomainsOf x
    indexDomainsOf (MkOpPred x) = indexDomainsOf x
    indexDomainsOf (MkOpPreImage x) = indexDomainsOf x
    indexDomainsOf (MkOpProduct x) = indexDomainsOf x
    indexDomainsOf (MkOpRange x) = indexDomainsOf x
    indexDomainsOf (MkOpRelationProj x) = indexDomainsOf x
    indexDomainsOf (MkOpRestrict x) = indexDomainsOf x
    indexDomainsOf (MkOpSlicing x) = indexDomainsOf x
    indexDomainsOf (MkOpSubsequence x) = indexDomainsOf x
    indexDomainsOf (MkOpSubset x) = indexDomainsOf x
    indexDomainsOf (MkOpSubsetEq x) = indexDomainsOf x
    indexDomainsOf (MkOpSubstring x) = indexDomainsOf x
    indexDomainsOf (MkOpSucc x) = indexDomainsOf x
    indexDomainsOf (MkOpSum x) = indexDomainsOf x
    indexDomainsOf (MkOpSupset x) = indexDomainsOf x
    indexDomainsOf (MkOpSupsetEq x) = indexDomainsOf x
    indexDomainsOf (MkOpTildeLeq x) = indexDomainsOf x
    indexDomainsOf (MkOpTildeLt x) = indexDomainsOf x
    indexDomainsOf (MkOpTogether x) = indexDomainsOf x
    indexDomainsOf (MkOpToInt x) = indexDomainsOf x
    indexDomainsOf (MkOpToMSet x) = indexDomainsOf x
    indexDomainsOf (MkOpToRelation x) = indexDomainsOf x
    indexDomainsOf (MkOpToSet x) = indexDomainsOf x
    indexDomainsOf (MkOpTrue x) = indexDomainsOf x
    indexDomainsOf (MkOpTwoBars x) = indexDomainsOf x
    indexDomainsOf (MkOpUnion x) = indexDomainsOf x
    indexDomainsOf (MkOpXor x) = indexDomainsOf x

instance DomainOf Constant where
    domainOf ConstantBool{}             = return DomainBool
    domainOf i@ConstantInt{}            = return $ DomainInt [RangeSingle (Constant i)]
    domainOf (ConstantEnum defn _ _ )   = return (DomainEnum defn Nothing Nothing)
    domainOf ConstantField{}            = fail "DomainOf-Constant-ConstantField"
    domainOf (ConstantAbstract x)       = domainOf (fmap Constant x)
    domainOf (DomainInConstant dom)     = return (fmap Constant dom)
    domainOf (TypedConstant x ty)       = domainOf (Typed (Constant x) ty)
    domainOf ConstantUndefined{}        = fail "DomainOf-Constant-ConstantUndefined"

    indexDomainsOf ConstantBool{}       = return []
    indexDomainsOf ConstantInt{}        = return []
    indexDomainsOf ConstantEnum{}       = return []
    indexDomainsOf ConstantField{}      = return []
    indexDomainsOf (ConstantAbstract x) = indexDomainsOf (fmap Constant x)
    indexDomainsOf DomainInConstant{}   = return []
    indexDomainsOf (TypedConstant x ty) = indexDomainsOf (Typed (Constant x) ty)
    indexDomainsOf ConstantUndefined{}  = return []

instance DomainOf (AbstractLiteral Expression) where

    domainOf (AbsLitTuple        xs) = DomainTuple  <$> mapM domainOf xs

    domainOf (AbsLitRecord       xs) = DomainRecord <$> sequence [ do t <- domainOf x ; return (n,t)
                                                                 | (n,x) <- xs ]

    domainOf (AbsLitVariant Nothing  _ _) = fail "Cannot calculate the domain of variant literal."
    domainOf (AbsLitVariant (Just t) _ _) = return (DomainVariant t)

    domainOf (AbsLitMatrix ind inn ) = DomainMatrix ind <$> (domainUnions =<< mapM domainOf inn)

    domainOf (AbsLitSet         [] ) = return $ DomainSet def attr (DomainAny "domainOf-AbsLitSet-[]" TypeAny)
        where attr = SetAttr (SizeAttr_Size 0)
    domainOf (AbsLitSet         xs ) = DomainSet def attr <$> (domainUnions =<< mapM domainOf xs)
        where attr = SetAttr (SizeAttr_MaxSize $ fromInt $ genericLength xs)

    domainOf (AbsLitMSet        [] ) = return $ DomainMSet def attr (DomainAny "domainOf-AbsLitMSet-[]" TypeAny)
        where attr = MSetAttr (SizeAttr_Size 0) OccurAttr_None
    domainOf (AbsLitMSet        xs ) = DomainMSet def attr <$> (domainUnions =<< mapM domainOf xs)
        where attr = MSetAttr (SizeAttr_MaxSize $ fromInt $ genericLength xs) OccurAttr_None

    domainOf (AbsLitFunction    [] ) = return $ DomainFunction def attr
                                        (DomainAny "domainOf-AbsLitFunction-[]-1" TypeAny)
                                        (DomainAny "domainOf-AbsLitFunction-[]-2" TypeAny)
        where attr = FunctionAttr (SizeAttr_Size 0) def def
    domainOf (AbsLitFunction    xs ) = DomainFunction def attr
                                                <$> (domainUnions =<< mapM (domainOf . fst) xs)
                                                <*> (domainUnions =<< mapM (domainOf . snd) xs)
        where attr = FunctionAttr (SizeAttr_MaxSize $ fromInt $ genericLength xs) def def

    domainOf (AbsLitSequence    [] ) = return $ DomainSequence def attr
                                        (DomainAny "domainOf-AbsLitSequence-[]" TypeAny)
        where attr = SequenceAttr (SizeAttr_Size 0) def
    domainOf (AbsLitSequence    xs ) = DomainSequence def attr
                                                <$> (domainUnions =<< mapM domainOf xs)
        where attr = SequenceAttr (SizeAttr_MaxSize (fromInt $ genericLength xs)) def

    domainOf (AbsLitRelation    [] ) = return $ DomainRelation def attr []
        where attr = RelationAttr (SizeAttr_Size 0) def
    domainOf (AbsLitRelation    xss) = do
        ty <- domainUnions =<< mapM (domainOf . AbsLitTuple) xss
        case ty of
            DomainTuple ts -> return (DomainRelation def attr ts)
            _ -> bug "expecting DomainTuple in domainOf"
        where attr = RelationAttr (SizeAttr_MaxSize (fromInt $ genericLength xss)) def

    domainOf (AbsLitPartition   [] ) = return $ DomainPartition def attr
                                        (DomainAny "domainOf-AbsLitPartition-[]" TypeAny)
        where attr = PartitionAttr (SizeAttr_Size 0) (SizeAttr_Size 0) False
    domainOf (AbsLitPartition   xss) = DomainPartition def attr <$> (domainUnions =<< mapM domainOf (concat xss))
        where attr = PartitionAttr (SizeAttr_MaxSize (fromInt $ genericLength xss))
                                   (SizeAttr_MaxSize (fromInt $ maximum [genericLength xs | xs <- xss]))
                                   False

    indexDomainsOf (AbsLitMatrix ind inn) = (ind :) <$> (mapM domainUnions =<< mapM indexDomainsOf inn)
    indexDomainsOf _ = return []



-- all the `Op`s

instance DomainOf (OpActive x) where
    domainOf _ = return DomainBool

instance DomainOf (OpAllDiff x) where
    domainOf _ = return DomainBool

instance DomainOf (OpAnd x) where
    domainOf _ = return DomainBool

instance DomainOf (OpApart x) where
    domainOf _ = return DomainBool

instance DomainOf (OpAttributeAsConstraint x) where
    domainOf _ = return DomainBool

instance DomainOf x => DomainOf (OpDefined x) where
    domainOf (OpDefined f) = do
        fDom <- domainOf f
        case fDom of
            DomainFunction _ _ fr _ -> return $ DomainSet def def fr
            _ -> fail "domainOf, OpDefined, not a function"

instance DomainOf x => DomainOf (OpDiv x) where
    domainOf (OpDiv x y) = do
        xDom :: Dom <- domainOf x
        yDom :: Dom <- domainOf y
        (iPat, i) <- quantifiedVar
        (jPat, j) <- quantifiedVar
        let vals = [essence| [ &i / &j
                             | &iPat : &xDom
                             , &jPat : &yDom
                             ] |]
        let low  = [essence| min(&vals) |]
        let upp  = [essence| max(&vals) |]
        return (DomainInt [RangeBounded low upp] :: Dom)

instance DomainOf (OpDontCare x) where
    domainOf _ = return DomainBool

instance DomainOf (OpDotLeq x) where
    domainOf _ = return DomainBool

instance DomainOf (OpDotLt x) where
    domainOf _ = return DomainBool

instance DomainOf (OpEq x) where
    domainOf _ = return DomainBool

instance (Pretty x, TypeOf x) => DomainOf (OpFactorial x) where
    domainOf op = mkDomainAny ("OpFactorial:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpFlatten x) where
    domainOf op = mkDomainAny ("OpFlatten:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpFreq x) where
    domainOf op = mkDomainAny ("OpFreq:" <++> pretty op) <$> typeOf op

instance DomainOf (OpGeq x) where
    domainOf _ = return DomainBool

instance DomainOf (OpGt x) where
    domainOf _ = return DomainBool

instance (Pretty x, TypeOf x, DomainOf x) => DomainOf (OpHist x) where
    domainOf op = mkDomainAny ("OpHist:" <++> pretty op) <$> typeOf op
    indexDomainsOf op@(OpHistForValues _ n) = indexDomainsOf n
    indexDomainsOf op@OpHistAll{} = defIndexDomainsOf op

instance DomainOf (OpIff x) where
    domainOf _ = return DomainBool

instance (Pretty x, TypeOf x) => DomainOf (OpImage x) where
    domainOf op = mkDomainAny ("OpImage:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpImageSet x) where
    domainOf op = mkDomainAny ("OpImageSet:" <++> pretty op) <$> typeOf op

instance DomainOf (OpImply x) where
    domainOf _ = return DomainBool

instance DomainOf (OpIn x) where
    domainOf _ = return DomainBool

instance (Pretty x, TypeOf x, ExpressionLike x, DomainOf x) => DomainOf (OpIndexing x) where
    domainOf (OpIndexing m i) = do
        iType <- typeOf i
        case iType of
            TypeBool{} -> return ()
            TypeInt{} -> return ()
            _ -> fail "domainOf, OpIndexing, not a bool or int index"
        mDom <- domainOf m
        case mDom of
            DomainMatrix _ inner -> return inner
            DomainTuple inners -> do
                iInt <- intOut "domainOf OpIndexing" i
                return $ atNote "domainOf" inners (fromInteger (iInt-1))
            _ -> fail "domainOf, OpIndexing, not a matrix or tuple"

    indexDomainsOf p@(OpIndexing m i) = do
        iType <- typeOf i
        case iType of
            TypeBool{} -> return ()
            TypeInt{} -> return ()
            _ -> fail "domainOf, OpIndexing, not a bool or int index"
        is <- indexDomainsOf m
        case is of
            [] -> fail ("indexDomainsOf{OpIndexing}, not a matrix domain:" <++> pretty p)
            (_:is') -> return is'

instance (Pretty x, TypeOf x) => DomainOf (OpIntersect x) where
    domainOf op = mkDomainAny ("OpIntersect:" <++> pretty op) <$> typeOf op

instance DomainOf (OpInverse x) where
    domainOf _ = return DomainBool

instance DomainOf (OpLeq x) where
    domainOf _ = return DomainBool

instance DomainOf (OpLexLeq x) where
    domainOf _ = return DomainBool

instance DomainOf (OpLexLt x) where
    domainOf _ = return DomainBool

instance DomainOf (OpLt x) where
    domainOf _ = return DomainBool

instance (Pretty x, TypeOf x, ExpressionLike x, DomainOf x, Domain () x :< x) => DomainOf (OpMax x) where
    domainOf (OpMax x)
        | Just xs <- listOut x
        , not (null xs) = do
        doms <- mapM domainOf xs
        let lows = fromList [ [essence| min(`&d`) |] | d <- doms ]
        let low  = [essence| max(&lows) |]
        let upps = fromList [ [essence| max(`&d`) |] | d <- doms ]
        let upp  = [essence| max(&upps) |]
        return (DomainInt [RangeBounded low upp] :: Dom)
    domainOf op = mkDomainAny ("OpMax:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x, ExpressionLike x, DomainOf x, Domain () x :< x) => DomainOf (OpMin x) where
    domainOf (OpMin x)
        | Just xs <- listOut x
        , not (null xs) = do
        doms <- mapM domainOf xs
        let lows = fromList [ [essence| min(`&d`) |] | d <- doms ]
        let low  = [essence| min(&lows) |]
        let upps = fromList [ [essence| max(`&d`) |] | d <- doms ]
        let upp  = [essence| min(&upps) |]
        return (DomainInt [RangeBounded low upp] :: Dom)
    domainOf op = mkDomainAny ("OpMin:" <++> pretty op) <$> typeOf op

instance DomainOf x => DomainOf (OpMinus x) where
    domainOf (OpMinus x y) = do
        xDom :: Dom <- domainOf x
        yDom :: Dom <- domainOf y
        let low = [essence| min(`&xDom`) - max(`&yDom`) |]
        let upp = [essence| max(`&xDom`) - min(`&yDom`) |]
        return (DomainInt [RangeBounded low upp] :: Dom)

instance (Pretty x, TypeOf x) => DomainOf (OpMod x) where
    domainOf op = mkDomainAny ("OpMod:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpNegate x) where
    domainOf op = mkDomainAny ("OpNegate:" <++> pretty op) <$> typeOf op

instance DomainOf (OpNeq x) where
    domainOf _ = return DomainBool

instance DomainOf (OpNot x) where
    domainOf _ = return DomainBool

instance DomainOf (OpOr x) where
    domainOf _ = return DomainBool

instance DomainOf (OpXor x) where
    domainOf _ = return DomainBool

instance (Pretty x, TypeOf x) => DomainOf (OpParticipants x) where
    domainOf op = mkDomainAny ("OpParticipants:" <++> pretty op) <$> typeOf op

instance DomainOf x => DomainOf (OpParts x) where
    domainOf (OpParts p) = do
        dom <- domainOf p
        case dom of
            DomainPartition _ _ inner -> return $ DomainSet def def $ DomainSet def def inner
            _ -> fail "domainOf, OpParts, not a partition"

instance (Pretty x, TypeOf x) => DomainOf (OpParty x) where
    domainOf op = mkDomainAny ("OpParty:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpPow x) where
    domainOf op = mkDomainAny ("OpPow:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpPowerSet x) where
    domainOf op = mkDomainAny ("OpPowerSet:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpPreImage x) where
    domainOf op = mkDomainAny ("OpPreImage:" <++> pretty op) <$> typeOf op

instance DomainOf x => DomainOf (OpPred x) where
    domainOf (OpPred x) = domainOf x        -- TODO: improve

instance (ExpressionLike x, DomainOf x) => DomainOf (OpProduct x) where
    domainOf (OpProduct x)
        | Just xs <- listOut x
        , not (null xs) = do
        (iPat, i) <- quantifiedVar
        doms <- mapM domainOf xs
        -- maximum absolute value in each domain
        let upps = fromList [ [essence| max([ |&i| | &iPat : &d ]) |]
                            | d <- doms ]
        -- a (too lax) upper bound is multiplying all those together
        let upp  = [essence| product(&upps) |]
        -- a (too lax) lower bound is -upp
        let low  = [essence| -1 * &upp |]
        return $ DomainInt [RangeBounded low upp]
    domainOf _ = return $ DomainInt [RangeBounded 1 1]

instance DomainOf x => DomainOf (OpRange x) where
    domainOf (OpRange f) = do
        fDom <- domainOf f
        case fDom of
            DomainFunction _ _ _ to -> return $ DomainSet def def to
            _ -> fail "domainOf, OpRange, not a function"

instance (Pretty x, TypeOf x) => DomainOf (OpRelationProj x) where
    domainOf op = mkDomainAny ("OpRelationProj:" <++> pretty op) <$> typeOf op

instance (DomainOf x, Dom :< x) => DomainOf (OpRestrict x) where
    domainOf (OpRestrict f x) = do
        d    <- project x
        fDom <- domainOf f
        case fDom of
            DomainFunction fRepr a _ to -> return (DomainFunction fRepr a d to)
            _ -> fail "domainOf, OpRestrict, not a function"

instance (Pretty x, DomainOf x) => DomainOf (OpSlicing x) where
    domainOf (OpSlicing x _ _) = domainOf x
    indexDomainsOf (OpSlicing x _ _) = indexDomainsOf x

instance DomainOf (OpSubsequence x) where
    domainOf _ = fail "domainOf{OpSubsequence}"

instance (Pretty x, TypeOf x) => DomainOf (OpSubset x) where
    domainOf op = mkDomainAny ("OpSubset:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpSubsetEq x) where
    domainOf op = mkDomainAny ("OpSubsetEq:" <++> pretty op) <$> typeOf op

instance DomainOf (OpSubstring x) where
    domainOf _ = fail "domainOf{OpSubstring}"

instance DomainOf x => DomainOf (OpSucc x) where
    domainOf (OpSucc x) = domainOf x        -- TODO: improve

instance (ExpressionLike x, DomainOf x) => DomainOf (OpSum x) where
    domainOf (OpSum x)
        | Just xs <- listOut x
        , not (null xs) = do
        doms <- mapM domainOf xs
        let lows = fromList [ [essence| min(`&d`) |] | d <- doms ]
        let low  = [essence| sum(&lows) |]
        let upps = fromList [ [essence| max(`&d`) |] | d <- doms ]
        let upp  = [essence| sum(&upps) |]
        return (DomainInt [RangeBounded low upp] :: Dom)
    domainOf _ = return $ DomainInt [RangeBounded 0 0]

instance DomainOf (OpSupset x) where
    domainOf _ = return DomainBool

instance DomainOf (OpSupsetEq x) where
    domainOf _ = return DomainBool

instance DomainOf (OpTildeLeq x) where
    domainOf _ = return DomainBool

instance DomainOf (OpTildeLt x) where
    domainOf _ = return DomainBool

instance DomainOf (OpToInt x) where
    domainOf _ = return $ DomainInt [RangeBounded 0 1]

instance (Pretty x, TypeOf x) => DomainOf (OpToMSet x) where
    domainOf op = mkDomainAny ("OpToMSet:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpToRelation x) where
    domainOf op = mkDomainAny ("OpToRelation:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpToSet x) where
    domainOf op = mkDomainAny ("OpToSet:" <++> pretty op) <$> typeOf op

instance DomainOf (OpTogether x) where
    domainOf _ = return DomainBool

instance DomainOf (OpTrue x) where
    domainOf _ = return DomainBool

instance (Pretty x, TypeOf x) => DomainOf (OpTwoBars x) where
    domainOf op = mkDomainAny ("OpTwoBars:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpUnion x) where
    domainOf op = mkDomainAny ("OpUnion:" <++> pretty op) <$> typeOf op

