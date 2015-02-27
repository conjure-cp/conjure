{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE QuasiQuotes #-}

module Conjure.Language.DomainOf
    ( domainOf, domainOfInternal
    , DomainOfResult(..)
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Ops
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.TH


data DomainOfResult x = DomainOfResultNoRepr  (Domain () x)
                      | DomainOfResultHasRepr (Domain HasRepresentation x)
    deriving (Eq, Ord, Show)

combineDOR :: Pretty x => [DomainOfResult x] -> (forall r . [Domain r x] -> Domain r x) -> DomainOfResult x
combineDOR dors f =
    let
        (repr, doms1, doms2) = unzip3
            [ case dor of
                DomainOfResultNoRepr  d -> (False,            d, defRepr d)
                DomainOfResultHasRepr d -> (True , forgetRepr d,         d)
            | dor <- dors
            ]
    in
        if False `elem` repr
            then DomainOfResultNoRepr  $ f doms1
            else DomainOfResultHasRepr $ f doms2


combineDOR' :: Pretty x => [DomainOfResult x] -> ([Domain () x] -> Domain () x) -> DomainOfResult x
combineDOR' dors f =
    let
        doms =
            [ case dor of
                DomainOfResultNoRepr  d ->            d
                DomainOfResultHasRepr d -> forgetRepr d
            | dor <- dors
            ]
    in
        DomainOfResultNoRepr  $ f doms

onDOR
    :: Functor m
    => DomainOfResult x
    -> (forall r . Default r => Domain r x -> m (Domain r x))
    -> m (DomainOfResult x)
onDOR (DomainOfResultNoRepr d) f = DomainOfResultNoRepr <$> f d
onDOR (DomainOfResultHasRepr d) f = DomainOfResultHasRepr <$> f d


class DomainOf a x where
    domainOfInternal :: MonadFail m => a -> m (DomainOfResult x)

instance DomainOf Expression Expression where
    domainOfInternal (Reference _ (Just refTo)) = domainOfInternal refTo
    domainOfInternal (Constant c) = domainOfInternal c
    domainOfInternal (AbstractLiteral c) = domainOfInternal c

    -- TODO: each operator should be calculating domains
    domainOfInternal [essence| &x * &y |] = do
        xDom <- domainOf x
        yDom <- domainOf y
        -- TODO: domainOf has to be able to generate uniq names
        let (iPat, i) = quantifiedVar "i"
        let (jPat, j) = quantifiedVar "j"
        let core = [essence| [ &i * &j
                             | &iPat : &xDom
                             , &jPat : &yDom
                             ]
                           |]
        let a    = [essence| min(&core) |]
        let b    = [essence| max(&core) |]
        return $ DomainOfResultNoRepr $ DomainInt [RangeBounded a b]

    domainOfInternal (Op (MkOpIndexing (OpIndexing m i))) = do
        iType <- typeOf i
        case iType of
            TypeBool{} -> return ()
            TypeInt{} -> return ()
            _ -> fail "domainOfInternal, OpIndexing, not a bool or int index"
        mDor  <- domainOfInternal m
        onDOR mDor $ \ mDom -> case mDom of
            DomainMatrix _ inner -> return inner
            DomainTuple inners -> do
                iInt <- intOut i
                return $ atNote "domainOfInternal" inners (fromInteger (iInt-1))
            _ -> fail "domainOfInternal, OpIndexing, not a matrix or tuple"

    domainOfInternal (Op (MkOpDefined (OpDefined f))) = do
        fDor <- domainOfInternal f
        onDOR fDor $ \ fDom -> case fDom of
            DomainFunction _ _ fr _ -> return $ DomainSet def def fr
            _ -> fail "domainOfInternal, OpDefined, not a function"

    domainOfInternal (Op (MkOpRange (OpRange f))) = do
        fDor <- domainOfInternal f
        onDOR fDor $ \ fDom -> case fDom of
            DomainFunction _ _ _ to -> return $ DomainSet def def to
            _ -> fail "domainOfInternal, OpRange, not a function"

    domainOfInternal (Op (MkOpRestrict (OpRestrict f (Domain d)))) = do
        fDor <- domainOfInternal f
        onDOR fDor $ \ fDom -> case fDom of
            DomainFunction fRepr a fr to -> do
                let d' = case reprAtTopLevel fr of
                            Nothing -> defRepr      d
                            Just r  -> changeRepr r d
                return (DomainFunction fRepr a d' to)
            _ -> fail "domainOfInternal, OpRestrict, not a function"

    domainOfInternal x = fail ("domainOfInternal{Expression} 1:" <+> pretty (show x))

instance DomainOf ReferenceTo Expression where
    domainOfInternal (Alias x) = domainOfInternal x
    domainOfInternal (DeclNoRepr  _ _ dom) = return (DomainOfResultNoRepr dom)
    domainOfInternal (DeclHasRepr _ _ dom) = return (DomainOfResultHasRepr dom)
    domainOfInternal x = fail ("domainOfInternal{ReferenceTo} 1:" <+> pretty x)

instance DomainOf Constant Expression where
    domainOfInternal   ConstantBool{} = return $ DomainOfResultNoRepr DomainBool
    domainOfInternal c@ConstantInt {} = return $ DomainOfResultNoRepr $ DomainInt [RangeSingle (Constant c)]
    domainOfInternal (ConstantAbstract (AbsLitTuple cs)) = do
        dors <- mapM domainOfInternal cs
        return (combineDOR dors DomainTuple)
    -- domainOfInternal (ConstantAbstract (AbsLitSet cs)) = do
    --     let size  = cs |> map normaliseConstant |> sortNub |> length |> fromInt
    --     doms <- mapM (domainOfInternal p) cs
    --     return (DomainSet () (SetAttr (SizeAttr_Size size)) (mconcat doms))
    -- domainOfInternal (ConstantAbstract (AbsLitMSet cs)) = do
    --     let size  = cs |> map normaliseConstant |> length |> fromInt
    --     doms <- mapM (domainOfInternal p) cs
    --     return (DomainMSet () (MSetAttr (SizeAttr_Size size) def) (mconcat doms))
    domainOfInternal x = fail ("domainOfInternal{Constant}:" <+> pretty x)

instance DomainOf (AbstractLiteral Expression) Expression where
    domainOfInternal (AbsLitTuple cs) = do
        dors <- mapM domainOfInternal cs
        return (combineDOR dors DomainTuple)
    domainOfInternal (AbsLitMatrix index values) = do
        dors <- mapM domainOfInternal values
        return (combineDOR' dors $ \ doms -> DomainMatrix index (mconcat doms))
    domainOfInternal x = fail ("domainOfInternal{AbstractLiteral}:" <+> pretty x)


domainOf :: MonadFail m => Expression -> m (Domain HasRepresentation Expression)
domainOf x = do
    dor <- domainOfInternal x
    return $ case dor of
        DomainOfResultNoRepr  d -> defRepr d
        DomainOfResultHasRepr d ->         d
