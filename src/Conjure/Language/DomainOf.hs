{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Conjure.Language.DomainOf
    ( domainOf, domainOfInternal
    , DomainOfResult(..)
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Constant
import Conjure.Language.Ops
import Conjure.Language.Pretty
import Conjure.Language.TypeOf


data DomainOfResult x = DomainOfResultNoRepr  (Domain () x)
                      | DomainOfResultHasRepr (Domain HasRepresentation x)
    deriving (Eq, Ord, Show)

combineDOR :: [DomainOfResult x] -> (forall r . [Domain r x] -> Domain r x) -> DomainOfResult x
combineDOR dors f =
    let
        (repr, doms1, doms2) = unzip3
            [ case dor of
                DomainOfResultNoRepr  d -> (False, forgetRepr d, forgetRepr d)
                DomainOfResultHasRepr d -> (True , forgetRepr d, forgetRepr d)
            | dor <- dors
            ]
    in
        if any (==False) repr
            then DomainOfResultNoRepr  $ f doms1
            else DomainOfResultHasRepr $ f doms2


combineDOR' :: [DomainOfResult x] -> ([Domain () x] -> Domain () x) -> DomainOfResult x
combineDOR' dors f =
    let
        doms =
            [ case dor of
                DomainOfResultNoRepr  d -> forgetRepr d
                DomainOfResultHasRepr d -> forgetRepr d
            | dor <- dors
            ]
    in
        DomainOfResultNoRepr  $ f doms


class DomainOf a x where
    domainOfInternal :: MonadFail m => a -> m (DomainOfResult x)

instance DomainOf Expression Expression where
    domainOfInternal (Reference _ (Just refTo)) = domainOfInternal refTo
    domainOfInternal (Constant c) = domainOfInternal c
    domainOfInternal (AbstractLiteral c) = domainOfInternal c
    domainOfInternal x = fail ("domainOfInternal{Expression} 1:" <+> pretty (show x))

-- instance DomainOf Expression Expression where
--     domainOfInternal p (Reference _ (Just refTo)) = domainOfInternal p refTo
--     domainOfInternal p x@(Op (MkOpIndexing (OpIndexing m i))) = do
--         mDomain <- domainOfInternal p m
--         iType   <- typeOf i
--         case (mDomain, iType) of
--             (DomainMatrix _ inner, TypeInt{}) -> return inner
--             (DomainTuple inners  , TypeInt{}) -> do
--                 iInt <- intOut i
--                 return (at inners (iInt-1))
--             _ -> fail $ vcat [ "domainOfInternal{Expression} 2.1"
--                              , pretty x
--                              , pretty mDomain
--                              , pretty iType
--                              ]
--     domainOfInternal _ x = forgetRepr <$> domainOfInternal (Proxy :: Proxy ()) x

instance DomainOf ReferenceTo Expression where
    domainOfInternal (Alias x) = domainOfInternal x
    domainOfInternal (DeclNoRepr  _ _ dom) = return (DomainOfResultNoRepr dom)
    domainOfInternal (DeclHasRepr _ _ dom) = return (DomainOfResultHasRepr dom)
    domainOfInternal x = fail ("domainOfInternal{ReferenceTo} 1:" <+> pretty x)

-- instance DomainOf HasRepresentation Expression ReferenceTo where
--     domainOfInternal p (Alias x) = domainOfInternal p x
--     domainOfInternal _ (DeclHasRepr _ _ dom) = return dom
--     domainOfInternal _ x = fail ("domainOfInternal{ReferenceTo} 2:" <+> pretty x)

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
        DomainOfResultNoRepr  d -> forgetRepr d
        DomainOfResultHasRepr d -> d
