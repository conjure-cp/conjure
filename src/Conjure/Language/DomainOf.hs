{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Conjure.Language.DomainOf ( domainOf ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Constant
import Conjure.Language.Ops
import Conjure.Language.Pretty
import Conjure.Language.TypeOf


class DomainOf r x a where
    domainOfInternal :: MonadFail m => Proxy r -> a -> m (Domain r x)

instance DomainOf () Expression Expression where
    domainOfInternal p (Reference _ (Just refTo)) = domainOfInternal p refTo
    domainOfInternal p (Constant c) = domainOfInternal p c
    domainOfInternal _ x = fail ("domainOfInternal{Expression} 1:" <+> pretty x)

instance DomainOf HasRepresentation Expression Expression where
    domainOfInternal p (Reference _ (Just refTo)) = domainOfInternal p refTo
    domainOfInternal p x@(Op (MkOpIndexing (OpIndexing m i))) = do
        mDomain <- domainOfInternal p m
        iType   <- typeOf i
        case (mDomain, iType) of
            (DomainMatrix _ inner, TypeInt{}) -> return inner
            (DomainTuple inners  , TypeInt{}) -> do
                iInt <- intOut i
                return (at inners (iInt-1))
            _ -> fail $ vcat [ "domainOfInternal{Expression} 2.1"
                             , pretty x
                             , pretty mDomain
                             , pretty iType
                             ]
    domainOfInternal _ x = fail ("domainOfInternal{Expression} 2.2:" <+> pretty x)

instance DomainOf () Expression ReferenceTo where
    domainOfInternal p (Alias x) = domainOfInternal p x
    domainOfInternal _ (DeclNoRepr  _ _ dom) = return dom
    domainOfInternal _ (DeclHasRepr _ _ dom) = return (forgetRepr dom)
    domainOfInternal _ x = fail ("domainOfInternal{ReferenceTo} 1:" <+> pretty x)

instance DomainOf HasRepresentation Expression ReferenceTo where
    domainOfInternal p (Alias x) = domainOfInternal p x
    domainOfInternal _ (DeclHasRepr _ _ dom) = return dom
    domainOfInternal _ x = fail ("domainOfInternal{ReferenceTo} 2:" <+> pretty x)

instance DomainOf () Expression Constant where
    domainOfInternal _   ConstantBool{} = return DomainBool
    domainOfInternal _ c@ConstantInt {} = return $ DomainInt [RangeSingle (Constant c)]
    domainOfInternal p (ConstantAbstract (AbsLitTuple cs)) = do
        doms <- mapM (domainOfInternal p) cs
        return (DomainTuple doms)
    domainOfInternal p (ConstantAbstract (AbsLitSet cs)) = do
        let size  = cs |> map normaliseConstant |> sortNub |> length |> fromInt
        doms <- mapM (domainOfInternal p) cs
        return (DomainSet () (SetAttr (SizeAttr_Size size)) (mconcat doms))
    domainOfInternal p (ConstantAbstract (AbsLitMSet cs)) = do
        let size  = cs |> map normaliseConstant |> length |> fromInt
        doms <- mapM (domainOfInternal p) cs
        return (DomainMSet () (MSetAttr (SizeAttr_Size size) def) (mconcat doms))
    domainOfInternal _ x = fail ("domainOfInternal{Constant}:" <+> pretty x)


domainOf :: MonadFail m => Expression -> m (Domain HasRepresentation Expression)
domainOf = domainOfInternal (Proxy :: Proxy HasRepresentation)

