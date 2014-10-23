{-# LANGUAGE MultiParamTypeClasses #-}

module Conjure.Language.DomainOf where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Ops
import Conjure.Language.Pretty
import Conjure.Language.IntContainer
import Conjure.Language.TypeOf


class DomainOf r x a where
    domainOf :: MonadFail m => Proxy r -> a -> m (Domain r x)

instance DomainOf () Expression Expression where
    domainOf p (Reference _ (Just refTo)) = domainOf p refTo
    domainOf _ x = bug ("domainOf{Expression} 1:" <+> pretty x)

instance DomainOf HasRepresentation Expression Expression where
    domainOf p (Reference _ (Just refTo)) = domainOf p refTo
    domainOf p x@(Op (MkOpIndexing (OpIndexing m i))) = do
        mDomain <- domainOf p m
        iType   <- typeOf i
        case (mDomain, iType) of
            (DomainMatrix _ inner, TypeInt{}) -> return inner
            (DomainTuple inners  , TypeInt{}) -> do
                iInt <- intOut i
                return (at inners (iInt-1))
            _ -> bug ("domainOf{Expression} 2.1:" <+> pretty x)
    domainOf _ x = bug ("domainOf{Expression} 2.2:" <+> pretty x)

instance DomainOf () Expression ReferenceTo where
    domainOf p (Alias x) = domainOf p x
    domainOf _ (DeclNoRepr  _ _ dom) = return dom
    domainOf _ (DeclHasRepr _ _ dom) = return (forgetRepr dom)
    domainOf _ x = bug ("domainOf{ReferenceTo} 1:" <+> pretty x)

instance DomainOf HasRepresentation Expression ReferenceTo where
    domainOf p (Alias x) = domainOf p x
    domainOf _ (DeclHasRepr _ _ dom) = return dom
    domainOf _ x = bug ("domainOf{ReferenceTo} 2:" <+> pretty x)

domainOf' :: MonadFail m => Expression -> m (Domain HasRepresentation Expression)
domainOf' = domainOf (Proxy :: Proxy HasRepresentation)

