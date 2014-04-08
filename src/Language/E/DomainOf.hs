{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.DomainOf
    ( errDomainOf
    , domainOf
    , innerDomainOf
    ) where

import Stuff.Generic
import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Pretty
import {-# SOURCE #-} Language.E.Evaluator.ToInt


errDomainOf :: MonadConjure m => E -> m E
errDomainOf p = do
    bsText <- bindersDoc
    err ErrFatal $ vcat [ "Cannot calculate the domain of" <+> prettyAsPaths p
                        , bsText
                        ]


domainOf :: MonadConjure m => E -> m E

domainOf x@[xMatch| _ := domain |] = return x

domainOf [xMatch| [Prim (S i)] := reference |] =
    if i `elem` ["_", "forAll", "exists", "sum"]
        then return [xMake| type.unknown := [] |]
        else do
            x <- errMaybeT "domainOf" lookupReference i
            domainOf x

domainOf p@[xMatch| [Prim (S i)] := metavar |] = do
    mx <- runMaybeT $ lookupMetaVar i
    case mx of
        Just x  -> domainOf x
        Nothing -> return p -- this is for hasDomain pattern matching in rules to work

domainOf [xMatch| [x] := topLevel.declaration.find .domain |] = domainOf x
domainOf [xMatch| [x] := topLevel.declaration.given.domain |] = domainOf x

domainOf [xMatch| [x] := domainInExpr |] = domainOf x

domainOf [xMatch| [x] := structural.single |] = domainOf x

domainOf [xMatch| [Prim (S n1)] := quanVar.name
                | [Prim (S n2)] := quanVar.within.quantified.quanVar.structural.single.reference
                | [d]           := quanVar.within.quantified.quanOverDom
                |] | n1 == n2 = return d

domainOf p@[xMatch| [] := quanVar |] = return p

domainOf p@[xMatch| [x] := operator.index.left
                  | [y] := operator.index.right
                  |] = do
    xDom <- domainOf x
    case xDom of
        [xMatch| [innerDom] := domain.matrix.inner |] -> return innerDom
        [xMatch|  innerDoms := domain.tuple.inners |] -> do
            yInt <- toInt y
            case yInt of
                Just (int, _)
                    | int >= 1 && int <= genericLength innerDoms
                    -> return $ innerDoms `genericIndex` (int - 1)
                _ -> return p
        _ -> return p

domainOf x = do
    mkLog "missing:domainOf" (pretty x)
    return x

innerDomainOf :: E -> Maybe E
innerDomainOf [xMatch| [ty] := domain.     set.inner  |] = return ty
innerDomainOf [xMatch| [ty] := domain.    mset.inner  |] = return ty
innerDomainOf [xMatch| tys  := domain.relation.inners |] = return [xMake| domain.tuple.inners := tys |]
innerDomainOf [xMatch| [fr] := domain.function.innerFrom
                     | [to] := domain.function.innerTo |] = return [xMake| domain.tuple.inners := [fr,to] |]
innerDomainOf _ = Nothing


