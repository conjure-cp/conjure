{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.DomainOf where

import Stuff.Generic
import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import {-# SOURCE #-} Language.E.Evaluator.ToInt


errDomainOf :: MonadConjure m => E -> m E
errDomainOf p = do
    bsText <- bindersDoc
    err ErrFatal $ vcat [ "Cannot calculate the domain of" <+> prettyAsPaths p
                        , bsText
                        ]


domainOf :: MonadConjure m => E -> m E

domainOf [xMatch| [Prim (S i)] := reference |] =
    if i == "_"
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
                _ -> errDomainOf p
        _ -> errDomainOf p

domainOf x = return x

