{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.DomainOf where

import Stuff.Generic
import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Pretty
import {-# SOURCE #-} Language.E.Evaluator.ToInt


errDomainOf :: (MonadConjure m, Pretty p) => p -> m a
errDomainOf p = err ErrFatal $ "Cannot calculate the domain of" <+> pretty p


domainOf :: MonadConjure m => E -> m E

domainOf [xMatch| [Prim (S i')] := reference |] = do
    let i = head $ splitOn "#" i'
    bs <- gets binders
    if i == "_"
        then return [xMake| type.unknown := [] |]
        else case [ x | Binder nm x <- bs, nm == i ] of
                (x:_) -> domainOf x
                _   -> do
                    let bsText = sep $ map (\ (Binder nm _) -> stringToDoc nm ) bs
                    err ErrFatal $ "Undefined reference: " <+> pretty i $$ bsText

domainOf p@[xMatch| [Prim (S i)] := metavar |] = do
    let j = '&' : i
    bs <- gets binders
    case [ x | Binder nm x <- bs, nm == j ] of
        [x] -> domainOf x
        _   -> return p -- this is for hasDomain pattern matching in rules to work

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

