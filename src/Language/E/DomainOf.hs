{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.E.DomainOf where

import Stuff.Generic
import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty


domainOf :: Monad m => E -> CompE m E

domainOf [xMatch| [Prim (S i)] := reference |] = do
    bs <- getsLocal binders
    if i == "_"
        then return [xMake| type.unknown := [] |]
        else case [ x | Binder nm x <- bs, nm == i ] of
                (x:_) -> domainOf x
                _   -> do
                    let bsText = sep $ map (\ (Binder nm _) -> stringToDoc nm ) bs
                    err ErrFatal $ "Undefined reference: " <+> pretty i $$ bsText

domainOf [xMatch| [Prim (S i)] := metavar |] = do
    let j = '&' : i
    bs <- getsLocal binders
    case [ x | Binder nm x <- bs, nm == j ] of
        [x] -> domainOf x
        _   -> err ErrFatal $ "Undefined reference: " <+> pretty j

domainOf [xMatch| [x] := topLevel.declaration.find .domain |] = domainOf x
domainOf [xMatch| [x] := topLevel.declaration.given.domain |] = domainOf x

domainOf [xMatch| [x] := domainInExpr |] = domainOf x

domainOf x = return x
