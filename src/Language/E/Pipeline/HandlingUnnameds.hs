{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.HandlingUnnameds ( handleUnnameds ) where

import Language.E


handleUnnameds :: MonadConjure m => Spec -> m Spec
handleUnnameds =
    handleUnnamedsCore >=>
    return . doReplacements

-- replaces letting t be new type of size n
-- with     letting t_fromUnnamed be domain int(1..n)
-- returns  ( the new spec
--          , (old E, new E)
--          )
handleUnnamedsCore :: MonadConjure m => Spec -> m (Spec, [(E, E)])
handleUnnamedsCore spec
    = flip runStateT []
    $ flip foreachStatement spec $ \ statement ->
        case statement of
            [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                   | [size]          := topLevel.letting.typeUnnamed
                   |] -> do
                modify $ \ st -> ( [xMake| reference := [Prim (S name)] |]
                                 , [xMake| reference := [Prim (S $ name `mappend` "_fromUnnamed")] |]
                                 ) : st
                let lb = [eMake| 1 |]
                let ub = size
                let newDom = [xMake| domain.int.ranges.range.fromTo := [lb,ub] |]
                let newDecl = [xMake| topLevel.letting.name.reference := [Prim (S $ name `mappend` "_fromUnnamed")]
                                    | topLevel.letting.domain         := [newDom]
                                    |]
                return [newDecl]
            _ -> return [statement]

doReplacements :: (Spec, [(E, E)]) -> Spec
doReplacements (Spec v s, mapping) = Spec v $ replaceAll mapping s

