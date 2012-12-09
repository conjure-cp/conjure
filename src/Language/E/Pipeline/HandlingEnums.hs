{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.HandlingEnums ( handleEnums ) where

import Language.E


handleEnums :: MonadConjure m => Spec -> m Spec
handleEnums =
    handleEnumsLetting >=>
    return . doReplacements >=>
    updateGivenFinds

-- replaces letting e be new type enum {a, b, c}
-- with     letting e_fromEnum be domain int(1..3)
-- returns ( the new spec
--         , (old E , new E)
--         ) 
handleEnumsLetting :: MonadConjure m => Spec -> m (Spec, [(E, E)])
handleEnumsLetting spec
    = flip runStateT []
    $ flip foreachStatement spec $ \ statement ->
        case statement of
            [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                   | values          := topLevel.letting.typeEnum.values
                   |] -> do
                let enumLookup = zip values $ map (\ i -> [xMake| value.literal := [Prim (I i)] |] ) [1..]
                modify $ \ st -> ( [xMake| reference := [Prim (S name)] |]
                                 , [xMake| reference := [Prim (S $ name `mappend` "_fromEnum")] |]
                                 ) : enumLookup ++ st
                let enumCount = genericLength values
                let lb = [eMake| 1 |]
                let ub = [xMake| value.literal := [Prim (I enumCount)] |]
                let newDom = [xMake| domain.int.ranges.range.fromTo := [lb,ub] |]
                let newDecl = [xMake| topLevel.letting.name.reference := [Prim (S $ name `mappend` "_fromEnum")]
                                    | topLevel.letting.domain         := [newDom]
                                    |]
                return [newDecl]
            _ -> return [statement]

doReplacements :: (Spec, [(E, E)]) -> Spec
doReplacements (Spec v s, mapping) = Spec v $ replaceAll mapping s

-- update those givens and finds whose domain refers to used-to-be-enums
updateGivenFinds :: MonadConjure m => Spec -> m Spec
updateGivenFinds spec = runIdentityT $ flip foreachStatement spec $ \ statement ->
    case statement of
        [xMatch| [findName] := topLevel.declaration.find.name
               | enumRanges := topLevel.declaration.find.domain.domain.enum.ranges
               |] -> do
            let newDom = [xMake| domain.int.ranges := enumRanges |]
            let newDecl = [xMake| topLevel.declaration.find.name   := [findName]
                                | topLevel.declaration.find.domain := [newDom]
                                |]
            return [newDecl]
        _ -> return [statement]

