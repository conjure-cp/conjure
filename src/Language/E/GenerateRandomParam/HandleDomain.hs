{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
module Language.E.GenerateRandomParam.HandleDomain(handleDomain) where

import Language.E
import Language.E.GenerateRandomParam.Data
import Language.E.GenerateRandomParam.Common

import Language.E.Up.Debug(upBug)

import Control.Arrow(arr)


_c :: Choice
_c = CInt 51  [RRange 0 49, RSingle 50 ]

handleDomain :: MonadConjure m => E -> m Choice
handleDomain [xMatch| _ := domain.bool |] =  return CBool

handleDomain [xMatch| ranges := domain.int.ranges |] = do
    cRanges <- mapM handleRange ranges
    let sortedRange =  sortOn snd cRanges
    return $ createChoice sortedRange

    where
    createChoice :: [(Integer,Range)] -> Choice
    createChoice = uncurry CInt . first (arr sum) . unzip

handleDomain [xMatch| [inner] := domain.set.inner
                    | attr    := domain.set.attributes.attrCollection|] = do
    dom <- handleDomain inner
    sizeRange <- handleSetAttributes dom attr
    return $ CSet sizeRange dom

handleDomain [xMatch| doms := domain.tuple.inners |]  =
   liftM CTuple (mapM handleDomain doms)

handleDomain [xMatch| [range] := domain.matrix.index
                    | [dom]   := domain.matrix.inner |]  = do
    (CInt _ ranges) <- handleDomain range
    dom'            <- handleDomain dom
    return $ CMatrix ranges dom'

handleDomain [xMatch| inners := domain.relation.inners
                    | attr   := domain.relation.attributes.attrCollection|] = do
    doms <- mapM handleDomain inners
    sizeRange <- handleRelAttributes doms attr
    return $ CRel sizeRange doms

handleDomain [xMatch| [from] := domain.function.innerFrom
                    | [to]   := domain.function.innerTo
                    | attrs  := domain.function.attributes.attrCollection |] = do
    [from',to'] <- mapM handleDomain [from,to]
    let fAttrs =  findAttrs (FAttrs False False False) attrs
        size   =  calcuateSize fAttrs from' to'
    {-error . show .pretty $  CFunc size fAttrs from' to'-}
    return $ CFunc size fAttrs from' to'

    where
    calcuateSize (FAttrs{fTotal=True, fInjective=True, fSurjective=True}) f t  =
       let (fromSize,toSize) = (findSize f, findSize t)
       in if   fromSize == toSize
          then RSingle fromSize
          else error "The domain and range must have equal size for a bijective total function"

    calcuateSize _ _ _ = error "Not done yet"

    findAttrs :: FAttrs -> [E] -> FAttrs
    findAttrs fa [] = fa
    findAttrs fa (x:xs) = findAttrs (f x) xs
        where
        f [xMatch| [Prim (S "total")]      := attribute.name.reference |] = fa{fTotal=True}
        f [xMatch| [Prim (S "surjective")] := attribute.name.reference |] = fa{fSurjective=True}
        f [xMatch| [Prim (S "injective")]  := attribute.name.reference |] = fa{fInjective=True}
        f [xMatch| [Prim (S "bijective")]  := attribute.name.reference |] = fa{fInjective=True,fSurjective=True}
        f _ = fa

handleDomain e = mkLog "U" (prettyAsPaths e <+> "\n"  ) >> return _c


handleRelAttributes :: MonadConjure m => [Choice] -> [E] -> m Range
handleRelAttributes doms es =
    handleSetAttributes' result
    where
    sorted = sort es
    -- To make sure size is at the front if present
    rev    = reverse sorted
    result = addSize rev sorted

    addSize ([xMatch| [Prim (S "size")] := attribute.nameValue.name.reference |] :_)
      _
      = rev

    addSize _
      ([xMatch| [Prim (S "maxSize")] := attribute.nameValue.name.reference |] :_)
      = rev

    addSize _ _   = rev ++ [ [xMake| attribute.nameValue.name.reference := [Prim (S "maxSize")]
                                   | attribute.nameValue.value.value.literal := [Prim (I n)] |] ]
        where n = product . map findSize $ doms

handleSetAttributes :: MonadConjure m => Choice -> [E] -> m Range
handleSetAttributes dom es =
    handleSetAttributes' result
    where
    sorted = sort es
    -- To make sure size is at the front if present
    rev    = reverse sorted
    result = addSize rev sorted

    addSize ([xMatch| [Prim (S "size")] := attribute.nameValue.name.reference |] :_)
      _
      = rev

    addSize _
      ([xMatch| [Prim (S "maxSize")] := attribute.nameValue.name.reference |] :_)
      = rev

    addSize _ _   = rev ++ [ [xMake| attribute.nameValue.name.reference := [Prim (S "maxSize")]
                                   | attribute.nameValue.value.value.literal := [Prim (I n)] |] ]
        where n = findSize dom


findSize :: Choice -> Integer
findSize CBool = 2
findSize (CInt size _)  = size
findSize (CTuple doms)  = product . map findSize $ doms
findSize (CRel range _) = 2 ^ countRange range

findSize (CSet range dom) = result
    where
    dSize = findSize dom
    result  = sizeFromRange range
    sizeFromRange :: Range -> Integer
    sizeFromRange (RSingle k)  = dSize `choose` k
    sizeFromRange (RRange a b) = sum . map (dSize `choose`  ) $ [a..b]

findSize (CMatrix ranges dom ) =  dSize ^ matSize
   where
   matSize = countRanges ranges
   dSize = findSize dom


handleSetAttributes' :: MonadConjure m => [E] -> m Range
handleSetAttributes' [] = _bugg "handleSetAttributes' no attributes"

handleSetAttributes' ([xMatch| [Prim (S "size")] := attribute.nameValue.name.reference
                             | [Prim (I n)]      := attribute.nameValue.value.value.literal|]
                      :_) =
    return $  RSingle n

handleSetAttributes' [[xMatch| [Prim (S "maxSize")] := attribute.nameValue.name.reference
                             | [Prim (I n)]         := attribute.nameValue.value.value.literal|]
                     ] =
    return $  RRange 0 n

handleSetAttributes' [[xMatch| [Prim (S "minSize")] := attribute.nameValue.name.reference
                             | [Prim (I a)]         := attribute.nameValue.value.value.literal|]
                     ,[xMatch| [Prim (S "maxSize")] := attribute.nameValue.name.reference
                             | [Prim (I b)]         := attribute.nameValue.value.value.literal|]
                     ] =
    return $  RRange a b

handleSetAttributes' _ = _bugg "handleSetAttributes': Can never happen"


handleRange :: MonadConjure m => E -> m (Integer,Range)
handleRange [xMatch| [Prim (I a),Prim (I b)] := range.fromTo.value.literal |] =
    return  (abs (b - a) +1, RRange a b )

handleRange [xMatch| [Prim (I n) ]  := range.single.value.literal |] =
    return (1, RSingle n)

handleRange e = do
    mkLog "unhandled" (prettyAsPaths e)
    return (1,RSingle (-99))


choose :: Integral a => a -> a -> a
_ `choose` 0 = 1
0 `choose` _ = 0
n `choose` r = (n-1) `choose` (r-1) * n `div` r




_bug :: String -> [E] -> t
_bug  s = upBug  ("HandleDomain: " ++ s)
_bugg :: String -> t
_bugg s = _bug s []

