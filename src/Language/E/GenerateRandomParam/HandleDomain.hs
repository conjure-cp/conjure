{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
module Language.E.GenerateRandomParam.HandleDomain(handleDomain,findSize) where

import Language.E
import Language.E.GenerateRandomParam.Data
import Language.E.GenerateRandomParam.Common
import Language.E.Up.Debug(upBug)

import Control.Arrow(arr)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.PrettyPrint as Pr

_c :: Choice
_c = CInt 51  [RRange 0 49, RSingle 50 ]

handleDomain :: MonadConjure m => EnumMap -> E -> m Choice
handleDomain _ [xMatch| _ := domain.bool |] =  return CBool

handleDomain _ [xMatch| ranges := domain.int.ranges |] = do
    cRanges <- mapM handleRange ranges
    let sortedRange =  sortOn snd cRanges
    return $ createChoice sortedRange

    where
    createChoice :: [(Integer,Range)] -> Choice
    createChoice = uncurry CInt . first (arr sum) . unzip

handleDomain em [xMatch| [Prim (S name)] := type.typeEnum |] =
    return $ CEnum len (RRange 0 (len - 1)) enums

    where 
    enums = fromMaybe (_bugg ("Enum missing from mapping in handleDomain" ++ T.unpack name)) 
            $ Map.lookup (T.unpack name) em 
    len   = genericLength enums

handleDomain em [xMatch| [inner] := domain.set.inner
                       | attr    := domain.set.attributes.attrCollection|] = do
    dom <- handleDomain em inner
    sizeRange <- handleSetAttributes dom attr
    return $ CSet sizeRange dom

handleDomain em [xMatch| doms := domain.tuple.inners |]  =
   liftM CTuple (mapM (handleDomain em) doms)

handleDomain em [xMatch| [range] := domain.matrix.index
                       | [dom]   := domain.matrix.inner |]  = do
    (CInt _ ranges) <- handleDomain em range
    dom'            <- handleDomain em dom
    return $ CMatrix ranges dom'

handleDomain em [xMatch| inners := domain.relation.inners
                       | attr   := domain.relation.attributes.attrCollection|] = do
    doms <- mapM (handleDomain em) inners
    sizeRange <- handleRelAttributes doms attr
    return $ CRel sizeRange doms

handleDomain em [xMatch| [from] := domain.function.innerFrom
                       | [to]   := domain.function.innerTo
                       | attrs  := domain.function.attributes.attrCollection |] = do
    [from',to'] <- mapM (handleDomain em) [from,to]
    let fAttrs =  findAttrs (FAttrs False False False) attrs
        size   =  calcuateSize fAttrs from' to'
    {-error . show .pretty $  CFunc size fAttrs from' to'-}
    return $ CFunc size fAttrs from' to'

    where
    --calcuateSize :: FAttrs -> Choice -> Choice -> Integer 
    calcuateSize (FAttrs{fTotal=True, fInjective=True, fSurjective=True}) f t  =
       let (fromSize,toSize) = (findSize f, findSize t)
       in if   fromSize == toSize
          then RSingle fromSize
          else  error . show $ hsep  ["The Domain size"
                                     , Pr.parens (pretty fromSize)
                                     , "and Range size"
                                     , Pr.parens (pretty toSize)
                                     , "must have a equal Length for a bijective function"
                                     ] 
                                     Pr.$+$  
                                     (nest 4 . vcat . map (pretty .show) $ [ f, t ])

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

handleDomain _ e = mkLog "U" (prettyAsPaths e <+> "\n"  ) >> return _c


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

findSize (CRel range vs) = result 
    where 
    dSize  = (product . map findSize) vs 
    result = sizeFromRange dSize range 

findSize (CSet range dom) = result
    where
    dSize  = findSize dom
    result = sizeFromRange dSize range

findSize (CMatrix ranges dom ) =  dSize ^ matSize
   where
   matSize = countRanges ranges
   dSize = findSize dom

-- for a set
sizeFromRange :: Integer -> Range -> Integer
sizeFromRange d (RSingle k)  =  d `choose` k
sizeFromRange d (RRange a b) = sum . map (d `choose`  ) $ [a..b]

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

