{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
module Language.E.GenerateRandomParam.HandleDomain(handleDomain,findSize,choose) where

import Language.E
import Language.E.GenerateRandomParam.Data
import Language.E.GenerateRandomParam.Common(countRanges)
import Language.E.Up.Debug(upBug)

import Control.Arrow(arr)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.PrettyPrint as Pr

--import Text.Groom(groom)

-- TO hold the size
data ASize = ASize
    {aMinSize :: Maybe Integer
    ,aMaxSize :: Maybe Integer
    ,aSize    :: Maybe Integer
    }deriving (Show)


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
    let fAttrs =  fixAttrs . findAttrs (FAttrs False False False) $ attrs
        size   =  calcuateSize
                  (sizeAttributes (ASize Nothing Nothing Nothing) attrs)
                  fAttrs from' to'
    {-error . show .pretty $  CFunc size fAttrs from' to'-}
    return $ CFunc size fAttrs from' to'

    where
    calcuateSize :: ASize -> FAttrs -> Choice -> Choice -> Range
    -- Assume these are used correctly
    calcuateSize (ASize{aSize =Just n}) _ _ _ = RSingle n
    calcuateSize (ASize{aMinSize=Just a, aMaxSize=Just b}) _ _ _ = RRange a b
    -- TODO handle other size cases

    calcuateSize (ASize{aMinSize=Just mi}) fa f t =
        fu (calcuateSize (ASize Nothing Nothing Nothing) fa f t )

        where
        fu (RSingle n)  | n > mi = RRange mi n
        fu (RSingle n)  | n < mi = RSingle mi
        fu (RRange a b) | a > mi = RRange mi b
        fu (RRange _ b) | b < mi = RSingle mi
        fu r                     = r

    calcuateSize (ASize{aMaxSize=Just ma}) fa f t =
        fu (calcuateSize (ASize Nothing Nothing Nothing) fa f t )

        where
        fu (RSingle n)  | n > ma = RSingle ma
        fu (RSingle n)  | n < ma = RRange n ma
        fu (RRange a b) | b > ma = RRange a ma
        fu (RRange a _) | a > ma = RSingle ma
        fu r                     = r

    calcuateSize _ (FAttrs{fInjective=True, fSurjective=True}) f t  =
       let (fromSize,toSize) = (findSize f, findSize t)
       in if   fromSize == toSize
          then RSingle fromSize
          else errr fromSize toSize f t "must have a equal Length for a bijective function"

    calcuateSize _ (FAttrs{fTotal=True}) f _     = RSingle (findSize f)

    calcuateSize _ (FAttrs{fInjective=True}) f t =
       let (fromSize,toSize) = (findSize f, findSize t)
       in  RRange 0 (min fromSize toSize)

    calcuateSize _ (FAttrs{fSurjective=True}) f t =
       let (fromSize,toSize) = (findSize f, findSize t)
       in fu fromSize toSize

       where
       fu a b | a == b = RSingle a
       fu a b | a > b  = RRange b a
       fu a b = errr a b f t "domain size must be >= the range's size"

    calcuateSize _ (FAttrs{fSurjective=False,fInjective=False}) f _ =
       let fromSize = findSize f
       in  RRange 0 fromSize

    fixAttrs :: FAttrs -> FAttrs
    fixAttrs fa@(FAttrs{fInjective=True,fSurjective=True,fTotal=False}) = fa{fTotal=True}
    fixAttrs fa = fa

    findAttrs :: FAttrs -> [E] -> FAttrs
    findAttrs fa []     = fa
    findAttrs fa (x:xs) = findAttrs (f x) xs
        where
        f [xMatch| [Prim (S name)] := attribute.name.reference |] =  case name of
            "injective"  -> fa{fInjective=True}
            "total"      -> fa{fTotal=True}
            "surjective" -> fa{fSurjective=True}
            "bijective"  -> fa{fInjective=True,fSurjective=True,fTotal=True}
            _            -> fa
        f _ = fa

    -- Shows a error when the attributes can not be vaild
    errr a b f t s =  error . show $ hsep
            ["The Domain size"
            , Pr.parens (pretty a)
            , "and Range size"
            , Pr.parens (pretty b)
            , s
            ]
            Pr.$+$
            (nest 4 . vcat . map (pretty .show) $ [ f, t ])


handleDomain _ e = mkLog "U" (prettyAsPaths e <+> "\n"  ) >> return _c

-- Process the size attribute until we get a size attribute which is assumed correct
sizeAttributes :: ASize -> [E] -> ASize
sizeAttributes s []     = s
sizeAttributes s (x:xs) = sizeAttributes (f x) xs
    where
    f [xMatch| [Prim (S name)] := attribute.nameValue.name.reference
             | [Prim (I n)]    := attribute.nameValue.value.value.literal|] =
        case (isJust . aSize $ s, name) of
          (True,_)      -> s
          (_,"size")    -> s{aSize = Just n,aMinSize = Just n, aMaxSize = Just n}
          (_,"minSize") -> s{aMinSize = Just n}
          (_,"maxSize") -> s{aMaxSize = Just n}
          (_,_)         -> s
    f _ = s

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
findSize (CInt size _)    = size
findSize (CEnum size _ _) = size
findSize (CTuple doms)    = product . map findSize $ doms

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


-- Assume function is sane

findSize fu@(CFunc  (RRange _ _) (FAttrs{fTotal=True}) _ _) =
    error . show . vcat $ ["HandleDomain: findSize: Invaild size for total function", pretty fu] 

findSize (CFunc (RSingle size) (FAttrs{fInjective=True, fSurjective=True, fTotal=_}) _ _) =
    product [1 .. size ]

findSize (CFunc range (FAttrs{fInjective=True, fTotal=total}) f t) =
    if isVaildSize total size 
    then sum . map (injSize' fromSize toSize) $ size
    else error "total function with invaild size specifed"

    where
    isVaildSize True [n] = n == fromSize
    isVaildSize True  _  = False
    isVaildSize False _  = True

    fromSize =  findSize f
    toSize   =  findSize t

    size = handleSize range
    handleSize (RSingle a)  = [a]
    handleSize (RRange a b) = [a..b]

    injSize' d r n = injSize  n d r 
    injSize :: (Integral n, Integral m, Show n) => n -> m -> m -> m
    injSize n _ _ | n < 0 = error $ "inj with negative size" ++ show n 
    injSize 0 _ _ = 1
    injSize 1 d r = d * r
    -- this case for size 2 is not really needed
    injSize 2 d r = sum [0..(d-1)]  * r * (r-1)
    injSize n d r = sum . map (\c -> r * injSize (n-1) c (r-1) ) $ [1 .. d - 1]



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

