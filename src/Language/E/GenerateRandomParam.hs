--{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.GenerateRandomParam ( generateRandomParam ) where

import Language.E
import Language.E.DomainOf(domainOf)
{-import Language.E.Up.Debug(prettyAsBoth,upBug)-}
import Language.E.Up.Debug(upBug)
import Language.E.Up.IO(getSpec)
import Language.E.Up.ReduceSpec(reduceSpec)
import Text.Groom(groom)

import Control.Arrow(arr)

type Essence      = Spec
type EssenceParam = Spec

-- for choices

data Choice =
    CInt Integer [Range]
    deriving (Show)

data Range  =
    RSingle Integer
  | RRange Integer Integer
    deriving (Show, Eq)

-- This assumes no overlapping ranges
instance Ord Range where
  (RSingle a ) <= (RSingle b ) = a <= b
  (RRange a _) <= (RRange c _) = a <= c
  (RRange _ b) <= (RSingle c ) = b <= c
  (RSingle a)  <= (RRange b _) = a <= b

_c :: Choice
_c = CInt 51  [RRange 0 49, RSingle 50 ]

evalChoice :: (MonadConjure m, RandomM m) => Choice -> m E
evalChoice (CInt size ranges) = do
    index <- rangeRandomM (0, fromIntegral size-1)
    let n = pickIth (toInteger index) ranges
    mkLog "Index" (pretty index)
    mkLog "Ranges" (pretty . show $ ranges)
    mkLog "Picked" (pretty n)
    return [xMake| value.literal := [Prim (I n )] |]

pickIth :: Integer -> [Range] -> Integer
pickIth _ [] = _bugg "pickIth no values"
pickIth 0 (RSingle i:_) = i
pickIth index (RRange a b:_ ) | index <= b - a =  [a..b] `genericIndex2`  index

pickIth index (RSingle _:xs)    = pickIth (index - 1) xs
pickIth index (RRange a b:xs) = pickIth (index - (b - a) - 1 ) xs

genericIndex2 :: [Integer] -> Integer -> Integer
genericIndex2 a b | b < 0 = error $ "genericIndex2" ++ show  (a,b)
genericIndex2 a b = genericIndex a b


generateRandomParam :: (MonadConjure m, RandomM m) => Essence -> m EssenceParam
generateRandomParam essence = do
    i <- rangeRandomM (0, 5)
    mkLog "rnd test" (pretty i)

    let stripped@(Spec _ _) = stripDecVars essence
    reduced@(Spec v e) <- reduceSpec stripped
    let es = statementAsList e

    --mkLog "Spec" (vcat $ map (\a -> prettyAsPaths a <+> "\n" ) (statementAsList f) )
    mkLog "GivensSpec" (pretty stripped)
    mkLog "Reduced" (pretty reduced)

    doms <-  mapM domainOf es
    mkLog "Doms" (vcat $ map (\a -> prettyAsPaths a <+> "\n" ) doms )

    choices <-  mapM handleDomain doms
    mkLog "Choices" ((pretty . groom) choices )

    givens <- mapM evalChoice choices

    let lettings = zipWith makeLetting es givens
    mkLog "Lettings" (vcat $ map pretty lettings)
    --mkLog "Lettings" (vcat $ map (\a -> prettyAsBoth a <+> "\n" ) lettings )

    let essenceParam = Spec v (listAsStatement lettings )
    --mkLog "EssenceParam" (pretty essenceParam)

    return essenceParam
    --return essence


makeLetting :: E -> E -> E
makeLetting given val =
    [xMake| topLevel.letting.name := [getRef given]
          | topLevel.letting.expr := [val]|]

    where
    getRef :: E -> E
    getRef [xMatch|  _  := topLevel.declaration.given.name.reference
                  | [n] := topLevel.declaration.given.name |] = n
    getRef e = _bug "getRef: should not happen" [e]


handleDomain :: MonadConjure m => E -> m Choice
handleDomain [xMatch| ranges := domain.int.ranges |] = do
    --mkLog "ranges" (pretty ranges)
    cRanges <- mapM handleRange ranges
    let sortedRange =  sortOn snd cRanges
    return $ createChoice sortedRange

    where
    createChoice :: [(Integer,Range)] -> Choice
    createChoice = uncurry CInt . first (arr sum) . unzip


handleDomain e = do
    mkLog "unhandled" (prettyAsPaths e)
    return _c


handleRange :: MonadConjure m => E -> m (Integer,Range)
handleRange [xMatch| [Prim (I a),Prim (I b)] := range.fromTo.value.literal |] =
    return  (abs (b - a) +1, RRange a b )

handleRange [xMatch| [Prim (I n) ]  := range.single.value.literal |] =
    return (1, RSingle n)

handleRange e = do
    mkLog "unhandled" (prettyAsPaths e)
    return (1,RSingle (-99))


stripDecVars :: Essence -> Essence
stripDecVars (Spec v x) = Spec v y
    where
        xs = statementAsList x
        ys = filter stays xs
        y  = listAsStatement ys

        stays [xMatch| _ := topLevel.declaration.given |] = True
        stays [xMatch| _ := topLevel.letting           |] = True
        stays [xMatch| _ := topLevel.where             |] = True
        stays _ = False


_r :: IO Essence -> IO [(Either Doc EssenceParam, LogTree)]
_r sp = do
    seed <- getStdGen
    spec <- sp
    return $ runCompE "gen" (set_stdgen seed >> generateRandomParam spec)

_d :: Choice -> IO [(Either Doc E, LogTree)]
_d c = do
    seed <- getStdGen
    return $ runCompE "gen" (set_stdgen seed >> evalChoice c)

-- _x  =<<  _r _i
_x :: [(Either Doc a, LogTree)] -> IO ()
_x ((_, lg):_) =   print (pretty lg)
_x _ = return ()

_getTest :: FilePath -> IO Spec
_getTest f = getSpec $ "/Users/bilalh/CS/conjure/test/generateParams/" ++ f  ++ ".essence"

_e :: IO Spec
_e = _getTest "enum-1"
_f :: IO Spec
_f = _getTest "func-1"
_i :: IO Spec
_i = _getTest "int-1"
_i2 :: IO Spec
_i2 = _getTest "int-2"
_l :: IO Spec
_l = _getTest "letting-1"
_p :: IO Spec
_p = _getTest "partition-1"
_s :: IO Spec
_s = _getTest "set-1"

_bug :: String -> [E] -> t
_bug  s = upBug  ("GenerateRandomParam: " ++ s)
_bugg :: String -> t
_bugg s = _bug s []
