{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Language.E.Evaluator.Full
    ( fullEvaluator
    , evalDomSize
    , evalDontCare
    , evalHasDomain
    , evalHasRepr
    , evalHasType
    , evalIndices
    , evalReplace
    , matrixEq
    , tupleEq
    , dotOrderDecomposeForTuples
    , dotOrderDecomposeForMatrices
    , stripStructuralSingle
    , stripUnnecessaryTyped
    , unrollQuantifiers
    , domSize
    ) where

import Bug
import Language.E.Imports
import Language.E.Definition
import Language.E.Helpers
import Language.E.CompE
import Language.E.DomainOf
import Language.E.MatchBind
import Language.E.TH
import Language.E.TypeOf
import Language.E.Pretty
import {-# SOURCE #-} Language.E.Evaluator.ToInt
import                Language.E.Evaluator.DataAboutQuantifiers


fullEvaluator :: MonadConjure m => E -> m (Maybe (E,[Binder]))

-- order integer domains given by a list of integers
-- savilerow expects them ordered
fullEvaluator (D (DomainInt rs))
    | Just xs <- view rs
    , let xsSorted = sortNub xs
    , xs /= xsSorted
    , let rsSorted = map (\ i -> RangeSingle [xMake| value.literal := [Prim (I i)] |] ) xsSorted
    = ret $ D $ DomainInt rsSorted
    where
        view1 (RangeSingle [xMatch| [Prim (I i)] := value.literal |]) = Just i
        view1 _ = Nothing
        view [] = Just []
        view (i:is) = (:) <$> view1 i <*> view is

fullEvaluator [eMatch| &a <-> &b |] = ret [eMake| &a = &b |]

-- this is a small hack to make rule language work.
fullEvaluator [xMatch| [Prim (S "!=")] := binOp.operator
                     | [Prim (S "_") ] := binOp.left .reference
                     | [Prim (S "_") ] := binOp.right.reference
                     |] = returnBool False
fullEvaluator [xMatch| [Prim (S "!=")] := binOp.operator
                     | [Prim (S "_") ] := binOp.right.reference
                     |] = returnBool True
fullEvaluator [xMatch| [Prim (S "=") ] := binOp.operator
                     | [Prim (S "_") ] := binOp.left .reference
                     | [Prim (S "_") ] := binOp.right.reference
                     |] = returnBool True
fullEvaluator [xMatch| [Prim (S "=") ] := binOp.operator
                     | [Prim (S "_") ] := binOp.right.reference
                     |] = returnBool False

-- this following evaluator is wrong if wither identifier is undefined.
-- adding it nevertheless, because we need it.
fullEvaluator [xMatch| [Prim (S "=") ] := binOp.operator
                     | [Prim (S  a ) ] := binOp.left .reference
                     | [Prim (S  b ) ] := binOp.right.reference
                     |] | a == b = returnBool True

fullEvaluator [xMatch| [Prim (I n)] := unaryOp.negate.value.literal
                     |] = returnInt (-n)
fullEvaluator [xMatch| [Prim (I n)] := unaryOp.factorial.value.literal
                     |] = returnInt (product [1..n])

fullEvaluator [xMatch| [Prim (S "+")] := binOp.operator
                     | [Prim (I a)  ] := binOp.left .value.literal
                     | [Prim (I b)  ] := binOp.right.value.literal
                     |] = returnInt $ a + b
fullEvaluator [xMatch| [Prim (S "-")] := binOp.operator
                     | [Prim (I a)  ] := binOp.left .value.literal
                     | [Prim (I b)  ] := binOp.right.value.literal
                     |] = returnInt $ a - b
fullEvaluator [xMatch| [Prim (S "*")] := binOp.operator
                     | [Prim (I a)  ] := binOp.left .value.literal
                     | [Prim (I b)  ] := binOp.right.value.literal
                     |] = returnInt $ a * b
fullEvaluator [xMatch| [Prim (S "/")] := binOp.operator
                     | [Prim (I a)  ] := binOp.left .value.literal
                     | [Prim (I b)  ] := binOp.right.value.literal
                     |] | b > 0 = returnInt $ a `div` b
fullEvaluator [xMatch| [Prim (S "%")] := binOp.operator
                     | [Prim (I a)  ] := binOp.left .value.literal
                     | [Prim (I b)  ] := binOp.right.value.literal
                     |] | b > 0 = returnInt $ a `mod` b
fullEvaluator [xMatch| [Prim (S "**")] := binOp.operator
                     | [Prim (I a)   ] := binOp.left .value.literal
                     | [Prim (I b)   ] := binOp.right.value.literal
                     |] | b > 0 = returnInt $ a ^ b

-- comparison on integer literals
fullEvaluator [xMatch| [Prim (S op)] := binOp.operator
                     | [Prim (I a )] := binOp.left .value.literal
                     | [Prim (I b )] := binOp.right.value.literal
                     |] | Just f <- lookup op comparators
                        = returnBool $ f a b
    where comparators = [ ( ">"  , (>)  )
                        , ( ">=" , (>=) )
                        , ( "<"  , (<)  )
                        , ( "<=" , (<=) )
                        , ( "="  , (==) )
                        , ( "!=" , (/=) )
                        , ( ".<" , (<)  )
                        , ( ".<=", (<=) )
                        ]

-- comparison on boolean literals
fullEvaluator [xMatch| [Prim (S op)] := binOp.operator
                     | [Prim (B a )] := binOp.left .value.literal
                     | [Prim (B b )] := binOp.right.value.literal
                     |] | Just f <- lookup op comparators
                        = returnBool $ f a b
    where comparators = [ ( ">"  , (>)  )
                        , ( ">=" , (>=) )
                        , ( "<"  , (<)  )
                        , ( "<=" , (<=) )
                        , ( "="  , (==) )
                        , ( "!=" , (/=) )
                        , ( ".<" , (<)  )
                        , ( ".<=", (<=) )
                        ]

fullEvaluator [eMatch| !false |] = ret [eMake| true  |]
fullEvaluator [eMatch| !true  |] = ret [eMake| false |]

fullEvaluator [eMatch| toInt(false) |] = ret [eMake| 0 |]
fullEvaluator [eMatch| toInt(true)  |] = ret [eMake| 1 |]

fullEvaluator [eMatch| &a = &b |]
    | [xMatch| [Prim a'] := value.literal |] <- a
    , [xMatch| [Prim b'] := value.literal |] <- b
    = returnBool (a' == b')

fullEvaluator [eMatch| &a = &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| _ := value.set.values |] <- a
    , [xMatch| _ := value.set.values |] <- b
    = ret [eMake| &a subsetEq &b /\ &b subsetEq &a |]

fullEvaluator [eMatch| &a = &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| _ := value.mset.values |] <- a
    , [xMatch| _ := value.mset.values |] <- b
    = ret [eMake| &a subsetEq &b /\ &b subsetEq &a |]

fullEvaluator [eMatch| &a = &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.function.values |] <- a
    , [xMatch| bs := value.function.values |] <- b
    = returnBool (sortNub as == sortNub bs)

fullEvaluator [eMatch| &a = &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.relation.values |] <- a
    , [xMatch| bs := value.relation.values |] <- b
    = returnBool (sort as == sort bs)

fullEvaluator [eMatch| &a = &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.partition.values |] <- a
    , [xMatch| bs := value.partition.values |] <- b
    = returnBool (sort (map sortPart as) == sort (map sortPart bs))
    where sortPart [xMatch| xs := part |] = [xMake| part := sort xs |]
          sortPart x = bug $ vcat [ "fullEvaluator"
                                  , pretty x
                                  , prettyAsPaths x
                                  ]

fullEvaluator [eMatch| &a != &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    = ret [eMake| !(&a = &b) |]

fullEvaluator [eMatch| &a = &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.mset.values |] <- a
    , [xMatch| bs := value.mset.values |] <- b
    = returnBool (sort as /= sort bs)

fullEvaluator [eMatch| &a = &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.function.values |] <- a
    , [xMatch| bs := value.function.values |] <- b
    = returnBool (sortNub as /= sortNub bs)


fullEvaluator [eMatch| &a subset &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.set.values |] <- a
    = ret $ conjunct $ [ [eMake| &i in &b |] | i <- as ]
                    ++ [ [eMake| &a != &b |] ]
fullEvaluator [eMatch| &a subsetEq &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.set.values |] <- a
    = ret $ conjunct [ [eMake| &i in &b |] | i <- as ]
fullEvaluator [eMatch| &b supset &a |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    = ret [eMake| &a subset &b |]
fullEvaluator [eMatch| &b supsetEq &a |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    = ret [eMake| &a subsetEq &b |]


fullEvaluator [eMatch| &a subset &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.mset.values |] <- a
    , [xMatch| bs := value.mset.values |] <- b
    = returnBool $ and [ freqInList i as <= freqInList i bs | i <- nub as ]
                && or  [ freqInList i as <  freqInList i bs | i <- nub as ]
fullEvaluator [eMatch| &a subsetEq &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.mset.values |] <- a
    , [xMatch| bs := value.mset.values |] <- b
    = returnBool $ and [ freqInList i as <= freqInList i bs | i <- nub as ]


fullEvaluator
    [eMatch| |&x| |]
    | [xMatch| [Prim (I i)] := value.literal |] <- x
    = returnInt (abs i)

fullEvaluator
    [xMatch| [x] := operator.twoBars
           | _   := operator.twoBars.domain
           |]
    = ret [eMake| domSize(&x) |]

fullEvaluator
    [xMatch| [x] := operator.twoBars
           | xs  := operator.twoBars.value.set.values
           |]
    | isFullyInstantiated x
    = returnInt (genericLength $ sortNub xs)

fullEvaluator
    [xMatch| [x] := operator.twoBars
           | xs  := operator.twoBars.typed.left.value.set.values
           |]
    | isFullyInstantiated x
    = returnInt (genericLength $ sortNub xs)

fullEvaluator
    [xMatch| [x] := operator.twoBars
           | xs  := operator.twoBars.value.mset.values
           |]
    | isFullyInstantiated x
    = returnInt (genericLength xs)

fullEvaluator
    [xMatch| [x] := operator.twoBars
           | xs  := operator.twoBars.type.left.value.mset.values
           |]
    | isFullyInstantiated x
    = returnInt (genericLength xs)


fullEvaluator
    [xMatch| xs := operator.allDiff.value.matrix.values |]
    | all isFullyInstantiated xs
    = returnBool $ length xs == length (nub xs)

fullEvaluator
    [xMatch| [Prim (S "=")] := binOp.operator
           | xs := binOp.left .value.matrix.values
           | ys := binOp.right.value.matrix.values
           |]
    | all isFullyInstantiated xs && all isFullyInstantiated ys
    = ret $ conjunct
          $ [xMake| value.literal := [Prim (B $ length xs == length ys)] |]
          : [ [eMake| &x = &y |] | (x,y) <- zip xs ys ]

fullEvaluator
    [eMatch| max(&a,&b) |]
    | [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    = returnInt $ if a' > b' then a' else b'

fullEvaluator
    [eMatch| min(&a,&b) |]
    | [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    = returnInt $ if a' < b' then a' else b'

fullEvaluator [xMatch| [Prim (S "/\\")] := binOp.operator
                     | [ ] := binOp.left.emptyGuard
                     | [x] := binOp.right
                     |] = ret x

fullEvaluator [xMatch| [Prim (S "/\\")] := binOp.operator
                     | [x] := binOp.left
                     | [ ] := binOp.right.emptyGuard
                     |] = ret x


-- in

fullEvaluator
    [xMatch| [Prim (S "in")] := binOp.operator
           | [x] := binOp.left
           | ys  := binOp.right.value.set.values
           |]
    | isFullyInstantiated x && all isFullyInstantiated ys
    = ret $ disjunct [ [eMake| &x = &y |] | y <- ys ]

fullEvaluator
    [xMatch| [Prim (S "in")] := binOp.operator
           | [x] := binOp.left
           | ys  := binOp.right.value.mset.values
           |]
    | isFullyInstantiated x && all isFullyInstantiated ys
    = ret $ disjunct [ [eMake| &x = &y |] | y <- ys ]

fullEvaluator
    [xMatch| [Prim (S "in")] := binOp.operator
           | [x] := binOp.left
           | ys  := binOp.right.value.relation.values
           |]
    | isFullyInstantiated x && all isFullyInstantiated ys
    = returnBool $ x `elem` ys

fullEvaluator p@[eMatch| &x in parts(&y) |]
    | isFullyInstantiated x && isFullyInstantiated y
    , [xMatch| ys := value.partition.values |] <- y
    = returnBool $ x `elem` map partToSet ys
    where partToSet [xMatch| is := part |] = [xMake| value.set.values := sortNub is |]
          partToSet i = bug $ vcat [ "fullEvaluator"
                                   , pretty p
                                   , prettyAsPaths p
                                   , pretty i
                                   , prettyAsPaths i
                                   ]


fullEvaluator [eMatch| freq(&m,&x) |]
    | isFullyInstantiated m, isFullyInstantiated x
    , [xMatch| mValues := value.mset.values |] <- m
    = returnInt $ genericLength [ () | i <- mValues, i == x ]


fullEvaluator [xMatch| [Prim (S "union")] := binOp.operator
                     | xs := binOp.left .value.set.values
                     | ys := binOp.right.value.set.values
                     |] = ret $ [xMake| value.set.values := nub (xs ++ ys) |]

fullEvaluator [xMatch| [Prim (S "union")] := binOp.operator
                     | xs := binOp.left .value.mset.values
                     | ys := binOp.right.value.mset.values
                     |] = ret $ [xMake| value.mset.values := xs ++ ys |]


fullEvaluator [xMatch| [Prim (S "intersect")] := binOp.operator
                     | xs := binOp.left .value.set.values
                     | ys := binOp.right.value.set.values
                     | [lhs] := binOp.left
                     | [rhs] := binOp.right
                     |]
                     | isFullyInstantiated lhs && isFullyInstantiated rhs
                     = let zs = nub [ i | i <- xs, i `elem` ys]
                       in  ret $ [xMake| value.set.values := zs |]

fullEvaluator [xMatch| [Prim (S "intersect")] := binOp.operator
                     | xs := binOp.left .value.mset.values
                     | ys := binOp.right.value.mset.values
                     | [lhs] := binOp.left
                     | [rhs] := binOp.right
                     |]
                     | isFullyInstantiated lhs && isFullyInstantiated rhs
                     = let
                            xsHistogram = map (\ x -> (headNote "fullEvaluator.xsHistogram" x, length x) ) $ group $ sort xs
                            ysHistogram = map (\ x -> (headNote "fullEvaluator.ysHistogram" x, length x) ) $ group $ sort ys
                            allKeys = nub $ map fst xsHistogram ++ map fst ysHistogram
                            zsHistogram = [ (k, min xsCount ysCount)
                                          | k <- allKeys
                                          , let xsCount = fromMaybe 0 (lookup k xsHistogram)
                                          , let ysCount = fromMaybe 0 (lookup k ysHistogram)
                                          ]
                            zs = concatMap (\ (x,num) -> replicate num x ) zsHistogram
                       in   ret [xMake| value.mset.values := zs |]


fullEvaluator [xMatch| [Prim (S "-")] := binOp.operator
                     | xs := binOp.left .value.set.values
                     | ys := binOp.right.value.set.values
                     | [lhs] := binOp.left
                     | [rhs] := binOp.right
                     |]
                     | isFullyInstantiated lhs && isFullyInstantiated rhs
                     = let zs = nub $ xs \\ ys
                       in  ret $ [xMake| value.set.values := zs |]

fullEvaluator [xMatch| [Prim (S "-")] := binOp.operator
                     | xs := binOp.left .value.mset.values
                     | ys := binOp.right.value.mset.values
                     | [lhs] := binOp.left
                     | [rhs] := binOp.right
                     |]
                     | isFullyInstantiated lhs && isFullyInstantiated rhs
                     = let
                            xsHistogram = map (\ x -> (headNote "fullEvaluator.xsHistogram" x, length x) ) $ group $ sort xs
                            ysHistogram = map (\ x -> (headNote "fullEvaluator.ysHistogram" x, length x) ) $ group $ sort ys
                            allKeys = nub $ map fst xsHistogram ++ map fst ysHistogram
                            zsHistogram = [ (k, zsCount)
                                          | k <- allKeys
                                          , let xsCount = fromMaybe 0 (lookup k xsHistogram)
                                          , let ysCount = fromMaybe 0 (lookup k ysHistogram)
                                          , let zsCount = xsCount - ysCount
                                          , zsCount > 0
                                          ]
                            zs = concatMap (\ (x,num) -> replicate num x ) zsHistogram
                       in   ret [xMake| value.mset.values := zs |]


-- toSet

fullEvaluator [eMatch| toSet(&f) |]
    | isFullyInstantiated f
    , [xMatch| fMappings := value.function.values |] <- f
    = let
        fTuples = map mappingToTuple fMappings
      in ret [xMake| value.set.values := fTuples |]

fullEvaluator [eMatch| toSet(&f) |]
    | isFullyInstantiated f
    , [xMatch| fTuples := value.relation.values |] <- f
    = ret [xMake| value.set.values := fTuples |]

-- toMSet

fullEvaluator [eMatch| toMSet(&f) |]
    | [xMatch| fMappings := value.function.values |] <- f
    = let fTuples = map mappingToTuple fMappings
      in  ret [xMake| value.mset.values := fTuples |]

fullEvaluator [eMatch| toMSet(&f) |]
    | isFullyInstantiated f
    , [xMatch| fTuples := value.relation.values |] <- f
    = ret [xMake| value.mset.values := fTuples |]

fullEvaluator
  p@[xMatch| [fn]  := functionApply.actual
           | xs    := functionApply.actual.value.function.values
           | [arg] := functionApply.args
           |]
        | isFullyInstantiated fn && isFullyInstantiated arg
        = let go ([xMatch| [a,b] := mapping |]:rest) | a == arg  = ret b
                                                     | otherwise = go rest
              go _ = err ErrFatal $ "Undefinedness:" <+> pretty p
          in  go xs

fullEvaluator
  p@[xMatch| [fn]  := functionApply.actual.typed.left
           | xs    := functionApply.actual.typed.left.value.function.values
           | [arg] := functionApply.args
           |]
        | isFullyInstantiated fn && isFullyInstantiated arg
        = let go ([xMatch| [a,b] := mapping |]:rest) | a == arg  = ret b
                                                     | otherwise = go rest
              go _ = err ErrFatal $ "Undefinedness:" <+> pretty p
          in  go xs

fullEvaluator
    [xMatch| [fn]  := functionApply.actual
           | xs    := functionApply.actual.value.relation.values
           | args  := functionApply.args
           |]
        | isFullyInstantiated fn && and [ isFullyInstantiated i || i == [eMake| _ |] | i <- args ]
        = let

            select :: [E] -> [E] -> Bool
            select  pat act = and [ i == [eMake| _ |] || i == j | (i,j) <- zip pat act ]

            project :: [E] -> [E] -> [E]
            project pat act = [ j | (i,j) <- zip pat act
                                  , i == [eMake| _ |]
                                  ]

            ys = [ [xMake| value.tuple.values := project args cols |]
                 | [xMatch| cols := value.tuple.values |] <- xs
                 , select args cols
                 ]

            zs = not $ null
                 [ ()
                 | [xMatch| cols := value.tuple.values |] <- xs
                 , cols == args
                 ]

          in
            if any (\ i -> i == [eMake| _ |] ) args
                then ret [xMake| value.relation.values := ys |]
                else returnBool zs

fullEvaluator
    [xMatch| [fn]  := functionApply.actual.typed.left
           | []    := functionApply.actual.typed.left.value.relation.values
           | args  := functionApply.args
           |]
        | isFullyInstantiated fn && all isFullyInstantiated args
        = returnBool True

fullEvaluator
    [xMatch| _ := functionApply
           | [Prim (S "permute")] := functionApply.actual.functionApply.actual.reference
           | [rel,permuteTuple']  := functionApply.actual.functionApply.args
           | idx                  := functionApply.args
           |]
        | [xMatch| permuteTuple := value.tuple.values |] <- permuteTuple'
        = do
            let eIntOut [xMatch| [Prim (I i)] := value.literal |] = i
                eIntOut _ = bug "eIntOut"
            let idx' = [ genericIndex idx (eIntOut i - 1) | i <- permuteTuple ]
            ret [xMake| functionApply.actual := [rel]
                      | functionApply.args   := idx'
                      |]

fullEvaluator [eMatch| preImage(&f,&x) |]
    | isFullyInstantiated f && isFullyInstantiated x
    , [xMatch| fValues := value.function.values |] <- f
    = let
        getPair [xMatch| [i,j] := mapping |] = (i,j)
        getPair _ = bug $ vcat [ "fullEvaluator.preImage(&f,&x)"
                               , "f:" <+> pretty f
                               , "x:" <+> pretty x
                               ]
        ys = [ i | val <- fValues
                 , let (i,j) = getPair val
                 , j == x
                 ]
      in
        ret [xMake| value.set.values := ys |]

fullEvaluator [eMatch| preImage(&f,&x) |]
    | [xMatch| fValues := typed.left.value.function.values |] <- f
    = let f' = [xMake| value.function.values := fValues |]
      in  fullEvaluator [eMake| preImage(&f',&x) |]

fullEvaluator [eMatch| defined(&f) |]
    | isFullyInstantiated f
    , [xMatch| fValues := value.function.values |] <- f
    = let
        getPair [xMatch| [i,j] := mapping |] = (i,j)
        getPair _ = bug $ vcat [ "fullEvaluator.defined(&f,&x)"
                               , "f:" <+> pretty f
                               ]
        fPairs = map getPair fValues
        ys     = sortNub $ map fst fPairs
      in
        ret [xMake| value.set.values := ys |]

fullEvaluator [eMatch| range(&f) |]
    | isFullyInstantiated f
    , [xMatch| fValues := value.function.values |] <- f
    = let
        getPair [xMatch| [i,j] := mapping |] = (i,j)
        getPair _ = bug $ vcat [ "fullEvaluator.range(&f,&x)"
                               , "f:" <+> pretty f
                               ]
        fPairs = map getPair fValues
        ys     = sortNub $ map snd fPairs
      in
        ret [xMake| value.set.values := ys |]

fullEvaluator [eMatch| inverse(&f,&g) |]
    | isFullyInstantiated f && isFullyInstantiated g
    , [xMatch| fValues := value.function.values |] <- f
    , [xMatch| gValues := value.function.values |] <- g
    = let
        reverseTuple [xMatch| is := value.tuple.values |] = [xMake| value.tuple.values := (reverse is) |]
        reverseTuple x = x

        fTuples = map                 mappingToTuple  fValues
        gTuples = map (reverseTuple . mappingToTuple) gValues

        fSet = [xMake| value.set.values := fTuples |]
        gSet = [xMake| value.set.values := gTuples |]

      in
        ret [eMake| &fSet = &gSet |]

fullEvaluator [xMatch| xs := domain.int.ranges.range.single.value.set.values |]
    = let ys = map (\ i -> [xMake| range.single := [i] |] ) xs
      in  ret [xMake| domain.int.ranges := ys |]

fullEvaluator [xMatch| []  := operator.index.right.slicer
                     | [x] := operator.index.left
                     |] = ret x

fullEvaluator p@[xMatch| vs           := operator.index.left .value.matrix.values
                       | ranges       := operator.index.left .value.matrix.indexrange.domain.int.ranges
                       | [Prim (I a)] := operator.index.right.value.literal
                       |]
    = case result of
        Just x  -> ret x
        Nothing -> userErr $ "Matrix indexing out of range in:" <+> pretty p
        where
            valsFromRange [xMatch| [Prim (I j)] := range.single.value.literal |] = [j]
            valsFromRange [xMatch| [i',j'] := range.fromTo |] =
                case (i',j') of
                    ([xMatch| [Prim (I i)] := value.literal |], [xMatch| [Prim (I j)] := value.literal |]) -> [i..j]
                    _ -> []
            valsFromRange [xMatch| [Prim (I j)] := range.from.value.literal |] = [j..]
            valsFromRange _ = []
            vals = concatMap valsFromRange ranges
            result = listToMaybe [ x | (x,y) <- zip vs vals , y == a ]

fullEvaluator p@[xMatch| vs           := operator.index.left .value.matrix.values
                       | [Prim (I i)] := operator.index.right.value.literal
                       |]
    | i >= 1 && i <= genericLength vs = ret (vs `genericIndex` (i-1))
    | otherwise = userErr $ "Matrix indexing out of range in:" <+> pretty p
fullEvaluator p@[xMatch| vs           := operator.index.left .value.tuple.values
                       | [Prim (I i)] := operator.index.right.value.literal
                       |]
    | i >= 1 && i <= genericLength vs = ret (vs `genericIndex` (i-1))
    | otherwise = userErr $ "Tuple indexing out of range in:" <+> pretty p

fullEvaluator [eMatch| &quan &i : &d , &guard . &body |]
    | D (DomainInt [RangeBounded a b]) <- d
    , [xMatch| [Prim (S quanStr)] := reference |] <- quan
    , [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    , a' == b'
    = do
    res1 <- guardOp quanStr [guard] body
    let res2 = [eMake| &res1 { &i --> &a } |]
    evalReplace res2

fullEvaluator [eMatch| &quan &_ : &d , &_ . &_ |]
    | D (DomainInt [RangeBounded a b]) <- d
    , [xMatch| [Prim (S quanStr)] := reference |] <- quan
    , [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    , a' > b'
    = do
        res <- identityOp quanStr
        ret res

fullEvaluator [eMatch| &quan &_ in {} , &_ . &_ |]
    | [xMatch| [Prim (S quanStr)] := reference |] <- quan
    = do
        res <- identityOp quanStr
        ret res

fullEvaluator [eMatch| &quan &_ in mset() , &_ . &_ |]
    | [xMatch| [Prim (S quanStr)] := reference |] <- quan
    = do
        res <- identityOp quanStr
        ret res

fullEvaluator p@[eMatch| sum &i : &d , &guard . &body |]
    | D (DomainInt [RangeBounded a b]) <- d
    , [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    , [xMake| emptyGuard := [] |] == guard
    , isFullyInstantiated p
    = ret $ summation [ replace i j body
                      | j' <- [a'..b']
                      , let j = [xMake| value.literal := [Prim (I j')] |]
                      ]

fullEvaluator [eMatch| &_ &i : &d . &body |]
    | D (DomainInt [RangeBounded a b]) <- d
    , a == b
    = ret $ replace i a body

-- typed.* ones
fullEvaluator
    [xMatch| [Prim (S op)] := binOp.operator
           | [lhs]         := binOp.left .typed.left
           | [rhs]         := binOp.right.typed.left
           |] = do
    fullEvaluator [xMake| binOp.operator := [Prim (S op)]
                                | binOp.left     := [lhs]
                                | binOp.right    := [rhs]
                                |]
fullEvaluator
    [xMatch| [Prim (S op)] := binOp.operator
           | [lhs]         := binOp.left .typed.left
           | [rhs]         := binOp.right
           |] = do
    fullEvaluator [xMake| binOp.operator := [Prim (S op)]
                                | binOp.left     := [lhs]
                                | binOp.right    := [rhs]
                                |]
fullEvaluator
    [xMatch| [Prim (S op)] := binOp.operator
           | [lhs]         := binOp.left
           | [rhs]         := binOp.right.typed.left
           |] = do
    fullEvaluator [xMake| binOp.operator := [Prim (S op)]
                                | binOp.left     := [lhs]
                                | binOp.right    := [rhs]
                                |]

fullEvaluator
    [xMatch| [lhs] := operator.index.left .typed.left
           | [rhs] := operator.index.right.typed.left
           |] =
    fullEvaluator [xMake| operator.index.left  := [lhs]
                        | operator.index.right := [rhs]
                        |]
fullEvaluator
    [xMatch| [lhs] := operator.index.left .typed.left
           | [rhs] := operator.index.right
           |] =
    fullEvaluator [xMake| operator.index.left  := [lhs]
                        | operator.index.right := [rhs]
                        |]
fullEvaluator
    [xMatch| [lhs] := operator.index.left
           | [rhs] := operator.index.right.typed.left
           |] =
    fullEvaluator [xMake| operator.index.left  := [lhs]
                        | operator.index.right := [rhs]
                        |]

fullEvaluator
    [xMatch| [i]             := withLocals.actual
           | [Prim (B True)] := withLocals.locals.topLevel.suchThat.value.literal
           |] = ret i

fullEvaluator _ = return Nothing


mappingToTuple :: E -> E
mappingToTuple [xMatch| [i,j] := mapping |] = [xMake| value.tuple.values := [i,j] |]
mappingToTuple p = bug $ vcat [ "mappingToTuple"
                              , pretty p
                              , prettyAsPaths p
                              ]

-- if something has a type annotation, it can either be useful or not.
-- it is useful if the calculated type isn't ==, so keep it
-- strip if the calculated type is ==
stripUnnecessaryTyped :: MonadConjure m => E -> m (Maybe (E,[Binder]))
stripUnnecessaryTyped [xMatch| [x]  := typed.left
                             | [ty] := typed.right.domainInExpr
                             |] = do
    ty'  <- typeOf x
    case ty' of
        [xMatch| _ := type.bool |] -> ret x
        [xMatch| _ := type.int  |] -> ret x
        _ ->  if ty == ty'
                then ret x
                else return Nothing
stripUnnecessaryTyped _ = return Nothing

-- we generally don't and actually don't even want to, unroll quantifiers.
-- but we need this for validateSolution
unrollQuantifiers :: MonadConjure m => E -> m (Maybe (E,[Binder]))
unrollQuantifiers [eMatch| &quan &iPre : &d , &guard . &body |]
    | D (DomainInt [RangeBounded a b]) <- d
    , [xMatch| [Prim (S quanStr)] := reference |] <- quan
    , [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    = do
    let i = case iPre of
                [xMatch| [single] := structural.single |] -> single
                _ -> iPre
    xs <- forM [a'..b'] $ \ n' -> do
        let n = [xMake| value.literal := [Prim (I n')] |]
        newGuard <- evalReplace [eMake| &guard { &i --> &n } |]
        newBody  <- evalReplace [eMake| &body  { &i --> &n } |]
        case (newGuard, newBody) of
            (Just (x,_), Just (y,_)) -> return $ Just ([x],y)
            _ -> return Nothing
    y <- unrollQuantifier quanStr (catMaybes xs)
    ret y
unrollQuantifiers [eMatch| &quan &iPre : &dom , &guard . &body |]
    | [xMatch| [Prim (S quanStr)] := reference |] <- quan
    , [xMatch| _ := domain.matrix |] <- dom
    = do
        let i = case iPre of
                    [xMatch| [single] := structural.single |] -> single
                    _ -> iPre
        -- mkLog "unrollQuantifiers" $ prettyAsPaths dom
        allValues <- getAllValues dom
        xs <- forM allValues $ \ n -> do
            -- mkLog "getAllValues" $ pretty n
            newGuard <- evalReplace [eMake| &guard { &i --> &n } |]
            newBody  <- evalReplace [eMake| &body  { &i --> &n } |]
            case (newGuard, newBody) of
                (Just (x,_), Just (y,_)) -> return $ Just ([x],y)
                _ -> return Nothing
        y <- unrollQuantifier quanStr (catMaybes xs)
        -- mkLog "unrollQuantifiers" $ pretty y
        ret y
unrollQuantifiers [eMatch| &quan &iPre : &dom , &guard . &body |]
    | [xMatch| rs := domain.int.ranges |] <- dom
    , Just numbers <- mapM singleRangeOut rs
    , [xMatch| [Prim (S quanStr)] := reference |] <- quan
    = do
    let i = case iPre of
                [xMatch| [single] := structural.single |] -> single
                _ -> iPre
    xs <- forM numbers $ \ n' -> do
        let n = [xMake| value.literal := [Prim (I n')] |]
        newGuard <- evalReplace [eMake| &guard { &i --> &n } |]
        newBody  <- evalReplace [eMake| &body  { &i --> &n } |]
        case (newGuard, newBody) of
            (Just (x,_), Just (y,_)) -> return $ Just ([x],y)
            _ -> return Nothing
    y <- unrollQuantifier quanStr (catMaybes xs)
    ret y
    where
        singleRangeOut [xMatch| [Prim (I x)] := range.single.value.literal |] = Just x
        singleRangeOut [xMatch| [Prim (I x)] :=              value.literal |] = Just x
        singleRangeOut _ = Nothing

unrollQuantifiers
    [xMatch| [Prim (S quanStr)] := quantified.quantifier.reference
           | [quanVar]          := quantified.quanVar.structural.single
           | as                 := quantified.quanOverDom.domain.set.attributes.attrCollection
           | [a,b]              := quantified.quanOverDom.domain.set.inner.domain.int.ranges.range.fromTo
           | []                 := quantified.quanOverOp
           | []                 := quantified.quanOverExpr
           | [guard]            := quantified.guard
           | [body ]            := quantified.body
           |]
    | [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    = do
    let lookupAttribute attr ( [xMatch| [Prim (S attr')] := attribute.nameValue.name.reference
                                      | [val]            := attribute.nameValue.value
                                      |] : _ )
                    | attr == attr' = Just val
        lookupAttribute attr (_:rest) = lookupAttribute attr rest
        lookupAttribute _ [] = Nothing

    let predicateSize = case lookupAttribute "size" as of
            Just [xMatch| [Prim (I i)] := value.literal |] -> (i ==)
            _ -> const True
    let predicateMinSize = case lookupAttribute "minSize" as of
            Just [xMatch| [Prim (I i)] := value.literal |] -> (i <=)
            _ -> const True
    let predicateMaxSize = case lookupAttribute "maxSize" as of
            Just [xMatch| [Prim (I i)] := value.literal |] -> (i >=)
            _ -> const True
    let sets = map (\ xs -> [xMake| value.set.values := xs |] )
             $ map (map (\ x -> [xMake| value.literal := [Prim (I x)] |] ))
             $ filter (predicateSize    . genericLength)
             $ filter (predicateMinSize . genericLength)
             $ filter (predicateMaxSize . genericLength)
             $ subsequences [a'..b']
    xs <- forM sets $ \ n -> do
        newGuard <- evalReplace [eMake| &guard { &quanVar --> &n } |]
        newBody  <- evalReplace [eMake| &body  { &quanVar --> &n } |]
        case (newGuard, newBody) of
            (Just (x,_), Just (y,_)) -> return $ Just ([x],y)
            _ -> return Nothing
    y <- unrollQuantifier quanStr (catMaybes xs)
    ret y

unrollQuantifiers
    [xMatch| quantifier    := quantified.quantifier
           | qs            := quantified.quanVar.structural.set
           | []            := quantified.quanOverDom
           | []            := quantified.quanOverOp.binOp.subsetEq
           | quanOverExpr  := quantified.quanOverExpr
           | [guard]       := quantified.guard
           | body          := quantified.body
           |] =
    let
        newGuard = conjunct
            [ [eMake| &i .< &j |]
            | (i,j) <- zip qs (tail qs)
            ]
        guard' = [eMake| &guard /\ &newGuard |]

        unroll []  = bug "unrollQuantifiers.structural.set"
        unroll [i] =
            [xMake| quantified.quantifier := quantifier
                  | quantified.quanVar.structural.single := [i]
                  | quantified.quanOverDom := []
                  | quantified.quanOverOp.binOp.in := []
                  | quantified.quanOverExpr := quanOverExpr
                  | quantified.guard := [guard']
                  | quantified.body := body
                  |]
        unroll (i:is) =
            [xMake| quantified.quantifier := quantifier
                  | quantified.quanVar.structural.single := [i]
                  | quantified.quanOverDom := []
                  | quantified.quanOverOp.binOp.in := []
                  | quantified.quanOverExpr := quanOverExpr
                  | quantified.guard.emptyGuard := []
                  | quantified.body := [unroll is]
                  |]
    in ret $ unroll qs

unrollQuantifiers _ = return Nothing

returnBool :: MonadConjure m => Bool -> m (Maybe (E,[Binder]))
returnBool i = ret [xMake| value.literal := [Prim (B i)] |]

returnBool' :: MonadConjure m => Bool -> [Binder] -> m (Maybe (E,[Binder]))
returnBool' i bs = return $ Just ([xMake| value.literal := [Prim (B i)] |], bs)

returnInt :: MonadConjure m => Integer -> m (Maybe (E,[Binder]))
-- returnInt i | i > 1000000 = return Nothing
returnInt i = ret [xMake| value.literal := [Prim (I i)] |]

ret :: MonadConjure m => E -> m (Maybe (E,[Binder]))
ret i = return $ Just (i, [])


evalHasType :: MonadConjure m => E -> m (Maybe (E,[Binder]))
evalHasType [eMatch| &s hasType &dom |] = do
    let
        -- these two are for enabling the use of underscore as a wildcard
        -- in hasType.
        replacerActual  [xMatch| [Prim (S "_")] := reference |] =
                        [xMake| type.unknown := [] |]
        replacerActual  i = i

        replacerPattern [xMatch| [Prim (S "_")] := reference |] =
                        [xMake| metavar := [Prim (S "_")] |]
        replacerPattern i = i

        typeUnknown = [xMake| type.unknown := [] |]

        -- this one is a hack, `relation` should match any relation type.
        patternHack
            [xMatch| is := type.relation.inners |]
            [xMatch| [] := type.relation.inners.type.unknown |] =
            [xMake| type.relation.inners := (replicate (length is) typeUnknown) |]
        patternHack _ x = x

    ts <- typeOf s
    let ts' = transform replacerActual  ts

    td <- typeOf dom
    let td' = patternHack ts' $ transform replacerPattern td

    (flag, bs) <- patternMatch td' ts'

    modify $ \ st -> st { binders = bs ++ binders st }
    returnBool' flag bs
evalHasType _ = return Nothing

evalHasDomain :: MonadConjure m => E -> m (Maybe (E,[Binder]))
evalHasDomain [eMatch| &x hasDomain &y |] = do
    dx <- domainOf x
    dy <- domainOf y
    (flag, bs) <- patternMatch (D dy) (D dx)
    returnBool' flag bs
evalHasDomain _ = return Nothing

evalHasRepr :: MonadConjure m => E -> m (Maybe (E,[Binder]))
evalHasRepr [eMatch| &x hasRepr &y |] =
    case x of
        [eMatch| &m[&_] |] ->
            evalHasRepr [eMake| &m hasRepr &y |]
        [xMatch| [Prim (S iden)] := metavar   |] -> do
            a <- errMaybeT "hasRepr" lookupMetaVar iden
            evalHasRepr [eMake| &a hasRepr &y |]
        [xMatch| [Prim (S iden )] := reference |] ->
            case identifierSplit iden of
                (_,_,Just idenReprName) ->
                    case y of
                        [xMatch| [Prim (S reprName)] := reference |] ->
                            returnBool $ idenReprName == reprName
                        _ -> err ErrFatal $ "Not a representation:" <+> pretty y
                _ -> returnBool False
        _ -> returnBool False
evalHasRepr _ = return Nothing

evalDomSize :: MonadConjure m => E -> m (Maybe (E,[Binder]))
evalDomSize [eMatch| domSize(&i) |] = ret =<< domSize i
evalDomSize _ = return Nothing

sumE :: [E] -> E
sumE []     = [eMake| 0 |]
sumE [x]    = x
sumE [x,y]  = [eMake| &x + &y |]
sumE (x:xs) = let sumxs = sumE xs in [eMake| &x + &sumxs |]

mulE :: [E] -> E
mulE []     = [eMake| 1 |]
mulE [x]    = x
mulE [x,y]  = [eMake| &x * &y |]
mulE (x:xs) = let mulxs = mulE xs in [eMake| &x * &mulxs |]

class DomSize a where
    domSize :: MonadConjure m => a -> m E

instance DomSize E where

    domSize (D d) = domSize d

    domSize [xMatch| _ := value.literal |] = return [eMake| 1 |]

    domSize [xMatch| [d] := domainInExpr |] = domSize d

    domSize [xMatch| [x] := structural.single |] = domSize x
    domSize [xMatch| [d] := quanVar.within.quantified.body.quantified.quanOverDom |] = domSize d

    domSize [xMatch| [Prim (S s)] := reference |] = do
        x <- errMaybeT "domSize" lookupReference s
        domSize x

    domSize [xMatch| [] := topLevel.declaration.given.typeInt
                   | [Prim (S nm)] := topLevel.declaration.given.name.reference
                   |] = return [xMake| reference := [Prim (S $ nm `mappend` "_size")] |]

    domSize [xMatch| vs := topLevel.letting.typeEnum.values |] =
        return [xMake| value.literal := [Prim (I (genericLength vs))] |]

    domSize [xMatch| [d] := topLevel.letting.domain |] = domSize d

    domSize p =
        err ErrFatal $ "domSize:" <+> prettyAsPaths p

instance DomSize Domain where

    domSize DomainBool = return [eMake| 2 |]
    domSize (DomainInt rs) = sumE <$> mapM domSize rs
    domSize (DomainEnum _ rs) = sumE <$> mapM domSize rs
    domSize (DomainTuple rs) = mulE <$> mapM domSize rs

    domSize (DomainMatrix index inner) = do
        indexSize <- domSize index
        innerSize <- domSize inner
        return [eMake| &indexSize ** &innerSize |]

    domSize (DomainSet attrs inner)
        | Just size <- lookupDomainAttribute "size" attrs = do
            sInner <- domSize inner
            return $ sInner `choose` size
    domSize (DomainSet attrs inner)
        | Just minSize <- lookupDomainAttribute "minSize" attrs
        , Just maxSize <- lookupDomainAttribute "maxSize" attrs = do
            sInner <- domSize inner
            (qStr, q) <- freshQuanVar "from domSize"
            return $ inQuan "sum" qStr
                (DomainInt [RangeBounded minSize maxSize])
                ( [xMake| emptyGuard := [] |]
                , sInner `choose` q
                )
    domSize (DomainSet _ inner) = do
        sInner <- domSize inner
        return [eMake| 2 ** &sInner |]

    domSize (DomainMSet attrs inn)
        | Just k <- lookupDomainAttribute "size" attrs = do
            n <- domSize inn
            return [eMake| (&n + &k - 1)! / (&k! * (&n-1)!) |]

    domSize (DomainMSet _ inner) = do
        sInner <- domSize inner
        return [eMake| 2 ** &sInner |]

    domSize (DomainFunction attrs a b)
        | Just _ <- lookupDomainAttribute "total" attrs
        , Just _ <- lookupDomainAttribute "injective" attrs = do
            aSize <- domSize a
            bSize <- domSize b
            return [eMake| &bSize! / (&bSize - &aSize)! |]
    domSize (DomainFunction attrs a b)
        | Just _ <- lookupDomainAttribute "total" attrs
        , Just _ <- lookupDomainAttribute "bijective" attrs = do
            aSize <- domSize a
            bSize <- domSize b
            return [eMake| &bSize! / (&bSize - &aSize)! |]
    domSize (DomainFunction attrs a b)
        | Just _ <- lookupDomainAttribute "total" attrs = do
            aSize <- domSize a
            bSize <- domSize b
            return [eMake| &bSize ** &aSize |]
    domSize (DomainFunction _ a b) = do
        aSize <- domSize a
        bSize <- domSize b
        return [eMake| (&bSize + 1) ** &aSize |]

    domSize (DomainRelation attrs rs)
        = domSize (DomainSet attrs (DomainTuple rs))

    domSize (DomainPartition attrs inner)
        = domSize (DomainSet attrs (DomainSet def inner))

    domSize p@(DomainOp{}) = bug $ "not implemented domSize for DomainOp:" <+> pretty p

    domSize (DomainHack x) = domSize x

instance DomSize Range where
    domSize RangeSingle{} = return [eMake| 1 |]
    domSize (RangeBounded fr to) = return [eMake| &to - &fr + 1 |]
    domSize r = userErr $ "Infinite domain: " <+> pretty r
    

choose :: E -> E -> E
choose n k = [eMake| &n! / (&k! * (&n - &k)!) |]

evalDontCare :: MonadConjure m => Bool -> E -> m (Maybe (E,[Binder]))
evalDontCare False [eMatch| dontCare(&_) |] = ret [eMake| true |]
evalDontCare True  [eMatch| dontCare(&a) |] = do
    let
        bottomOfDomain DomainBool = return [eMake| false |]
        bottomOfDomain (DomainInt []) = return [eMake| 0 |]
        bottomOfDomain (DomainInt (r:_)) = bottomOfRange r
        bottomOfDomain _ = Nothing

        bottomOfRange (RangeSingle x) = return x
        bottomOfRange (RangeLowerBounded x) = return x
        bottomOfRange (RangeUpperBounded x) = return x
        bottomOfRange (RangeBounded x _) = return x
        bottomOfRange _ = Nothing

    da <- domainOf a
    case bottomOfDomain da of
        Just v  -> ret [eMake| &a = &v |]
        Nothing -> do
            ta <- typeOf a
            case ta of
                [xMatch| is := type.tuple.inners |] ->
                    ret $ conjunct [ [eMake| dontCare(&a[&i]) |]
                                   | j <- [1..genericLength is]
                                   , let i = [xMake| value.literal := [Prim (I j)] |]
                                   ]
                _ -> return Nothing
-- evalDontCare True  [eMatch| dontCare(&i) |] = ret [eMake| dontCare(&i) |]
    -- error $ show $ vcat [ "dontCareDom", pretty i, prettyAsPaths i ]
evalDontCare _ _ = return Nothing


evalIndices :: MonadConjure m => E -> m (Maybe (E,[Binder]))
evalIndices p@[xMatch| [a,b] := operator.indices |] = do
    bInt <- toInt b
    case bInt of
        Nothing         -> userErr $ "Second argument is not an integer:" <+> pretty p
        Just (bInt', _) -> indices a bInt'
    where
        -- indices (matrix) (integer)
        indices :: MonadConjure m => E -> Integer -> m (Maybe (E,[Binder]))
        indices = indicesCore

        indicesCore :: MonadConjure m => E -> Integer -> m (Maybe (E,[Binder]))
        indicesCore [xMatch| [Prim (S iden)] := reference |] i = do
            res <- errMaybeT "indices" lookupReference iden
            indices res i
        indicesCore [xMatch| [d] := topLevel.declaration.find .domain |] i = indices d i
        indicesCore [xMatch| [d] := topLevel.declaration.given.domain |] i = indices d i
        indicesCore [xMatch| [d] := quanVar.within.quantified.quanOverDom |] i = indices d i
        indicesCore (D (DomainMatrix index _)) 0 = ret (D index)
        indicesCore (D (DomainMatrix _ inner)) i = indices (D inner) (i-1)
        indicesCore [eMatch| &m[&x] |] i = do
            tym <- typeOf m
            case tym of
                [xMatch| _ := type.matrix |] -> indices m (i+1)
                [xMatch| _ := type.tuple  |] -> do
                    dm <- domainOf m
                    mxInt <- toInt x
                    case (dm, mxInt) of
                        ( DomainTuple ds , Just (xInt, _) )
                            | xInt >= 1 && xInt <= genericLength ds
                            -> do
                                let next = ds `genericIndex` (xInt - 1)
                                indices (D next) i
                        _ -> bug $ vcat [ "Full.indices", pretty m, pretty x ]
                _ -> return Nothing
        indicesCore m i = do
            mkLog "missing:indices" $
                vcat [ pretty m
                     , prettyAsPaths m
                     , pretty i
                     ]
            return Nothing

evalIndices _ = return Nothing

evalReplace :: MonadConjure m => E -> m (Maybe (E,[Binder]))
evalReplace
    [xMatch| [a] := operator.replace.arg1
           | [b] := operator.replace.old
           | [c] := operator.replace.new
           |] =
    let
        helper  old  new now | old == now = new
        helper  old  new (Tagged t xs) = Tagged t $ map (helper old new) xs
        helper _old _new other = other
    in  ret $ helper b c a
evalReplace _ = return Nothing

tupleEq :: MonadConjure m => E -> m (Maybe (E,[Binder]))
-- tupleEq x | trace (show $ "tupleEq:" <+> pretty x) False = undefined
tupleEq [eMatch| &a = &b |] = do
    ta <- typeOf a
    tb <- typeOf b
    case (ta,tb) of
        ([xMatch| is := type.tuple.inners |], _) ->
            ret $ conjunct [ [eMake| &a[&i] = &b[&i] |]
                           | j <- [1..genericLength is]
                           , let i = [xMake| value.literal := [Prim (I j)] |]
                           ]
        (_, [xMatch| is := type.tuple.inners |]) ->
            ret $ conjunct [ [eMake| &a[&i] = &b[&i] |]
                           | j <- [1..genericLength is]
                           , let i = [xMake| value.literal := [Prim (I j)] |]
                           ]
        _ -> return Nothing
tupleEq [eMatch| &a != &b |] = do
    ta <- typeOf a
    tb <- typeOf b
    case (ta,tb) of
        ([xMatch| is := type.tuple.inners |], _) ->
            ret $ disjunct [ [eMake| &a[&i] != &b[&i] |]
                           | j <- [1..genericLength is]
                           , let i = [xMake| value.literal := [Prim (I j)] |]
                           ]
        (_, [xMatch| is := type.tuple.inners |]) ->
            ret $ disjunct [ [eMake| &a[&i] != &b[&i] |]
                           | j <- [1..genericLength is]
                           , let i = [xMake| value.literal := [Prim (I j)] |]
                           ]
        _ -> return Nothing
tupleEq [eMatch| &a[&i] |] = do
    miInt <- toInt i
    case miInt of
        Nothing        -> return Nothing
        Just (iInt, _) ->
            case a of
                [xMatch| vs := value.tuple.values |] -> ret $ vs `genericIndex` (iInt - 1)
                _ -> return Nothing
tupleEq _ = return Nothing

dotOrderDecomposeForTuples :: MonadConjure m => E -> m (Maybe (E,[Binder]))
dotOrderDecomposeForTuples [eMatch| &a .< &b |] = do
    ta <- typeOf a
    tb <- typeOf b
    melems <- case (ta,tb) of
        ([xMatch| is := type.tuple.inners |], _) ->
            return $ Just [ ( [eMake| &a[&i] |]
                            , [eMake| &b[&i] |] )
                          | j <- [1..genericLength is]
                          , let i = [xMake| value.literal := [Prim (I j)] |]
                          ]
        (_, [xMatch| is := type.tuple.inners |]) ->
            return $ Just [ ( [eMake| &a[&i] |]
                            , [eMake| &b[&i] |] )
                          | j <- [1..genericLength is]
                          , let i = [xMake| value.literal := [Prim (I j)] |]
                          ]
        ([xMatch| is := type.matrix.inner.type.tuple.inners |], _) -> do
            return $ Just [ ( [eMake| &a[&i] |]
                            , [eMake| &b[&i] |] )
                          | j <- [1..genericLength is]
                          , let i = [xMake| value.literal := [Prim (I j)] |]
                          ]
        (_, [xMatch| is := type.matrix.inner.type.tuple.inners |]) ->
            return $ Just [ ( [eMake| &a[&i] |]
                            , [eMake| &b[&i] |] )
                          | j <- [1..genericLength is]
                          , let i = [xMake| value.literal := [Prim (I j)] |]
                          ]
        _ -> return Nothing
    case melems of
        Nothing -> return Nothing
        Just elems -> do
            let
                go [] = bug "dotOrderDecomposeForTuples.(.<)"
                go [(i,j)] = [eMake| &i .< &j |]
                go ((i,j):rest) = let rest' = go rest
                                  in  [eMake| &i .< &j \/ (&i = &j /\ &rest') |]
            ret $ go elems
dotOrderDecomposeForTuples [eMatch| &a .<= &b |] = do
    ta <- typeOf a
    tb <- typeOf b
    melems <- case (ta,tb) of
        ([xMatch| is := type.tuple.inners |], _) ->
            return $ Just [ ( [eMake| &a[&i] |]
                            , [eMake| &b[&i] |] )
                          | j <- [1..genericLength is]
                          , let i = [xMake| value.literal := [Prim (I j)] |]
                          ]
        (_, [xMatch| is := type.tuple.inners |]) ->
            return $ Just [ ( [eMake| &a[&i] |]
                            , [eMake| &b[&i] |] )
                          | j <- [1..genericLength is]
                          , let i = [xMake| value.literal := [Prim (I j)] |]
                          ]
        _ -> return Nothing
    case melems of
        Nothing -> return Nothing
        Just elems -> do
            let
                go [] = bug "dotOrderDecomposeForTuples.(.<=)"
                go [(i,j)] = [eMake| &i .<= &j |]
                go ((i,j):rest) = let rest' = go rest
                                  in  [eMake| &i .< &j \/ (&i = &j /\ &rest') |]
            ret $ go elems
dotOrderDecomposeForTuples _ = return Nothing

dotOrderDecomposeForMatrices :: MonadConjure m => E -> m (Maybe (E,[Binder]))
dotOrderDecomposeForMatrices [eMatch| &a .< &b |] = do
    ta <- typeOf a
    tb <- typeOf b
    mindex <- case (ta,tb) of
        ([xMatch| [innerTy] := type.matrix.inner |], _) | isAbstractType innerTy -> do
            da <- domainOf a
            case da of
                DomainMatrix ind _ -> return (Just ind)
                _ -> return Nothing
        (_, [xMatch| [innerTy] := type.matrix.inner |]) | isAbstractType innerTy -> do
            db <- domainOf b
            case db of
                DomainMatrix ind _ -> return (Just ind)
                _ -> return Nothing
        _ -> return Nothing
    case mindex of
        Nothing -> return Nothing
        Just index -> do
            (q1Text, q1) <- freshQuanVar "dotOrderDecomposeForMatrices"
            (q2Text, q2) <- freshQuanVar "dotOrderDecomposeForMatrices"
            let body1 = inQuan "forAll" q2Text index
                            ( [eMake| &q2 < &q1 |]
                            , [eMake| &a[&q2] = &b[&q2] |]
                            )
            let body2 = [eMake| &a[&q1] .< &b[&q1] |]
            let body  = [eMake| &body1 /\ &body2 |]
            ret $ inQuan "exists" q1Text index ( [xMake| emptyGuard := [] |]
                                               , body
                                               )
dotOrderDecomposeForMatrices [eMatch| &a .<= &b |] = do
    ta <- typeOf a
    tb <- typeOf b
    mindex <- case (ta,tb) of
        ([xMatch| [innerTy] := type.matrix.inner |], _) | isAbstractType innerTy -> do
            da <- domainOf a
            case da of
                DomainMatrix ind _ -> return (Just ind)
                _ -> return Nothing
        (_, [xMatch| [innerTy] := type.matrix.inner |]) | isAbstractType innerTy -> do
            db <- domainOf b
            case db of
                DomainMatrix ind _ -> return (Just ind)
                _ -> return Nothing
        _ -> return Nothing
    case mindex of
        Nothing -> return Nothing
        Just index -> do
            (q1Text, q1) <- freshQuanVar "dotOrderDecomposeForMatrices"
            (q2Text, q2) <- freshQuanVar "dotOrderDecomposeForMatrices"
            let body1 = inQuan "forAll" q2Text index
                            ( [eMake| &q2 < &q1 |]
                            , [eMake| &a[&q2] = &b[&q2] |]
                            )
            let body2 = [eMake| &a[&q1] .<= &b[&q1] |]
            let body  = [eMake| &body1 /\ &body2 |]
            ret $ inQuan "exists" q1Text index ( [xMake| emptyGuard := [] |]
                                               , body
                                               )
dotOrderDecomposeForMatrices _ = return Nothing

isAbstractType :: E -> Bool
isAbstractType [xMatch| _ := type.bool |] = False
isAbstractType [xMatch| _ := type.int  |] = False
isAbstractType [xMatch| [i] := type.matrix.inner |] = isAbstractType i
isAbstractType _ = True

matrixEq :: MonadConjure m => E -> m (Maybe (E, [Binder]))
matrixEq [eMatch| &a = &b |] = do
    da <- (Just <$> domainOf a) `catchError` (\ _ -> return Nothing )
    db <- (Just <$> domainOf b) `catchError` (\ _ -> return Nothing )
    case (da,db) of
        (Just (DomainMatrix ia _), _) -> do
            (quanVarStr, quanVar) <- freshQuanVar "matrixEq"
            ret $ inForAll quanVarStr ia ( [xMake| emptyGuard := [] |]
                                         , [eMake| &a[&quanVar] = &b[&quanVar] |]
                                         )
        (_, Just (DomainMatrix ia _)) -> do
            (quanVarStr, quanVar) <- freshQuanVar "matrixEq"
            ret $ inForAll quanVarStr ia ( [xMake| emptyGuard := [] |]
                                         , [eMake| &a[&quanVar] = &b[&quanVar] |]
                                         )
        _ -> case (a,b) of
            ([xMatch| as := value.matrix.values |], [xMatch| bs := value.matrix.values |]) ->
                ret $ conjunct [ [eMake| &i = &j |]
                               | (i,j) <- zip as bs
                               ]
            _ -> return Nothing
matrixEq [eMatch| &a != &b |] = do
    da <- (Just <$> domainOf a) `catchError` (\ _ -> return Nothing )
    db <- (Just <$> domainOf b) `catchError` (\ _ -> return Nothing )
    case (da,db) of
        (Just (DomainMatrix ia _), _) -> do
            (quanVarStr, quanVar) <- freshQuanVar "matrixEq"
            let res = inForAll quanVarStr ia ( [xMake| emptyGuard := [] |]
                                             , [eMake| &a[&quanVar] = &b[&quanVar] |]
                                             )
            ret [eMake| !&res |]
        (_, Just (DomainMatrix ia _)) -> do
            (quanVarStr, quanVar) <- freshQuanVar "matrixEq"
            let res = inForAll quanVarStr ia ( [xMake| emptyGuard := [] |]
                                             , [eMake| &a[&quanVar] = &b[&quanVar] |]
                                             )
            ret [eMake| !&res |]
        _ -> case (a,b) of
            ([xMatch| as := value.matrix.values |], [xMatch| bs := value.matrix.values |]) ->
                let toNegate = conjunct [ [eMake| &i = &j |]
                                        | (i,j) <- zip as bs
                                        ]
                in  ret [eMake| ! &toNegate |]
            _ -> return Nothing
matrixEq _ = return Nothing


stripStructuralSingle :: MonadConjure m => E -> m (Maybe (E, [Binder]))
stripStructuralSingle [xMatch| [Prim (S nm)] := structural.single.reference |] = do
    mx <- runMaybeT $ lookupReference nm
    case mx of
        Just [xMatch| [Prim (S nm')] := quanVar.name |] | nm == nm' -> return Nothing
        _ -> ret [xMake| reference := [Prim (S nm)] |]
stripStructuralSingle [xMatch| [x] := structural.single |] = ret x
stripStructuralSingle _ = return Nothing

freqInList :: Eq a => a -> [a] -> Int
freqInList y xs = length $ filter (y==) xs


getAllValues :: MonadConjure m => E -> m [E]
getAllValues [xMatch| [fr,to] := domain.matrix.index.domain.int.ranges.range.fromTo
                 | [index] := domain.matrix.index
                 | [inner] := domain.matrix.inner
                 |]
    | [xMatch| [Prim (I frI)] := value.literal |] <- fr
    , [xMatch| [Prim (I toI)] := value.literal |] <- to
    = do
        i <- getAllValues inner
        let vals = replicateM (fromInteger $ toI - frI + 1) i
        let
            wrap :: [E] -> E
            wrap vs = [xMake| value.matrix.values := vs
                            | value.matrix.indexrange := [index]
                            |]
        return $ map wrap vals
getAllValues [xMatch| [fr,to] := domain.int.ranges.range.fromTo |]
    | [xMatch| [Prim (I frI)] := value.literal |] <- fr
    , [xMatch| [Prim (I toI)] := value.literal |] <- to
    = do
        let wrap i = [xMake| value.literal := [Prim (I i)] |]
        return $ map wrap [frI .. toI]
getAllValues d = bug $ "Don't know how to calculate all values of this domain: " <+> prettyAsPaths d

