{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Language.E.Evaluator.Full
    ( fullEvaluator
    , evalDomSize
    , evalHasDomain
    , evalHasRepr
    , evalHasType
    , evalIndices
    , evalReplace
    , matrixEq
    , stripStructuralSingle
    , stripUnnecessaryTyped
    , tupleEq
    , unrollQuantifiers
    , domSize
    ) where

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
fullEvaluator [xMatch| rs := domain.int.ranges |]
    | Just xs <- view rs
    , let xsSorted = sortNub xs
    , xs /= xsSorted
    , let rsSorted = map (\ i -> [xMake| value.literal := [Prim (I i)] |] ) xsSorted
    = ret [xMake| domain.int.ranges := rsSorted |]
    where
        view1 [xMatch| [Prim (I i)] := range.single.value.literal |] = Just i
        view1 _ = Nothing
        view [] = Just []
        view (i:is) = (:) <$> view1 i <*> view is

fullEvaluator [xMatch| [Prim (I n)] := unaryOp.negate.value.literal
                     |] = returnInt (-n)

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

fullEvaluator [xMatch| [Prim (S op)] := binOp.operator
                     | [Prim (I a )] := binOp.left .value.literal
                     | [Prim (I b )] := binOp.right.value.literal
                     |] | Just f <- lookup op comparators
                        = returnBool $ f a b
    where comparators = [ ( ">" , (>)  )
                        , ( ">=", (>=) )
                        , ( "<" , (<)  )
                        , ( "<=", (<=) )
                        , ( "=" , (==) )
                        , ( "!=", (/=) )
                        ]
 
fullEvaluator [xMatch| [Prim (S op)] := binOp.operator
                     | [Prim (B a )] := binOp.left .value.literal
                     | [Prim (B b )] := binOp.right.value.literal
                     |] | Just f <- lookup op comparators
                        = returnBool $ f a b
    where comparators = [ ( ">" , (>)  )
                        , ( ">=", (>=) )
                        , ( "<" , (<)  )
                        , ( "<=", (<=) )
                        , ( "=" , (==) )
                        , ( "!=", (/=) )
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
    , [xMatch| as := value.set.values |] <- a
    , [xMatch| bs := value.set.values |] <- b
    = returnBool (sortNub as == sortNub bs)

fullEvaluator [eMatch| &a = &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.mset.values |] <- a
    , [xMatch| bs := value.mset.values |] <- b
    = returnBool (sort as == sort bs)

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
          sortPart x = x

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
    , [xMatch| bs := value.set.values |] <- b
    = returnBool (sortNub as /= sortNub bs && and [ i `elem` bs | i <- as ])
fullEvaluator [eMatch| &a subsetEq &b |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.set.values |] <- a
    , [xMatch| bs := value.set.values |] <- b
    = returnBool $ and [ i `elem` bs | i <- as ]
fullEvaluator [eMatch| &b supset &a |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.set.values |] <- a
    , [xMatch| bs := value.set.values |] <- b
    = returnBool (sortNub as /= sortNub bs && and [ i `elem` bs | i <- as ])
fullEvaluator [eMatch| &b supsetEq &a |]
    | isFullyInstantiated a
    , isFullyInstantiated b
    , [xMatch| as := value.set.values |] <- a
    , [xMatch| bs := value.set.values |] <- b
    = returnBool $ and [ i `elem` bs | i <- as ]


fullEvaluator
    [xMatch| [x] := operator.twoBars
           | xs  := operator.twoBars.value.set.values
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
    [xMatch| xs := operator.allDiff.value.matrix.values |]
    | all isFullyInstantiated xs
    = returnBool $ length xs == length (nub xs)

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
    = returnBool $ x `elem` ys

fullEvaluator
    [xMatch| [Prim (S "in")] := binOp.operator
           | [x] := binOp.left
           | ys  := binOp.right.value.mset.values
           |]
    | isFullyInstantiated x && all isFullyInstantiated ys
    = returnBool $ x `elem` ys

fullEvaluator
    [xMatch| [Prim (S "in")] := binOp.operator
           | [x] := binOp.left
           | ys  := binOp.right.value.relation.values
           |]
    | isFullyInstantiated x && all isFullyInstantiated ys
    = returnBool $ x `elem` ys

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
                            xsHistogram = map (\ x -> (head x, length x) ) $ group $ sort xs
                            ysHistogram = map (\ x -> (head x, length x) ) $ group $ sort ys
                            allKeys = nub $ map fst xsHistogram ++ map fst ysHistogram
                            zsHistogram = [ (k, min xsCount ysCount)
                                          | k <- allKeys
                                          , let xsCount = fromMaybe 0 (lookup k xsHistogram)
                                          , let ysCount = fromMaybe 0 (lookup k ysHistogram)
                                          ]
                            zs = concatMap (\ (x,num) -> replicate num x ) zsHistogram
                       in   ret [xMake| value.mset.values := zs |]

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

          in
            ret [xMake| value.relation.values := ys |]

fullEvaluator [eMatch| preImage(&f,&x) |]
    | isFullyInstantiated f && isFullyInstantiated x
    , [xMatch| fValues := value.function.values |] <- f
    = let
        getPair [xMatch| [i,j] := mapping |] = (i,j)
        getPair _ = bug $ vcat [ "fullEvaluator.preImage(&f,&x)"
                               , "f:" <+> pretty f
                               , "x:" <+> pretty x
                               ]
        ys = [ j | val <- fValues
                 , let (i,j) = getPair val
                 , i == x
                 ]
      in
        ret [xMake| value.set.values := ys |]

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
        getPair [xMatch| [i,j] := mapping |] = (i,j)
        getPair _ = bug $ vcat [ "fullEvaluator.inverse(&f,&g)"
                               , "f:" <+> pretty f
                               , "g:" <+> pretty g
                               ]
        fPairs = map getPair fValues
        gPairs = map getPair gValues
      in
        returnBool $ fPairs == map swap gPairs

fullEvaluator [xMatch| xs := domain.int.ranges.range.single.value.set.values |]
    = let ys = map (\ i -> [xMake| range.single := [i] |] ) xs
      in  ret [xMake| domain.int.ranges := ys |]

fullEvaluator [xMatch| vs           := operator.index.left .value.matrix.values
                     | ranges       := operator.index.left .value.matrix.indexrange.domain.int.ranges
                     | [Prim (I a)] := operator.index.right.value.literal
                     |] | a `elem` vals
                        , Just x <- listToMaybe [ x | (x,y) <- zip vs vals , y == a ]
                        = ret x
                        where
                            valsFromRange [xMatch| [Prim (I j)] := range.single.value.literal |] = [j]
                            valsFromRange [xMatch| [i',j'] := range.fromTo |] =
                                case (i',j') of
                                    ([xMatch| [Prim (I i)] := value.literal |], [xMatch| [Prim (I j)] := value.literal |]) -> [i..j]
                                    _ -> []
                            valsFromRange [xMatch| [Prim (I j)] := range.from.value.literal |] = [j..]
                            valsFromRange _ = []
                            vals = concatMap valsFromRange ranges
fullEvaluator [xMatch| vs           := operator.index.left .value.matrix.values
                     | [Prim (I i)] := operator.index.right.value.literal
                     |] | i >= 1 && i <= genericLength vs
                        = ret (vs `genericIndex` (i-1))
fullEvaluator [xMatch| vs           := operator.index.left .value.tuple.values
                     | [Prim (I i)] := operator.index.right.value.literal
                     |] | i >= 1 && i <= genericLength vs
                        = ret (vs `genericIndex` (i-1))


fullEvaluator [eMatch| &quan &i : int(&a..&b) , &guard . &body |]
    | [xMatch| [Prim (S quanStr)] := reference |] <- quan
    , [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    , a' == b'
    = do
    res1 <- guardOp quanStr [guard] body
    let res2 = [eMake| &res1 { &i --> &a } |]
    evalReplace res2

fullEvaluator [eMatch| &quan &_ : int(&a..&b) , &_ . &_ |]
    | [xMatch| [Prim (S quanStr)] := reference |] <- quan
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

fullEvaluator p@[eMatch| sum &i : int(&a..&b) , &guard . &body |]
    | [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    , [xMake| emptyGuard := [] |] == guard
    , isFullyInstantiated p
    = ret $ summation [ replace i j body
                      | j' <- [a'..b']
                      , let j = [xMake| value.literal := [Prim (I j')] |]
                      ]

fullEvaluator [eMatch| &_ &i : int(&a..&b) . &body |]
    | a == b
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
           |] = do
    fullEvaluator [xMake| operator.index.left  := [lhs]
                                | operator.index.right := [rhs]
                                |]
fullEvaluator
    [xMatch| [lhs] := operator.index.left .typed.left
           | [rhs] := operator.index.right
           |] = do
    fullEvaluator [xMake| operator.index.left  := [lhs]
                                | operator.index.right := [rhs]
                                |]
fullEvaluator
    [xMatch| [lhs] := operator.index.left
           | [rhs] := operator.index.right.typed.left
           |] = do
    fullEvaluator [xMake| operator.index.left  := [lhs]
                                | operator.index.right := [rhs]
                                |]

fullEvaluator _ = return Nothing


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
unrollQuantifiers [eMatch| &quan &i : int(&a..&b) , &guard . &body |]
    | [xMatch| [Prim (S quanStr)] := reference |] <- quan
    , [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    = do
    xs <- forM [a'..b'] $ \ n' -> do
        let n = [xMake| value.literal := [Prim (I n')] |]
        newGuard <- evalReplace [eMake| &guard { &i --> &n } |]
        newBody  <- evalReplace [eMake| &body  { &i --> &n } |]
        case (newGuard, newBody) of
            (Just (x,_), Just (y,_)) -> return $ Just ([x],y)
            _ -> return Nothing
    y <- unrollQuantifier quanStr (catMaybes xs)
    ret y
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
unrollQuantifiers _ = return Nothing

returnBool :: MonadConjure m => Bool -> m (Maybe (E,[Binder]))
returnBool i = ret [xMake| value.literal := [Prim (B i)] |]

returnBool' :: MonadConjure m => Bool -> [Binder] -> m (Maybe (E,[Binder]))
returnBool' i bs = return $ Just ([xMake| value.literal := [Prim (B i)] |], bs)

returnInt :: MonadConjure m => Integer -> m (Maybe (E,[Binder]))
returnInt i | i > 1000 = return Nothing
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

    -- mkLog "debug s" $ pretty s
    ts <- typeOf s
    -- mkLog "debug ts" $ pretty ts
    let ts' = transform replacerActual  ts
    -- mkLog "debug ts'" $ pretty ts'

    -- mkLog "debug d" $ pretty dom
    td <- typeOf dom
    -- mkLog "debug td" $ prettyAsPaths td
    let td' = patternHack ts' $ transform replacerPattern td
    -- mkLog "debug td'" $ prettyAsPaths td'

    (flag, bs) <- patternMatch td' ts'

    modify $ \ st -> st { binders = bs ++ binders st }
    returnBool' flag bs
evalHasType _ = return Nothing

evalHasDomain :: MonadConjure m => E -> m (Maybe (E,[Binder]))
evalHasDomain [eMatch| &x hasDomain &y |] = do
    dx <- domainOf x
    dy <- domainOf y
    (flag, bs) <- patternMatch dy dx
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

domSize :: MonadConjure m => E -> m E
domSize [xMatch| _ := value.literal |] = return [eMake| 1 |]
domSize [xMatch| [Prim (S s)] := reference |] = do
    x <- errMaybeT "domSize" lookupReference s
    domSize x
domSize [xMatch| rs := domain.int.ranges |] = do
    xs <- mapM domSize rs
    return $ sumE xs
domSize [xMatch| [fr,to] := range.fromTo |] =
    return [eMake| &to - &fr + 1 |]
domSize [xMatch| [_] := range.single |] =
    return [eMake| 1 |]

domSize [xMatch| rs := domain.tuple.inners |] = do
    xs <- mapM domSize rs
    return $ mulE xs

domSize [xMatch| [t] := domain.set.inner |] = do
    x <- domSize t
    return [eMake| 2 ** &x |]

domSize [dMatch| mset (size &s) of &inn |] = do
    innSize <- domSize inn
    return [eMake| &s * &innSize |]
domSize [xMatch| [t] := domain.mset.inner |] = do
    x <- domSize t
    return [eMake| 2 ** &x |]

domSize [xMatch| [a] := domain.function.innerFrom
               | [b] := domain.function.innerTo
               |] = do
    aSize <- domSize a
    bSize <- domSize b
    return [eMake| &aSize * &bSize |]

domSize [xMatch| [] := topLevel.declaration.given.typeInt
               | [Prim (S nm)] := topLevel.declaration.given.name.reference
               |] = return [xMake| reference := [Prim (S $ nm `mappend` "_size")] |]

domSize p =
    err ErrFatal $ "domSize:" <+> prettyAsPaths p


evalIndices :: MonadConjure m => E -> m (Maybe (E,[Binder]))
evalIndices p@[xMatch| [a,b] := operator.indices |] = do
    bInt <- toInt b
    case bInt of
        Nothing         -> err ErrFatal $ "Second argument is not an integer:" <+> pretty p
        Just (bInt', _) -> indices a bInt'
    where
        -- indices (matrix) (integer)
        indices :: MonadConjure m => E -> Integer -> m (Maybe (E,[Binder]))
        indices [xMatch| [Prim (S iden)] := reference |] i = do
            res <- errMaybeT "indices" lookupReference iden
            indices res i
        indices [xMatch| [d] := topLevel.declaration.find .domain |] i = indices d i
        indices [xMatch| [d] := topLevel.declaration.given.domain |] i = indices d i
        indices [xMatch| [index] := domain.matrix.index |] 0 = ret index
        indices [xMatch| [inner] := domain.matrix.inner |] i = indices inner (i-1)
        indices [eMatch| &m[&_]                         |] i = indices m (i+1)
        indices [xMatch| [d] := quanVar.within.quantified.quanOverDom |] i = indices d i
        indices m i = do
            mkLog "missing:indices" $ vcat [ pretty m
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
    ta <- flip const (show $ "fromFullEVal" <+> pretty a) $ typeOf a
    tb <- flip const (show $ "fromFullEVal" <+> pretty b) $ typeOf b
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
    ta <- flip const (show $ "fromFullEVal" <+> pretty a) $ typeOf a
    tb <- flip const (show $ "fromFullEVal" <+> pretty b) $ typeOf b
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


matrixEq :: MonadConjure m => E -> m (Maybe (E, [Binder]))
matrixEq [eMatch| &a = &b |] = do
    da <- (Just <$> domainOf a) `catchError` (\ _ -> return Nothing )
    db <- (Just <$> domainOf b) `catchError` (\ _ -> return Nothing )
    case (da,db) of
        (Just [xMatch| [ia] := domain.matrix.index |],_) -> do
            (quanVarStr, quanVar) <- freshQuanVar "matrixEq"
            ret $ inForAll quanVarStr ia ( [xMake| emptyGuard := [] |]
                                         , [eMake| &a[&quanVar] = &b[&quanVar] |]
                                         )
        (_,Just [xMatch| [ia] := domain.matrix.index |]) -> do
            (quanVarStr, quanVar) <- freshQuanVar "matrixEq"
            ret $ inForAll quanVarStr ia ( [xMake| emptyGuard := [] |]
                                         , [eMake| &a[&quanVar] = &b[&quanVar] |]
                                         )
        _ -> return Nothing
matrixEq [eMatch| &a != &b |] = do
    da <- (Just <$> domainOf a) `catchError` (\ _ -> return Nothing )
    db <- (Just <$> domainOf b) `catchError` (\ _ -> return Nothing )
    case (da,db) of
        (Just [xMatch| [ia] := domain.matrix.index |],_) -> do
            (quanVarStr, quanVar) <- freshQuanVar "matrixEq"
            let res = inForAll quanVarStr ia ( [xMake| emptyGuard := [] |]
                                             , [eMake| &a[&quanVar] = &b[&quanVar] |]
                                             )
            ret [eMake| !&res |]
        (_,Just [xMatch| [ia] := domain.matrix.index |]) -> do
            (quanVarStr, quanVar) <- freshQuanVar "matrixEq"
            let res = inForAll quanVarStr ia ( [xMake| emptyGuard := [] |]
                                             , [eMake| &a[&quanVar] = &b[&quanVar] |]
                                             )
            ret [eMake| !&res |]
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

