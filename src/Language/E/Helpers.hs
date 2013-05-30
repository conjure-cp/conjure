{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Helpers where

import Language.E.Imports
import Language.E.Definition
import Language.E.Definition.Literal
import Language.E.Pretty
import Language.E.CompE
import Language.E.TH



conjunct :: [E] -> E
conjunct []     = [eMake| true |]
conjunct [x]    = x
conjunct (x:xs) = let y = conjunct xs in [eMake| &x /\ &y |]


disjunct :: [E] -> E
disjunct []     = [eMake| false |]
disjunct [x]    = x
disjunct (x:xs) = let y = disjunct xs in [eMake| &x \/ &y |]

summation :: [E] -> E
summation []     = [eMake| 0 |]
summation [x]    = x
summation (x:xs) = let y = summation xs in [eMake| &x + &y |]


domainNeedsRepresentation :: E -> Bool
domainNeedsRepresentation [xMatch|  _  := domain.set          |] = True
domainNeedsRepresentation [xMatch|  _  := domain.mset         |] = True
domainNeedsRepresentation [xMatch|  _  := domain.function     |] = True
domainNeedsRepresentation [xMatch|  _  := domain.relation     |] = True
domainNeedsRepresentation [xMatch|  _  := domain.partition    |] = True
domainNeedsRepresentation [xMatch| [i] := domain.matrix.inner |] = domainNeedsRepresentation i
domainNeedsRepresentation _ = False


freshQuanVar :: MonadConjure m => Doc -> m (Text, E)
freshQuanVar from = do
    quanVarStr <- nextUniqueName
    mkLog "gensym" (from <+> pretty quanVarStr)
    let quanVar = [xMake| structural.single.reference := [Prim $ S quanVarStr] |]
    return (quanVarStr, quanVar)

inForAll :: Text -> E -> (E,E) -> E
inForAll = inQuan "forAll"

inQuan :: Text -> Text -> E -> (E,E) -> E
inQuan quan quanVar quanOverDom (guard,body) =
    let
        out = 
            [xMake| quantified.quantifier.reference                := [Prim $ S quan]
                  | quantified.quanVar.structural.single.reference := [Prim $ S quanVar ]
                  | quantified.quanOverDom                         := [quanOverDom]
                  | quantified.quanOverOp                          := []
                  | quantified.quanOverExpr                        := []
                  | quantified.guard                               := [guard]
                  | quantified.body                                := [body]
                  |]
    -- in  fromMaybe out (tryUnrollForAll out)
    in  out

inForAlls :: [(Text,E)] -> (E,E) -> E
inForAlls = inQuans "forAll"

-- inQuans quanStr [(quanVar,quanOverDom)] -> (guard,body)
inQuans :: Text -> [(Text,E)] -> (E,E) -> E
inQuans quan = go . reverse
    where
        go [] _ = error "inQuans.go"
        go [(i,j)]    this         = inQuan quan i j this
        go ((i,j):ks) (guard,body) = go ks ( [xMake| emptyGuard := [] |]
                                           , inQuan quan i j (guard, body)
                                           )

tryUnrollForAll :: E -> Maybe E
tryUnrollForAll
    [xMatch| [Prim (S "forAll")] := quantified.quantifier.reference
           | [Prim (S quanVar)]  := quantified.quanVar.structural.single.reference
           | ranges              := quantified.quanOverDom.domain.int.ranges
           | []                  := quantified.quanOverOp
           | []                  := quantified.quanOverExpr
           | []                  := quantified.guard.emptyGuard
           | [body]              := quantified.body
           |] = do
    let
        valueIntFrom [xMatch| [Prim (I i)] := value.literal |] = Just i
        valueIntFrom _ = Nothing

        collectInts [xMatch| [ i ] := range.single |] = Just [i]
        collectInts [xMatch| [i,j] := range.fromTo |] = do
            iInt <- valueIntFrom i
            jInt <- valueIntFrom j
            return $ map (\ x -> [xMake| value.literal := [Prim (I x)] |] )
                     [iInt .. jInt]
        collectInts _ = Nothing

    ints <- concatMapM collectInts ranges
    return $ conjunct
        [ replace
            [xMake| structural.single.reference := [Prim (S quanVar)] |]
            x
            body
        | x <- ints
        ]
tryUnrollForAll _ = Nothing



-- given a matrix domain, split it to its indices and the inner domain
splitMatrixDomain :: E -> ([E], E)
splitMatrixDomain [xMatch| [xIndex] := domain.matrix.index
                         | [xInner] := domain.matrix.inner
                         |] = first (xIndex:) (splitMatrixDomain xInner)
splitMatrixDomain x = ([], x)


-- given a list of index domains an and inner domain, construct a matrix
-- domain
mkMatrixDomain :: [E] -> E -> E
mkMatrixDomain []     j = j
mkMatrixDomain (i:is) j = [xMake| domain.matrix.index := [i]
                                | domain.matrix.inner := [mkMatrixDomain is j]
                                |]


splitOpIndex :: E -> (E, [E])
splitOpIndex = second reverse . helper
    where
        helper [xMatch| [a] := operator.index.left
                      | [b] := operator.index.right
                      |] = second (b:) (helper a)
        helper x = (x, [])


-- given indixers and an expression, create an indexed expression.
-- given [1,2,3] and m --> m[1,2,3]
mkIndexedExpr :: [E] -> E -> E
mkIndexedExpr = go . reverse
    where
        go []     x = x
        go (i:is) x = let y = go is x in [eMake| &y[&i] |]


toEssenceLiteral :: E -> Maybe EssenceLiteral
toEssenceLiteral (Prim (B x)) = return $ ELB x
toEssenceLiteral (Prim (I x)) = return $ ELI x
toEssenceLiteral [xMatch| [x] := value.literal          |] = toEssenceLiteral x
toEssenceLiteral [xMatch| xs  := value.matrix   .values
                        | [r] := value.matrix   .indexrange
                        |] = ELMatrix    <$> mapM toEssenceLiteral xs
                                         <*> return (Just r)
toEssenceLiteral [xMatch| xs  := value.matrix   .values |] = ELMatrix    <$> mapM toEssenceLiteral xs
                                                                         <*> return Nothing
toEssenceLiteral [xMatch| xs  := value.tuple    .values |] = ELTuple     <$> mapM toEssenceLiteral xs
toEssenceLiteral [xMatch| xs  := value.set      .values |] = ELSet       <$> mapM toEssenceLiteral xs
toEssenceLiteral [xMatch| xs  := value.mset     .values |] = ELMSet      <$> mapM toEssenceLiteral xs
toEssenceLiteral [xMatch| xs  := value.function .values |] = ELFunction  <$> mapM helper xs
    where
        helper [xMatch| [a,b] := mapping |] = (,) <$> toEssenceLiteral a <*> toEssenceLiteral b
        helper _ = Nothing
toEssenceLiteral [xMatch| xs  := value.relation .values |] = ELRelation  <$> mapM helper xs
    where
        helper [xMatch| ys := value.tuple.values |] = mapM toEssenceLiteral ys
        helper _ = Nothing
toEssenceLiteral [xMatch| xs  := value.partition.values |] = ELPartition <$> mapM helper xs
    where
        helper [xMatch| ys  := part |] = mapM toEssenceLiteral ys
        helper _ = Nothing
toEssenceLiteral _ = Nothing

fromEssenceLiteral :: EssenceLiteral -> E
fromEssenceLiteral (ELB         x ) = [xMake| value.literal := [Prim (B x)] |]
fromEssenceLiteral (ELI         x ) = [xMake| value.literal := [Prim (I x)] |]
fromEssenceLiteral (ELTuple     xs) = [xMake| value.tuple    .values := map fromEssenceLiteral xs |]
fromEssenceLiteral (ELMatrix xs Nothing ) = [xMake| value.matrix   .values := map fromEssenceLiteral xs |]
fromEssenceLiteral (ELMatrix xs (Just r)) = [xMake| value.matrix   .values := map fromEssenceLiteral xs
                                                  | value.matrix   .indexrange := [r]
                                                  |]
fromEssenceLiteral (ELSet       xs) = [xMake| value.set      .values := map fromEssenceLiteral xs |]
fromEssenceLiteral (ELMSet      xs) = [xMake| value.mset     .values := map fromEssenceLiteral xs |]
fromEssenceLiteral (ELFunction  xs) = [xMake| value.function .values := map helper xs |]
    where
        helper (a,b) = [xMake| mapping := [fromEssenceLiteral a, fromEssenceLiteral b] |]
fromEssenceLiteral (ELRelation  xs) = [xMake| value.relation .values := map helper xs |]
    where
        helper ys = [xMake| value.tuple.literal := map fromEssenceLiteral ys |]
fromEssenceLiteral (ELPartition xs) = [xMake| value.partition.values := map helper xs |]
    where
        helper ys = [xMake| part := map fromEssenceLiteral ys |]

isFullyInstantiated :: E -> Bool
isFullyInstantiated x = case toEssenceLiteral x of
    Nothing -> False
    Just _  -> True


lookupAttr :: Text -> [E] -> Maybe E
lookupAttr attrName attrs = listToMaybe $
    [ val
    | [xMatch| [Prim (S nm)] := attribute.nameValue.name.reference
             | [val]         := attribute.nameValue.value
             |] <- attrs
    , nm == attrName
    ] ++
    [ [xMake| emptyGuard := [] |]
    | [xMatch| [Prim (S nm)] := attribute.name.reference
             |] <- attrs
    , nm == attrName
    ]

