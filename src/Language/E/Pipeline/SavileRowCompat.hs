{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.SavileRowCompat where

import Language.E
import Language.E.Pipeline.NoGuards ( conjureNoGuards )
import Language.E.Pipeline.NoTuples ( conjureNoTuples )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )


savilerowCompat
    :: MonadConjure m
    => Spec
    -> m Spec
savilerowCompat
     =  return
    >=> recordSpec >=> sliceIfTooFewIndices
    -- >=> recordSpec >=> valueMatrixToLetting
    >=> recordSpec >=> conjureNoTuples
    >=> recordSpec >=> conjureNoGuards
    >=> recordSpec >=> (return . onSpec toIntIsNoOp)
    >=> recordSpec >=> (return . atMostOneSuchThat)
    >=> recordSpec >=> (return . removeMinMaxInt)
    >=> recordSpec >=> (return . langEPrime)
    >=> recordSpec


onSpec :: (E -> E) -> Spec -> Spec
onSpec f (Spec v xs) = Spec v $ f xs


toIntIsNoOp :: E -> E
toIntIsNoOp [eMatch| toInt(&x) |] = x
toIntIsNoOp (Tagged t xs) = Tagged t $ map toIntIsNoOp xs
toIntIsNoOp p = p

sliceIfTooFewIndices :: MonadConjure m => Spec -> m Spec
sliceIfTooFewIndices (Spec v xs) = withBindingScope' $ Spec v <$> sliceIfTooFewIndicesE xs

sliceIfTooFewIndicesE :: MonadConjure m => E -> m E
sliceIfTooFewIndicesE p@[xMatch| _ := operator.index |] = do
    let (is,j) = breakIndices p
    jDom <- lookupDomain j
    case jDom of
        Just d@[xMatch| _ := domain.matrix |] -> do
            let nb = matrixDomainNbDims d
            let nbSlicers = nb - length is
            if nbSlicers == 0
                then return p
                else do
                    let is' = is ++ replicate nbSlicers [xMake| slicer := [] |]
                    let result = mkIndexedExpr is' j
                    mkLog "addSlicing" $ sep [pretty p, pretty result]
                    return result
        _ -> return p
    where
        breakIndices = first reverse . go
            where
                go [xMatch| [left ] := operator.index.left
                          | [right] := operator.index.right
                          |] = first (right:) (go left)
                go m = ([], m)

        lookupDomain d@[xMatch| _ := domain |] = return $ Just d
        lookupDomain [xMatch| [Prim (S nm)] := reference |] = do
            res <- errMaybeT "lookupDomain" lookupReference nm
            lookupDomain res
        lookupDomain [xMatch| [d] := topLevel.declaration.find .domain |] = return $ Just d
        lookupDomain [xMatch| [d] := topLevel.declaration.given.domain |] = return $ Just d
        lookupDomain [xMatch| [d] := topLevel.letting          .domain |] = return $ Just d
        lookupDomain _ = return Nothing

        matrixDomainNbDims :: E -> Int
        matrixDomainNbDims [xMatch| [inner] := domain.matrix.inner |] = 1 + matrixDomainNbDims inner
        matrixDomainNbDims _ = 0
sliceIfTooFewIndicesE p@(Tagged t xs) = do
    introduceStuff p
    Tagged t <$> mapM sliceIfTooFewIndicesE xs
sliceIfTooFewIndicesE p = return p


-- savilerow doesn't support inline value matrices.
-- this transformation may not always be valid, but will leave it in anyway.
-- conjure generates value matrices, especially in a deref form.
-- lift them to lettings, a temp solution.
valueMatrixToLetting :: MonadConjure m => Spec -> m Spec
valueMatrixToLetting (Spec v statement) = do
    let valueMatrices = nub [ m
                            | [eMatch| &m[&_] |] <- universe statement
                            , case m of
                                [xMatch| _ := value.matrix |] -> True
                                [xMatch| _ := structural.single.value.matrix |] -> True
                                _ -> False
                            ]
    paired <- forM valueMatrices $ \ m -> do
        s <- nextUniqueName
        return (m, s)
    let newLettings = [ [xMake| topLevel.letting.name.reference := [Prim (S s)]
                              | topLevel.letting.expr           := [m]
                              |]
                      | (m,s) <- paired
                      ]
    let f i = case i `lookup` paired of
                Just j  -> [xMake| reference := [Prim (S j)] |]
                Nothing -> i

    let statement' = transform f statement
    return $ Spec v $ listAsStatement $ newLettings ++ statementAsList statement'


langEPrime :: Spec -> Spec
langEPrime (Spec _ xs) = Spec ("ESSENCE'", [1,0]) xs


removeMinMaxInt :: Spec -> Spec
removeMinMaxInt (Spec v x) =
    let

        isMinMaxIntDecl [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference
                               | [] := topLevel.declaration.given.domain.domain.int.ranges
                               |]
            | nm `elem` ["MININT", "MAXINT"] = True
        isMinMaxIntDecl _ = False

        xs = filter (not . isMinMaxIntDecl) (statementAsList x)

        minIntRef = [xMake| reference := [Prim (S "MININT")] |]
        maxIntRef = [xMake| reference := [Prim (S "MAXINT")] |]

        stripFromRanges p@[xMatch| [fr,to] := domain.int.ranges.range.fromTo |]
            | fr == minIntRef && to == maxIntRef = [xMake| domain.int.ranges := [] |]
            | fr == minIntRef                    = [xMake| domain.int.ranges.range.to   := [to] |]
            |                    to == maxIntRef = [xMake| domain.int.ranges.range.from := [fr] |]
            | otherwise                          = p
        stripFromRanges p = p

    in

        Spec v $ transform stripFromRanges $ listAsStatement xs


