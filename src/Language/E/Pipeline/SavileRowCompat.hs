{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.SavileRowCompat where

import Language.E
import Language.E.Pipeline.NoGuards ( conjureNoGuards )
import Language.E.Pipeline.NoTuples ( conjureNoTuples )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )


savilerowCompat :: (Monad m, Functor m)
    => Spec
    -> CompE m Spec
savilerowCompat
     =  return . onSpec toIntIsNoOp
    >=> onSpecM sliceIfTooFewIndices
    >=> conjureNoTuples
    >=> conjureNoGuards
    >=> return . atMostOneSuchThat
    >=> return . langEPrime


onSpec :: (E -> E) -> Spec -> Spec
onSpec f (Spec v xs) = Spec v $ map f xs

onSpecM :: Monad m => (E -> CompE m E) -> Spec -> CompE m Spec
onSpecM f (Spec v xs) = do
    mapM_ introduceStuff xs
    Spec v <$> mapM f xs


toIntIsNoOp :: E -> E
toIntIsNoOp [eMatch| toInt(&x) |] = x
toIntIsNoOp (Tagged t xs) = Tagged t $ map toIntIsNoOp xs
toIntIsNoOp p = p


sliceIfTooFewIndices :: (Monad m) => E -> CompE m E
sliceIfTooFewIndices p@[xMatch| _ := operator.index |] = do
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

        mkIndexedExpr = go . reverse
            where
                go []     x = x
                go (i:is) x = let y = go is x in [eMake| &y[&i] |]

        lookupDomain d@[xMatch| _ := domain |] = return $ Just d
        lookupDomain [xMatch| [Prim (S nm)] := reference |] = do
            mres <- runMaybeT $ lookupBinder nm
            case mres of
                Nothing  -> return Nothing
                Just res -> lookupDomain res
        lookupDomain [xMatch| [d] := topLevel.declaration.find .domain |] = return $ Just d
        lookupDomain [xMatch| [d] := topLevel.declaration.given.domain |] = return $ Just d
        lookupDomain [xMatch| [d] := topLevel.letting          .domain |] = return $ Just d
        lookupDomain _ = return Nothing

        matrixDomainNbDims :: E -> Int
        matrixDomainNbDims [xMatch| [inner] := domain.matrix.inner |] = 1 + matrixDomainNbDims inner
        matrixDomainNbDims _ = 0
sliceIfTooFewIndices (Tagged t xs) = Tagged t <$> mapM sliceIfTooFewIndices xs
sliceIfTooFewIndices p = return p


langEPrime :: Spec -> Spec
langEPrime (Spec _ xs) = Spec ("ESSENCE'", [1,0]) xs


