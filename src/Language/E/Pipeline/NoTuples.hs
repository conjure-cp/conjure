{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.E.Pipeline.NoTuples where

import Language.E
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Language.E.Pipeline.ReadIn

import qualified Data.Set as S
import qualified Data.Map as M


conjureNoTuples :: (Monad m, Functor m)
    => (FilePath, Text)
    -> CompE m Spec
conjureNoTuples spectobe = do
    spec  <- readSpec spectobe
    ( makeIdempotent noTuplesSpec    >=>
      trySimplifySpec                >=>
      return . atMostOneSuchThat ) spec


noTuplesSpec :: (Functor m, Monad m) => Spec -> CompE m (Spec, Bool)
noTuplesSpec specIn@(Spec v statements) = do
    (statements',(tuplesToExplode,matrixOfTuplesToExplode)) <-
        runWriterT $ forM statements $ \ statement ->
            case checkTopLevel statement of
                Nothing      -> return [statement]
                Just (f,n,d) -> do
                    -- lift $ mkLog "noTuplesSpec" $ prettyAsPaths statement
                    case checkTupleDomain d of
                        Just ts -> do
                            newDecls <- forM (zip [(1 :: Int) ..] ts) $ \ (i,t) -> do
                                tell ([n],[])
                                let n' = n ++ "_tuple" ++ show i
                                return $ f n' t
                            return newDecls
                        Nothing ->
                            case checkMatrixOfTupleDomain d of
                                Just (indices,tuples) -> do
                                    tell ([],[(n,length indices)])
                                    -- lift $ mkLog "index" $ prettyList id " £" indices
                                    -- lift $ mkLog "inner" $ prettyList id " £" tuples
                                    newDecls <- forM (zip [(1 :: Int) ..] tuples) $ \ (i,t) -> do
                                        let n' = n ++ "_tuple" ++ show i
                                        let t' = constructMatrixDomain indices t
                                        return $ f n' t'
                                    return newDecls
                                Nothing -> return [statement]
    let statementsOut = concat statements'
    if and [null tuplesToExplode, null matrixOfTuplesToExplode, sameLength statements statementsOut, statements == statementsOut]
        then return (specIn, False)
        else do
            s' <- ( renameMatrixOfTupleIndexes (M.fromList matrixOfTuplesToExplode) >=>
                    renameTupleIndexes (S.fromList tuplesToExplode) ) (Spec v statementsOut)
            return (s', True)

checkTopLevel :: E -> Maybe (String -> E -> E, String, E)
checkTopLevel [xMatch| [Prim (S n)] := topLevel.declaration.find.name.reference
                     | [d]          := topLevel.declaration.find.domain |] =
    let
        f n' d' = [xMake| topLevel.declaration.find.name.reference := [Prim (S n')]
                        | topLevel.declaration.find.domain := [d']
                        |]
    in  Just (f,n,d)
checkTopLevel [xMatch| [Prim (S n)] := topLevel.declaration.given.name.reference
                     | [d]          := topLevel.declaration.given.domain |] =
    let
        f n' d' = [xMake| topLevel.declaration.given.name.reference := [Prim (S n')]
                        | topLevel.declaration.given.domain := [d']
                        |]
    in  Just (f,n,d)
checkTopLevel _ = Nothing


-- handling top level tuples
checkTupleDomain :: E -> Maybe [E]
checkTupleDomain [xMatch| is := domain.tuple.inners |] = Just is
checkTupleDomain _ = Nothing

renameTupleIndexes :: Monad m => S.Set String -> Spec -> CompE m Spec
renameTupleIndexes identifiers = traverseSpec Nothing f Nothing
    where
        f [xMatch| [Prim (S i)] := operator.index.left.reference
                 | [Prim (I j)] := operator.index.right.value.literal
                 |] | i `S.member` identifiers = return [xMake| reference := [Prim $ S $ i ++ "_tuple" ++ show j ] |]
        f p = return p


-- handling top level "matrix of tuples"
checkMatrixOfTupleDomain :: E -> Maybe ( [E]    -- indices
                                       , [E]    -- tuple components
                                       )
checkMatrixOfTupleDomain [xMatch| is := domain.tuple.inners |] = Just ([], is)
checkMatrixOfTupleDomain [xMatch| [i] := domain.matrix.index
                                | [j] := domain.matrix.inner
                                |] = do (is,js) <- checkMatrixOfTupleDomain j
                                        return (i:is,js)
checkMatrixOfTupleDomain _ = Nothing


constructMatrixDomain ::
       [E] -- indices
    -> E   -- inner domain
    -> E
constructMatrixDomain []     x = x
constructMatrixDomain (i:is) x = let y  = constructMatrixDomain is x
                                 in  [xMake| domain.matrix.index := [i]
                                           | domain.matrix.inner := [y]
                                           |]

renameMatrixOfTupleIndexes :: Monad m => M.Map String Int -> Spec -> CompE m Spec
renameMatrixOfTupleIndexes identifiers = traverseSpec Nothing f Nothing
    where
        f p@(viewIndexed -> ( [xMatch| [Prim (S i)] := reference |]
                          , js
                          )
            ) = case i `M.lookup` identifiers of
                    Just num | length js > num -> do
                        let indicesBefore = take num js
                        (tupleIndex, indicesAfter) <- case drop num js of
                                                        ([xMatch| [Prim (I a)] := value.literal |]:as) -> return (a,as)
                                                        _ -> err ErrFatal $ "in renameMatrixOfTupleIndexes at:" <+> pretty p
                        return $ mkIndexed [xMake| reference := [Prim $ S $ i ++ "_tuple" ++ show tupleIndex] |]
                                           (indicesBefore ++ indicesAfter)
                    _ -> return p
        f p = return p

viewIndexed :: E -> (E,[E])
viewIndexed [xMatch| [i] := operator.index.left
                   | [j] := operator.index.right
                   |] = let (i',js) = viewIndexed i
                        in  (i',js ++ [j])
viewIndexed p = (p,[])

mkIndexed :: E -> [E] -> E
mkIndexed x [] = x
mkIndexed x (i:is) = let y = [eMake| &x[&i] |]
                     in  mkIndexed y is
