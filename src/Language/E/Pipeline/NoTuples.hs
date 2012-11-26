{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.NoTuples ( conjureNoTuples ) where

import Language.E
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )

import qualified Data.Set as S
import qualified Data.Map as M


conjureNoTuples
    :: MonadConjure m
    => Spec
    -> m Spec
conjureNoTuples =
    makeIdempotent noTuplesSpec >=>
    simplifySpec >=>
    return . atMostOneSuchThat


noTuplesSpec :: MonadConjure m => Spec -> m (Spec, Bool)
noTuplesSpec specIn@(Spec v statementsOrig) = do
    let statements = statementAsList statementsOrig
    (statements',(tuplesToExplode,matrixOfTuplesToExplode)) <-
        runWriterT $ forM statements $ \ statement ->
            case checkTopLevel statement of
                Nothing      -> return [statement]
                Just (f,n,d) ->
                    case checkTupleDomain d of
                        Just ts -> do
                            lift $ mkLog "removedDecl" $ pretty statement
                            -- returning newDecls:
                            forM (zip [(1 :: Int) ..] ts) $ \ (i,t) -> do
                                tell ([n],[])
                                let n' = mconcat [ n, "_tuple", stringToText (show i) ]
                                return $ f n' t
                        Nothing ->
                            case checkMatrixOfTupleDomain d of
                                Just (indices,tuples) -> do
                                    lift $ mkLog "removedDecl" $ pretty statement
                                    tell ([],[(n,length indices)])
                                    -- returning newDecls:
                                    forM (zip [(1 :: Int) ..] tuples) $ \ (i,t) -> do
                                        let n' = mconcat [ n, "_tuple", stringToText (show i) ]
                                        let t' = constructMatrixDomain indices t
                                        return $ f n' t'
                                Nothing -> return [statement]
    let statementsOut = concat statements'
    if and [null tuplesToExplode, null matrixOfTuplesToExplode, sameLength statements statementsOut, statements == statementsOut]
        then return (specIn, False)
        else do
            s' <- ( renameMatrixOfTupleIndexes (M.fromList matrixOfTuplesToExplode) >=>
                    renameTupleIndexes (S.fromList tuplesToExplode) )
                  (Spec v $ listAsStatement statementsOut)
            return (s', True)

checkTopLevel :: E -> Maybe (Text -> E -> E, Text, E)
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

renameTupleIndexes :: MonadConjure m => S.Set Text -> Spec -> m Spec
renameTupleIndexes identifiers = bottomUpSpec' f
    where
        f [xMatch| [Prim (S i)] := operator.index.left.reference
                 | [Prim (I j)] := operator.index.right.value.literal
                 |] | let (base,mregion,mrepr) = identifierSplit i
                    , base `S.member` identifiers
                    = let i' = identifierConstruct (mconcat [base, "_tuple", stringToText (show j)])
                                                   mregion
                                                   mrepr
                      in  return [xMake| reference := [Prim (S i')] |]
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

renameMatrixOfTupleIndexes :: MonadConjure m => M.Map Text Int -> Spec -> m Spec
renameMatrixOfTupleIndexes identifiers = bottomUpSpec' f
    where
        f p@(viewIndexed -> ( [xMatch| [Prim (S i)] := reference |]
                            , js
                            )
            ) = do
            let (base, mregion, mrepr) = identifierSplit i
            case base `M.lookup` identifiers of
                Just num | length js > num -> do
                    let indicesBefore = take num js
                    (tupleIndex, indicesAfter) <- case drop num js of
                        ([xMatch| [Prim (I a)] := value.literal |] : as) -> return (a,as)
                        _ -> err ErrFatal $ "in renameMatrixOfTupleIndexes at:" <+> pretty p
                    let i' = identifierConstruct (mconcat [base, "_tuple", stringToText (show tupleIndex)])
                                                 mregion
                                                 mrepr
                    return $ mkIndexed [xMake| reference := [Prim (S i')] |]
                                       (indicesBefore `mappend` indicesAfter)
                _ -> return p
        f p = return p

viewIndexed :: E -> (E,[E])
viewIndexed [xMatch| [i] := operator.index.left
                   | [j] := operator.index.right
                   |] = let (i',js) = viewIndexed i
                        in  (i',js `mappend` [j])
viewIndexed p = (p,[])

-- given an expression and a list of indexers, producers an indexed expression.
mkIndexed :: E -> [E] -> E
mkIndexed = foldl (\ x i -> [eMake| &x[&i] |])
