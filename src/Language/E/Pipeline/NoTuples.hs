{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.NoTuples ( noTuplesSpec ) where

import Language.E

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M



noTuplesSpec :: MonadConjure m => Spec -> m Spec
noTuplesSpec = makeIdempotent helper
    where helper (Spec v s) = do
            (s' , b1) <- noTuplesE s
            (s'', b2) <- noTupleDomsInQuanEs s'
            return (Spec v s'', b1 || b2)


noTuplesE :: MonadConjure m => E -> m (E, Bool)
noTuplesE statementIn = do
    let statements = statementAsList statementIn
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
        then return (statementIn, False)
        else do
            s' <- ( renameMatrixOfTupleIndexes (M.fromList matrixOfTuplesToExplode) >=>
                    renameTupleIndexes (S.fromList tuplesToExplode) )
                  (listAsStatement statementsOut)
            return (s', True)


noTupleDomsInQuanEs :: MonadConjure m => E -> m (E, Bool)
noTupleDomsInQuanEs inp@(Tagged t xs) = do
    (inp', flag) <- noTupleDomsInQuanE inp
    if flag
        then return (inp', True)
        else do
            (ys, bools) <- unzip <$> mapM noTupleDomsInQuanEs xs
            return $ if or bools
                        then (Tagged t ys, True)
                        else (inp, False)
noTupleDomsInQuanEs x = return (x, False)

noTupleDomsInQuanE :: MonadConjure m => E -> m (E, Bool)
noTupleDomsInQuanE inp = do
    (mout, (tuplesToExplode, matrixOfTuplesToExplode)) <- runWriterT (helper inp)
    case mout of
        Nothing  -> return (inp, False)
        Just out -> do
            out' <- ( renameMatrixOfTupleIndexes (M.fromList matrixOfTuplesToExplode) >=>
                      renameTupleIndexes (S.fromList tuplesToExplode)
                    ) out
            mkLog "noTupleDomsInQuan" $ vcat [ pretty inp
                                             , "~~>"
                                             , pretty out'
                                             ]
            return (out', True)
    where
        helper
            [xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
                   | [Prim (S quanVar)]    := quantified.quanVar.structural.single.reference
                   | [domain]              := quantified.quanOverDom
                   | []                    := quantified.quanOverOp
                   | []                    := quantified.quanOverExpr
                   | [guard]               := quantified.guard
                   | [body]                := quantified.body
                   |] =
            case checkTupleDomain domain of
                Just tuples -> do
                    let quanVars = [ (n, t)
                                   | (i,t) <- zip [ (1::Int) .. ] tuples
                                   , let n  = mconcat [ quanVar, "_tuple", stringToText (show i) ]
                                   ]
                    forM_ quanVars $ \ (n,_) -> tell ([n],[])
                    let out = inQuans quantifier quanVars (guard, body)
                    return (Just out)
                Nothing ->
                    case checkMatrixOfTupleDomain domain of
                        Just (indices,tuples) -> do
                            let quanVars = [ (n, t')
                                           | (i,t) <- zip [ (1::Int) .. ] tuples
                                           , let n  = mconcat [ quanVar, "_tuple", stringToText (show i) ]
                                           , let t' = constructMatrixDomain indices t
                                           ]
                            tell ([], [(quanVar, length indices)])
                            let out = inQuans quantifier quanVars (guard, body)
                            return (Just out)
                        Nothing -> return Nothing
        helper _ = return Nothing


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

renameTupleIndexes :: MonadConjure m => S.HashSet Text -> E -> m E
renameTupleIndexes identifiers = bottomUpE' f
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

renameMatrixOfTupleIndexes :: MonadConjure m => M.HashMap Text Int -> E -> m E
renameMatrixOfTupleIndexes identifiers = bottomUpE' f
    where
        -- f p | trace (show $ vcat [ "renameMatrixOfTupleIndexes"
                                 -- , pretty p
                                 -- ]
                    -- ) False = undefined
        f p@(viewIndexed -> (iExpr, js)) = do
            maybe_i <- case iExpr of
                [xMatch| [Prim (S i)] := reference |] -> return $ Just i
                [xMatch| [Prim (S i)] := structural.single.reference |] -> return $ Just i
                _ -> return Nothing
            case maybe_i of
                Nothing -> return p
                Just i  -> do
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

viewIndexed :: E -> (E,[E])
viewIndexed [xMatch| [i] := operator.index.left
                   | [j] := operator.index.right
                   |] = let (i',js) = viewIndexed i
                        in  (i',js `mappend` [j])
viewIndexed p = (p,[])

-- given an expression and a list of indexers, producers an indexed expression.
mkIndexed :: E -> [E] -> E
mkIndexed = foldl (\ x i -> [eMake| &x[&i] |])
