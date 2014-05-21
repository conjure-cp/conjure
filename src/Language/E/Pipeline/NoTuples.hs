{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.NoTuples
    ( allNoTuplesSpec
    , allNoTuplesE
    ) where

import Bug
import Language.E
import Language.E.Evaluator.Full ( unrollQuantifiers )

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M



allNoTuplesSpec :: MonadConjure m => Spec -> m Spec
allNoTuplesSpec (Spec v x) = Spec v <$> allNoTuplesE x


allNoTuplesE :: MonadConjure m => E -> m E
allNoTuplesE = makeIdempotent helper
    where helper s0 = do
            (s1, b1) <- noTuplesE s0
            (s2, b2) <- noTupleDomsInQuanEs s1
            (s3, b3) <- noTupleLiterals s2
            (s4, b4) <- unrollIfNeeded s3
            (s5, (Any b5, _)) <- runWriterT $ simplify s4
            let bFinal = or [b1,b2,b3,b4,b5]
            return (s5, bFinal)


noTupleLiterals :: MonadConjure m => E -> m (E, Bool)
noTupleLiterals inp = do
    (outp, Any flag) <- runWriterT $ bottomUpE helper inp
    when flag $ mkLog "noTupleLiterals" $ sep [ pretty inp, "~~>", pretty outp ]
    return (outp, flag)
    where
        helper p@(splitOpIndex -> (_, [])) = return p
        helper p@(splitOpIndex -> (x, is)) = case toEssenceLiteral x of
            Nothing -> return p
            Just lit -> possiblyTupley lit is

        possiblyTupley lit [] = do
            let out = fromEssenceLiteral lit
            return out
        possiblyTupley (ELMatrix xs mr) (i:is) = do
            xs' <- sequence [ possiblyTupley x is | x <- xs ]
            let matrix = case mr of
                    Nothing -> [xMake| value.matrix.values := xs' |]
                    Just r  -> [xMake| value.matrix.values := xs'
                                     | value.matrix.indexrange := [r]
                                     |]
            let out = mkIndexedExpr [i] matrix
            return out
        possiblyTupley (ELTuple  xs) ([xMatch| [Prim (I i)] := value.literal |] : is) = do
            tell (Any True)
            let out = mkIndexedExpr is (fromEssenceLiteral $ xs `genericIndex` (i-1))
            return out
        possiblyTupley lit is = do
            let out = mkIndexedExpr is $ fromEssenceLiteral lit
            return out


unrollIfNeeded :: MonadConjure m => E -> m (E, Bool)
unrollIfNeeded inp = do
    (outp, Any flag) <- runWriterT $ helper inp
    when flag $ mkLog "noTupleUnrollIfNeeded" $ sep [ pretty inp, "~~>", pretty outp ]
    return (outp, flag)
    where
        helper p@[xMatch| _ := quantified |] | containsTupleIndexing' p = do
            mres <- lift $ unrollQuantifiers p
            case mres of
                Nothing -> return p
                Just (out,_) -> do tell (Any True) ; helper out
        helper (Tagged t xs) = Tagged t <$> mapM helper xs
        helper t = return t

        containsTupleIndexing' x@(Tagged _ xs) =
            containsTupleIndexing x || any containsTupleIndexing' xs
        containsTupleIndexing' x =
            containsTupleIndexing x

        containsTupleIndexing (splitOpIndex -> (_, [])) = False
        containsTupleIndexing (splitOpIndex -> (x, _ )) =
            case toEssenceLiteral x of
                Just lit | containsTupleLiteral lit -> True
                _ -> False

        containsTupleLiteral (ELTuple _) = True
        containsTupleLiteral (ELMatrix xs _) = any containsTupleLiteral xs
        containsTupleLiteral _ = False


noTuplesE :: MonadConjure m => E -> m (E, Bool)
noTuplesE statementIn = do
    let statements = statementAsList statementIn
    (statements',(tuplesToExplode,matrixOfTuplesToExplode,directReplacements)) <-
        runWriterT $ forM statements $ \ statement -> withBindingScope $ do
            lift $ introduceStuff statement
            case checkTopLevel statement of
                Nothing      -> return [statement]
                Just (f,n,d) ->
                    case checkTupleDomain d of
                        Just ts -> do
                            let exploded_values =
                                    [ [xMake| reference := [Prim (S nm)] |]
                                    | i <- [1 .. length ts]
                                    , let nm = mconcat [ n, "_tuple", stringToText (show i) ]
                                    ]
                            let exploded = [xMake| value.tuple.values := exploded_values |]
                            tell ([],[],[(n, exploded)])
                            -- returning newDecls:
                            outs <- forM (zip [(1 :: Int) ..] ts) $ \ (i,t) -> do
                                tell ([n],[],[])
                                let n' = mconcat [ n, "_tuple", stringToText (show i) ]
                                return $ f n' t
                            lift $ mkLog "removedDecl" $ vcat $ [ pretty statement
                                                                , "Added the following:"
                                                                ] ++ map pretty outs
                            return outs
                        Nothing ->
                            case checkMatrixOfTupleDomain d of
                                Just (indices,tuples) -> do
                                    lift $ mkLog "matrixToTuple" $ name statement <> "∑" <> pretty (length indices)
                                    tell ([],[(n,length indices)],[])
                                    -- returning newDecls:
                                    outs <- forM (zip [(1 :: Int) ..] tuples) $ \ (i,t) -> do
                                        let n' = mconcat [ n, "_tuple", stringToText (show i) ]
                                        let t' = constructMatrixDomain indices t
                                        return $ f n' t'
                                    lift $ mkLog "removedDecl" $ vcat $ [ pretty statement
                                                                        , "Added the following:"
                                                                        ] ++ map pretty outs
                                    return outs
                                Nothing -> return [statement]
    let statementsOut = concat statements'
    if and [null tuplesToExplode, null matrixOfTuplesToExplode, sameLength statements statementsOut, statements == statementsOut]
        then return (statementIn, False)
        else do
            s' <- ( 
                    renameTupleIndexes (S.fromList tuplesToExplode)                 >=>
                    renameMatrixOfTupleIndexes (M.fromList matrixOfTuplesToExplode) >=>
                    doDirectReplacements directReplacements                         >=>
                    valueMatrixOfTuple
                  )
                  (listAsStatement statementsOut)
            return (s', True)

    where
        name [xMatch| [Prim (S _n)] := topLevel.declaration.find.name.reference  |] = pretty _n
        name _f = pretty _f

        doDirectReplacements pairs = bottomUpE' f
            where
                f p@[xMatch| [Prim (S nm)] := reference |] = case lookup nm pairs of
                    Nothing -> return p
                    Just r  -> do
                        mkLog "noTuplesReplacement" $ sep [ pretty p, "~~>", pretty r ]
                        return r
                f p = return p


noTupleDomsInQuanEs :: MonadConjure m => E -> m (E, Bool)
noTupleDomsInQuanEs (StatementAndNext this next) = withBindingScope' $ do
    introduceStuff this
    (this', b1) <- noTupleDomsInQuanEs this
    (next', b2) <- noTupleDomsInQuanEs next
    return ( StatementAndNext this' next'
           , b1 || b2
           )
noTupleDomsInQuanEs inp@(Tagged t xs) = withBindingScope' $ do
    introduceStuff inp
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
noTupleDomsInQuanE inp = withBindingScope' $ do
    introduceStuff inp
    (mout, (tuplesToExplode, matrixOfTuplesToExplode)) <- runWriterT (helper inp)
    case mout of
        Nothing  -> return (inp, False)
        Just out -> do
            out' <- ( 
                    renameTupleIndexes (S.fromList tuplesToExplode)                 >=>
                    renameMatrixOfTupleIndexes (M.fromList matrixOfTuplesToExplode) >=>
                    valueMatrixOfTuple
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
                    let quanVarReplacement =
                            let xs = [ [xMake| structural.single.reference := [Prim (S i)] |]
                                     | i <- map fst quanVars
                                     ]
                            in  [xMake| value.tuple.values := xs |]
                    let replacer = replace [xMake| reference := [Prim (S quanVar)] |]
                                           quanVarReplacement
                    let guard' = replacer guard
                    let body'  = replacer body
                    let out = inQuans quantifier quanVars (guard', body')
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
        f p@[xMatch| [Prim (S i)] := operator.index.left.reference
                   | [Prim (I j)] := operator.index.right.value.literal
                   |] | let (base,mregion,mrepr) = identifierSplit i
                      , base `S.member` identifiers = do
            let
                i' = identifierConstruct (mconcat [base, "_tuple", stringToText (show j)])
                                         mregion
                                         mrepr
                r  = [xMake| reference := [Prim (S i')] |]
            mkLog "noTuplesReplacement" $ sep [ pretty p, "~~>", pretty r ]
            return r
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
                            let out = mkIndexed [xMake| reference := [Prim (S i')] |]
                                               (indicesBefore `mappend` indicesAfter)
                            mkLog "noTuplesReplacement" $ sep [ pretty p, "~~>", pretty out ]
                            bottomUpE' f out
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

valueMatrixOfTuple :: MonadConjure m => E -> m E
valueMatrixOfTuple = bottomUpE' f
    where
        f p@[eMatch| &m[&i,&j] |] = do
            tym <- typeOf m
            mjint <- toInt j
            case mjint of
                Just (jInt, _) -> do
                    let
                        project [xMatch| tupleValues := value.tuple.values |] =
                            if jInt >= 1 && jInt <= genericLength tupleValues
                                then genericIndex tupleValues (jInt - 1)
                                else userErr $ "Out of range tuple indexing:" <+> pretty p
                        project tupleValues = bug $ "valueMatrixOfTuple.project" <+> pretty tupleValues
                    case tym of
                        [xMatch| _ := type.matrix.inner.type.tuple |] ->
                            case m of
                                [xMatch| matrixValues := value.matrix.values |] -> do
                                    let n = [xMake| value.matrix.values := map project matrixValues |]
                                    return [eMake| &n[&i] |]
                                _ -> return p
                        _ -> return p
                _ -> return p
        f p = return p

