{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.Full where

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

fullEvaluator [eMatch| !false |] = ret [eMake| true  |]
fullEvaluator [eMatch| !true  |] = ret [eMake| false |]

fullEvaluator [eMatch| toInt(false) |] = ret [eMake| 0 |]
fullEvaluator [eMatch| toInt(true)  |] = ret [eMake| 1 |]

fullEvaluator [eMatch| &a = &b |]
    | [xMatch| [Prim a'] := value.literal |] <- a
    , [xMatch| [Prim b'] := value.literal |] <- b
    = returnBool (a' == b')

fullEvaluator [eMatch| &a = &b |]
    | [xMatch| [Prim a'] := structural.single.value.literal |] <- a
    , [xMatch| [Prim b'] := structural.single.value.literal |] <- b
    = returnBool (a' == b')

fullEvaluator [eMatch| &a != &b |]
    | [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    = returnBool (a' /= b')

fullEvaluator [xMatch| [Prim (S "/\\")] := binOp.operator
                     | [ ] := binOp.left.emptyGuard
                     | [x] := binOp.right
                     |] = ret x

fullEvaluator [xMatch| [Prim (S "/\\")] := binOp.operator
                     | [x] := binOp.left
                     | [ ] := binOp.right.emptyGuard
                     |] = ret x

-- fullEvaluator p@[eMatch| &quan &i : int(&a..&b) , &guard . &body |]
    -- | [xMatch| [Prim (S quanStr)] := reference |] <- quan
    -- , [xMatch| [Prim (I a')] := value.literal |] <- a
    -- , [xMatch| [Prim (I b')] := value.literal |] <- b
    -- , a' == b'
    -- = do
    -- res1 <- guardOp quanStr [guard] body
    -- let res2 = [eMake| &res1 { &i --> &a } |]
    -- mres3 <- evalReplace res2
    -- case mres3 of
        -- Just (res3, _) -> do
            -- mkLog "fullEvaluator 1" $ pretty p
            -- mkLog "fullEvaluator 2" $ pretty res3
            -- return mres3
        -- Nothing -> do
            -- mkLog "fullEvaluator Nothing" $ pretty p
            -- return Nothing

fullEvaluator p@[eMatch| &quan &_ : int(&a..&b) , &_ . &_ |]
    | [xMatch| [Prim (S quanStr)] := reference |] <- quan
    , [xMatch| [Prim (I a')] := value.literal |] <- a
    , [xMatch| [Prim (I b')] := value.literal |] <- b
    , a' > b'
    = do
        mkLog "fullEvaluator empty range" $ pretty p
        res <- identityOp quanStr
        ret res

fullEvaluator _ = return Nothing


returnBool :: MonadConjure m => Bool -> m (Maybe (E,[Binder]))
returnBool i = ret [xMake| value.literal := [Prim (B i)] |]

returnBool' :: MonadConjure m => Bool -> [Binder] -> m (Maybe (E,[Binder]))
returnBool' i bs = return $ Just ([xMake| value.literal := [Prim (B i)] |], bs)

returnInt :: MonadConjure m => Integer -> m (Maybe (E,[Binder]))
returnInt i | i > 1000 = return Nothing
returnInt i = ret [xMake| value.literal := [Prim (I i)] |]

returnInt' :: MonadConjure m => Integer -> [Binder] -> m (Maybe (E,[Binder]))
returnInt' i bs = return $ Just ([xMake| value.literal := [Prim (I i)] |], bs)

ret :: MonadConjure m => E -> m (Maybe (E,[Binder]))
ret i = return $ Just (i, [])


evalHasType :: MonadConjure m => E -> m (Maybe (E,[Binder]))
evalHasType [eMatch| &s hasType &dom |] = do
    ts <- typeOf s
    td <- typeOf dom
    (flag, bs) <- patternMatch td ts
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
        [xMatch| [Prim (S iden')] := metavar   |] -> do
            let iden = "&" `mappend` iden'
            bs <- gets binders
            case [ a | Binder nm a <- bs, nm == iden ] of
                [a] -> evalHasRepr [eMake| &a hasRepr &y |]
                _   -> err ErrFatal $ "Undefined reference: " <+> pretty iden'
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
    where
        sumE []     = [eMake| 0 |]
        sumE [x]    = x
        sumE [x,y]  = [eMake| &x + &y |]
        sumE (x:xs) = let sumxs = sumE xs in [eMake| &x + &sumxs |]

        retSum = return . sumE

        domSize [xMatch| [Prim (S s)] := reference |] = do
            mx <- runMaybeT $ lookupBinder s
            case mx of
                Nothing -> err ErrFatal $ "identifier not bound:" <+> pretty s
                Just x  -> domSize x
        domSize [xMatch| rs := domain.int.ranges |] = do
            xs <- mapM domSize rs
            retSum xs
        domSize [xMatch| [fr,to] := range.fromTo |] =
            return [eMake| &to - &fr + 1 |]
        domSize [xMatch| [_] := range.single |] =
            return [eMake| 1 |]

        domSize [xMatch| rs := domain.tuple.inners |] = do
            xs <- mapM domSize rs
            retSum xs

        domSize [xMatch| [t] := domain.set.inner |] = do
            x <- domSize t
            return [eMake| 2 ** &x |]

        domSize [dMatch| mset (size &s) of &inn |] = do
            innSize <- domSize inn
            return [eMake| &s * &innSize |]
        domSize [xMatch| [t] := domain.mset.inner |] = do
            x <- domSize t
            return [eMake| 2 ** &x |]

        domSize p =
            err ErrFatal $ "domSize:" <+> prettyAsPaths p

evalDomSize _ = return Nothing

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
            mres <- runMaybeT $ lookupBinder iden
            case mres of
                Nothing  -> err ErrFatal $ "(evalIndices) Undefined reference:" <+> pretty iden
                Just res -> indices res i
        indices [xMatch| [d] := topLevel.declaration.find .domain |] i = indices d i
        indices [xMatch| [d] := topLevel.declaration.given.domain |] i = indices d i
        indices [xMatch| [index] := domain.matrix.index |] 0 = ret index
        indices [xMatch| [inner] := domain.matrix.inner |] i = indices inner (i-1)
        indices [eMatch| &m[&_]                         |] i = indices m (i+1)
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
    case ta of
        [xMatch| is := type.tuple.inners |] ->
            ret $ conjunct [ [eMake| &a[&i] = &b[&i] |]
                           | j <- [1..genericLength is]
                           , let i = [xMake| value.literal := [Prim (I j)] |]
                           ]
        _ -> return Nothing
tupleEq [eMatch| &a != &b |] = do
    ta <- flip const (show $ "fromFullEVal" <+> pretty a) $ typeOf a
    case ta of
        [xMatch| is := type.tuple.inners |] ->
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
            (quanVarStr, quanVar) <- freshQuanVar
            ret $ inForAll quanVarStr ia [eMake| &a[&quanVar] = &b[&quanVar] |]
        (_,Just [xMatch| [ia] := domain.matrix.index |]) -> do
            (quanVarStr, quanVar) <- freshQuanVar
            ret $ inForAll quanVarStr ia [eMake| &a[&quanVar] = &b[&quanVar] |]
        _ -> return Nothing
matrixEq _ = return Nothing


