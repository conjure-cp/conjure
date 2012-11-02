{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.Full where

import Stuff.FunkyT

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


fullEvaluator :: (Functor m, Monad m) => E -> CompE m (Maybe (E,[Binder]))
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

fullEvaluator [eMatch| !false |] = ret [eMake| true  |]
fullEvaluator [eMatch| !true  |] = ret [eMake| false |]

fullEvaluator [eMatch| toInt(false) |] = ret [eMake| 0 |]
fullEvaluator [eMatch| toInt(true)  |] = ret [eMake| 1 |]

fullEvaluator [xMatch| [Prim (S "=")] := binOp.operator
                     | [Prim x] := binOp.left .value.literal
                     | [Prim y] := binOp.right.value.literal
                     |]
                     | x == y
                     = ret [eMake| true |]

fullEvaluator [xMatch| [Prim (S "/\\")] := binOp.operator
                     | [ ] := binOp.left.emptyGuard
                     | [x] := binOp.right
                     |] = ret x

fullEvaluator [xMatch| [Prim (S "/\\")] := binOp.operator
                     | [x] := binOp.left
                     | [ ] := binOp.right.emptyGuard
                     |] = ret x


fullEvaluator _ = return Nothing


returnBool :: (Functor m, Monad m) => Bool -> CompE m (Maybe (E,[Binder]))
returnBool i = ret [xMake| value.literal := [Prim (B i)] |]

returnBool' :: (Functor m, Monad m) => Bool -> [Binder] -> CompE m (Maybe (E,[Binder]))
returnBool' i bs = return $ Just ([xMake| value.literal := [Prim (B i)] |], bs)

returnInt :: (Functor m, Monad m) => Integer -> CompE m (Maybe (E,[Binder]))
returnInt i = ret [xMake| value.literal := [Prim (I i)] |]

returnInt' :: (Functor m, Monad m) => Integer -> [Binder] -> CompE m (Maybe (E,[Binder]))
returnInt' i bs = return $ Just ([xMake| value.literal := [Prim (I i)] |], bs)

ret :: (Functor m, Monad m) => E -> CompE m (Maybe (E,[Binder]))
ret i = return $ Just (i, [])


evalHasType :: (Functor m, Monad m) => E -> CompE m (Maybe (E,[Binder]))
evalHasType [eMatch| &s hasType &dom |] = do
    ts <- typeOf s
    td <- typeOf dom
    (flag, bs) <- patternMatch td ts
    modifyLocal $ \ st -> st { binders = bs ++ binders st }
    returnBool' flag bs
evalHasType _ = return Nothing

evalHasDomain :: (Functor m, Monad m) => E -> CompE m (Maybe (E,[Binder]))
evalHasDomain [eMatch| &x hasDomain &y |] = do
    dx <- domainOf x
    dy <- domainOf y
    (flag, bs) <- patternMatch dy dx
    returnBool' flag bs
evalHasDomain _ = return Nothing

evalHasRepr :: (Functor m, Monad m) => E -> CompE m (Maybe (E,[Binder]))
evalHasRepr [eMatch| &x hasRepr &y |] =
    case x of
        [eMatch| &m[&_] |] ->
            evalHasRepr [eMake| &m hasRepr &y |]
        [xMatch| [Prim (S iden')] := metavar   |] -> do
            let iden = '&' : iden'
            bs <- getsLocal binders
            case [ a | Binder nm a <- bs, nm == iden ] of
                [a] -> evalHasRepr [eMake| &a hasRepr &y |]
                _   -> err ErrFatal $ "Undefined reference: " <+> pretty iden'
        [xMatch| [Prim (S iden )] := reference |] ->
            case splitOn "#" iden of
                [_,idenReprName] ->
                    case y of
                        [xMatch| [Prim (S reprName)] := reference |] ->
                            returnBool $ idenReprName == reprName
                        _ -> err ErrFatal $ "Not a representation:" <+> pretty y
                _ -> returnBool False
        _ -> returnBool False
evalHasRepr _ = return Nothing

evalDomSize :: (Functor m, Monad m) => E -> CompE m (Maybe (E,[Binder]))
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

evalIndices :: (Functor m, Monad m) => E -> CompE m (Maybe (E,[Binder]))
evalIndices p@[xMatch| [a,b] := operator.indices |] = do
    bInt <- toInt b
    case bInt of
        Nothing         -> err ErrFatal $ "Second argument is not an integer:" <+> pretty p
        Just (bInt', _) -> indices a bInt'
    where
        -- indices (matrix) (integer)
        indices :: (Functor m, Monad m) => E -> Integer -> CompE m (Maybe (E,[Binder]))
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

evalReplace :: (Functor m, Monad m) => E -> CompE m (Maybe (E,[Binder]))
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

tupleEq :: (Functor m, Monad m) => E -> CompE m (Maybe (E,[Binder]))
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


matrixEq :: (Functor m, Monad m) => E -> CompE m (Maybe (E, [Binder]))
matrixEq [eMatch| &a = &b |] = do
    da <- (Just <$> domainOf a) `catchError` (\ _ -> return Nothing )
    case da of
        Just [xMatch| [ia] := domain.matrix.index |] -> do
            (quanVarStr, quanVar) <- freshQuanVar
            ret $ inForAll quanVarStr ia [eMake| &a[&quanVar] = &b[&quanVar] |]
        _ -> return Nothing
matrixEq _ = return Nothing

