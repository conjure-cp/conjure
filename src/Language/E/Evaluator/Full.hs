{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.Full where

import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.DomainOf
import Language.E.MatchBind
import Language.E.TH
import Language.E.TypeOf
import Language.E.Pretty
import {-# SOURCE #-} Language.E.Evaluator.ToInt


fullEvaluator :: Monad m => E -> CompE m (Maybe E)
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


returnBool :: Monad m => Bool -> CompE m (Maybe E)
returnBool i = ret [xMake| value.literal := [Prim (B i)] |]

returnInt :: Monad m => Integer -> CompE m (Maybe E)
returnInt i = ret [xMake| value.literal := [Prim (I i)] |]

ret :: Monad m => E -> CompE m (Maybe E)
ret = return . Just


evalHasType :: (Functor m, Monad m) => E -> CompE m (Maybe E)
evalHasType [eMatch| &s hasType &dom |] = do
    ts <- typeOf s
    td <- typeOf dom
    b  <- patternMatch td ts
    returnBool b
evalHasType _ = return Nothing

evalHasDomain :: Monad m => E -> CompE m (Maybe E)
evalHasDomain [eMatch| &x hasDomain &y |] = do
    dx <- domainOf x
    dy <- domainOf y
    b  <- patternMatch dy dx
    returnBool b
evalHasDomain _ = return Nothing

evalHasRepr :: Monad m => E -> CompE m (Maybe E)
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

evalDomSize :: Monad m => E -> CompE m (Maybe E)
evalDomSize [eMatch| domSize(&i) |] = Just <$> domSize i
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
        domSize [xMatch| rs := domain.tuple.inners |] = do
            xs <- mapM domSize rs
            retSum xs
        domSize [dMatch| mset (size &s) of &inn |] = do
            innSize <- domSize inn
            return [eMake| &s * &innSize |]
        domSize p =
            err ErrFatal $ "domSize:" <+> prettyAsPaths p

evalDomSize _ = return Nothing

evalIndices :: (Functor m, Monad m) => E -> CompE m (Maybe E)
evalIndices p@[xMatch| [a,b] := operator.indices |] = do
    bInt <- toInt b
    case bInt of
        Nothing    -> err ErrFatal $ "Second argument is not an integer:" <+> pretty p
        Just bInt' -> indices a bInt'
    where
        -- indices (matrix) (integer)
        indices :: (Functor m, Monad m) => E -> Integer -> CompE m (Maybe E)
        indices [xMatch| [Prim (S iden)] := reference |] i = do
            mres <- runMaybeT $ lookupBinder iden
            case mres of
                Nothing  -> err ErrFatal $ "(evalIndices) Undefined reference:" <+> pretty iden
                Just res -> indices res i
        indices [xMatch| [d] := topLevel.declaration.find .domain |] i = indices d i
        indices [xMatch| [d] := topLevel.declaration.given.domain |] i = indices d i
        indices [xMatch| [index] := domain.matrix.index |] 0 = return $ Just index
        indices [xMatch| [inner] := domain.matrix.inner |] i = indices inner (i-1)
        indices [eMatch| &m[&_]                         |] i = indices m (i+1)
        indices m i = do
            mkLog "missing:indices" $ vcat [ pretty m
                                           , prettyAsPaths m
                                           , pretty i
                                           ]
            return Nothing
evalIndices _ = return Nothing

evalReplace :: Monad m => E -> CompE m (Maybe E)
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

