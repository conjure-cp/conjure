{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.E.Evaluator.Full where

import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.DomainOf
import Language.E.MatchBind
import Language.E.TH
import Language.E.TypeOf
import Language.E.Pretty


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
evalHasType _p@[eMatch| &s hasType &dom |] = do
    ts <- typeOf s
    td <- typeOf dom
    -- b  <- typeUnify ts td
    b  <- patternMatch td ts
    -- if b
    --     then do
    --         mkLog "debug:evalHasType" $ pretty p
    --         mkLog "debug:evalHasType" $ pretty ts
    --         mkLog "debug:evalHasType" $ pretty td
    --     else return ()
    returnBool b
evalHasType _ = return Nothing

evalHasDomain :: Monad m => E -> CompE m (Maybe E)
evalHasDomain _p@[eMatch| &x hasDomain &y |] = do
    -- mkLog "evalHasDomain" $ pretty p
    dx <- domainOf x
    dy <- domainOf y
    -- mkLog "evalHasDomain" $ pretty dx
    -- mkLog "evalHasDomain" $ prettyAsPaths dx
    -- mkLog "evalHasDomain" $ pretty dy
    -- mkLog "evalHasDomain" $ prettyAsPaths dy
    b  <- patternMatch dy dx
    returnBool b
evalHasDomain _ = return Nothing

evalHasRepr :: Monad m => E -> CompE m (Maybe E)
evalHasRepr _p@[eMatch| &x hasRepr &y |] = do
    -- mkLog "evalHasRepr" $ pretty p
    -- mkLog "evalHasRepr" $ prettyAsPaths x
    -- mkLog "evalHasRepr" $ prettyAsPaths y
    case (x,y) of
        ( [xMatch| [Prim (S iden )] := reference |] , [xMatch| [Prim (S reprName)] := reference |] ) -> do
            -- mkLog "evalHasRepr" "1"
            case splitOn "#" iden of
                [_,idenReprName] -> returnBool $ idenReprName == reprName
                _ -> return Nothing
        ( [xMatch| [Prim (S iden')] := metavar   |] , _ ) -> do
            -- mkLog "evalHasRepr" "2"
            let iden = '&' : iden'
            bs <- getsLocal binders
            case [ a | Binder nm a <- bs, nm == iden ] of
                [a] -> evalHasRepr [eMake| &a hasRepr &y |]
                _   -> err ErrFatal $ "Undefined reference: " <+> pretty iden'
        _ -> do
            -- mkLog "evalHasRepr" "3"
            return Nothing
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
        domSize p = do
            err ErrFatal $ "domSize:" <+> prettyAsPaths p

evalDomSize _ = return Nothing

evalIndices :: Monad m => E -> CompE m (Maybe E)
evalIndices [xMatch| [a,b] := operator.indices |] = indices a b
    where
        -- indices (matrix) (integer)
        indices [xMatch| [Prim (S iden)] := reference |] i = do
            mres <- runMaybeT $ lookupBinder iden
            case mres of
                Nothing  -> err ErrFatal $ "(evalIndices) Undefined reference:" <+> pretty iden
                Just res -> indices res i
        indices [xMatch| [d] := topLevel.declaration.find .domain |] i = indices d i
        indices [xMatch| [d] := topLevel.declaration.given.domain |] i = indices d i
        indices [xMatch| [index] := domain.matrix.index |]
                [xMatch| [Prim (I 0)] := value.literal  |] = return $ Just index
        indices [xMatch| [inner] := domain.matrix.inner |]
                [xMatch| [Prim (I i)] := value.literal  |] = indices inner [xMake| value.literal := [Prim (I $ i-1)] |]
        indices m i = do
            mkLog "debug:indices" $ vcat [ pretty m
                                         , prettyAsPaths m
                                         , pretty i
                                         , prettyAsPaths i
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

