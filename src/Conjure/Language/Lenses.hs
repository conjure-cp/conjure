{-# LANGUAGE KindSignatures #-}

module Conjure.Language.Lenses where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Ops
import Conjure.Language.Pretty


-- | To use a lens for constructing stuf.
make :: (Proxy Identity -> (a, b)) -> a
make  f = fst (f (Proxy :: Proxy Identity))

-- | To use a lens for deconstructing stuf.
match :: (Proxy (m :: * -> *) -> (a, b -> m c)) -> b -> m c
match f = snd (f Proxy)

tryMatch :: (Proxy Maybe -> (a, b -> Maybe c)) -> b -> Maybe c
tryMatch f = match f

matchOr :: c -> (Proxy Maybe -> (a, b -> Maybe c)) -> b -> c
matchOr defOut f inp = fromMaybe defOut (match f inp)

matchDef :: (Proxy Maybe -> (a, b -> Maybe b)) -> b -> b
matchDef f inp = matchOr inp f inp


--------------------------------------------------------------------------------
-- Lenses (for a weird definition of lens) -------------------------------------
--------------------------------------------------------------------------------


opPlus
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opPlus _ =
    ( \ x y -> injectOp (MkOpPlus (OpPlus [x,y]))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpPlus (OpPlus [x,y]) -> return (x,y)
                _ -> na ("Lenses.opPlus:" <++> pretty p)
    )


opMinus
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opMinus _ =
    ( \ x y -> injectOp (MkOpMinus (OpMinus x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpMinus (OpMinus x y) -> return (x,y)
                _ -> na ("Lenses.opMinus:" <++> pretty p)
    )


opTimes
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opTimes _ =
    ( \ x y -> injectOp (MkOpTimes (OpTimes [x,y]))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpTimes (OpTimes [x,y]) -> return (x,y)
                _ -> na ("Lenses.opTimes:" <++> pretty p)
    )


opDiv
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opDiv _ =
    ( \ x y -> injectOp (MkOpDiv (OpDiv x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpDiv (OpDiv x y) -> return (x,y)
                _ -> na ("Lenses.opDiv:" <++> pretty p)
    )


opMod
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opMod _ =
    ( \ x y -> injectOp (MkOpMod (OpMod x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpMod (OpMod x y) -> return (x,y)
                _ -> na ("Lenses.opMod:" <++> pretty p)
    )


opPow
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opPow _ =
    ( \ x y -> injectOp (MkOpPow (OpPow x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpPow (OpPow x y) -> return (x,y)
                _ -> na ("Lenses.opPow:" <++> pretty p)
    )


opNegate
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x
       , x -> m x
       )
opNegate _ =
    ( injectOp . MkOpNegate . OpNegate
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpNegate (OpNegate x) -> return x
                _ -> na ("Lenses.opNegate:" <++> pretty p)
    )


opDontCare
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x
       , x -> m x
       )
opDontCare _ =
    ( injectOp . MkOpDontCare . OpDontCare
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpDontCare (OpDontCare x) -> return x
                _ -> na ("Lenses.opDontCare:" <++> pretty p)
    )


opToInt
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x
       , x -> m x
       )
opToInt _ =
    ( injectOp . MkOpToInt . OpToInt
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpToInt (OpToInt x) -> return x
                _ -> na ("Lenses.opToInt:" <++> pretty p)
    )


opToSet
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x
       , x -> m x
       )
opToSet _ =
    ( injectOp . MkOpToSet . OpToSet
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpToSet (OpToSet x) -> return x
                _ -> na ("Lenses.opToSet:" <++> pretty p)
    )


opToMSet
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x
       , x -> m x
       )
opToMSet _ =
    ( injectOp . MkOpToMSet . OpToMSet
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpToMSet (OpToMSet x) -> return x
                _ -> na ("Lenses.opToMSet:" <++> pretty p)
    )


opFunctionImage
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> [x] -> x
       , x -> m (x, [x])
       )
opFunctionImage _ =
    ( \ x ys -> injectOp $ MkOpFunctionImage $ OpFunctionImage x ys
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpFunctionImage (OpFunctionImage x ys) -> return (x,ys)
                _ -> na ("Lenses.opFunctionImage:" <++> pretty p)
    )


opIndexing
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opIndexing _ =
    ( \ x y -> injectOp (MkOpIndexing (OpIndexing x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpIndexing (OpIndexing x y) -> return (x,y)
                _ -> na ("Lenses.opIndexing:" <++> pretty p)
    )


opIndexing'
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> [x] -> x
       , x -> m (x,[x])
       )
opIndexing' proxy =
    ( foldl (make opIndexing)
    , \ p -> case projectOp p of
                Just (MkOpIndexing (OpIndexing x i)) -> do
                    (m,is) <- snd (opIndexing' proxy) x
                    return (m, is ++ [i])
                _ -> return (p, [])
    )


opFlatten
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x
       , x -> m x
       )
opFlatten _ =
    ( injectOp . MkOpFlatten . OpFlatten
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpFlatten (OpFlatten x) -> return x
                _ -> na ("Lenses.opFlatten:" <++> pretty p)
    )


opIn
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opIn _ =
    ( \ x y -> injectOp (MkOpIn (OpIn x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpIn (OpIn x y) -> return (x,y)
                _ -> na ("Lenses.opIn:" <++> pretty p)
    )


opIntersect
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opIntersect _ =
    ( \ x y -> injectOp (MkOpIntersect (OpIntersect x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpIntersect (OpIntersect x y) -> return (x,y)
                _ -> na ("Lenses.opIntersect:" <++> pretty p)
    )


opUnion
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opUnion _ =
    ( \ x y -> injectOp (MkOpUnion (OpUnion x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpUnion (OpUnion x y) -> return (x,y)
                _ -> na ("Lenses.opUnion:" <++> pretty p)
    )


opSubsetEq
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opSubsetEq _ =
    ( \ x y -> injectOp (MkOpSubsetEq (OpSubsetEq x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpSubsetEq (OpSubsetEq x y) -> return (x,y)
                _ -> na ("Lenses.opSubsetEq:" <++> pretty p)
    )


opEq
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opEq _ =
    ( \ x y -> injectOp (MkOpEq (OpEq x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpEq (OpEq x y) -> return (x,y)
                _ -> na ("Lenses.opEq:" <++> pretty p)
    )


opNeq
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opNeq _ =
    ( \ x y -> injectOp (MkOpNeq (OpNeq x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpNeq (OpNeq x y) -> return (x,y)
                _ -> na ("Lenses.opNeq:" <++> pretty p)
    )


opLt
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opLt _ =
    ( \ x y -> injectOp (MkOpLt (OpLt x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpLt (OpLt x y) -> return (x,y)
                _ -> na ("Lenses.opLt:" <++> pretty p)
    )


opLeq
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opLeq _ =
    ( \ x y -> injectOp (MkOpLeq (OpLeq x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpLeq (OpLeq x y) -> return (x,y)
                _ -> na ("Lenses.opLeq:" <++> pretty p)
    )


opGt
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opGt _ =
    ( \ x y -> injectOp (MkOpGt (OpGt x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpGt (OpGt x y) -> return (x,y)
                _ -> na ("Lenses.opGt:" <++> pretty p)
    )


opGeq
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opGeq _ =
    ( \ x y -> injectOp (MkOpGeq (OpGeq x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpGeq (OpGeq x y) -> return (x,y)
                _ -> na ("Lenses.opGeq:" <++> pretty p)
    )


opOr
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( [x] -> x
       , x -> m [x]
       )
opOr _ =
    ( injectOp . MkOpOr . OpOr
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpOr (OpOr xs) -> return xs
                _ -> na ("Lenses.opOr:" <++> pretty p)
    )


opAnd
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( [x] -> x
       , x -> m [x]
       )
opAnd _ =
    ( injectOp . MkOpAnd . OpAnd
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpAnd (OpAnd xs) -> return xs
                _ -> na ("Lenses.opAnd:" <++> pretty p)
    )


opImply
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opImply _ =
    ( \ x y -> injectOp (MkOpImply (OpImply x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpImply (OpImply x y) -> return (x,y)
                _ -> na ("Lenses.opImply:" <++> pretty p)
    )


opNot
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x
       , x -> m x
       )
opNot _ =
    ( injectOp . MkOpNot . OpNot
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpNot (OpNot x) -> return x
                _ -> na ("Lenses.opNot:" <++> pretty p)
    )


opProduct
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( [x] -> x
       , x -> m [x]
       )
opProduct _ =
    ( injectOp . MkOpTimes . OpTimes
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpTimes (OpTimes xs) -> return xs
                _ -> na ("Lenses.opProduct:" <++> pretty p)
    )


opSum
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( [x] -> x
       , x -> m [x]
       )
opSum _ =
    ( injectOp . MkOpPlus . OpPlus
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpPlus (OpPlus xs) -> return xs
                _ -> na ("Lenses.opSum:" <++> pretty p)
    )


opFilter
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opFilter _ =
    ( \ x y -> injectOp (MkOpFilter (OpFilter x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpFilter (OpFilter x y) -> return (x,y)
                _ -> na ("Lenses.opFilter:" <++> pretty p)
    )


opMapOverDomain
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opMapOverDomain _ =
    ( \ x y -> injectOp (MkOpMapOverDomain (OpMapOverDomain x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpMapOverDomain (OpMapOverDomain x y) -> return (x,y)
                _ -> na ("Lenses.opMapOverDomain:" <++> pretty p)
    )


opMapInExpr
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => proxy (m :: * -> *)
    -> ( x -> x -> x
       , x -> m (x,x)
       )
opMapInExpr _ =
    ( \ x y -> injectOp (MkOpMapInExpr (OpMapInExpr x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpMapInExpr (OpMapInExpr x y) -> return (x,y)
                _ -> na ("Lenses.opMapInExpr:" <++> pretty p)
    )


opAllDiff
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => Proxy (m :: * -> *)
    -> ( x -> x
       , x -> m x
       )
opAllDiff _ =
    ( injectOp . MkOpAllDiff . OpAllDiff
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpAllDiff (OpAllDiff x) -> return x
                _ -> na ("Lenses.opAllDiff:" <++> pretty p)
    )


constantInt
    :: MonadFail m
    => Proxy (m :: * -> *)
    -> ( Int -> Expression
       , Expression -> m Int
       )
constantInt _ =
    ( Constant . ConstantInt
    , \ p -> case p of
            (Constant (ConstantInt i)) -> return i
            _ -> na ("Lenses.constantInt:" <++> pretty p)
    )


setLiteral
    :: MonadFail m
    => Proxy (m :: * -> *)
    -> ( [Expression] -> Expression
       , Expression -> m [Expression]
       )
setLiteral _ =
    ( AbstractLiteral . AbsLitSet
    , \ p -> case p of
        Constant (ConstantSet xs) -> return (map Constant xs)
        AbstractLiteral (AbsLitSet xs) -> return xs
        _ -> na ("Lenses.setLiteral:" <+> pretty p)
    )

