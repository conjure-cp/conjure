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
match :: (Proxy (MaybeT m) -> (a, b)) -> b
match f = snd (f (Proxy :: Proxy (MaybeT m)))


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
                _ -> fail ("N/A opPlus:" <++> pretty p)
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
                _ -> fail ("N/A opMinus:" <++> pretty p)
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
                _ -> fail ("N/A opDontCare:" <++> pretty p)
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
                _ -> fail ("N/A opIndexing:" <++> pretty p)
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
                _ -> fail ("N/A opIn:" <++> pretty p)
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
                _ -> fail ("N/A opSubsetEq:" <++> pretty p)
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
                _ -> fail ("N/A opEq:" <++> pretty p)
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
                _ -> fail ("N/A opLt:" <++> pretty p)
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
                _ -> fail ("N/A opLeq:" <++> pretty p)
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
                _ -> fail ("N/A opGt:" <++> pretty p)
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
                _ -> fail ("N/A opGeq:" <++> pretty p)
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
                _ -> fail ("N/A opOr:" <++> pretty p)
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
                _ -> fail ("N/A opAnd:" <++> pretty p)
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
                _ -> fail ("N/A opSum:" <++> pretty p)
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
                _ -> fail ("N/A opFilter:" <++> pretty p)
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
                _ -> fail ("N/A opMapOverDomain:" <++> pretty p)
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
                _ -> fail ("N/A opMapInExpr:" <++> pretty p)
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
            _ -> fail ("N/A constantInt:" <++> pretty p)
    )

