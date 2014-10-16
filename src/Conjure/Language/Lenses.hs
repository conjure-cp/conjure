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

