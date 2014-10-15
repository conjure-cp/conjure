module Conjure.Language.Lenses where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Ops
import Conjure.Language.Pretty


--------------------------------------------------------------------------------
-- Lenses (for a weird definition of lens) -------------------------------------
--------------------------------------------------------------------------------


opIndexing
    :: ( OperatorContainer x
       , Pretty x
       , MonadFail m
       )
    => ( x -> x -> x
       , x -> m (x,x)
       )
opIndexing =
    ( \ x y -> injectOp (MkOpIndexing (OpIndexing x y))
    , \ p -> do
            op <- projectOp p
            case op of
                MkOpIndexing (OpIndexing x y) -> return (x,y)
                _ -> fail ("N/A opIndexing:" <++> pretty p)
    )

constantInt
    :: MonadFail m
    => ( Int -> Expression
       , Expression -> m Int
       )
constantInt =
    ( Constant . ConstantInt
    , \ p -> case p of
            (Constant (ConstantInt i)) -> return i
            _ -> fail ("N/A constantInt:" <++> pretty p)
    )
