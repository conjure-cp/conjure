{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.Phases.HandleEnums where

-- 4 cases:
-- 1. a top level decision variable whose domain is "DEnum e (Range Identifier)" and e is defined in a LettingType.
-- 2.                                                                            and e is defined in a GivenType.
-- 3. a domain in QuantifiedExpr    whose domain is "DEnum e (Range Identifier)" and e is defined in a LettingType.
-- 4.                                                                            and e is defined in a GivenType.

-- for every LettingType, create a (Map Identifier Int)
-- replace any identifier referring to a value of this type with the corresponding int.
-- replace every domain DEnum e (Range Identifier) with a DInt (Range Expr)

-- for every GivenType, create a parameter __count_e.
-- there cannot be any identifiers from this domain
-- replace every domain DEnum e (Range Identifier) with a DInt (Range Expr)

-- instantiator has to create a (Map Identifier Int), replace identifiers with ints and set __count_e.
-- there can be things like:
--      given foo new type enum
--      given a,b : foo
--      find x : foo(a..b)

import Control.Monad.Error ( MonadError )

import Nested
import PrintUtils ( Doc )
import Language.Essence


handleEnums :: MonadError (Nested Doc) m => Spec -> m Spec
handleEnums sp = do
    forM (topLevels sp) $ t -> case t of
        Left (LettingType)
    return sp
