{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Essence.Binding where

import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState )
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
-- import GHC.Generics ( Generic )

import GenericOps.Core
import PrintUtils
import ParsePrint

import {-# SOURCE #-} Language.Essence.Identifier
import {-# SOURCE #-} Language.Essence.Domain
import {-# SOURCE #-} Language.Essence.Expr
import Language.Essence.Lambda
import Language.Essence.QuantifierDecl
import Language.Essence.Type
import Language.Essence.Where



addBinding' :: (MonadError Doc m, MonadState BindingsMap m) => Binding -> m ()

bindingName :: Binding -> String

data Binding
    = Find          Identifier Domain
    | Given         Identifier Domain
    | LettingType   Identifier Type
    | GivenType     Identifier Type
    | LettingDomain Identifier Domain
    | LettingExpr   Identifier Expr
    | LettingLambda Identifier Lambda
    | LettingQuan   Identifier QuantifierDecl

instance Eq Binding
instance Ord Binding
instance Read Binding
instance Show Binding
instance Data Binding
instance Typeable Binding
-- instance Generic Binding

instance NodeTag Binding
instance Hole Binding
instance GPlate Binding
instance MatchBind Binding
instance ParsePrint Binding

instance ParsePrint [Either Binding Where]
