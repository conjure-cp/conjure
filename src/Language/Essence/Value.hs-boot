module Language.Essence.Value where

import Data.Generics ( Data )
import Data.Typeable ( Typeable )
-- import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary )

import GenericOps.Core ( NodeTag, Hole, GPlate, MatchBind)
import ParsePrint ( ParsePrint )

import {-# SOURCE #-} Language.Essence.Domain
import {-# SOURCE #-} Language.Essence.Expr
import {-# SOURCE #-} Language.Essence.Identifier
import {-# SOURCE #-} Language.Essence.Type



toVTuple :: [Expr] -> Expr

data Value = VHole Identifier
    | VBool   Bool
    | VInt   Integer
    | VEnum  Identifier Type
    | VMatrix    [Expr]         -- uniform type.
    | VTuple     [Expr]
    | VSet       [Expr]         -- uniform type. unique.
    | VMSet      [Expr]         -- uniform type.
    | VFunction  [Expr]         -- VTuple#2. uniform type.
    | VRelation  [Expr]         -- VTuple. uniform type.
    | VPartition [Expr]         -- VSet. uniform type.

instance Eq Value
instance Ord Value
instance Read Value
instance Show Value
instance Data Value
instance Typeable Value
-- instance Generic Value

instance NodeTag Value
instance Hole Value
instance GPlate Value 
instance MatchBind Value
instance ParsePrint Value
instance Arbitrary Value
instance TypeOf Value
instance DomainOf Value
