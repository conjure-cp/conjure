module Language.Essence.QuantifiedExpr where

import Data.Generics ( Data )
import Data.Typeable ( Typeable )

import GenericOps.Core ( NodeTag, Hole, GPlate, MatchBind )
import ParsePrint ( ParsePrint )
import Language.Essence.Type ( TypeOf )



data QuantifiedExpr

instance Eq QuantifiedExpr
instance Ord QuantifiedExpr
instance Read QuantifiedExpr
instance Show QuantifiedExpr
instance Data QuantifiedExpr
instance Typeable QuantifiedExpr

instance NodeTag QuantifiedExpr
instance Hole QuantifiedExpr
instance GPlate QuantifiedExpr
instance MatchBind QuantifiedExpr
instance ParsePrint QuantifiedExpr
instance TypeOf QuantifiedExpr
