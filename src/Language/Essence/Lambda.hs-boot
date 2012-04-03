module Language.Essence.Lambda where

import Data.Generics ( Data )
import Data.Typeable ( Typeable )
-- import GHC.Generics ( Generic )

import GenericOps.Core ( NodeTag, Hole, GPlate, MatchBind )
import ParsePrint ( ParsePrint )
import {-# SOURCE #-} Language.Essence.Type ( TypeOf )



data Lambda

instance Eq Lambda
instance Ord Lambda
instance Read Lambda
instance Show Lambda
instance Data Lambda
instance Typeable Lambda
-- instance Generic Lambda

instance NodeTag Lambda
instance Hole Lambda
instance GPlate Lambda
instance MatchBind Lambda
instance ParsePrint Lambda
instance TypeOf Lambda
