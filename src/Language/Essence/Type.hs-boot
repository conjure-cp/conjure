module Language.Essence.Type where

import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import Test.QuickCheck ( Arbitrary )

import GenericOps.Core ( NodeTag, Hole, GPlate, MatchBind )
import ParsePrint ( ParsePrint )



class TypeOf a

data Type

instance Eq Type
instance Ord Type
instance Read Type
instance Show Type
instance Data Type
instance Typeable Type

instance NodeTag Type
instance Hole Type
instance GPlate Type
instance MatchBind Type
instance ParsePrint Type
instance Arbitrary Type
instance TypeOf Type



data AnyTypeEnum

instance Eq AnyTypeEnum
instance Ord AnyTypeEnum
instance Read AnyTypeEnum
instance Show AnyTypeEnum
instance Data AnyTypeEnum
instance Typeable AnyTypeEnum

instance NodeTag AnyTypeEnum
instance Hole AnyTypeEnum
instance GPlate AnyTypeEnum
instance MatchBind AnyTypeEnum
instance ParsePrint AnyTypeEnum
instance Arbitrary AnyTypeEnum
