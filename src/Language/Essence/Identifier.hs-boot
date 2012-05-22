module Language.Essence.Identifier where

import Data.Generics ( Data )
import Data.String ( IsString )
import Data.Typeable ( Typeable )
-- import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary )

import GenericOps.Core ( NodeTag, Hole, GPlate, MatchBind )
import ParsePrint ( ParsePrint )

-- import {-# SOURCE #-} Language.Essence.Type ( TypeOf )



newtype Identifier = Identifier { identifierToString :: String }

instance IsString Identifier
instance Eq Identifier
instance Ord Identifier
instance Read Identifier
instance Show Identifier
instance Data Identifier
instance Typeable Identifier
-- instance Generic Identifier

instance NodeTag Identifier
instance Hole Identifier
instance GPlate Identifier
instance MatchBind Identifier
instance ParsePrint Identifier
instance Arbitrary Identifier
-- instance TypeOf Identifier



scopeIdentifiers :: GPlate a => (String -> String) -> a -> a

identifierRenamer :: String -> String -> Identifier -> Identifier
