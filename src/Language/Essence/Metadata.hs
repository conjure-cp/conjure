{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Essence.Metadata where

import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )

import GenericOps.Core ( NodeTag, Hole, GPlate, gplate, gplateLeaf, MatchBind )
import ParsePrint ( ParsePrint, parse, pretty )



data Metadata = Metadata
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Metadata

instance Hole Metadata

instance GPlate Metadata where
    gplate p@Metadata = gplateLeaf p

instance MatchBind  Metadata

instance ParsePrint Metadata where
    parse = undefined
    pretty = undefined
