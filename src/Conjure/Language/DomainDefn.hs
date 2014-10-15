{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable #-}

module Conjure.Language.DomainDefn where

-- conjure
import Conjure.Prelude
import Conjure.Language.Name
import Conjure.Language.Pretty

-- aeson
import qualified Data.Aeson.Types as JSON


data DomainDefn
    = DDEnum DomainDefnEnum
    | DDUnnamed DomainDefnUnnamed
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize DomainDefn
instance Hashable  DomainDefn
instance ToJSON    DomainDefn where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  DomainDefn where parseJSON = JSON.genericParseJSON jsonOptions


data DomainDefnEnum = DomainDefnEnum Name [Name]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize DomainDefnEnum
instance Hashable  DomainDefnEnum
instance ToJSON    DomainDefnEnum where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  DomainDefnEnum where parseJSON = JSON.genericParseJSON jsonOptions
instance Pretty DomainDefnEnum where
    pretty (DomainDefnEnum name _) = "enumerated" <+> pretty name


data DomainDefnUnnamed = DomainDefnUnnamed Name
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize DomainDefnUnnamed
instance Hashable  DomainDefnUnnamed
instance ToJSON    DomainDefnUnnamed where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  DomainDefnUnnamed where parseJSON = JSON.genericParseJSON jsonOptions
instance Pretty DomainDefnUnnamed where
    pretty (DomainDefnUnnamed name) = "unnamed" <+> pretty name

