{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.Language.DomainDefn where

-- conjure
import Conjure.Prelude
import Conjure.Language.Name

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


data DomainDefnUnnamed = DomainDefnUnnamed Name
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize DomainDefnUnnamed
instance Hashable  DomainDefnUnnamed
instance ToJSON    DomainDefnUnnamed where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  DomainDefnUnnamed where parseJSON = JSON.genericParseJSON jsonOptions

