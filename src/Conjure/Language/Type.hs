{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.Language.Type where

-- conjure
import Conjure.Prelude
import Conjure.Language.DomainDefn

-- aeson
import qualified Data.Aeson as JSON


data Type
    = TypeAny
    | TypeBool
    | TypeInt
    | TypeEnum DomainDefnEnum
    | TypeUnnamed DomainDefnUnnamed
    | TypeTuple [Type]
    | TypeMatrix Type Type
    | TypeSet Type
    | TypeMSet Type
    | TypeFunction Type Type
    | TypeRelation [Type]
    | TypePartition Type
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Type
instance Hashable  Type
instance ToJSON    Type where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  Type where parseJSON = JSON.genericParseJSON jsonOptions
