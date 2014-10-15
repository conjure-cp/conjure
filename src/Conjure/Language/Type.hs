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

typeUnify :: Type -> Type -> Bool
typeUnify TypeAny _ = True
typeUnify _ TypeAny = True
typeUnify TypeBool TypeBool = True
typeUnify TypeInt TypeInt = True
typeUnify (TypeEnum a) (TypeEnum b) = a == b
typeUnify (TypeUnnamed a) (TypeUnnamed b) = a == b
typeUnify (TypeTuple as) (TypeTuple bs) = and (zipWith typeUnify as bs)
typeUnify (TypeMatrix a1 a2) (TypeMatrix b1 b2) = and (zipWith typeUnify [a1,a2] [b1,b2])
typeUnify (TypeSet a) (TypeSet b) = typeUnify a b
typeUnify (TypeMSet a) (TypeMSet b) = typeUnify a b
typeUnify (TypeFunction a1 a2) (TypeFunction b1 b2) = and (zipWith typeUnify [a1,a2] [b1,b2])
typeUnify (TypeRelation as) (TypeRelation bs) = and (zipWith typeUnify as bs)
typeUnify (TypePartition a) (TypePartition b) = typeUnify a b
typeUnify _ _ = False
