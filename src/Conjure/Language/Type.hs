{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Conjure.Language.Type
    ( Type(..)
    , typeUnify
    , typesUnify
    , mostDefined
    , homoType
    , innerTypeOf
    , isPrimitiveType
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name
import Conjure.Language.Pretty

-- aeson
import qualified Data.Aeson as JSON


data Type
    = TypeAny
    | TypeBool
    | TypeInt
    | TypeEnum Name
    | TypeUnnamed Name
    | TypeTuple [Type]
    | TypeList Type
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

instance Pretty Type where
    pretty TypeAny = "?"
    pretty TypeBool = "bool"
    pretty TypeInt = "int"
    pretty (TypeEnum nm ) = pretty nm
    pretty (TypeUnnamed nm) = pretty nm
    pretty (TypeTuple xs) = (if length xs <= 1 then "tuple" else prEmpty)
                         <> prettyList prParens "," xs
    pretty (TypeList x) = prBrackets (pretty x)
    pretty (TypeMatrix index inner) = "matrix indexed by"
                                  <+> prBrackets (pretty index)
                                  <+> "of" <+> pretty inner
    pretty (TypeSet x) = "set of" <+> pretty x
    pretty (TypeMSet x) = "mset of" <+> pretty x
    pretty (TypeFunction fr to) = "function" <+> pretty fr <+> "-->" <+> pretty to
    pretty (TypePartition x) = "partition from" <+> pretty x
    pretty (TypeRelation xs) = prettyList prParens " *" xs

-- | Check whether two types unify or not.
typeUnify :: Type -> Type -> Bool
typeUnify TypeAny _ = True
typeUnify _ TypeAny = True
typeUnify TypeBool TypeBool = True
typeUnify TypeInt TypeInt = True
typeUnify (TypeEnum a) (TypeEnum b) = a == b
typeUnify (TypeUnnamed a) (TypeUnnamed b) = a == b
typeUnify (TypeTuple as) (TypeTuple bs) = and (zipWith typeUnify as bs)
typeUnify (TypeList a) (TypeList b) = typeUnify a b
typeUnify (TypeMatrix a1 a2) (TypeMatrix b1 b2) = and (zipWith typeUnify [a1,a2] [b1,b2])
typeUnify (TypeSet a) (TypeSet b) = typeUnify a b
typeUnify (TypeMSet a) (TypeMSet b) = typeUnify a b
typeUnify (TypeFunction a1 a2) (TypeFunction b1 b2) = and (zipWith typeUnify [a1,a2] [b1,b2])
typeUnify (TypeRelation as) (TypeRelation bs) = and (zipWith typeUnify as bs)
typeUnify (TypePartition a) (TypePartition b) = typeUnify a b
typeUnify _ _ = False

-- | Check whether a given list of types unify with each other or not.
typesUnify :: [Type] -> Bool
typesUnify ts = and [ typeUnify i j | i <- ts, j <- ts ]

-- | Given a list of types return "the most defined" one in this list.
--   This is to get rid of TypeAny's if there are any.
--   Precondition: `typesUnify`
mostDefined :: [Type] -> Type
mostDefined = foldr f TypeAny
    where
        f :: Type -> Type -> Type
        f TypeAny x = x
        f x TypeAny = x
        f _ x@TypeBool{} = x
        f _ x@TypeInt{} = x
        f _ x@TypeEnum{} = x
        f _ x@TypeUnnamed{} = x
        f (TypeTuple as) (TypeTuple bs) = TypeTuple (zipWith f as bs)
        f (TypeList a) (TypeList b) = TypeList (f a b)
        f (TypeMatrix a1 a2) (TypeMatrix b1 b2) = TypeMatrix (f a1 b1) (f a2 b2)
        f (TypeSet a) (TypeSet b) = TypeSet (f a b)
        f (TypeMSet a) (TypeMSet b) = TypeMSet (f a b)
        f (TypeFunction a1 a2) (TypeFunction b1 b2) = TypeFunction (f a1 b1) (f a2 b2)
        f (TypeRelation as) (TypeRelation bs) = TypeRelation (zipWith f as bs)
        f (TypePartition a) (TypePartition b) = TypePartition (f a b)
        f _ _ = TypeAny

homoType :: Doc -> [Type] -> Type
homoType msg [] = userErr $ "empty collection, what's the type?" <++> ("When working on:" <+> msg)
homoType msg xs =
    if typesUnify xs
        then mostDefined xs
        else userErr $ "not a homoType:" <++> ("When working on:" <+> msg)

innerTypeOf :: MonadFail m => Type -> m Type
innerTypeOf (TypeList t) = return t
innerTypeOf (TypeMatrix _ t) = return t
innerTypeOf (TypeSet t) = return t
innerTypeOf (TypeMSet t) = return t
innerTypeOf (TypeFunction a b) = return (TypeTuple [a,b])
innerTypeOf (TypeRelation ts) = return (TypeTuple ts)
innerTypeOf (TypePartition t) = return (TypeSet t)
innerTypeOf t = fail ("innerTypeOf:" <+> pretty (show t))

isPrimitiveType :: Type -> Bool
isPrimitiveType TypeBool{} = True
isPrimitiveType TypeInt{} = True
isPrimitiveType (TypeMatrix index inner) = and [isPrimitiveType index, isPrimitiveType inner]
isPrimitiveType _ = False
