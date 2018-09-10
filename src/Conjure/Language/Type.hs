{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Conjure.Language.Type
    ( Type(..)
    , typeUnify
    , typesUnify
    , mostDefined
    , homoType
    , matrixNumDims
    , innerTypeOf
    , isPrimitiveType
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name
import Conjure.Language.Pretty


data Type
    = TypeAny
    | TypeBool
    | TypeInt
    | TypeEnum Name
    | TypeUnnamed Name
    | TypeTuple [Type]
    | TypeRecord [(Name, Type)]
    | TypeVariant [(Name, Type)]
    | TypeList Type
    | TypeMatrix Type Type
    | TypeSet Type
    | TypeMSet Type
    | TypeFunction Type Type
    | TypeSequence Type
    | TypeRelation [Type]
    | TypePartition Type
    | TypePermutation Type
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Type
instance Hashable  Type
instance ToJSON    Type where toJSON = genericToJSON jsonOptions
instance FromJSON  Type where parseJSON = genericParseJSON jsonOptions

instance Pretty Type where
    pretty TypeAny = "?"
    pretty TypeBool = "bool"
    pretty TypeInt = "int"
    pretty (TypeEnum nm ) = pretty nm
    pretty (TypeUnnamed nm) = pretty nm
    pretty (TypeTuple xs) = (if length xs <= 1 then "tuple" else prEmpty)
                         <> prettyList prParens "," xs
    pretty (TypeRecord xs) = "record" <+> prettyList prBraces ","
        [ pretty nm <+> ":" <+> pretty ty | (nm, ty) <- xs ]
    pretty (TypeVariant xs) = "variant" <+> prettyList prBraces ","
        [ pretty nm <+> ":" <+> pretty ty | (nm, ty) <- xs ]
    pretty (TypeList x) = prBrackets (pretty x)
    pretty (TypeMatrix index innerNested)
        = "matrix indexed by" <+> prettyList prBrackets "," indices
                              <+> "of" <+> pretty inner
        where
            (indices,inner) = first (index:) $ collect innerNested
            collect (TypeMatrix i j) = first (i:) $ collect j
            collect x = ([],x)
    pretty (TypeSet x) = "set of" <+> pretty x
    pretty (TypeMSet x) = "mset of" <+> pretty x
    pretty (TypeFunction fr to) = "function" <+> pretty fr <+> "-->" <+> pretty to
    pretty (TypeSequence x) = "sequence of" <+> pretty x
    pretty (TypePartition x) = "partition from" <+> pretty x
    pretty (TypeRelation xs) = "relation of" <+> prettyList prParens " *" xs
    pretty (TypePermutation x) = "permutation of" <+> pretty x

-- | Check whether two types unify or not.
typeUnify :: Type -> Type -> Bool
typeUnify TypeAny _ = True
typeUnify _ TypeAny = True
typeUnify TypeBool TypeBool = True
typeUnify TypeInt TypeInt = True
typeUnify TypeInt TypeEnum{} = True
typeUnify TypeEnum{} TypeInt = True
typeUnify (TypeEnum a) (TypeEnum b) = a == b || a == "?" || b == "?"    -- the "?" is a hack so sameToSameToBool works
typeUnify (TypeUnnamed a) (TypeUnnamed b) = a == b
typeUnify (TypeTuple [TypeAny]) TypeTuple{} = True
typeUnify TypeTuple{} (TypeTuple [TypeAny]) = True
typeUnify (TypeTuple as) (TypeTuple bs) = (length as == length bs) && and (zipWith typeUnify as bs)
typeUnify (TypeRecord as) (TypeRecord bs)
    | length as /= length bs = False
    | otherwise = and [ case lookup n bs of
                             Nothing -> False
                             Just b -> typeUnify a b
                      | (n,a) <- as
                      ]
typeUnify (TypeVariant as) (TypeVariant bs)
    | length as /= length bs = False
    | otherwise = and [ case lookup n bs of
                             Nothing -> False
                             Just b -> typeUnify a b
                      | (n,a) <- as
                      ]
typeUnify (TypeList a) (TypeList b) = typeUnify a b
typeUnify (TypeMatrix a1 a2) (TypeMatrix b1 b2) = and (zipWith typeUnify [a1,a2] [b1,b2])
typeUnify (TypeList a) (TypeMatrix _ b) = typeUnify a b
typeUnify (TypeMatrix _ a) (TypeList b) = typeUnify a b
typeUnify (TypeList a) (TypeSequence b) = typeUnify a b
typeUnify (TypeSequence a) (TypeList b) = typeUnify a b
typeUnify (TypeSet a) (TypeSet b) = typeUnify a b
typeUnify (TypeMSet a) (TypeMSet b) = typeUnify a b
typeUnify (TypeFunction a1 a2) (TypeFunction b1 b2) = and (zipWith typeUnify [a1,a2] [b1,b2])
typeUnify (TypeSequence a) (TypeSequence b) = typeUnify a b
typeUnify (TypeRelation [TypeAny]) TypeRelation{} = True                -- also hacks to make sameToSameToBool work
typeUnify TypeRelation{} (TypeRelation [TypeAny]) = True
typeUnify (TypeRelation as) (TypeRelation bs) = (length as == length bs) && and (zipWith typeUnify as bs)
typeUnify (TypePartition a) (TypePartition b) = typeUnify a b
typeUnify (TypePermutation a) (TypePermutation b) = typeUnify a b
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
        f (TypeTuple [TypeAny]) x = x
        f x (TypeTuple [TypeAny]) = x
        f (TypeTuple as) (TypeTuple bs) | length as == length bs = TypeTuple (zipWith f as bs)
        f (TypeRecord as) (TypeRecord bs)
            | sort (map fst as) == sort (map fst bs) =
                TypeRecord [ case lookup n bs of
                                Nothing -> bug "mostDefined.TypeRecord"
                                Just b  -> (n, f a b)
                           | (n,a) <- as
                           ]
            | typeUnify (TypeRecord as) (TypeRecord bs) = TypeAny
            | otherwise = TypeAny
        f (TypeVariant as) (TypeVariant bs)
            | sort (map fst as) == sort (map fst bs) =
                TypeVariant [ case lookup n bs of
                                Nothing -> bug "mostDefined.TypeVariant"
                                Just b  -> (n, f a b)
                           | (n,a) <- as
                           ]
            | typeUnify (TypeVariant as) (TypeVariant bs) = TypeAny
            | otherwise = TypeAny
        f (TypeList a) (TypeList b) = TypeList (f a b)
        f (TypeMatrix a1 a2) (TypeMatrix b1 b2) = TypeMatrix (f a1 b1) (f a2 b2)
        f (TypeList a) (TypeMatrix _ b) = TypeList (f a b)
        f (TypeMatrix _ a) (TypeList b) = TypeList (f a b)
        f (TypeSet a) (TypeSet b) = TypeSet (f a b)
        f (TypeMSet a) (TypeMSet b) = TypeMSet (f a b)
        f (TypeFunction a1 a2) (TypeFunction b1 b2) = TypeFunction (f a1 b1) (f a2 b2)
        f (TypeSequence a) (TypeSequence b) = TypeSequence (f a b)
        f (TypeRelation [TypeAny]) x = x
        f x (TypeRelation [TypeAny]) = x
        f (TypeRelation as) (TypeRelation bs) | length as == length bs = TypeRelation (zipWith f as bs)
        f (TypePartition a) (TypePartition b) = TypePartition (f a b)
        f (TypePermutation a) (TypePermutation b) = TypePermutation (f a b)
        f _ _ = TypeAny

matrixNumDims :: Type -> Int
matrixNumDims (TypeMatrix _ t) = 1 + matrixNumDims t
matrixNumDims (TypeList     t) = 1 + matrixNumDims t
matrixNumDims _ = 0

homoType :: MonadFail m => Doc -> [Type] -> m Type
homoType msg [] = fail $ "empty collection, what's the type?" <++> ("When working on:" <+> msg)
homoType msg xs =
    if typesUnify xs
        then return (mostDefined xs)
        else fail $ vcat [ "Not uniformly typed:" <+> msg
                         , "Involved types are:" <+> vcat (map pretty xs)
                         ]

innerTypeOf :: MonadFail m => Type -> m Type
innerTypeOf (TypeList t) = return t
innerTypeOf (TypeMatrix _ t) = return t
innerTypeOf (TypeSet t) = return t
innerTypeOf (TypeMSet t) = return t
innerTypeOf (TypeFunction a b) = return (TypeTuple [a,b])
innerTypeOf (TypeSequence t) = return (TypeTuple [TypeInt,t])
innerTypeOf (TypeRelation ts) = return (TypeTuple ts)
innerTypeOf (TypePartition t) = return (TypeSet t)
innerTypeOf (TypePermutation t) = return (TypePermutation t)
innerTypeOf t = fail ("innerTypeOf:" <+> pretty (show t))

isPrimitiveType :: Type -> Bool
isPrimitiveType TypeBool{} = True
isPrimitiveType TypeInt{} = True
isPrimitiveType (TypeMatrix index inner) = and [isPrimitiveType index, isPrimitiveType inner]
isPrimitiveType _ = False
