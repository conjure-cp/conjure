{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Conjure.Language.Constant where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name
import Conjure.Language.Domain
import Conjure.Language.Type

import Conjure.Language.TypeOf
import Conjure.Language.AdHoc
import Conjure.Language.Pretty

-- aeson
import qualified Data.Aeson as JSON

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), oneof )



data Constant
    = ConstantBool Bool
    | ConstantInt Int
    | ConstantEnum Name   {- name for the enum domain -}
                   [Name] {- values in the enum domain -}
                   Name   {- the literal -}
    | ConstantTuple [Constant]
    | ConstantMatrix (Domain () Constant) [Constant]
    | ConstantSet [Constant]
    | ConstantMSet [Constant]
    | ConstantFunction [(Constant, Constant)]
    | ConstantRelation [[Constant]]
    | ConstantPartition [[Constant]]
    | DomainInConstant (Domain () Constant)
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Constant
instance Hashable  Constant
instance ToJSON    Constant where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  Constant where parseJSON = JSON.genericParseJSON jsonOptions

instance Arbitrary Constant where
    arbitrary = oneof
        [ ConstantBool <$> arbitrary
        , ConstantInt <$> arbitrary
        ]

instance TypeOf Constant where
    typeOf ConstantBool{}            = return TypeBool
    typeOf ConstantInt{}             = return TypeInt
    typeOf (ConstantEnum defn _ _  ) = return (TypeEnum defn)
    typeOf (ConstantTuple        xs) = TypeTuple    <$> mapM typeOf xs
    typeOf (ConstantMatrix ind inn ) = TypeMatrix   <$> typeOf ind <*> (homoType <$> mapM typeOf inn)
    typeOf (ConstantSet         xs ) = TypeSet      <$> (homoType <$> mapM typeOf xs)
    typeOf (ConstantMSet        xs ) = TypeMSet     <$> (homoType <$> mapM typeOf xs)
    typeOf (ConstantFunction    xs ) = TypeFunction <$> (homoType <$> mapM (typeOf . fst) xs)
                                                    <*> (homoType <$> mapM (typeOf . fst) xs)
    typeOf (ConstantRelation    xss) = do
        ty <- homoType <$> mapM (typeOf . ConstantTuple) xss
        case ty of
            TypeTuple ts -> return (TypeRelation ts)
            _ -> bug "expecting TypeTuple in typeOf"
    typeOf (ConstantPartition   xss) = TypePartition <$> (homoType <$> mapM typeOf (concat xss))
    typeOf (DomainInConstant dom) = typeOf dom

instance Pretty Constant where
    pretty (ConstantBool False) = "false"
    pretty (ConstantBool True) = "true"
    pretty (ConstantInt x) = pretty x
    pretty (ConstantEnum _ _ x) = pretty x
    pretty (ConstantTuple xs) = (if length xs < 2 then "tuple" else prEmpty) <+> prettyList prParens "," xs
    pretty (ConstantMatrix index xs) = let f i = prBrackets (i <> ";" <+> pretty index) in prettyList f "," xs
    pretty (ConstantSet       xs ) =                prettyList prBraces "," xs
    pretty (ConstantMSet      xs ) = "mset"      <> prettyList prParens "," xs
    pretty (ConstantFunction  xs ) = "function"  <> prettyListDoc prParens "," [ pretty a <+> "-->" <+> pretty b | (a,b) <- xs ]
    pretty (ConstantRelation  xss) = "relation"  <> prettyListDoc prParens "," [ pretty (ConstantTuple xs)       | xs <- xss   ]
    pretty (ConstantPartition xss) = "partition" <> prettyListDoc prParens "," [ prettyList prBraces "," xs      | xs <- xss   ]
    pretty (DomainInConstant d) = "`" <> pretty d <> "`"

instance ExpressionLike Constant where
    fromInt = ConstantInt
    intOut (ConstantInt x) = return x
    intOut c = fail ("Expecting an integer, but found:" <+> pretty c)
    fromBool = ConstantBool
    boolOut (ConstantBool x) = return x
    boolOut c = fail ("Expecting a boolean, but found:" <+> pretty c)

instance ReferenceContainer Constant where
    fromName name = bug ("ReferenceContainer{Constant} --" <+> pretty name)

normaliseConstant :: Constant -> Constant
normaliseConstant x@ConstantBool{} = x
normaliseConstant x@ConstantInt{}  = x
normaliseConstant x@ConstantEnum{} = x
normaliseConstant (ConstantTuple xs) = ConstantTuple $ map normaliseConstant xs
normaliseConstant (ConstantMatrix d xs) = ConstantMatrix d $ map normaliseConstant xs
normaliseConstant (ConstantSet xs) = ConstantSet $ sortNub $ map normaliseConstant xs
normaliseConstant (ConstantMSet xs) = ConstantMSet $ sort $ map normaliseConstant xs
normaliseConstant (ConstantFunction xs) = ConstantFunction $ sortNub
    [ (normaliseConstant x, normaliseConstant y) | (x, y) <- xs ]
normaliseConstant (ConstantRelation xss) = ConstantRelation $ sortNub $ map (map normaliseConstant) xss
normaliseConstant (ConstantPartition xss) = ConstantPartition $ sortNub $ map (sortNub . map normaliseConstant) xss
normaliseConstant (DomainInConstant d) = DomainInConstant (fmap normaliseConstant d)
