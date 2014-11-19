{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Conjure.Language.Constant where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.AbstractLiteral

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
    | ConstantAbstract (AbstractLiteral Constant)
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
    typeOf ConstantBool{}           = return TypeBool
    typeOf ConstantInt{}            = return TypeInt
    typeOf (ConstantEnum defn _ _ ) = return (TypeEnum defn)
    typeOf (ConstantAbstract x    ) = typeOf x
    typeOf (DomainInConstant dom) = typeOf dom

instance Pretty Constant where
    pretty (ConstantBool False) = "false"
    pretty (ConstantBool True ) = "true"
    pretty (ConstantInt  x    ) = pretty x
    pretty (ConstantEnum _ _ x) = pretty x
    pretty (ConstantAbstract x) = pretty x
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
normaliseConstant (ConstantAbstract x) = ConstantAbstract (normaliseAbsLit normaliseConstant x)
normaliseConstant (DomainInConstant d) = DomainInConstant (fmap normaliseConstant d)
