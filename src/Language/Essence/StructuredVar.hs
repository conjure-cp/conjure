{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.StructuredVar where

import Control.Applicative
import Data.Generics ( Data )
import Data.String ( IsString, fromString )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary, arbitrary, oneof, choose )

import GenericOps.Core ( NodeTag, Hole, GPlate, MatchBind )
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty, prettyList )
import qualified PrintUtils as Pr

import Language.Essence.Identifier



data StructuredVar = STuple  [StructuredVar]
                   | SMatrix [StructuredVar]
                   | I Identifier
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance IsString StructuredVar where
    fromString = I . Identifier

instance NodeTag StructuredVar

instance Hole StructuredVar

instance GPlate StructuredVar

instance MatchBind StructuredVar

instance ParsePrint StructuredVar where
    parse = choiceTry [ do reserved "tuple"; STuple <$> parens (sepBy1 parse comma)
                      , STuple  <$> parens   (countSepAtLeast 2 parse comma)
                      , SMatrix <$> brackets (countSepAtLeast 2 parse comma)
                      , I . Identifier <$> identifier
                      ]
    pretty (STuple  ss) = prettyList Pr.parens   Pr.comma ss
    pretty (SMatrix ss) = prettyList Pr.brackets Pr.comma ss
    pretty (I        i) = pretty i

instance Arbitrary StructuredVar where
    arbitrary = oneof [ do (i,j) <- arbitrary; ks <- arbitrary; return $ STuple  (i:j:ks)
                      , do (i,j) <- arbitrary; ks <- arbitrary; return $ SMatrix (i:j:ks)
                      , I . Identifier . return <$> choose ('a', 'z')
                      ]
