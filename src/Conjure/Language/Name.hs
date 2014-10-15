{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Conjure.Language.Name where

-- conjure
import Conjure.Prelude
import Stuff.Pretty

-- text
import qualified Data.Text as T

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose )


newtype Name = Name Text
    deriving (Eq, Ord, Show, Data, Typeable, Generic, IsString, Serialize, Hashable, ToJSON, FromJSON, Monoid)

instance Arbitrary Name where
    arbitrary = do
        ch <- choose ('a', 'z')
        return $ Name $ T.pack [ch]
    shrink (Name n) = [ Name (T.drop 1 n) | T.length n > 1 ]

instance Pretty Name where
    pretty (Name n) = pretty n
