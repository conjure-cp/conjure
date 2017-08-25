{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Conjure.Language.Name where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Pretty

-- text
import qualified Data.Text as T

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose )


data Name = Name Text | MachineName Text Int [Text] | NameMetaVar String
    deriving (Show, Data, Typeable, Generic)

instance Eq Name where
    Name x == Name y = x == y
    x == y = show (pretty x) == show (pretty y)

instance Ord Name where
    compare (Name x) (Name y) = compare x y
    compare x y = compare (show (pretty x)) (show (pretty y))

instance Serialize Name
instance Hashable  Name
instance ToJSON    Name where toJSON = genericToJSON jsonOptions
instance FromJSON  Name where parseJSON = genericParseJSON jsonOptions

instance Arbitrary Name where
    arbitrary = do
        ch <- choose ('a', 'z')
        return $ Name $ T.pack [ch]
    shrink (Name n) = [ Name (T.drop 1 n) | T.length n > 1 ]
    shrink _ = []

instance IsString Name where
    fromString = Name . fromString

instance Pretty Name where
    pretty (Name n) = pretty n
    pretty (MachineName base n rest) = pretty base <> pretty n <> hcat (map pretty rest)
    pretty (NameMetaVar n) = "&" <> pretty n

instance Monoid Name where
    mempty = ""
    mappend (Name a) (Name b) = Name (mappend a b)
    mappend (MachineName base n rest) (Name new) = MachineName base n (rest++[new])
    mappend (Name a) (MachineName base n rest) = MachineName (mappend a base) n rest
    mappend a b = bug $ "mappend{Name}" <+> vcat [pretty (show a), pretty (show b)]
