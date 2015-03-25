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


data Name = Name Text | MachineName Text Int [Text]
    deriving (Show, Data, Typeable, Generic)

instance Eq Name where
    (==) = (==) `on` (show . pretty)

instance Ord Name where
    compare = compare `on` (show . pretty)

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

instance Monoid Name where
    mempty = ""
    mappend (Name a) (Name b) = Name (mappend a b)
    mappend (MachineName base n rest) (Name new) = MachineName base n (rest++[new])
    mappend _ _ = bug "mappend{Name}"
