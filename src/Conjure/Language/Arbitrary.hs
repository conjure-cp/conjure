module Conjure.Language.Arbitrary
    ( AnyDomainTuple(..)
    , AnyConstantTuple(..)
    ) where

-- conjure
import Conjure.Language.Definition

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose, vectorOf )


newtype AnyDomainTuple a = AnyDomainTuple (Domain a)
    deriving (Show)

instance Arbitrary a => Arbitrary (AnyDomainTuple a) where
    arbitrary = do
        arity <- choose (2 :: Int, 10)
        xs    <- vectorOf arity arbitrary
        return $ AnyDomainTuple $ DomainTuple xs


newtype AnyConstantTuple = AnyConstantTuple Constant
    deriving (Show)

instance Arbitrary AnyConstantTuple where
    arbitrary = do
        arity <- choose (2 :: Int, 10)
        xs    <- vectorOf arity arbitrary
        return $ AnyConstantTuple $ ConstantTuple xs

