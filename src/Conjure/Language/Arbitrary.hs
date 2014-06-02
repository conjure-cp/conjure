module Conjure.Language.Arbitrary
    ( AnyDomainTuple(..)
    , AnyConstantTuple(..)
    ) where

-- conjure
import Conjure.Language.Definition

-- base
import Data.List ( subsequences )

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose, vectorOf )


newtype AnyDomainTuple a = AnyDomainTuple (Domain a)
    deriving (Show)

instance Arbitrary a => Arbitrary (AnyDomainTuple a) where
    arbitrary = do
        arity <- choose (2 :: Int, 10)
        xs    <- vectorOf arity arbitrary
        return $ AnyDomainTuple $ DomainTuple xs
    shrink (AnyDomainTuple d) =
        case d of
            DomainTuple xs -> 
                [ AnyDomainTuple (DomainTuple ys)
                | ys <- subsequences xs
                , length ys >= 1
                , length ys < length xs
                ]
            _ -> []


newtype AnyConstantTuple = AnyConstantTuple Constant
    deriving (Show)

instance Arbitrary AnyConstantTuple where
    arbitrary = do
        arity <- choose (2 :: Int, 10)
        xs    <- vectorOf arity arbitrary
        return $ AnyConstantTuple $ ConstantTuple xs
    shrink (AnyConstantTuple c) =
        case c of
            ConstantTuple xs -> 
                [ AnyConstantTuple (ConstantTuple ys)
                | ys <- subsequences xs
                , length ys >= 1
                , length ys < length xs
                ]
            _ -> []

