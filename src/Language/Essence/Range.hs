{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Range where

import Control.Applicative
import Data.Generics ( Data )
import Data.Maybe ( fromMaybe )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary, arbitrary )
import Test.QuickCheck.Gen ( oneof )

import GenericOps.Core ( NodeTag, Hole, GPlate, gplate, gplateLeaf, gplateSingle, gplateUniList, MatchBind )
import ParsecUtils ( comma, dot, optionMaybe, sepBy, try )
import ParsePrint ( ParsePrint, parse, pretty, prettyList )
import PrintUtils ( (<>) )
import qualified PrintUtils as Pr



data Range a = RAll | RList [a] | RFromTo (Maybe a) (Maybe a)
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Data a => NodeTag (Range a)

instance Hole (Range a)

instance (Eq a, Show a, Data a, GPlate a) => GPlate (Range a) where
    gplate RAll = gplateLeaf RAll
    gplate (RList xs) = gplateUniList RList xs
    gplate p@(RFromTo Nothing  Nothing ) = gplateLeaf p
    gplate   (RFromTo Nothing  (Just y)) = gplateSingle  (\ y'      -> RFromTo Nothing   (Just y') ) y
    gplate   (RFromTo (Just x) Nothing ) = gplateSingle  (\ x'      -> RFromTo (Just x') Nothing   ) x
    gplate   (RFromTo (Just x) (Just y)) = gplateUniList (\ [x',y'] -> RFromTo (Just x') (Just y') ) [x,y]

instance MatchBind a => MatchBind (Range a)

instance ParsePrint a => ParsePrint (Range a) where
    parse = try pRList <|> pRFromTo
        where
            pRList = do
                i <- optionMaybe parse
                dot; dot
                j <- optionMaybe parse
                return $ RFromTo i j
            pRFromTo = RList <$> sepBy parse comma
    pretty RAll = error "do not call pretty Range.RAll"
    pretty (RList      xs) = prettyList id Pr.comma xs
    pretty (RFromTo fr to) = fr' <> ".." <> to'
        where
            fr' = fromMaybe Pr.empty (pretty <$> fr)
            to' = fromMaybe Pr.empty (pretty <$> to)

instance Arbitrary a => Arbitrary (Range a) where
    arbitrary = oneof
        [ return RAll
        , RList   <$> arbitrary
        , RFromTo <$> arbitrary <*> arbitrary
        ]
