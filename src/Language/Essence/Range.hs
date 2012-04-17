{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Range where

import Control.Applicative
import Data.Generics ( Data )
import Data.Maybe ( fromMaybe, maybeToList )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary, arbitrary )
import Test.QuickCheck.Gen ( oneof )

import GenericOps.Core ( NodeTag, Hole
                       , GPlate, gplate, gplateLeaf, gplateError
                       , GNode, mkG, fromG
                       , MatchBind )
import Language.EssenceLexerP
import ParsePrint ( ParsePrint, parse, pretty, prettyListDoc )
import PrintUtils ( (<>) )
import qualified PrintUtils as Pr



data Range a = RAll | RFromTo [Either a (Maybe a, Maybe a)]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Data a => NodeTag (Range a)

instance Hole (Range a)

instance (Eq a, Show a, Data a, GPlate a) => GPlate (Range a) where
    gplate RAll = gplateLeaf RAll
    gplate (RFromTo xs) =
        ( map mkG $ concatMap (\ t -> case t of Left a -> [a]; Right (b,c) -> maybeToList b ++ maybeToList c ) xs
        , RFromTo . consume xs
        )

consume :: GPlate a => [Either b (Maybe b, Maybe b)] -> [GNode] -> [Either a (Maybe a, Maybe a)]

consume (Left _                 :xs) (y:ys) = case fromG y of
    Just z  -> Left z : consume xs ys
    Nothing -> gplateError "Range{1}"
                                                              
consume (Right (Nothing,Nothing):xs)    ys  = Right (Nothing,Nothing) : consume xs ys

consume (Right (Just _ ,Nothing):xs) (y:ys) = case fromG y of
    Just z  -> Right (Just z, Nothing) : consume xs ys
    Nothing -> gplateError "Range{2}"
                                                              
consume (Right (Nothing,Just _ ):xs) (y:ys) = case fromG y of
    Just z  -> Right (Nothing, Just z) : consume xs ys
    Nothing -> gplateError "Range{3}"

consume (Right (Just _ ,Just _ ):xs) (y1:y2:ys) = case (fromG y1, fromG y2) of
    (Just z1, Just z2) -> Right (Just z1, Just z2) : consume xs ys
    _ -> gplateError "Range{4}"

consume [] [] = []

consume _ _ = gplateError "Range{5}"


instance MatchBind a => MatchBind (Range a)

instance ParsePrint a => ParsePrint (Range a) where
    parse = RFromTo <$> sepBy one comma
        where
            one = Right <$> singleRange
                  <||>
                  Left  <$> parse
            singleRange = do dot; dot; j <- optionMaybe parse; return (Nothing, j)
                        <||>
                          do 
                              i <- parse
                              dot; dot
                              j <- optionMaybe parse
                              return (Just i,j)
    pretty RAll = error "do not call pretty Range.RAll"
    pretty (RFromTo xs) = prettyListDoc id Pr.comma (map one xs)
        where
            one (Left a) = pretty a
            one (Right (a,b)) = fromMaybe Pr.empty (pretty <$> a) <> ".." <>
                                fromMaybe Pr.empty (pretty <$> b)

instance Arbitrary a => Arbitrary (Range a) where
    arbitrary = oneof
        [ return RAll
        , RFromTo <$> arbitrary
        ]
