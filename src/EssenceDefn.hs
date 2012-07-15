{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module EssenceDefn where

import Generic
import Pretty

import Data.Text ( Text )
import qualified Data.Map as M


data Primitive = I Integer | B Bool
    deriving (Eq, Ord, Read, Show)

instance Pretty Primitive where
    pretty (I x) = pretty $ show x
    pretty (B x) = pretty $ show x


type E = Generic Text Primitive ()

injectInt :: Integer -> E
injectInt = Prim . I

injectBool :: Bool -> E
injectBool = Prim . B

mkPlus :: E -> E -> E
mkPlus a b = Named "plus" $ Object $ M.fromList [ ("left" , a)
                                                , ("right", b)
                                                ]

plusEvaluate :: E -> [E]
plusEvaluate ([generic|@a|]) = [a,a]
plusEvaluate ([generic|value:@a|]) = [a]
-- plusEvaluate ([generic|plus:{left:@x,right:@y}|]) = [x,y]

ex :: E
ex = mkPlus (injectInt 1) (injectInt 2)
