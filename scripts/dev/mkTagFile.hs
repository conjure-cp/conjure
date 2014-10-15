-- see mkTagFile.sh

module Main where

import Control.Applicative
import Control.Monad
import Data.List


main :: IO ()
main = interact $ \ inp ->
    let
        xs = lines inp
        constructors = intercalate "\n    | "
                        [ "T" ++ x
                        | x <- xs
                        ]
        fromStrings  = intercalate "\n"
                        [ "            fromString' \"" ++ x ++ "\" = " ++ c
                        | x <- xs
                        , let c = "Tag $ T.pack $ " ++ show x
                        ]

    in
        unlines
            [ "{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}"
            , "module Stuff.Generic.Tag where"
            , "import Conjure.Prelude"
            , "import Conjure.Language.Pretty"
            , "import Data.Serialize ( Serialize(..) )"
            , "import qualified Data.Text as T"
            , "import Test.QuickCheck ( Arbitrary(..), oneof )"
            , "allTags :: [String]"
            , "allTags = " ++ show xs
            , "newtype Tag = Tag T.Text "
            , "    deriving (Eq, Ord, Show, Data, Typeable, Generic, Hashable, ToJSON)"
            , "instance Serialize Tag where"
            , "    put (Tag t) = put (T.unpack t)"
            , "    get = fmap (Tag . T.pack) get"
            , "instance Pretty Tag where"
            , "    pretty (Tag t) = pretty t"
            , "instance IsString Tag where"
            , "    fromString t | t `elem` allTags = Tag (T.pack t)"
            , "    fromString t = error $ \"Unknown tag: \" ++ t"
            , "instance Arbitrary Tag where"
            , "    arbitrary = fmap (Tag . T.pack) $ oneof $ map return allTags"
            ]


