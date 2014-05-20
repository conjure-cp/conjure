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
            [ "{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}"
            , "module Stuff.Generic.Tag where"
            , "import Stuff.Pretty"
            , "import Data.String ( IsString(..) )"
            , "import GHC.Generics ( Generic )"
            , "import Control.DeepSeq ( NFData(..) )"
            , "import Data.Serialize ( Serialize(..) )"
            , "import Data.Hashable ( Hashable(..) )"
            , "import Data.Aeson ( ToJSON(..) )"
            , "import qualified Data.Text as T"
            , "newtype Tag = Tag T.Text "
            , "    deriving (Eq, Ord, Show, Generic, Hashable, NFData, ToJSON)"
            , "instance Serialize Tag where"
            , "    put (Tag t) = put (T.unpack t)"
            , "    get = fmap (Tag . T.pack) get"
            , "instance Pretty Tag where"
            , "    pretty (Tag t) = pretty t"
            , "instance IsString Tag where"
            , "    fromString t | t `elem` " ++ show xs ++ " = Tag (T.pack t)"
            , "    fromString t = error $ \"Unknown tag: \" ++ t"
            ]


