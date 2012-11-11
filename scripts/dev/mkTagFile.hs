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
                        , let c = "T" ++ x
                        ]

    in
        unlines
            [ "{-# LANGUAGE DeriveDataTypeable #-}"
            , "module Stuff.Generic.Tag where"
            , "import Stuff.Pretty"
            , "import Data.Char ( isSpace )"
            , "import Data.Data ( Data, Typeable )"
            , "import Data.String ( IsString(..) )"
            , "data Tag = " ++ constructors
            , "    deriving (Eq, Ord, Read, Show, Data, Typeable)"
            , "instance Pretty Tag where"
            , "    pretty = pretty . drop 1 . show"
            , "instance IsString Tag where"
            , "    fromString = fromString' . filter (not . isSpace)"
            , "        where"
            , fromStrings
            , "            fromString' t = error $ \"Unknown tag: \" ++ t"
            ]


