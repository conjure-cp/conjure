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
            [ "{-# LANGUAGE DeriveGeneric #-}"
            , "module Stuff.Generic.Tag where"
            , "import Stuff.Pretty"
            , "import Data.Char ( isSpace )"
            , "import Data.String ( IsString(..) )"
            , "import GHC.Generics ( Generic )"
            , "import Control.DeepSeq ( NFData(..) )"
            , "import Control.DeepSeq.Generics ( genericRnf )"
            , "import Data.Serialize ( Serialize(..) )"
            , "import Data.Hashable ( Hashable(..) )"
            , "import Data.Hashable.Generic ( gHashWithSalt )"
            , "data Tag = " ++ constructors
            , "    deriving (Eq, Ord, Show, Generic)"
            , "instance Serialize Tag"
            , "instance Hashable Tag where"
            , "    hashWithSalt s x = gHashWithSalt s x"
            , "    {-# INLINEABLE hashWithSalt #-}"
            , "instance NFData Tag where"
            , "    rnf x = genericRnf x"
            , "    {-# INLINEABLE rnf #-}"
            , "instance Pretty Tag where"
            , "    pretty = pretty . drop 1 . show"
            , "instance IsString Tag where"
            , "    fromString = fromString' . filter (not . isSpace)"
            , "        where"
            , fromStrings
            , "            fromString' t = error $ \"Unknown tag: \" ++ t"
            ]


