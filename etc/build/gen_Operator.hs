
import Control.Applicative
import Data.Maybe
import Data.List
import System.Directory
import System.Environment


main :: IO ()
main = do
    let modulesDir = "src/Conjure/Language/Ops"
    modules <- sort . map (head . splitOn '.')
                    . filter (".hs" `isSuffixOf`)
                    . filter (/="Generated.hs")
                    . filter (/="Common.hs")
                <$> getDirectoryContents modulesDir
    let datName = "Operator"
    let consModifier m = "MkOp" ++ m ++ " (Op" ++ m ++ " x)"
    let patModifier m = "MkOp" ++ m ++ " x"

    writeFile (modulesDir ++ "/Generated.hs") $ unlines $ concat
        [ [ "{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}"
          , "{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}"
          , ""
          , "module Conjure.Language.Ops.Generated" ]
        , [ "    ( Ops(..), valuesInIntDomain, evaluateOp, Fixity(..)"
          , "    , functionals, operators" ]
        , [ "    , Op" ++ m ++ "(..)" | m <- modules ]
        , [ "    ) where"
          , ""
          , "-- conjure"
          , "import Conjure.Prelude"
          , "import Conjure.Language.Ops.Common"
          , ""
          ]

        , [ "import Conjure.Language.Ops." ++ m
          | m <- modules
          ]

        , [ ""
          , "data Ops x"
          ]
        , [ "    = " ++ consModifier (head modules)        ]
        , [ "    | " ++ consModifier m | m <- tail modules ]
        , [ "    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)"
          , ""
          , "instance Serialize x => Serialize (Ops x)"
          , "instance Hashable  x => Hashable  (Ops x)"
          , "instance ToJSON    x => ToJSON    (Ops x) where toJSON = genericToJSON jsonOptions"
          , "instance FromJSON  x => FromJSON  (Ops x) where parseJSON = genericParseJSON jsonOptions"
          ]

        , [ ""
          , "instance (TypeOf x, Show x, Pretty x, ExpressionLike x) => TypeOf (Ops x) where"
          ]
        , [ "    typeOf (" ++ patModifier m ++ ") = typeOf x"
          | m <- modules
          ]

        , [ ""
          , "instance EvaluateOp Ops where"
          ]
        , [ "    evaluateOp (" ++ patModifier m ++ ") = evaluateOp x"
          | m <- modules
          ]

        , [ ""
          , "instance Pretty x => Pretty (Ops x) where"
          ]
        , [ "    prettyPrec prec (" ++ patModifier m ++ ") = prettyPrec prec x"
          | m <- modules
          ]

        ]

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn ch (ch2:rest) | ch == ch2 = splitOn ch rest
splitOn ch rest =
    let (before, after) = span (/=ch) rest
    in  before : splitOn ch after
