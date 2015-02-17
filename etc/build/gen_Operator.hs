{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ( (<$>) )
import Data.Maybe ( Maybe(..) )
import Data.List ( sort, isSuffixOf )
import System.Directory ( getDirectoryContents )
import Control.Exception ( catch, IOException )


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

    let outText = unlines $ concat
            [ [ "{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}"
              , "{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}"
              , ""
              , "module Conjure.Language.Ops.Generated" ]
            , [ "    ( Ops(..)"
              , "    , valuesInIntDomain"
              ]
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
              , "instance (TypeOf x, Show x, Pretty x, ExpressionLike x, ReferenceContainer x) => TypeOf (Ops x) where"
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
              , "instance SimplifyOp Ops where"
              ]
            , [ "    simplifyOp inj (" ++ patModifier m ++ ") = simplifyOp (inj . MkOp" ++ m ++ ") x"
              | m <- modules
              ]

            , [ ""
              , "instance (Pretty x, ExpressionLike x) => Pretty (Ops x) where"
              ]
            , [ "    prettyPrec prec (" ++ patModifier m ++ ") = prettyPrec prec x"
              | m <- modules
              ]

            ]

    outText' <- catch (Just <$> readFile (modulesDir ++ "/Generated.hs"))
                      (\ (e :: IOException) -> return Nothing )
    if and [ Just (length outText) /= (length <$> outText')
           , Just outText /= outText'
           ]
        then do
            putStrLn $ "Generating " ++ modulesDir ++ "/Generated.hs"
            writeFile (modulesDir ++ "/Generated.hs") outText
        else
            putStrLn $ "Reusing " ++ modulesDir ++ "/Generated.hs"

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn ch (ch2:rest) | ch == ch2 = splitOn ch rest
splitOn ch rest =
    let (before, after) = span (/=ch) rest
    in  before : splitOn ch after
