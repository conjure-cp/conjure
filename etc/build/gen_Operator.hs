{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ( (<$>) )
import Data.Maybe ( Maybe(..) )
import Data.List ( sort, isSuffixOf )
import System.Directory ( getDirectoryContents )
import Control.Exception ( catch, IOException )


main :: IO ()
main = do
    let modulesDir = "src/Conjure/Language/Expression/Op"
    modules <- sort . map (head . splitOn '.')
                    . filter (".hs" `isSuffixOf`)
                <$> getDirectoryContents modulesDir
    let datName = "Operator"
    let consModifier m = "MkOp" ++ m ++ " (Op" ++ m ++ " x)"
    let patModifier m = "MkOp" ++ m ++ " x"

    let outText = unlines $ concat
            [ [ "{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}"
              , "{-# LANGUAGE UndecidableInstances #-}"
              , ""
              , "module Conjure.Language.Expression.Op.Internal.Generated" ]
            , [ "    ( Op(..)"
              , "    , valuesInIntDomain"
              ]
            , [ "    , Op" ++ m ++ "(..)" | m <- modules ]
            , [ "    ) where"
              , ""
              , "-- conjure"
              , "import Conjure.Prelude"
              , "import Conjure.Language.Expression.Op.Internal.Common"
              , ""
              ]

            , [ "import Conjure.Language.Expression.Op." ++ m
              | m <- modules
              ]

            , [ ""
              , "data Op x"
              ]
            , [ "    = " ++ consModifier (head modules)        ]
            , [ "    | " ++ consModifier m | m <- tail modules ]
            , [ "    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)"
              , ""
              , "instance Serialize x => Serialize (Op x)"
              , "instance Hashable  x => Hashable  (Op x)"
              , "instance ToJSON    x => ToJSON    (Op x) where toJSON = genericToJSON jsonOptions"
              , "instance FromJSON  x => FromJSON  (Op x) where parseJSON = genericParseJSON jsonOptions"
              ]

            , [ ""
              , "instance (TypeOf x, Show x, Pretty x, ExpressionLike x, ReferenceContainer x) => TypeOf (Op x) where"
              ]
            , [ "    typeOf (" ++ patModifier m ++ ") = typeOf x"
              | m <- modules
              ]

            , [ ""
              , "instance ( Pretty x"
              , "         , ExpressionLike x"
              , "         , DomainOf x x"
              , "         , TypeOf x"
              , "         , Domain () x :< x"
              , "         ) => DomainOf (Op x) x where"
              ]
            , [ "    domainOf (" ++ patModifier m ++ ") = domainOf x"
              | m <- modules
              ]

            , [ ""
              , "instance EvaluateOp Op where"
              ]
            , [ "    evaluateOp (" ++ patModifier m ++ ") = evaluateOp x"
              | m <- modules
              ]

            , [ ""
              , "instance SimplifyOp Op where"
              ]
            , [ "    simplifyOp inj (" ++ patModifier m ++ ") = simplifyOp (inj . MkOp" ++ m ++ ") x"
              | m <- modules
              ]

            , [ ""
              , "instance (Pretty x, ExpressionLike x) => Pretty (Op x) where"
              ]
            , [ "    prettyPrec prec (" ++ patModifier m ++ ") = prettyPrec prec x"
              | m <- modules
              ]

            ]

    outText' <- catch (Just <$> readFile (modulesDir ++ "/Internal/Generated.hs"))
                      (\ (e :: IOException) -> return Nothing )
    if and [ Just (length outText) /= (length <$> outText')
           , Just outText /= outText'
           ]
        then do
            putStrLn $ "Generating " ++ modulesDir ++ "/Internal/Generated.hs"
            writeFile (modulesDir ++ "/Internal/Generated.hs") outText
        else
            putStrLn $ "Reusing " ++ modulesDir ++ "/Internal/Generated.hs"

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn ch (ch2:rest) | ch == ch2 = splitOn ch rest
splitOn ch rest =
    let (before, after) = span (/=ch) rest
    in  before : splitOn ch after
