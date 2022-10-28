{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ( (<$>) )
import Data.Maybe ( Maybe(..) )
import Data.List ( sort, isSuffixOf, intercalate )
import System.Directory ( getDirectoryContents, createDirectoryIfMissing )
import Control.Exception ( catch, IOException )


main :: IO ()
main = do
    let opDir   = "src/Conjure/Language/Expression/Op"
    let outFile = "src/Conjure/Language/Expression/Internal/Generated.hs"
    createDirectoryIfMissing True "src/Conjure/Language/Expression/Internal"
    operators <- sort . map (head . splitOn '.')
                      . filter (".hs" `isSuffixOf`)
                  <$> getDirectoryContents opDir
    let datName = "Operator"
    let consModifier m = "MkOp" ++ m ++ " (Op" ++ m ++ " x)"
    let patModifier m = "MkOp" ++ m ++ " x"

    let outText = unlines $ concat
            [ [ "{-# OPTIONS_GHC -fno-warn-orphans #-}"
              , ""
              , "module Conjure.Language.Expression.Internal.Generated where"
              , ""
              , "import Conjure.Prelude"
              , "import Conjure.Language.AdHoc"
              , "import Conjure.Language.Definition"
              , "import Conjure.Language.Expression.Op"
              , ""
              ]

            , concat
                [ [ "instance Op" ++ m ++ " Expression :< Expression where"
                  , "    inject = Op . MkOp" ++ m
                  , "    project (Op (MkOp" ++ m ++ " x)) = return x"
                  , "    project _ = failDoc \"projecting Op" ++ m ++ "\""
                  ]
                | m <- operators
                ]

            ]

    outText' <- catch (Just <$> readFile outFile)
                      (\ (e :: IOException) -> return Nothing )
    if and [ Just (length outText) /= (length <$> outText')
           , Just outText /= outText'
           ]
        then do
            putStrLn $ "Generating " ++ outFile
            writeFile outFile outText
        else
            putStrLn $ "Reusing " ++ outFile

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn ch (ch2:rest) | ch == ch2 = splitOn ch rest
splitOn ch rest =
    let (before, after) = span (/=ch) rest
    in  before : splitOn ch after
