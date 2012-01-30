
import Data.List

main = mapM_ putStrLn
            $ map (intercalate ", ")
            $ subsequences
            $ words "total partial injective surjective bijective"



newtype Function = Function [[Bool]]

instance Show Function where
    show (Function rs) = unlines $ map showRow rs
        where showRow = unwords . map showVal
              showVal False = "0"
              showVal True  = "1"

f1 = Function [ [False, False, True ]
              , [False, True , False]
              , [False, False, False]
              ]
