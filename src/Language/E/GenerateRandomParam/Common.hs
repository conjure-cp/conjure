module Language.E.GenerateRandomParam.Common where

import Language.E.GenerateRandomParam.Data

countRanges :: [Range] -> Integer
countRanges = sum . map countRange

countRange :: Range -> Integer
countRange (RSingle _ ) = 1
countRange (RRange a b) =  b - a + 1

