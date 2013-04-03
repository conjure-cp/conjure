{-# LANGUAGE CPP #-}
module Language.E.GenerateRandomParam.Common where

import Language.E hiding(mkLog)

import Language.E.GenerateRandomParam.Data

mkLog :: MonadConjure m => String -> Doc -> m ()

#ifdef UP_DEBUG 
import qualified Language.E  as LE
mkLog = LE.mkLog
#else
mkLog _ _ = return ()
#endif

countRanges :: [Range] -> Integer
countRanges = sum . map countRange

countRange :: Range -> Integer
countRange (RSingle _ ) = 1
countRange (RRange a b) =  b - a + 1


