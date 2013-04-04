{-# LANGUAGE CPP #-}
module Language.E.GenerateRandomParam.Common where

import Language.E hiding(mkLog)

import Language.E.GenerateRandomParam.Data


#ifdef UP_DEBUG 
import qualified Language.E  as LE
mkLog :: MonadConjure m => String -> Doc -> m ()
mkLog = LE.mkLog
#else
mkLog :: MonadConjure m => String -> Doc -> m ()
mkLog _ _ = return ()
#endif

countRanges :: [Range] -> Integer
countRanges = sum . map countRange

countRange :: Range -> Integer
countRange (RSingle _ ) = 1
countRange (RRange a b) =  b - a + 1


