{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.E.GenerateRandomParam.Common where

import Language.E hiding(mkLog,trace)
import Debug.Trace(trace)

import Language.E.GenerateRandomParam.Data


#ifdef UP_DEBUG 
import qualified Language.E  as LE
mkLog :: MonadConjure m => String -> Doc -> m ()
mkLog nm doc= do 
    let padding = replicate (12 - (length nm)) ' ' 
    LE.mkLog nm (trace (renderWide $ pretty (padding ++ nm) <> " " <+> doc) doc)
#else
mkLog :: MonadConjure m => String -> Doc -> m ()
mkLog _ _ = return ()
#endif

countRanges :: [Range] -> Integer
countRanges = sum . map countRange

countRange :: Range -> Integer
countRange (RSingle _ ) = 1
countRange (RRange a b) =  b - a + 1


