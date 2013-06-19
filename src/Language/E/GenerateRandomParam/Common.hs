{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.E.GenerateRandomParam.Common where

import Language.E hiding(mkLog,trace)
import Debug.Trace(trace)

import Language.E.GenerateRandomParam.Data


import qualified Language.E  as LE
#ifdef UP_DEBUG_TRACE 
mkLog :: MonadConjure m => String -> Doc -> m ()
mkLog nm doc= do 
    let padding = replicate (12 - (length nm)) ' ' 
    LE.mkLog nm (trace (renderWide $ pretty (padding ++ nm) <> " " <+> doc) doc)
#else
mkLog :: MonadConjure m => String -> Doc -> m ()
mkLog nm doc = LE.mkLog nm doc
#endif

countRanges :: [Range] -> Integer
countRanges = sum . map countRange

countRange :: Range -> Integer
countRange (RSingle _ ) = 1
countRange (RRange a b) =  b - a + 1

printPretty :: Pretty a => String ->  a -> IO ()
printPretty s p = putStrLn $ 'ː' :s ++ 'ː' : '\n' : (renderNormal) p

printPrettym :: Pretty a => String -> [a] -> IO ()
printPrettym s arr= putStrLn  $'ː' : s ++  'ː' : '\n' :  (renderNormal . vcat . map pretty) arr

