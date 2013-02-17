{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up where

import Language.E

import Language.E.Up.EprimeToEssence
import Language.E.Up.Data
import Language.E.Up.ReduceSpec
import Language.E.Up.GatherInfomation
import Language.E.Up.RepresentationTree
import Language.E.Up.EvaluateTree
import Language.E.Up.AddEssenceTypes
import Language.E.Up.IO
import Language.E.Up.Debug



translateSolution :: Text -> Text -> Text -> Text -> Text -> Spec
translateSolution specF solF orgF param orgParam = undefined 

