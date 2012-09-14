module Language.E.Pipeline.ConjureRepr where

import Language.E
import Language.E.Pipeline.ApplyRepr ( applyRepr )
import Language.E.Pipeline.ReadIn


conjureRepr :: (Monad m, Functor m)
    => (FilePath, Text)
    -> [(FilePath, Text)]
    -> CompE m Spec
conjureRepr spectobe rulestobe = do
    spec  <- readSpec spectobe
    rules <- mapM readRuleRepr rulestobe
    applyRepr spec rules

