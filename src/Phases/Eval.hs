module Phases.Eval where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO )
import Data.Generics.Uniplate.Direct ( transformBiM )

import Language.Essence
import Language.EssenceEvaluator ( runEvaluateExpr )


evalSpec :: (Applicative m, MonadIO m) => Spec -> m Spec
evalSpec s = do
    let eval = fmap fst . runEvaluateExpr (topLevelBindings s)
    transformBiM eval s
