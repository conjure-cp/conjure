{-
 - Module      : Conjure.UI.VariableStrengthening
 - Description : Strengthen variables using type- and domain-inference.
 - Copyright   : Billy Brown 2017
 - License     : BSD3
 
 Preprocessing step that attempts to strengthen variables at the Essence class level, using methods described in the "Reformulating Essence Specifications for Robustness" paper.
-}
module Conjure.UI.VariableStrengthening
  (
    strengthenVariables
  ) where

import Conjure.Language.Definition ( Model )

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: Model  -- ^ Model to have its variables strengthened
                    -> Model  -- ^ Model with its variables strengthened
strengthenVariables m = m
