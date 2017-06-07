{-
 - Module      : Conjure.Process.StrengthenVariables
 - Description : Strengthen variables using type- and domain-inference.
 - Copyright   : Billy Brown 2017
 - License     : BSD3
 
 Processing step that attempts to strengthen variables at the Essence class level, using methods described in the "Reformulating Essence Specifications for Robustness" paper.
-}

module Conjure.Process.StrengthenVariables
  (
    strengthenVariables
  ) where

import Debug.Trace

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: (MonadLog m, MonadFail m, MonadUserError m)
                    => Model    -- ^ Model to have its variables strengthen.ed
                    -> m Model  -- ^ Model with its variables strengthened.
strengthenVariables = runNameGen . resolveNames >=> return . strengthenVariables'

-- | Make passes over the variables until no changes are left to be made.
strengthenVariables' :: Model -- ^ Model to have its variables strengthened.
                     -> Model -- ^ Possibly updated model.
strengthenVariables' = trace "strengthening"
