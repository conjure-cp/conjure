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

import Prelude ( (++), ($), map, show )

import Debug.Trace

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Expression

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: Model  -- ^ Model to have its variables strengthen.ed
                    -> Model  -- ^ Model with its variables strengthened.
strengthenVariables m@Model { mStatements = stmts }
  -- Instead of working iterativelty, this should keep making changes until no more are possible.
  -- Does the order of changes applied make a difference?
  -- This actually needs to pass some construct down for resolving references.
  = traceShow (map functionAttributes stmts) m

-- | Make function attributes as constraining as possible,
-- | with the aim of achieving total and bijective.
-- | Ignore statements that are not functions.
functionAttributes :: Statement -- ^ Statement to have function attributes changed.
                   -> Statement -- ^ Statement with function attributes possibly changed.
functionAttributes (Declaration (FindOrGiven fg n d@(DomainFunction _ _ _ _)))
  = trace (show fg ++ " function") $
    Declaration (FindOrGiven fg n (functionAttributes' d))
functionAttributes (Declaration (Letting n (Domain d@(DomainFunction _ _ _ _))))
  = trace "Letting function" $
    Declaration (Letting n (Domain (functionAttributes' d)))
functionAttributes s = s

functionAttributes' :: Domain () Expression -- ^ Domain of the function to have its attributes constrained.
                    -> Domain () Expression -- ^ Domain of the function with its attributes constrained.
functionAttributes' d@(DomainFunction _ a lb up)
  -- This will somehow need to reference domains, so it may need the full model or something,
  -- unless it takes some sort of expanded domain.
  = traceShow a d
