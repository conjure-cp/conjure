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

import Control.Monad

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )

-- | Mark whether or not the contained value has been modified by an operation.
--   Modifications propagate.
data Modified a = Modified   a  -- ^ The value has been modified by an operation.
                | UnModified a  -- ^ The value has not been modified by an operation.
                deriving (Eq, Show)

-- | Force a 'Modified' to be Modified.
forceModified :: Modified a -> Modified a
forceModified (UnModified x) = Modified x
forceModified x = x

-- | Extract the element from a 'Modified'.
stripModified :: Modified a -> a
stripModified (Modified x)   = x
stripModified (UnModified x) = x

-- | Count the number of modified items in a list.
countModified :: [Modified a] -- ^ List of possibly modified items.
            -> Int          -- ^ Number of modified items.
countModified = sum . map countModified'
  where countModified' (Modified   _) = 1
        countModified' (UnModified _) = 0

-- | Use a 'Modified' value in a function and propagate the 'Modified' result.
--   If the value was already modified but is unmodified by the function, it remains
--   marked as modified. If it was not already modified, then it is only marked
--   as modified if the function does so.
useModified :: (a -> Modified a)  -- ^ Function for a value which determines whether it is modified.
            -> Modified a         -- ^ Value that may already have been modified.
            -> Modified a         -- ^ Result of the function on the value, with modification propagated.
useModified f (Modified   x) = forceModified (f x)
useModified f (UnModified x) = f x

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: (MonadLog m, MonadFail m, MonadUserError m)
                    => Model -> m Model
strengthenVariables = runNameGen . resolveNames >=> return . core
  -- Perform a pass in which all variable strengthening functions are applied.
  where core model = case foldr folded (UnModified model) [ functionAttributes
                                                          ] of
                          -- Recurse if any changes were made
                          Modified model'   -> core model'
                          UnModified model' -> model'
        -- Wrapper to call the passed function on each statement,
        -- passing the model along with it and tracking modification.
        folded f = useModified (\m@Model { mStatements = stmts } ->
                                let stmts' = map (`f` m) stmts
                                    modified = if countModified stmts' == 0
                                                  then UnModified
                                                  else Modified
                                    in modified m { mStatements = map stripModified stmts' })

-- | Extract the Modified flag of a domain to its statement.
extractModifiedToStmt :: Statement                        -- ^ Statement containing the domain.
                      -> Modified (Domain () Expression)  -- ^ Domain that may have been modified.
                      -> Modified Statement               -- ^ Statement possibly marked as modified
                                                          --   with the updated domain.
extractModifiedToStmt (Declaration (FindOrGiven forg n _)) (Modified d')
  = Modified (Declaration (FindOrGiven forg n d'))
extractModifiedToStmt (Declaration (Letting n _)) (Modified d')
  = Modified (Declaration (Letting n (Domain d')))
extractModifiedToStmt d _
  = UnModified d

-- | Make the attributes of function variables as constrictive as possible.
functionAttributes :: Statement           -- ^ Statement to constrain.
                   -> Model               -- ^ Model as context.
                   -> Modified Statement  -- ^ Possibly updated statement, marked as changed or not.
functionAttributes decl@(Declaration (FindOrGiven _ _ d@DomainFunction{}))
  = extractModifiedToStmt decl . constrainFunctionDomain d
functionAttributes decl@(Declaration (Letting _ (Domain d@DomainFunction{})))
  = extractModifiedToStmt decl . constrainFunctionDomain d
functionAttributes s = \_ -> UnModified s

-- | Constrain the domain of a function given the context of a model.
constrainFunctionDomain :: Domain () Expression             -- ^ Current domain of the function.
                        -> Model                            -- ^ Context of the model.
                        -> Modified (Domain () Expression)  -- ^ Possibly modified domain.
constrainFunctionDomain d@(DomainFunction r attrs from to) _
  = case attrs of
         -- If a function is surjective and its domain and codomain are equal, then it is bijective
         FunctionAttr s p JectivityAttr_Surjective ->
           if from == to
              then Modified (DomainFunction r (FunctionAttr s p JectivityAttr_Bijective) from to)
              else UnModified d
         -- If a function is bijective, then it is total
         FunctionAttr s PartialityAttr_Partial j@JectivityAttr_Bijective ->
           Modified (DomainFunction r (FunctionAttr s PartialityAttr_Total j) from to)
         _ -> UnModified d
constrainFunctionDomain d _ = UnModified d
