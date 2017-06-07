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

data Modified a = Modified a
                | UnModified a
                deriving (Eq, Show)

-- | Force a 'Modified' to be Modified.
forceModified :: Modified a -> Modified a
forceModified (UnModified x) = Modified x
forceModified x = x

-- | Extract the element from a 'Modified'.
stripModified :: Modified a -> a
stripModified (Modified x) = x
stripModified (UnModified x) = x

-- | Count the number of modified items in a list.
numModified :: [Modified a] -- ^ List of possibly modified items.
            -> Int          -- ^ Number of modified items.
numModified = sum . map (\x -> case x of
                                    Modified _   -> 1
                                    UnModified _ -> 0)

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: (MonadLog m, MonadFail m, MonadUserError m)
                    => Model    -- ^ Model to have its variables strengthen.ed
                    -> m Model  -- ^ Model with its variables strengthened.
strengthenVariables = runNameGen . resolveNames >=> return . core
  -- Perform a pass in which all variable strengthening functions are applied
  where core model = case foldr ($) (UnModified model) [ functionAttributes
                                                       ] of
                          -- Recurse if any changes were made
                          Modified model'   -> core model'
                          UnModified model' -> model'

-- | Make the attributes of function variables as constrictive as possible.
functionAttributes :: Modified Model  -- ^ Model to constrain.
                   -> Modified Model  -- ^ Possibly updated model, marked as changed or not.
functionAttributes m = case m of
                            Modified model   -> forceModified $ core model
                            UnModified model -> core model
  where core model@Model { mStatements = stmts }
          = let stmts' = map (`functionDecls` model) stmts
                -- Determine whether any statements were modified
                modified = if numModified stmts' == 0
                              then UnModified
                              else Modified
                in modified model { mStatements = map stripModified stmts' }
        -- Constrain function domains only on function declarations
        functionDecls decl@(Declaration (FindOrGiven _ _ d@DomainFunction{}))
          = extractModifiedToStmt decl . constrainFunctionDomain d
        functionDecls decl@(Declaration (Letting _ (Domain d@DomainFunction{})))
          = extractModifiedToStmt decl . constrainFunctionDomain d
        functionDecls s = \_ -> UnModified s
        -- Extract the Modified flag of a domain to its statement
        extractModifiedToStmt (Declaration (FindOrGiven forg n _)) (Modified d')
          = Modified (Declaration (FindOrGiven forg n d'))
        extractModifiedToStmt (Declaration (Letting n _)) (Modified d')
          = Modified (Declaration (Letting n (Domain d')))
        extractModifiedToStmt d _
          = UnModified d

-- | Constrain the domain of a function given the context of a model.
constrainFunctionDomain :: Domain () Expression             -- ^ Current domain of the function.
                        -> Model                            -- ^ Context of the model.
                        -> Modified (Domain () Expression)  -- ^ Possibly modified domain.
constrainFunctionDomain d@(DomainFunction r attrs from to) _
  = case attrs of
         FunctionAttr s _ JectivityAttr_Surjective ->
           if from == to
              then Modified (DomainFunction r (FunctionAttr s PartialityAttr_Total JectivityAttr_Bijective) from to)
              else UnModified d
         _ -> UnModified d
constrainFunctionDomain d _ = UnModified d
