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

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: (MonadLog m, MonadFail m, MonadUserError m)
                    => Model -> m Model
strengthenVariables = runNameGen . resolveNames >=> core
  where core model = do
          model' <- foldM folded model [ functionAttributes
                                       ]
          if model == model'
             then return model'
             else core   model'
        folded m@Model { mStatements = stmts } f
          = return m { mStatements = map (`f` m) stmts }

-- | Make the attributes of function variables as constrictive as possible.
functionAttributes :: Statement -- ^ Statement to constrain.
                   -> Model     -- ^ Model as context.
                   -> Statement -- ^ Possibly updated statement.
functionAttributes (Declaration (FindOrGiven forg n d@DomainFunction{})) m
  = Declaration (FindOrGiven forg n (constrainFunctionDomain d m))
functionAttributes (Declaration (Letting n (Domain d@DomainFunction{}))) m
  = Declaration (Letting n (Domain (constrainFunctionDomain d m)))
functionAttributes s _ = s

-- | Constrain the domain of a function given the context of a model.
constrainFunctionDomain :: (Eq r, Pretty r)
                        => Domain r Expression  -- ^ Current domain of the function.
                        -> Model                -- ^ Context of the model.
                        -> Domain r Expression  -- ^ Possibly modified domain.
constrainFunctionDomain d@(DomainFunction r attrs from to) _
  = case attrs of
         -- If a function is surjective and its domain and codomain are equal, then it is bijective
         FunctionAttr s p JectivityAttr_Surjective ->
           if from == to
              then DomainFunction r (FunctionAttr s p JectivityAttr_Bijective) from to
              else d
         -- If a function is bijective, then it is total
         FunctionAttr s PartialityAttr_Partial j@JectivityAttr_Bijective ->
           DomainFunction r (FunctionAttr s PartialityAttr_Total j) from to
         _ -> d
constrainFunctionDomain d _ = d
