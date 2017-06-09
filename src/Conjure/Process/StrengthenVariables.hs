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
-- The following two imports are required together
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.DomainSizeOf ( domainSizeOf )
import Conjure.Language.NameResolution ( resolveNames )

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: (MonadFail m, MonadLog m, MonadUserError m)
                    => Model -> m Model
strengthenVariables = runNameGen . resolveNames >=> core
  where core model = do
          model' <- foldM folded model [ functionAttributes
                                       ]
          if model == model'
             then return model'
             else core   model'
        -- Apply the function to every statement and fold it into the model
        folded m@Model { mStatements = stmts } f
          = foldM (\m' s -> do
                    s' <- f m s
                    -- This is map, but unwraps the value from the monad
                    return m' { mStatements = s' : mStatements m' })
                  m { mStatements = [] }
                  (reverse stmts)

-- | Make the attributes of function variables as constrictive as possible.
functionAttributes :: (MonadFail m, MonadLog m)
                   => Model       -- ^ Model as context.
                   -> Statement   -- ^ Statement to constrain.
                   -> m Statement -- ^ Possibly updated statement.
functionAttributes m (Declaration (FindOrGiven forg n d@DomainFunction{})) = do
  d' <- constrainFunctionDomain d m
  return $ Declaration (FindOrGiven forg n d')
functionAttributes m (Declaration (Letting n (Domain d@DomainFunction{}))) = do
  d' <- constrainFunctionDomain d m
  return $ Declaration (Letting n (Domain d'))
functionAttributes _ s = return s

-- | Constrain the domain of a function given the context of a model.
constrainFunctionDomain :: (MonadFail m, MonadLog m, Default r, Eq r, Pretty r)
                        => Domain r Expression      -- ^ Current domain of the function.
                        -> Model                    -- ^ Context of the model.
                        -> m (Domain r Expression)  -- ^ Possibly modified domain.
constrainFunctionDomain d@(DomainFunction r attrs from to) _
  = case attrs of
         -- If a function is surjective and its domain and codomain are equal, then it is total and bijective
         FunctionAttr s _ JectivityAttr_Surjective ->
           case domainSizeOf from of
                Left  e        -> logInfo e >> return d
                Right fromSize ->
                  case domainSizeOf to :: Either Doc Expression of
                       Left  e      -> logInfo e >> return d
                       Right toSize ->
                         if fromSize == toSize
                            then return $ DomainFunction r (FunctionAttr s PartialityAttr_Total JectivityAttr_Bijective) from to
                            else return d
         -- If a function is injective or bijective, then it is total
         FunctionAttr s PartialityAttr_Partial j@JectivityAttr_Injective ->
           return $ totalDomainFunction s j
         FunctionAttr s PartialityAttr_Partial j@JectivityAttr_Bijective ->
           return $ totalDomainFunction s j
         _ -> return d
    where totalDomainFunction s j = DomainFunction r (FunctionAttr s PartialityAttr_Total j) from to
constrainFunctionDomain d _ = return d
