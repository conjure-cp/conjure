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

import Data.List ( union )

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )
-- These two are needed together
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.DomainSizeOf ( domainSizeOf )

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: (MonadFail m, MonadLog m, MonadUserError m)
                    => Model -> m Model
strengthenVariables = runNameGen . (resolveNames >=> core)
  where core model = do model' <- foldM folded model [ functionAttributes
                                                     ]
                        if model == model'
                           then return model'
                           else core model'
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
  d' <- constrainFunctionDomain n d m
  return $ Declaration (FindOrGiven forg n d')
functionAttributes m (Declaration (Letting n (Domain d@DomainFunction{}))) = do
  d' <- constrainFunctionDomain n d m
  return $ Declaration (Letting n (Domain d'))
functionAttributes _ s = return s

-- | Constrain the domain of a function given the context of a model.
constrainFunctionDomain :: (MonadFail m, MonadLog m)
                        => Name                     -- ^ Name of the function.
                        -> Domain () Expression     -- ^ Current domain of the function.
                        -> Model                    -- ^ Context of the model.
                        -> m (Domain () Expression) -- ^ Possibly modified domain.
constrainFunctionDomain n d@DomainFunction{} m
  = surjectiveIsTotalBijective d >>=
    definedForAllIsTotal n (suchThat m)
constrainFunctionDomain _ d _ = return d

-- | Extract the such that expressions from a model.
suchThat :: Model -> [Expression]
suchThat = foldr fromSuchThat [] . mStatements
  where fromSuchThat (SuchThat es) a = a `union` es
        fromSuchThat _             a = a

-- | If a function is surjective or bijective and its domain and codomain
--   are of equal size, then it is total and bijective.
surjectiveIsTotalBijective :: (MonadFail m, MonadLog m)
                           => Domain () Expression      -- ^ Domain of the function.
                           -> m (Domain () Expression)  -- ^ Possibly modified domain.
surjectiveIsTotalBijective d@(DomainFunction r (FunctionAttr s PartialityAttr_Partial j) from to)
  | j == JectivityAttr_Surjective || j == JectivityAttr_Bijective = do
    (fromSize, toSize) <- functionDomainSizes from to
    if fromSize == toSize
       then return $ DomainFunction r (FunctionAttr s PartialityAttr_Total JectivityAttr_Bijective) from to
       else return d
surjectiveIsTotalBijective d = return d

-- | Calculate the sizes of the domain and codomain of a function.
functionDomainSizes :: (MonadFail m)
                    => Domain () Expression       -- ^ The function's domain.
                    -> Domain () Expression       -- ^ The function's codomain.
                    -> m (Expression, Expression) -- ^ The sizes of the two.
functionDomainSizes from to = do
  fromSize <- domainSizeOf from
  toSize   <- domainSizeOf to
  return (fromSize, toSize)

-- | If a function is defined for all values in its domain, then it is total.
definedForAllIsTotal :: (MonadFail m, MonadLog m)
                     => Name                      -- ^ Name of the function.
                     -> [Expression]              -- ^ Such that constraints.
                     -> Domain () Expression      -- ^ Domain of the function.
                     -> m (Domain () Expression)  -- ^ Possibly modified domain.
definedForAllIsTotal n cs (DomainFunction r (FunctionAttr s PartialityAttr_Partial j) from to)
  | any definedForAll cs
    = return $ DomainFunction r (FunctionAttr s PartialityAttr_Total j) from to
  where
        definedForAll (Op (MkOpAnd (OpAnd (Comprehension e gorcs))))
          = e `containsReference` n &&
            gorcs `containsGenerator` from &&
            hasNoConditions gorcs
        definedForAll _ = False
        hasNoConditions = not . any isCondition
        isCondition Condition{} = True
        isCondition _           = False
definedForAllIsTotal _ _ d = return d

-- | Determine whether an expression contains a reference to a variable.
containsReference :: Expression -> Name -> Bool
containsReference e n = any nameEqual $ universe e
  where nameEqual (Reference n' _) = n == n'
        nameEqual _                = False

-- | Determine whether a list of generators or conditions contains a generator for a given domain.
containsGenerator :: [GeneratorOrCondition] -> Domain () Expression -> Bool
containsGenerator gorcs d = any containsGenerator' gorcs
  where containsGenerator' (Generator (GenDomainNoRepr  _ d')) = d == d'
        containsGenerator' _ = False
