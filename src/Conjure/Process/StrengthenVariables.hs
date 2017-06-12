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
import Control.Monad.Writer

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
                                       , reduceDomains
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

-- | Reduce the domain of a variable used in arithmetic expressions.
reduceDomains :: (MonadFail m, MonadLog m)
              => Model        -- ^ Model as context.
              -> Statement    -- ^ Statement to constraint.
              -> m Statement  -- ^ Possibly updated statement.
reduceDomains m (Declaration (FindOrGiven forg n d)) | isIntDomain d = do
  d' <- arithmeticReduceDomain n d m
  return $ Declaration (FindOrGiven forg n d')
reduceDomains m (Declaration (Letting n (Domain d))) | isIntDomain d = do
  d' <- arithmeticReduceDomain n d m
  return $ Declaration (Letting n (Domain d'))
reduceDomains _ s = return s

-- | Determine whether a domain is ultimately an integer domain.
isIntDomain :: Domain () Expression -> Bool
isIntDomain (DomainIntE _)  = True
isIntDomain DomainInt{}     = True
isIntDomain (DomainReference _ (Just d)) = isIntDomain d
isIntDomain _               = False

-- | Arithmetic relation between constants or references, which can be nested.
data ArithRelation = ArithEqual ArithRelation ArithRelation -- ^ Equality of relations.
                   | ArithAdd   ArithRelation ArithRelation -- ^ Addition of relations.
                   | ArithSub   ArithRelation ArithRelation -- ^ Subtraction of relations.
                   | ArithMul   ArithRelation ArithRelation -- ^ Multiplication of relations.
                   | ArithDiv   ArithRelation ArithRelation -- ^ Division of relations.
                   | ArithMax   ArithRelation ArithRelation -- ^ Maximum of relations.
                   | ArithMin   ArithRelation ArithRelation -- ^ Minimum of relations.
                   | ArithConst Constant                    -- ^ Constant value.
                   | ArithRef   Name                        -- ^ Reference to a value.
                   deriving (Eq, Show)

-- | Reduce the domain of a variable by constraining it with arithmetic relations with other variables in the model.
arithmeticReduceDomain :: (MonadFail m, MonadLog m, Default r, Eq r, Pretty r)
                        => Name                     -- ^ Name of the variable.
                        -> Domain r Expression      -- ^ Current domain of the function.
                        -> Model                    -- ^ Model for context.
                        -> m (Domain r Expression)  -- ^ Possibly modified domain.
arithmeticReduceDomain n d m = do
  es <- findCallingExpressions n m
  logInfo (stringToDoc $ show n ++ " : " ++ show es)
  logInfo (stringToDoc $ show n ++ " :\n\t" ++ concatMap (show . exprToArithRel) es)
  return d

-- | Find "such that" expressions in the model that reference the given variable.
findCallingExpressions :: (MonadFail m, MonadLog m)
                       => Name            -- ^ Variable name being referenced.
                       -> Model           -- ^ Model for context.
                       -> m [Expression]  -- ^ Expressions referencing the variable.
findCallingExpressions n m
  = let es = getExpressions $ head $ filter isSuchThat $ mStatements m
        in return $ foldr (\e a -> a ++ execWriter (varInExp n e)) [] es
    where isSuchThat (SuchThat _) = True
          isSuchThat _            = False
          getExpressions (SuchThat es) = es
          getExpressions _             = []

-- | Find the highest level expression referencing a name in an expression tree.
varInExp :: Name                            -- ^ Variable name being referenced.
         -> Expression                      -- ^ Expression at the root of the tree.
         -> Writer [Expression] Expression  -- ^ Writer containing expressions referencing the variable.
varInExp n e@(Reference x _) | n == x = tell [e]  >> return e
varInExp _ e@(Constant _)             = return e
varInExp _ e@(ExpressionMetaVar _)    = return e
varInExp n e                          = if null (execWriter $ descendM (varInExp n) e)
                                           then return e
                                           else tell [e] >> return e

-- | Attempt to convert an expression to an arithmetic relation.
exprToArithRel :: Expression          -- ^ Expression to convert.
               -> Maybe ArithRelation -- ^ Possible arithmetic relation representation.
exprToArithRel (Op (MkOpEq (OpEq l r)))
  = ArithEqual <$> exprToArithRel l <*> exprToArithRel r
exprToArithRel (Op (MkOpSum     (OpSum     (AbstractLiteral (AbsLitMatrix _ [l, r])))))
  = ArithAdd   <$> exprToArithRel l <*> exprToArithRel r
exprToArithRel (Op (MkOpMinus   (OpMinus   l r)))
  = ArithSub   <$> exprToArithRel l <*> exprToArithRel r
exprToArithRel (Op (MkOpProduct (OpProduct (AbstractLiteral (AbsLitMatrix _ [l, r])))))
  = ArithMul   <$> exprToArithRel l <*> exprToArithRel r
exprToArithRel (Op (MkOpDiv     (OpDiv     l r)))
  = ArithDiv   <$> exprToArithRel l <*> exprToArithRel r
exprToArithRel (Constant  c)   = Just $ ArithConst c
exprToArithRel (Reference n _) = Just $ ArithRef n
exprToArithRel _               = Nothing
