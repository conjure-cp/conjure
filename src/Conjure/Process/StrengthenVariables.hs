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
import Conjure.Compute.DomainOf ( domainOf )

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: (MonadFail m, MonadLog m, MonadUserError m)
                    => Model -> m Model
strengthenVariables = runNameGen . (resolveNames >=> core)
  where core model = foldM folded model [ functionAttributes
                                        , reduceDomains
                                        ]
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
constrainFunctionDomain :: (MonadFail m, MonadLog m)
                        => Domain () Expression     -- ^ Current domain of the function.
                        -> Model                    -- ^ Context of the model.
                        -> m (Domain () Expression) -- ^ Possibly modified domain.
constrainFunctionDomain d@(DomainFunction r attrs from to) _
  = case attrs of
         -- If a function is surjective or bijective and its domain and codomain
         -- are of equal size, then it is total and bijective
         FunctionAttr s PartialityAttr_Partial JectivityAttr_Surjective ->
           totalAndBijectiveDomainComparison s
         FunctionAttr s PartialityAttr_Partial JectivityAttr_Bijective  ->
           totalAndBijectiveDomainComparison s
         _ -> return d
    where totalAndBijectiveDomainComparison s
            = case domainSizeOf from of
                   Left  e        -> logInfo e >> return d
                   Right fromSize ->
                     case domainSizeOf to :: Either Doc Expression of
                          Left  e      -> logInfo e >> return d
                          Right toSize ->
                            if fromSize == toSize
                               then return $ DomainFunction r (FunctionAttr s PartialityAttr_Total JectivityAttr_Bijective) from to
                               else return d
constrainFunctionDomain d _ = return d

-- | Reduce the domain of a variable used in arithmetic expressions.
reduceDomains :: (MonadFail m, MonadLog m, NameGen m)
              => Model        -- ^ Model as context.
              -> Statement    -- ^ Statement to constraint.
              -> m Statement  -- ^ Possibly updated statement.
reduceDomains m (Declaration (FindOrGiven forg n d)) | isIntDomain d
  = (Declaration . FindOrGiven forg n) <$> arithmeticReduceDomain n d m
reduceDomains m (Declaration (Letting n (Domain d))) | isIntDomain d
  = (Declaration . Letting n . Domain) <$> arithmeticReduceDomain n d m
reduceDomains _ s = return s

-- | Determine whether a domain is ultimately an integer domain.
isIntDomain :: Domain () Expression -> Bool
isIntDomain (DomainIntE _)  = True
isIntDomain DomainInt{}     = True
isIntDomain (DomainReference _ (Just d)) = isIntDomain d
isIntDomain _               = False

-- | Arithmetic relation of constants or references, which combine to form a tree.
data ArithRelation = ArithConst    Constant       -- ^ Constant value.
                   | ArithRef      Expression     -- ^ Variable reference.
                   | ArithRelation ArithRelType
                                   ArithRelation
                                   ArithRelation  -- ^ Relation between relations.
                   deriving (Eq, Show)

-- | Type of parent arithmetic relation, which always has two children.
data ArithRelType = ArithEqual
                  | ArithAdd | ArithSub | ArithMul | ArithDiv
                  | ArithMin | ArithMax
                  deriving (Eq, Show)

-- | Compare an 'ArithRef' relation to a 'Name' for equality.
arithRefEq :: ArithRelation -- ^ An 'ArithRef' relation.
           -> Name          -- ^ A name.
           -> Bool          -- ^ Whether or not the name refers to the given reference.
arithRefEq (ArithRef (Reference n _)) x = n == x
arithRefEq _                          _ = False

-- | Reduce the domain of a variable by constraining it with arithmetic relations with other variables in the model.
arithmeticReduceDomain :: (MonadFail m, MonadLog m, NameGen m)
                        => Name                     -- ^ Name of the variable.
                        -> Domain () Expression     -- ^ Current domain of the function.
                        -> Model                    -- ^ Model for context.
                        -> m (Domain () Expression) -- ^ Possibly modified domain.
arithmeticReduceDomain n d m
  = findCallingExpressions n m >>=
    foldM (\d' e ->
           case exprToArithRel e >>= liftEqual n of
                Just ar -> arithRelToDom ar >>= combineDomains d'
                Nothing -> return d')
    d

-- | Find "such that" expressions in the model that reference the given variable.
findCallingExpressions :: (MonadFail m, MonadLog m)
                       => Name            -- ^ Variable name being referenced.
                       -> Model           -- ^ Model for context.
                       -> m [Expression]  -- ^ Expressions referencing the variable.
findCallingExpressions n
  = return . foldr (\e a -> a ++ execWriter (varInExp n e)) [] .
             getExpressions . head . filter isSuchThat . mStatements
    where isSuchThat (SuchThat _) = True
          isSuchThat _            = False
          getExpressions (SuchThat es) = es
          getExpressions _             = []

-- | Find the highest level expression referencing a name in an expression tree.
varInExp :: Name                            -- ^ Variable name being referenced.
         -> Expression                      -- ^ Expression at the root of the tree.
         -> Writer [Expression] Expression  -- ^ Writer containing expressions referencing the variable.
-- When a reference to the variable is found, save it to the writer
varInExp n e@(Reference x _) | x == n = tell [e]  >> return e
varInExp _ e@(Constant _)             = return e
varInExp _ e@(ExpressionMetaVar _)    = return e
-- If the writer contains an expression from a lower level, replace the expression with
-- the current one, in order to get the top-level expression containing the reference
varInExp n e                          = if null (execWriter $ descendM (varInExp n) e)
                                           then return e
                                           else tell [e] >> return e

-- | Attempt to convert an expression to an arithmetic relation.
exprToArithRel :: Expression          -- ^ Expression to convert.
               -> Maybe ArithRelation -- ^ Possible arithmetic relation representation.
exprToArithRel (Op (MkOpEq (OpEq l r)))
  = ArithRelation ArithEqual <$> exprToArithRel l <*> exprToArithRel r
exprToArithRel (Op (MkOpSum     (OpSum     (AbstractLiteral (AbsLitMatrix _ [l, r])))))
  = ArithRelation ArithAdd   <$> exprToArithRel l <*> exprToArithRel r
exprToArithRel (Op (MkOpMinus   (OpMinus   l r)))
  = ArithRelation ArithSub   <$> exprToArithRel l <*> exprToArithRel r
exprToArithRel (Op (MkOpProduct (OpProduct (AbstractLiteral (AbsLitMatrix _ [l, r])))))
  = ArithRelation ArithMul   <$> exprToArithRel l <*> exprToArithRel r
exprToArithRel (Op (MkOpDiv     (OpDiv     l r)))
  = ArithRelation ArithDiv   <$> exprToArithRel l <*> exprToArithRel r
exprToArithRel (Constant  c) = Just $ ArithConst c
exprToArithRel n@Reference{} = Just $ ArithRef n
-- No support for other types of expressions
exprToArithRel _               = Nothing

-- | Lift the 'ArithEqual' node of a relation tree to the top level,
--   with the reference to the given variable on the right side. If
--   the tree is too complex to transform, 'Nothing' is returned.
liftEqual :: Name                 -- ^ Name of the variable to appear on the right of the equality.
          -> ArithRelation        -- ^ Relation tree to transform.
          -> Maybe ArithRelation  -- ^ Possibly transformed relation tree with the variable alone on the right.
liftEqual n = varToRight >=> liftEqual'
  where liftEqual' a@(ArithRelation ArithEqual _ x@(ArithRef _)) | arithRefEq x n = Just a
        liftEqual' a@(ArithRef      _)                           | arithRefEq a n = Just a
        liftEqual' (ArithRelation ArithEqual l r@(ArithRelation _ _ r'))
          = flip (ArithRelation ArithEqual) r' <$> transferBranchRL l r >>= liftEqual' 
        liftEqual' _ = Nothing
        -- Transfer a branch from the right side of the tree to the left side.
        transferBranchRL a (ArithRef      _)            = Just a
        transferBranchRL _ (ArithRelation ArithMin _ _) = Nothing
        transferBranchRL _ (ArithRelation ArithMax _ _) = Nothing
        -- Perform the inverse of the operation to both sides of the relation
        transferBranchRL a (ArithRelation t        l _) = Just $ ArithRelation (arithRelInverse t) a l
        transferBranchRL _ _                            = Nothing
        -- Attempt to transform the tree so that the variable in question ends up on the furthest right
        varToRight a@(ArithRelation _ _ x@(ArithRef _)) | arithRefEq x n = Just a
        varToRight (ArithRelation ArithMin _ _) = Nothing
        varToRight (ArithRelation ArithMax _ _) = Nothing
        varToRight (ArithRelation t l r)
          -- Already in the right branch, so recurse and propagate 'Maybe'
          | not (branchContainsVar l) && branchContainsVar r
            = ArithRelation t l <$> varToRight r
          -- It is in the left branch, so recurse on it and flip the children at this level
          | branchContainsVar l && not (branchContainsVar r)
            = flipArithRel . flip (ArithRelation t) r <$> varToRight l
        varToRight a@(ArithRef _) | arithRefEq a n = Just a
        varToRight _ = Nothing

        branchContainsVar x@(ArithRef   _) | arithRefEq x n = True
        branchContainsVar (ArithRelation _ l r)   = branchContainsVar l || branchContainsVar r
        branchContainsVar _                       = False

-- | Flip the terms of an arithmetic relation.
flipArithRel :: ArithRelation -> ArithRelation
-- Equality, addition and multiplication are commutative
flipArithRel (ArithRelation t@ArithEqual l r)
  = ArithRelation t r l
flipArithRel (ArithRelation t@ArithAdd   l r)
  = ArithRelation t r l
flipArithRel (ArithRelation t@ArithMul   l r)
  = ArithRelation t r l
-- Flip the terms of a subtraction: a - b = -b + a
flipArithRel (ArithRelation ArithSub l r)
  = ArithRelation ArithAdd
                  (ArithRelation ArithSub
                                 (ArithConst $ ConstantInt 0)
                                 r)
                  l
-- Flip the terms of a divison: a/b = 1/b * a
flipArithRel (ArithRelation ArithDiv l r)
  = ArithRelation ArithMul
                  (ArithRelation ArithDiv
                                 (ArithConst $ ConstantInt 1)
                                 r)
                  l
-- Cannot flip other relations
flipArithRel a = a

-- | Get the inverse of an arithmetic relation type.
arithRelInverse :: ArithRelType -> ArithRelType
arithRelInverse ArithEqual = ArithEqual
arithRelInverse ArithAdd   = ArithSub
arithRelInverse ArithSub   = ArithAdd
arithRelInverse ArithMul   = ArithDiv
arithRelInverse ArithDiv   = ArithMul
arithRelInverse ArithMin   = ArithMax
arithRelInverse ArithMax   = ArithMin

-- | Reduce an arithmetic relation onto the domain of the variable of interest.
arithRelToDom :: (MonadFail m, MonadLog m, NameGen m)
              => ArithRelation -> m (Domain () Expression)
arithRelToDom (ArithRef   x) = domainOf x
arithRelToDom (ArithConst x) = domainOf x
-- FIXME: used only for testing purposes
arithRelToDom _              = return $ DomainInt [RangeBounded (Constant (ConstantInt 0))
                                                                (Constant (ConstantInt 5))]

-- | Combine two domains into a third which encompasses both.
combineDomains :: (MonadFail m, MonadLog m)
               => Domain () Expression -> Domain () Expression -> m (Domain () Expression)
-- RangeBounded - combine bounds with min and max
combineDomains (DomainInt [RangeBounded lb1 ub1])
               (DomainInt [RangeBounded lb2 ub2])
  = return $ DomainInt [RangeBounded (Op (MkOpMax (OpMax $ mkMatrix [lb1, lb2])))
                                     (Op (MkOpMin (OpMin $ mkMatrix [ub1, ub2])))]
-- RangeLowerBounded - combine lower bounds and set upper bound if present
combineDomains (DomainInt [RangeBounded lb1 ub])
               (DomainInt [RangeLowerBounded lb2])
  = return $ DomainInt [RangeBounded (Op (MkOpMax (OpMax $ mkMatrix [lb1, lb2]))) ub]
combineDomains d1@(DomainInt [RangeLowerBounded _])
               d2@(DomainInt [RangeBounded{}])
  = combineDomains d2 d1
combineDomains (DomainInt [RangeLowerBounded lb1])
               (DomainInt [RangeLowerBounded lb2])
  = return $ DomainInt [RangeLowerBounded (Op (MkOpMax (OpMax $ mkMatrix [lb1, lb2])))]
-- RangeUpperBounded - combine upper bounds and set lower bound if present
combineDomains (DomainInt [RangeBounded lb ub1])
               (DomainInt [RangeUpperBounded ub2])
  = return $ DomainInt [RangeBounded lb (Op (MkOpMin (OpMin $ mkMatrix [ub1, ub2])))]
combineDomains d1@(DomainInt [RangeUpperBounded _])
               d2@(DomainInt [RangeBounded{}])
  = combineDomains d2 d1
combineDomains (DomainInt [RangeUpperBounded ub1])
               (DomainInt [RangeUpperBounded ub2])
  = return $ DomainInt [RangeUpperBounded (Op (MkOpMin (OpMin $ mkMatrix [ub1, ub2])))]
-- RangeOpen - use other domain
combineDomains d1 (DomainInt [RangeOpen]) = return d1
combineDomains (DomainInt [RangeOpen]) d2 = return d2
-- RangeSingle - ignore other domain
combineDomains d@(DomainInt [RangeSingle{}]) _ = return d
combineDomains _ d@(DomainInt [RangeSingle{}]) = return d
-- Cannot combine (for now)
combineDomains d1 _ = return d1

-- | Make a matrix expression from a list of expressions.
mkMatrix :: [Expression] -> Expression
mkMatrix es = AbstractLiteral
                (AbsLitMatrix
                  (DomainInt [RangeBounded (Constant (ConstantInt 1))
                                           (Constant (ConstantInt $ toInteger $ length es))])
                  es)
