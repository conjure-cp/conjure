{-
 - Module      : Conjure.Process.StrengthenVariables
 - Description : Strengthen variables using type- and domain-inference.
 - Copyright   : Billy Brown 2017
 - License     : BSD3
 
 Processing step that attempts to strengthen variables at the Essence class level, using methods described in the "Reformulating Essence Specifications for Robustness" paper.
-}

{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.StrengthenVariables
  (
    strengthenVariables
  ) where

import Data.List ( delete, nub, union )
import Data.Maybe ( mapMaybe )

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.Domain.AddAttributes
import Conjure.Language.NameResolution ( resolveNames )
-- These two are needed together
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.DomainSizeOf ( domainSizeOf )
import Conjure.Compute.DomainOf ( domainOf )

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: (MonadFail m, MonadLog m, MonadUserError m)
                    => Model -> m Model
strengthenVariables = runNameGen . (resolveNames >=> core)
  where core :: (MonadFail m, MonadLog m, MonadUserError m, NameGen m) => Model -> m Model
        core model = do model' <- foldM folder model [ functionAttributes
                                                     , setAttributes
                                                     , setConstraints
                                                     , variableSize
                                                     ]
                        if model == model'
                           then return model'
                           else resolveNames model' >>= core
        -- Apply the function to every statement and fold it into the model
        folder :: (MonadFail m, MonadLog m)
               => Model -> (Model -> Declaration -> m Model) -> m Model
        folder m@Model { mStatements = stmts } f = foldM (applyToDecl f) m stmts
        -- Apply the function only to declarations
        applyToDecl f m (Declaration d) = f m d
        applyToDecl _ m _               = return m

-- | Update a declaration in a model.
updateDeclaration :: Declaration  -- ^ Old declaration to be removed.
                  -> Declaration  -- ^ New declaration to be inserted.
                  -> Model        -- ^ Model to be updated.
                  -> Model        -- ^ Updated model.
updateDeclaration d d' m@Model { mStatements = stmts }
  | d == d'   = m
  | otherwise = m { mStatements = Declaration d' : delete (Declaration d) stmts }

-- | Merge a list of constraints into a model.
mergeConstraints :: Model         -- ^ Model to be updated.
                 -> [Expression]  -- ^ Constraints to merge into the model.
                 -> Model         -- ^ Updated model with new constraints.
mergeConstraints m@Model { mStatements = stmts } cs
  = m { mStatements = map mergeConstraints' stmts }
  where
    mergeConstraints' (SuchThat cs') = SuchThat $ cs' `union` cs
    mergeConstraints' s              = s

-- | Remove a list of constraints from a model.
removeConstraints :: Model        -- ^ Model to have the constraint removed.
                  -> [Expression] -- ^ Constraints to remove.
                  -> Model        -- ^ Updated model with constraints removed.
removeConstraints m@Model { mStatements = stmts } cs
  = m { mStatements = map removeConstraints' stmts }
  where
    removeConstraints' (SuchThat cs') = SuchThat $ filter (`notElem` cs) cs'
    removeConstraints' s              = s

-- | Make the attributes of function variables as constrictive as possible.
functionAttributes :: (MonadFail m, MonadLog m)
                   => Model       -- ^ Model as context.
                   -> Declaration -- ^ Statement to constrain.
                   -> m Model     -- ^ Possibly updated model.
functionAttributes m f@(FindOrGiven forg n d@DomainFunction{}) = do
  d' <- constrainFunctionDomain n d m
  let f' = FindOrGiven forg n d'
  return $ updateDeclaration f f' m
functionAttributes m _ = return m

-- | Constrain the domain of a function given the context of a model.
constrainFunctionDomain :: (MonadFail m, MonadLog m)
                        => Name                     -- ^ Name of the function.
                        -> Domain () Expression     -- ^ Current domain of the function.
                        -> Model                    -- ^ Context of the model.
                        -> m (Domain () Expression) -- ^ Possibly modified domain.
constrainFunctionDomain n d@DomainFunction{} m
  = surjectiveIsTotalBijective d  >>=
    totalInjectiveIsBijective     >>=
    definedForAllIsTotal n m
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
surjectiveIsTotalBijective d@(DomainFunction _ (FunctionAttr _ PartialityAttr_Partial j) from to)
  | j == JectivityAttr_Surjective || j == JectivityAttr_Bijective = do
    (fromSize, toSize) <- functionDomainSizes from to
    if fromSize == toSize
       then addAttributesToDomain d [ ("total", Nothing), ("bijective", Nothing) ]
       else return d
surjectiveIsTotalBijective d = return d

-- | If a function is total and injective, and its domain and codomain
--   are of equal size, then it is bijective.
totalInjectiveIsBijective :: (MonadFail m, MonadLog m)
                          => Domain () Expression
                          -> m (Domain () Expression)
totalInjectiveIsBijective d@(DomainFunction _ (FunctionAttr _ PartialityAttr_Total JectivityAttr_Injective) from to) = do
  (fromSize, toSize) <- functionDomainSizes from to
  if fromSize == toSize
     then addAttributesToDomain d [ ("bijective", Nothing) ]
     else return d
totalInjectiveIsBijective d = return d

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
                     -> Model                     -- ^ Model for context.
                     -> Domain () Expression      -- ^ Domain of the function.
                     -> m (Domain () Expression)  -- ^ Possibly modified domain.
definedForAllIsTotal n m d@(DomainFunction _ (FunctionAttr _ PartialityAttr_Partial _) from _)
  | any definedIn $ findInUncondForAll isOp m
    = addAttributesToDomain d [ ("total", Nothing) ]
  where
    -- Look for operator expressions but leave comprehensions up to findInUncondForAll
    isOp (Op (MkOpAnd (OpAnd Comprehension{}))) = False
    isOp Op{} = True
    isOp _    = False
    -- Is the function called with parameters generated from its domain in an expression?
    definedIn e = any (funcCalledWithGenParams n from) (universe e) &&
                  not (hasImpliesOrOrs e)
    -- Implies or Or may ignore a function call, making it undefined for the domain value
    hasImpliesOrOrs = any isImplyOrOr . universe
    isImplyOrOr [essence| &_ -> &_ |] = True
    isImplyOrOr [essence| &_ \/ &_ |] = True
    isImplyOrOr _                     = False
definedForAllIsTotal _ _ d = return d

-- | Determine whether a function is called with values generated from its domain.
funcCalledWithGenParams :: Name                 -- ^ Name of the function being called.
                        -> Domain () Expression -- ^ Function domain.
                        -> Expression           -- ^ Expression which may be the function call.
                        -> Bool
funcCalledWithGenParams n d (Op (MkOpRelationProj (OpRelationProj (Reference n' _) ps)))
  = n' == n && all genArgMatchesDom ps
    where
      -- Is a function argument generated from its domain?
      genArgMatchesDom (Just (Reference _ (Just refDom)))
        = case refDom of
               DeclNoRepr Quantified _ d' _           -> d == d'
               InComprehension (GenDomainNoRepr _ d') -> d == d'
               _                                      -> False
      genArgMatchesDom _ = False
funcCalledWithGenParams _ _ _ = False

-- | Make the attributes of a set as constrictive as possible.
setAttributes :: (MonadFail m, MonadLog m, NameGen m)
              => Model        -- ^ Model as context.
              -> Declaration  -- ^ Statement to constrain.
              -> m Model      -- ^ Possibly updated model.
setAttributes m f@(FindOrGiven forg n d@DomainSet{}) = do
  d' <- setSizeFromConstraint n d m
  let f' = FindOrGiven forg n d'
  return $ updateDeclaration f f' m
setAttributes m _ = return m

-- | Constrain the size of a set from constraints on it.
setSizeFromConstraint :: (MonadFail m, MonadLog m, NameGen m)
                      => Name                     -- ^ Name of the set being worked on.
                      -> Domain () Expression     -- ^ Domain of the set to possibly modify.
                      -> Model                    -- ^ Model for context.
                      -> m (Domain () Expression) -- ^ Possibly modified set domain.
setSizeFromConstraint n d = foldM subsetMinSize d . findInUncondForAll isSubsetOf
  where
    subsetMinSize d' [essence| &l subset   &_ |] = minSizeFromFunction d' l
    subsetMinSize d' [essence| &l subsetEq &_ |] = minSizeFromFunction d' l
    subsetMinSize d' _ = return d'

    isSubsetOf (Op (MkOpSubset   (OpSubset   _ (Reference n' _)))) = n == n'
    isSubsetOf (Op (MkOpSubsetEq (OpSubsetEq _ (Reference n' _)))) = n == n'
    isSubsetOf _ = False

-- | Find an expression at any depth of unconditional forAll expressions.
findInUncondForAll :: (Expression -> Bool) -> Model -> [Expression]
findInUncondForAll p = concatMap findInForAll . suchThat
  where
    findInForAll e | p e = [e]
    findInForAll (Op (MkOpAnd (OpAnd (Comprehension e gorcs))))
                   | all (not . isCondition) gorcs = findInForAll e
    findInForAll [essence| &x /\ &y |]
                   = findInForAll x `union` findInForAll y
    findInForAll _ = []

    isCondition Condition{} = True
    isCondition _           = False

-- | Set the minimum size of a set based on it being a superset of the range of a total function
minSizeFromFunction :: (MonadFail m, MonadLog m, NameGen m)
                    => Domain () Expression     -- ^ Domain of the set for which to change to minimum size.
                    -> Expression               -- ^ Expression from which the minimum size is being inferred.
                    -> m (Domain () Expression) -- ^ Set domain with a possible change of its minimum size.
minSizeFromFunction d [essence| range(&r) |] = do
  f <- getFunDom r
  case f of
       DomainFunction _ (FunctionAttr _ PartialityAttr_Total _) _ _
         -> return $ setSetMinSize (Constant $ ConstantInt 1) d
       _ -> return d
minSizeFromFunction d _ = return d

-- | Look for a function domain, allowing it to be generated in a comprehension.
getFunDom :: (MonadFail m, MonadLog m, NameGen m)
          => Expression
          -> m (Domain () Expression)
getFunDom (Reference _ (Just (DeclNoRepr _ _ d@DomainFunction{} _))) = return d
getFunDom (Reference _ (Just (InComprehension (GenInExpr _ e))))     = getFunDom e
getFunDom (Reference _ (Just (DeclNoRepr _ _ d@DomainSet{}  _)))     = return $ domFromContainer d
getFunDom (Reference _ (Just (DeclNoRepr _ _ d@DomainMSet{} _)))     = return $ domFromContainer d
getFunDom e = domainOf e

-- | Extract a "leaf" domain from a container domain.
domFromContainer :: Domain () Expression -> Domain () Expression
domFromContainer (DomainSet  _ _ d') = d'
domFromContainer (DomainMSet _ _ d') = d'
domFromContainer d'                  = d'

-- | Set the minSize attribute of a set.
setSetMinSize :: Expression           -- ^ New minimum size to apply.
              -> Domain () Expression -- ^ Set domain to modify.
              -> Domain () Expression -- ^ Possibly modified set domain.
setSetMinSize n (DomainSet r (SetAttr s) d) = DomainSet r (minSetSize s) d
  where minSetSize :: SizeAttr Expression -> SetAttr Expression
        minSetSize SizeAttr_None                   = SetAttr $ SizeAttr_MinSize n
        minSetSize (SizeAttr_MinSize    s')        = SetAttr $ SizeAttr_MinSize $ mkMax n s'
        minSetSize (SizeAttr_MaxSize    s')        = SetAttr $ SizeAttr_MinMaxSize n s'
        minSetSize (SizeAttr_MinMaxSize minS maxS) = SetAttr $ SizeAttr_MinMaxSize (mkMax n minS) maxS
        minSetSize a                               = SetAttr a
setSetMinSize _ d = d

-- | Make a maximum expression between two expressions.
--   Two max expressions are merged into one.
--   The max between a value and a max adds the value to the max (if not present).
--   If the expressions are the same, no max is made and the value is returned.
mkMax :: Expression -> Expression -> Expression
mkMax (Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es1)))))
      (Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es2)))))
        = make opMax $ fromList $ es1 `union` es2
mkMax i m@(Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es)))))
          | i `elem` es = m
          | otherwise   = make opMax $ fromList $ i : es
mkMax m@(Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es))))) i
          | i `elem` es = m
          | otherwise   = make opMax $ fromList $ i : es
mkMax i e | i == e      = e
          | otherwise   = make opMax $ fromList [ i, e ]

-- | Add constraints on a set.
setConstraints :: (MonadFail m, MonadLog m)
               => Model        -- ^ Model as context.
               -> Declaration  -- ^ Statement to constrain.
               -> m Model      -- ^ Possibly updated model.
setConstraints m (FindOrGiven _ n DomainSet{}) = do
  cs <- funcRangeEqSet n m
  return $ mergeConstraints m cs
setConstraints m _ = return m

-- | Equate the range of a function to a set of the former is a subset of the latter
--   and all values in the set are results of the function.
funcRangeEqSet :: (MonadFail m, MonadLog m)
               => Name            -- ^ Name of the set.
               -> Model           -- ^ Model for context.
               -> m [Expression]  -- ^ Equality constraints between range and set.
funcRangeEqSet n m = return $ mapMaybe equateFuncRangeAndSet $
                     mapMaybe (forAllForSubset m) $
                     findInUncondForAll isSubsetEqOf m
  where
    isSubsetEqOf (Op (MkOpSubsetEq (OpSubsetEq _ (Reference n' _)))) = n == n'
    isSubsetEqOf _ = False

-- | Create an expression that equates the range of a function which may come from
--   a forAll, and a set.
equateFuncRangeAndSet :: (Expression, Expression) -- (function range, set)
                      -> Maybe Expression         -- Possible constraint equating the two.
equateFuncRangeAndSet (r@[essence| range(&f) |], s) = nestReference f (make opEq r s)
  where
    nestReference :: Expression -> Expression -> Maybe Expression
    nestReference (Reference _ (Just DeclNoRepr{})) e = Just e
    nestReference (Reference _ (Just (InComprehension g@(GenInExpr _ r')))) e
      = nestReference r' $ make opAnd $ Comprehension e [Generator g]
    nestReference _ _ = Nothing
equateFuncRangeAndSet _ = Nothing

-- | Find a forAll expressions generating from a variable and equating a function
--   call result to the generated values.
forAllForSubset :: Model                          -- ^ Model for context.
                -> Expression                     -- ^ Subset expression.
                -> Maybe (Expression, Expression) -- ^ Values to be equated.
forAllForSubset m (Op (MkOpSubsetEq (OpSubsetEq r@(Op (MkOpRange (OpRange f)))
                                                s@Reference{})))
  | not $ null $ findInUncondForAll (funcCallEqGenerated f s) m = Just (r, s)
forAllForSubset _ _ = Nothing

-- | Determine whether a function is a called and has its result equated to a value generated
--   from the set of interest.
funcCallEqGenerated :: Expression -- ^ Function reference.
                    -> Expression -- ^ Variable to generate from.
                    -> Expression -- ^ Expression to check.
                    -> Bool       -- ^ Does the equation have the desired terms.
funcCallEqGenerated f s [essence| &e = &x |]
  = isFuncCall e && isGenerated x
  where
    -- Is the left side a call to the function of interest?
    isFuncCall (Op (MkOpRelationProj (OpRelationProj f' _))) = f == f'
    isFuncCall _ = False
    -- Is the right side variable generated from the set of interest?
    isGenerated (Reference _ (Just (InComprehension (GenInExpr _ g)))) = s == g
    isGenerated _ = False
funcCallEqGenerated _ _ _ = False

-- | Lift a variable size constraint to an attribute.
variableSize :: (MonadFail m, MonadLog m)
             => Model        -- ^ Model as context.
             -> Declaration  -- ^ Statement to give attribute.
             -> m Model      -- ^ Possibly updated model.
variableSize m decl@(FindOrGiven forg n dom) | validDom dom = do
  let exprs = mapMaybe (sizeConstraint n) $ suchThat m
  case mapMaybe sizeAttrFromConstr (nub exprs) of
       -- Only one size attribute is valid
       attr@[_] -> do
         dom' <- addAttributesToDomain dom attr
         return $ updateDeclaration decl (FindOrGiven forg n dom') $
                  removeConstraints m exprs
       _        -> return m
  where
    validDom DomainSet{}       = True
    validDom DomainMSet{}      = True
    validDom DomainFunction{}  = True
    validDom DomainSequence{}  = True
    validDom DomainPartition{} = True
    validDom _              = False
variableSize m _ = return m

-- | Find an expression constraining the size of a variable.
sizeConstraint :: Name              -- ^ Name of the variable with a constrained size.
               -> Expression        -- ^ Expression in which to look for the size constraint.
               -> Maybe Expression  -- ^ The expression constraining the size of the variable.
sizeConstraint n e
  = let v = case e of
                 -- Check both sides of the operator, but ignore (in)equations
                 -- of two find variables
                 [essence| |&var| =  &e' |] | not (hasFind e') -> Just var
                 [essence| |&var| <  &e' |] | not (hasFind e') -> Just var
                 [essence| |&var| <= &e' |] | not (hasFind e') -> Just var
                 [essence| |&var| >  &e' |] | not (hasFind e') -> Just var
                 [essence| |&var| >= &e' |] | not (hasFind e') -> Just var
                 [essence| &e' =  |&var| |] | not (hasFind e') -> Just var
                 [essence| &e' <  |&var| |] | not (hasFind e') -> Just var
                 [essence| &e' <= |&var| |] | not (hasFind e') -> Just var
                 [essence| &e' >  |&var| |] | not (hasFind e') -> Just var
                 [essence| &e' >= |&var| |] | not (hasFind e') -> Just var
                 _ -> Nothing
        in case v of
                Just (Reference n' _) | n == n' -> Just e
                _                               -> Nothing
  where
    hasFind = any isFind . universe
    isFind (Reference _ (Just (DeclNoRepr Find _ _ _))) = True
    isFind (Reference _ (Just (DeclHasRepr Find _ _)))  = True
    isFind _ = False

-- | Make a size attribute from a constraint on the size of a variable.
sizeAttrFromConstr :: Expression -> Maybe (AttrName, Maybe Expression)
sizeAttrFromConstr [essence| |&_| =  &s |] = Just ("size", Just s)
sizeAttrFromConstr [essence| |&_| <  &s |] = Just ("maxSize", Just (s - 1))
sizeAttrFromConstr [essence| |&_| <= &s |] = Just ("maxSize", Just s)
sizeAttrFromConstr [essence| |&_| >  &s |] = Just ("minSize", Just (s + 1))
sizeAttrFromConstr [essence| |&_| >= &s |] = Just ("minSize", Just s)
sizeAttrFromConstr [essence| &e =  |&s| |] = sizeAttrFromConstr [essence| |&s| =  &e |]
sizeAttrFromConstr [essence| &e <  |&s| |] = sizeAttrFromConstr [essence| |&s| >  &e |]
sizeAttrFromConstr [essence| &e <= |&s| |] = sizeAttrFromConstr [essence| |&s| >= &e |]
sizeAttrFromConstr [essence| &e >  |&s| |] = sizeAttrFromConstr [essence| |&s| <  &e |]
sizeAttrFromConstr [essence| &e >= |&s| |] = sizeAttrFromConstr [essence| |&s| <= &e |]
sizeAttrFromConstr _ = Nothing
