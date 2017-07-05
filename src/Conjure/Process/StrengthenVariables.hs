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
                                                     , mSetSizeOccur
                                                     , mSetOccur
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
    definedForAllIsTotal n m      >>=
    fullRangeIsSurjective n m     >>=
    diffArgResultIsInjective n m
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
surjectiveIsTotalBijective d@(DomainFunction _ (FunctionAttr _ p j) from to)
  | (p == PartialityAttr_Partial && j == JectivityAttr_Bijective) ||
    j == JectivityAttr_Surjective = do
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

-- | If all values in the range of a function are mapped to, then it is surjective.
fullRangeIsSurjective :: (MonadFail m, MonadLog m)
                      => Name                     -- ^ The function's name.
                      -> Model                    -- ^ Model for context.
                      -> Domain () Expression     -- ^ The function's domain.
                      -> m (Domain () Expression) -- ^ Possibly modified function domain.
fullRangeIsSurjective n m d@(DomainFunction _ (FunctionAttr _ _ ject) from to)
  | (ject == JectivityAttr_None || ject == JectivityAttr_Injective) &&
    -- Are the variables generated from the domain and codomain respectively?
    any (uncurry varsAreGen)
      -- Extract the function parameter and function value
      (mapMaybe existsEqVals
        -- Find the desired pattern being matched
        (findInUncondForAll isExistsEq m))
      = addAttributesToDomain d [ ("surjective", Nothing) ]
  where
    -- Get the domain value and codomain value used in the function call
    existsEqVals [essence| exists &i :  &_ . &f(&i') = &j |] = funcEqCoVal f i i' j
    existsEqVals [essence| exists &i in &_ . &f(&i') = &j |] = funcEqCoVal f i i' j
    existsEqVals _ = Nothing
    funcEqCoVal (Reference n' _) i i' j | n == n' = flip (,) j <$> checkVarsEq i i'
    funcEqCoVal _ _ _ _ = Nothing
    isExistsEq = isJust . existsEqVals
    -- Try equating the variables references and return the reference if they are equal
    checkVarsEq :: AbstractPattern -> Expression -> Maybe Expression
    checkVarsEq (Single i) e@(Reference i' _) | i == i' = Just e
    checkVarsEq _ _ = Nothing
    -- Make sure that the variables are generated from the domain and codomain of the function
    varsAreGen (Reference _ (Just (DeclNoRepr Quantified _ from' _)))
               (Reference _ (Just (DeclNoRepr Quantified _ to'   _)))
                 = from == from' && to == to'
    varsAreGen iDom jDom = case domInCompRef iDom of
                                Just from'
                                  -> case domInCompRef jDom of
                                          Just to' -> from == from' && to == to'
                                          _ -> False
                                _ -> False
    -- Extract a domain from a reference in a comprehension
    domInCompRef dom = case domInComprehension dom of
                            Just (GenInExpr _ (Reference _ (Just (Alias (Domain dom')))))
                              -> Just dom'
                            _ -> Nothing
    -- Extract an InComprehension domain
    domInComprehension (Reference _ (Just (InComprehension dom))) = Just dom
    domInComprehension _ = Nothing
fullRangeIsSurjective _ _ d = return d

-- | If all distinct inputs to a function have distinct results, then it is injective.
--   It will also be total if there are no conditions other than the disequality between
--   the two inputs.
diffArgResultIsInjective :: (MonadFail m, MonadLog m)
                         => Name                     -- ^ The function's name.
                         -> Model                    -- ^ Model for context.
                         -> Domain () Expression     -- ^ The function's domain.
                         -> m (Domain () Expression) -- ^ Possibly modified function domain.
diffArgResultIsInjective n m d@(DomainFunction _ (FunctionAttr _ _ ject) from _)
  | (ject == JectivityAttr_None || ject == JectivityAttr_Surjective) &&
    not (null $ findInUncondForAll isDistinctDisequality m)
    = addAttributesToDomain d [ ("injective", Nothing)
                              -- It is known that no inputs are ignored
                              , ("total", Nothing) ]
  where
    -- Match a very specific pattern, which will also add the total attribute
    isDistinctDisequality [essence| &i != &j -> &f(&i') != &f'(&j') |]
      | f == f' && i == i' && j == j'
        = case f of
               Reference n' _ | n == n'
                 -> domIsGen i && domIsGen j
               _ -> False
    isDistinctDisequality _ = False
    -- Extract a domain reference from a comprehension
    domIsGen (Reference _ (Just (DeclNoRepr Quantified _ dom _))) = from == dom
    domIsGen d' = case domInComprehension d' of
                       Just (GenInExpr _ (Reference _ (Just (Alias (Domain dom))))) -> from == dom
                       _ -> False
    -- Extract an InComprehension domain
    domInComprehension (Reference _ (Just (InComprehension dom))) = Just dom
    domInComprehension _ = Nothing
diffArgResultIsInjective _ _ d = return d

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
setSetMinSize n (DomainSet r (SetAttr s) d) = DomainSet r (SetAttr $ mergeMinSize n s) d
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

-- | Make a minimum expression between two expressions.
--   Two min expressions are merged into one.
--   The min between a value and a min adds the value to the min (if not present).
--   If the expressions are the same, no min is made and the value is returned.
mkMin :: Expression -> Expression -> Expression
mkMin (Op (MkOpMin (OpMin (AbstractLiteral (AbsLitMatrix _ es1)))))
      (Op (MkOpMin (OpMin (AbstractLiteral (AbsLitMatrix _ es2)))))
        = make opMin $ fromList $ es1 `union` es2
mkMin i m@(Op (MkOpMin (OpMin (AbstractLiteral (AbsLitMatrix _ es)))))
          | i `elem` es = m
          | otherwise   = make opMin $ fromList $ i : es
mkMin m@(Op (MkOpMin (OpMin (AbstractLiteral (AbsLitMatrix _ es))))) i
          | i `elem` es = m
          | otherwise   = make opMin $ fromList $ i : es
mkMin i e | i == e      = e
          | otherwise   = make opMin $ fromList [ i, e ]

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
             -> Declaration  -- ^ Declaration to give attribute.
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

-- | The maxSize, and minOccur attributes of an mset affect its maxOccur and minSize attributes.
mSetSizeOccur :: (MonadFail m, MonadLog m)
              => Model        -- ^ Model as context.
              -> Declaration  -- ^ Declaration to give attribute.
              -> m Model      -- ^ Possibly updated model.
mSetSizeOccur m decl@(FindOrGiven forg n d@DomainMSet{})
  = case d of
         -- Ordering is important here, as there is a rule that applies
         -- to maxSize and minOccur, but none that applies to minSize
         -- and maxOccur. size uses the maxSize rule, but can ignore a
         -- minOccur because it cannot have its minSize changed.
         -- size -> maxOccur
         d'@(DomainMSet _ (MSetAttr (SizeAttr_Size mx) _) _)
           -> updateDomain $ mkMaxOccur d' mx
         -- minOccur -> minSize
         d'@(DomainMSet _ (MSetAttr _ (OccurAttr_MinOccur mn)) _)
           -> updateDomain $ mkMinSize d' mn
         d'@(DomainMSet _ (MSetAttr _ (OccurAttr_MinMaxOccur mn _)) _)
           -> updateDomain $ mkMinSize d' mn
         -- maxSize -> maxOccur
         d'@(DomainMSet _ (MSetAttr (SizeAttr_MaxSize mx) _) _)
           -> updateDomain $ mkMaxOccur d' mx
         d'@(DomainMSet _ (MSetAttr (SizeAttr_MinMaxSize _ mx) _) _)
           -> updateDomain $ mkMaxOccur d' mx
         _ -> return m
  where
    mkMinSize (DomainMSet r (MSetAttr s o) d') mn
      = let sizeAttr = mergeMinSize mn s
            in DomainMSet r (MSetAttr sizeAttr o) d'
    mkMinSize dom _ = dom

    mkMaxOccur (DomainMSet r (MSetAttr s o) d') mx
      = let occAttr = mergeMaxOccur mx o
            in DomainMSet r (MSetAttr s occAttr) d'
    mkMaxOccur dom _ = dom

    updateDomain dom = do
      let decl' = FindOrGiven forg n dom
      return $ updateDeclaration decl decl' m
mSetSizeOccur m _ = return m

-- | Merge an expression into the min field of a 'SizeAttr'.
mergeMinSize :: Expression -> SizeAttr Expression -> SizeAttr Expression
mergeMinSize e SizeAttr_None = SizeAttr_MinSize e
mergeMinSize e (SizeAttr_MinSize mn) = SizeAttr_MinSize $ mkMax e mn
mergeMinSize e (SizeAttr_MaxSize mx)
  | e == mx   = SizeAttr_Size e
  | otherwise = SizeAttr_MinMaxSize e mx
mergeMinSize e (SizeAttr_MinMaxSize mn mx) = SizeAttr_MinMaxSize (mkMax e mn) mx
mergeMinSize _ s = s

-- | Merge an expression into the max field of a 'SizeAttr'.
mergeMaxSize :: Expression -> SizeAttr Expression -> SizeAttr Expression
mergeMaxSize e SizeAttr_None = SizeAttr_MaxSize e
mergeMaxSize e (SizeAttr_MinSize mn)
  | e == mn   = SizeAttr_Size e
  | otherwise = SizeAttr_MinMaxSize e mn
mergeMaxSize e (SizeAttr_MaxSize mx) = SizeAttr_MaxSize $ mkMin e mx
mergeMaxSize e (SizeAttr_MinMaxSize mn mx) = SizeAttr_MinMaxSize mn (mkMin e mx)
mergeMaxSize _ s = s

-- | Merge an expression into the min field of an 'OccurAttr'.
mergeMinOccur :: Expression -> OccurAttr Expression -> OccurAttr Expression
mergeMinOccur e OccurAttr_None = OccurAttr_MinOccur e
mergeMinOccur e (OccurAttr_MinOccur mn) = OccurAttr_MinOccur $ mkMax mn e
mergeMinOccur e (OccurAttr_MaxOccur mx) = OccurAttr_MinMaxOccur e mx
mergeMinOccur e (OccurAttr_MinMaxOccur mn mx) = OccurAttr_MinMaxOccur (mkMax e mn) mx

-- | Merge an expression into the max field of an 'OccurAttr'.
mergeMaxOccur :: Expression -> OccurAttr Expression -> OccurAttr Expression
mergeMaxOccur e OccurAttr_None = OccurAttr_MaxOccur e
mergeMaxOccur e (OccurAttr_MinOccur mn) = OccurAttr_MinMaxOccur mn e
mergeMaxOccur e (OccurAttr_MaxOccur mx) = OccurAttr_MaxOccur $ mkMin e mx
mergeMaxOccur e (OccurAttr_MinMaxOccur mn mx) = OccurAttr_MinMaxOccur mn (mkMin e mx)

-- | Infer multiset occurrence attributes from constraints.
mSetOccur :: (MonadFail m, MonadLog m)
          => Model        -- ^ Model for context.
          -> Declaration  -- ^ Multiset declaration to update.
          -> m Model      -- ^ Updated model.
mSetOccur m decl@(FindOrGiven Find n dom@(DomainMSet r (MSetAttr s _) d))
  = let freqs = nub $ findInUncondForAll isFreq m
        dom' = foldr (\f d' ->
                      case d' of
                           DomainMSet _ (MSetAttr _ o') _
                             -> DomainMSet r (MSetAttr s (inferOccur f o')) d
                           _ -> d')
                     dom freqs
        decl' = FindOrGiven Find n dom'
        in return $ updateDeclaration decl decl' m
  where
    isFreq [essence| freq(&x, &v) =  &e |] = isRefTo x && isGen v && isConst e
    isFreq [essence| freq(&x, &v) <  &e |] = isRefTo x && isGen v && isConst e
    isFreq [essence| freq(&x, &v) <= &e |] = isRefTo x && isGen v && isConst e
    isFreq [essence| freq(&x, &v) >  &e |] = isRefTo x && isGen v && isConst e
    isFreq [essence| freq(&x, &v) >= &e |] = isRefTo x && isGen v && isConst e
    isFreq [essence| &e =  freq(&x, &v) |] = isRefTo x && isGen v && isConst e
    isFreq [essence| &e <  freq(&x, &v) |] = isRefTo x && isGen v && isConst e
    isFreq [essence| &e <= freq(&x, &v) |] = isRefTo x && isGen v && isConst e
    isFreq [essence| &e >  freq(&x, &v) |] = isRefTo x && isGen v && isConst e
    isFreq [essence| &e >= freq(&x, &v) |] = isRefTo x && isGen v && isConst e
    isFreq _ = False
    -- Make sure that the correct variable is referred to
    isRefTo (Reference n' _) = n == n'
    isRefTo _ = False
    -- Make sure that the value is generated from the mset's domain
    isGen (Reference _ (Just (DeclNoRepr Quantified _ d' _))) = d == d'
    isGen _ = False
    -- Make sure that the mset is being equated to a constant
    isConst (Constant ConstantInt{}) = True
    isConst _ = False
    -- Convert the constraint into an occurrence attribute
    inferOccur [essence| freq(&_, &_) =  &e |] o'
      = mergeMinOccur e $ mergeMaxOccur e o'
    inferOccur [essence| freq(&_, &_) <  &e |] o'
      = mergeMaxOccur (e - 1) o'
    inferOccur [essence| freq(&_, &_) <= &e |] o'
      = mergeMaxOccur e o'
    inferOccur [essence| freq(&_, &_) >  &e |] o'
      = mergeMinOccur (e + 1) o'
    inferOccur [essence| freq(&_, &_) >= &e |] o'
      = mergeMinOccur e o'
    inferOccur _ o' = o'
mSetOccur m _ = return m
