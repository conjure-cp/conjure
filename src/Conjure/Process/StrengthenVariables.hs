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

import Data.List ( nub, union )
import Data.Maybe ( mapMaybe )

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.Domain.AddAttributes
import Conjure.Language.NameResolution ( resolveNames )
-- These two are needed together
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.DomainSizeOf ( domainSizeOf )
import Conjure.Compute.DomainOf ( domainOf )

import Data.Generics.Uniplate.Zipper ( Zipper, zipper, down, right, up, fromZipper, hole, replaceHole )

type ExpressionZ = Zipper Expression Expression

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: (MonadFail m, MonadLog m, MonadUserError m)
                    => Model -> m Model
strengthenVariables = runNameGen . (resolveNames >=> core . suchThatToBack . fixRelationProj)
  where core :: (MonadFail m, MonadLog m, MonadUserError m, NameGen m) => Model -> m Model
        core model = do model' <- foldM folder model [ functionAttributes
                                                     , setAttributes
                                                     , setConstraints
                                                     , variableSize
                                                     , mSetSizeOccur
                                                     , mSetOccur
                                                     , forAllIneqToIneqSum
                                                     , fasterIteration
                                                     ] >>= resolveNames
                        -- Make another pass if there were changes, but ignore models
                        -- with machine names, as they don't play well with resolveNames
                        if model == model' || any containsMachineName (suchThat model)
                           then return model'
                           else core model'
        -- Apply the function to every statement and fold it into the model
        folder :: (MonadFail m, MonadLog m)
               => Model -> (Model -> Declaration -> m Model) -> m Model
        folder m@Model { mStatements = stmts } f = foldM (applyToDecl f) m stmts
        -- Apply the function only to declarations
        applyToDecl f m (Declaration d) = f m d
        applyToDecl _ m _               = return m
        -- Push all of the constraints to a single statement at the back
        suchThatToBack m@Model { mStatements = stmts }
          = m { mStatements = suchThatToBack' stmts [] }
        suchThatToBack' [] es = [SuchThat es]
        suchThatToBack' (SuchThat cs:ss) es = suchThatToBack' ss (es `union` cs)
        suchThatToBack' (s:ss) es = s : suchThatToBack' ss es
        -- | Does an expression contain a reference with a machine name?
        containsMachineName = any isMachineName . universe
        isMachineName (Reference MachineName{} _) = True
        isMachineName _                           = False

-- | Update a declaration in a model.
updateDeclaration :: Declaration  -- ^ New declaration value.
                  -> Model        -- ^ Model to be updated.
                  -> Model        -- ^ Updated model.
updateDeclaration d@(FindOrGiven _ n' _) m@Model { mStatements = stmts }
  = m { mStatements = map updateDeclaration' stmts }
  where
    -- Replace the declaration in-place
    updateDeclaration' (Declaration (FindOrGiven _ n _))
      | n == n' = Declaration d
    updateDeclaration' s = s
updateDeclaration _ m = m

-- | Merge a list of constraints into a model.
mergeConstraints :: Model         -- ^ Model to be updated.
                 -> [Expression]  -- ^ Constraints to merge into the model.
                 -> Model         -- ^ Updated model with new constraints.
mergeConstraints m [] = m
mergeConstraints m@Model { mStatements = stmts } cs
  = m { mStatements = mergeConstraints' stmts }
  where
    mergeConstraints' (SuchThat cs':ss) = SuchThat (cs' `union` cs) : ss
    mergeConstraints' (s:ss) = s : mergeConstraints' ss
    mergeConstraints' [] = [SuchThat cs]

-- | Remove a list of constraints from a model.
removeConstraints :: Model        -- ^ Model to have the constraint removed.
                  -> [Expression] -- ^ Constraints to remove.
                  -> Model        -- ^ Updated model with constraints removed.
removeConstraints m@Model { mStatements = stmts } cs
  = m { mStatements = filter (not . emptySuchThat) $ map removeConstraints' stmts }
  where
    removeConstraints' (SuchThat cs') = SuchThat $ filter (`notElem` cs) cs'
    removeConstraints' s              = s

    emptySuchThat (SuchThat []) = True
    emptySuchThat _             = False

-- | Make the attributes of function variables as constrictive as possible.
functionAttributes :: (MonadFail m, MonadLog m)
                   => Model       -- ^ Model as context.
                   -> Declaration -- ^ Statement to constrain.
                   -> m Model     -- ^ Possibly updated model.
functionAttributes m (FindOrGiven forg n d@DomainFunction{}) = do
  d' <- constrainFunctionDomain n d m
  let f' = FindOrGiven forg n d'
  return $ updateDeclaration f' m
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
  where fromSuchThat (SuchThat es) = (`union` es)
        fromSuchThat _             = id

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
funcCalledWithGenParams n d (Op (MkOpImage (OpImage (Reference n' _) param)))
  = n' == n && genArgMatchesDom param
    where
      -- Is a function argument generated from the function's domain?
      genArgMatchesDom (Reference _ (Just refDom))
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
setAttributes m (FindOrGiven forg n d@DomainSet{}) = do
  d' <- setSizeFromConstraint n d m
  let f' = FindOrGiven forg n d'
  return $ updateDeclaration f' m
setAttributes m _ = return m

-- | Constrain the size of a set from constraints on it.
setSizeFromConstraint :: (MonadFail m, MonadLog m, NameGen m)
                      => Name                     -- ^ Name of the set being worked on.
                      -> Domain () Expression     -- ^ Domain of the set to possibly modify.
                      -> Model                    -- ^ Model for context.
                      -> m (Domain () Expression) -- ^ Possibly modified set domain.
setSizeFromConstraint n d = foldM subsetMinSize d . findInUncondForAll isSubsetOf
  where
    isSubsetOf (Op (MkOpSubset   (OpSubset   _ (Reference n' _)))) = n == n'
    isSubsetOf (Op (MkOpSubsetEq (OpSubsetEq _ (Reference n' _)))) = n == n'
    isSubsetOf _ = False

    subsetMinSize d' [essence| range(&l) subset   &_ |] = minSizeFromFunction d' l
    subsetMinSize d' [essence| range(&l) subsetEq &_ |] = minSizeFromFunction d' l
    subsetMinSize d' [essence| &l subset   &_ |] = minSizeFromSet d' l -- could be more precise
    subsetMinSize d' [essence| &l subsetEq &_ |] = minSizeFromSet d' l
    subsetMinSize d' _ = return d'

-- | Find an expression at any depth of unconditional forAll expressions.
findInUncondForAll :: (Expression -> Bool) -> Model -> [Expression]
findInUncondForAll p = map hole . findInUncondForAllZ p

-- | Find an expression at any depth of unconditional forAll expressions,
--   returning a Zipper containing the expression's context.
findInUncondForAllZ :: (Expression -> Bool) -> Model -> [ExpressionZ]
findInUncondForAllZ p = concatMap (findInForAll . zipper) . suchThat
  where
    findInForAll e | p (hole e) = [e]
    findInForAll z
      = case hole z of
             Op (MkOpAnd (OpAnd (Comprehension _ gorcs)))
               | all (not . isCondition) gorcs
                 -> maybe [] findInForAll (down z >>= down)
             [essence| &_ /\ &_ |]
                 -> maybe [] findInForAll (down z)
                    `union`
                    maybe [] findInForAll (right z >>= down)
             -- Only accept OR cases if both sides contain a match
             [essence| &_ \/ &_ |]
                 -> let leftResult  = maybe [] findInForAll (down z)
                        rightResult = maybe [] findInForAll (right z >>= down)
                        in if not (null leftResult) && not (null rightResult)
                              then leftResult `union` rightResult
                              else []
             _   -> []
    isCondition Condition{} = True
    isCondition _           = False

-- | Set the minimum size of a set based on it being a superset of another.
minSizeFromSet :: (MonadFail m, MonadLog m, NameGen m)
               => Domain () Expression      -- ^ Domain of the set for which to change the minimum size.
               -> Expression                -- ^ Expression from which the minimum size is being inferred.
               -> m (Domain () Expression)  -- ^ Set domain with a possible change of its minimum size.
minSizeFromSet d@(DomainSet r (SetAttr size) dom) sub = do
  subDom <- domainOf sub
  case subDom of
       DomainSet _ (SetAttr subSize) _
         -> return $ DomainSet r (SetAttr $ mergeSizeOnMin size subSize) dom
       _ -> return d
  where
    -- Merge the minSize of the right SizeAttr into the left
    mergeSizeOnMin :: SizeAttr Expression -> SizeAttr Expression -> SizeAttr Expression
    mergeSizeOnMin SizeAttr_None (SizeAttr_Size s) = SizeAttr_MinSize s
    mergeSizeOnMin SizeAttr_None s@(SizeAttr_MinSize _) = s
    mergeSizeOnMin SizeAttr_None (SizeAttr_MinMaxSize s _) = SizeAttr_MinSize s
    mergeSizeOnMin (SizeAttr_MinSize mn) (SizeAttr_Size s) = SizeAttr_MinSize $ mkMax mn s
    mergeSizeOnMin (SizeAttr_MinSize mn) (SizeAttr_MinSize s) = SizeAttr_MinSize $ mkMax mn s
    mergeSizeOnMin (SizeAttr_MinSize mn) (SizeAttr_MinMaxSize s _) = SizeAttr_MinSize $ mkMax mn s
    mergeSizeOnMin (SizeAttr_MinMaxSize mn mx) (SizeAttr_Size s) = SizeAttr_MinMaxSize (mkMax mn s) mx
    mergeSizeOnMin (SizeAttr_MinMaxSize mn mx) (SizeAttr_MinSize s) = SizeAttr_MinMaxSize (mkMax mn s) mx
    mergeSizeOnMin (SizeAttr_MinMaxSize mn mx) (SizeAttr_MinMaxSize s _) = SizeAttr_MinMaxSize (mkMax mn s) mx
    mergeSizeOnMin s _ = s
minSizeFromSet d _ = return d

-- | Set the minimum size of a set based on it being a superset of the range of a total function.
minSizeFromFunction :: (MonadFail m, MonadLog m, NameGen m)
                    => Domain () Expression     -- ^ Domain of the set for which to change to minimum size.
                    -> Expression               -- ^ Expression from which the minimum size is being inferred.
                    -> m (Domain () Expression) -- ^ Set domain with a possible change of its minimum size.
minSizeFromFunction d r = do
  f <- getFunDom r
  case f of
       DomainFunction _ (FunctionAttr _ PartialityAttr_Total _) _ _
         -> return $ setSetMinSize (Constant $ ConstantInt 1) d
       _ -> return d

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
    isFuncCall (Op (MkOpImage (OpImage f' _))) = f == f'
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
variableSize m (FindOrGiven forg n dom) | validDom dom = do
  let exprs = mapMaybe (sizeConstraint n) $ suchThat m
  case mapMaybe sizeAttrFromConstr (nub exprs) of
       -- Only one size attribute is valid
       attr@[_] -> do
         dom' <- addAttributesToDomain dom attr
         return $ updateDeclaration (FindOrGiven forg n dom') $
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
  = let v = case matching e ineqOps of
                 -- Check both sides of the operator, but ignore (in)equations
                 -- of two find variables
                 Just (_, ([essence| |&var| |], e'))
                   | not (hasFind e') -> Just var
                 Just (_, (e', [essence| |&var| |]))
                   | not (hasFind e') -> Just var
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
sizeAttrFromConstr e = case matching e oppIneqOps of
                            -- Try again with the terms swapped
                            Just (f, (e', s@(Op (MkOpTwoBars (OpTwoBars _)))))
                              -> sizeAttrFromConstr $ make f s e'
                            _ -> Nothing
  where
    oppIneqOps = [ (opEq, opEq), (opLt, opGt), (opLeq, opGeq), (opGt, opLt), (opGeq, opLeq) ]

-- | The maxSize, and minOccur attributes of an mset affect its maxOccur and minSize attributes.
mSetSizeOccur :: (MonadFail m, MonadLog m)
              => Model        -- ^ Model as context.
              -> Declaration  -- ^ Declaration to give attribute.
              -> m Model      -- ^ Possibly updated model.
mSetSizeOccur m (FindOrGiven forg n d@DomainMSet{})
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
      return $ updateDeclaration decl' m
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
mSetOccur m (FindOrGiven Find n dom@(DomainMSet r (MSetAttr s _) d))
  = let freqs = findInUncondForAll isFreq m
        dom' = foldr (\f d' ->
                      case d' of
                           DomainMSet _ (MSetAttr _ o') _
                             -> DomainMSet r (MSetAttr s (inferOccur f o')) d
                           _ -> d')
                     dom freqs
        decl' = FindOrGiven Find n dom'
        in return $ updateDeclaration decl' m
  where
    isFreq e = let valid x v e' = isRefTo x && isGen v && isConst e'
                   in case matching e ineqOps of
                           Just (_, ([essence| freq(&x, &v) |], e')) -> valid x v e'
                           Just (_, (e', [essence| freq(&x, &v) |])) -> valid x v e'
                           _ -> False
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

-- | An (in)equality in a forAll implies that the (in)equality also applies to
--   the sums of both terms.
forAllIneqToIneqSum :: (MonadFail m, MonadLog m, NameGen m)
                    => Model
                    -> Declaration
                    -> m Model
forAllIneqToIneqSum m _
  = fmap (mergeConstraints m . map fromZipper . mapMaybe mkConstraint) $
          filterM partsAreNumeric $
          mapMaybe matchParts $
          findInUncondForAllZ (isJust . matchParts . zipper) m
  where
    matchParts :: ExpressionZ -> Maybe (Generator, Maybe ExpressionZ)
    matchParts z
      = case hole z of
             Op (MkOpAnd (OpAnd (Comprehension e [Generator g])))
               -> case matching e ineqOps of
                       Just (_, (e1, e2)) -> matchComponents z g e1 e2
                       _                  -> Nothing
             _ -> Nothing
    -- Match the components of the expression of interest
    matchComponents :: ExpressionZ -> Generator -> Expression -> Expression
                    -> Maybe (Generator, Maybe ExpressionZ)
    matchComponents z g e1 e2
      | refInExpr (namesFromGenerator g) e1 && refInExpr (namesFromGenerator g) e2
        = Just (g, down z >>= down)
    matchComponents _ _ _ _ = Nothing
    -- Is a name referred to in an expression?
    refInExpr names = any (hasName names) . universe
    hasName names (Reference name _) = name `elem` names
    hasName _     _                  = False
    -- Are the parts of the matched expression numeric?
    partsAreNumeric :: (MonadFail m, MonadLog m, NameGen m)
                    => (Generator, Maybe ExpressionZ) -> m Bool
    partsAreNumeric (_, Just z)
      = case matching (hole z) ineqOps of
             Just (_, (e1, e2)) -> (&&) <$> domainIsNumeric e1 <*> domainIsNumeric e2
             Nothing            -> return False
    partsAreNumeric _ = return False
    domainIsNumeric :: (MonadFail m, MonadLog m, NameGen m)
                    => Expression -> m Bool
    domainIsNumeric e = case domainOf e of
                             Right DomainInt{}           -> return True
                             Right (DomainAny _ TypeInt) -> return True
                             _                           -> return False
    -- Replace the forAll with the (in)equality between sums
    mkConstraint :: (Generator, Maybe ExpressionZ) -> Maybe ExpressionZ
    mkConstraint (gen, Just z)
      = case matching (hole z) ineqOps of
             Just (f, (e1, e2))
               -> let mkSumOf = Op . MkOpSum . OpSum . flip Comprehension [Generator gen]
                      e1' = mkSumOf e1
                      e2' = mkSumOf e2
                      -- Two steps to get out of the forAll, and replace it with the constraint
                      in replaceHole (make f e1' e2') <$> (up z >>= up)
             _ -> Nothing
    mkConstraint _ = Nothing

-- | Get the list of names from a generator.
namesFromGenerator :: Generator -> [Name]
namesFromGenerator (GenDomainNoRepr a _)  = namesFromAbstractPattern a
namesFromGenerator (GenDomainHasRepr n _) = [n]
namesFromGenerator (GenInExpr a _)        = namesFromAbstractPattern a

-- | Get the list of names from an abstract pattern.
namesFromAbstractPattern :: AbstractPattern -> [Name]
namesFromAbstractPattern (Single n)        = [n]
namesFromAbstractPattern (AbsPatTuple ns)  = concatMap namesFromAbstractPattern ns
namesFromAbstractPattern (AbsPatMatrix ns) = concatMap namesFromAbstractPattern ns
namesFromAbstractPattern (AbsPatSet ns)    = concatMap namesFromAbstractPattern ns
namesFromAbstractPattern _                 = []

-- | Lens function over a binary expression.
type BinExprLens m = Proxy m -> (Expression -> Expression -> Expression,
                                 Expression -> m (Expression, Expression))

-- | Get the lens for an expression and the values it matches.
matching :: Expression
         -> [(BinExprLens Maybe, BinExprLens Identity)]
         -> Maybe (BinExprLens Identity, (Expression, Expression))
matching e ops = case mapMaybe (\(f1, f2) -> (,) f2 <$> match f1 e) ops of
                      [x] -> Just x
                      _   -> Nothing

-- | (In)equality operator lens pairs.
ineqOps :: [(BinExprLens Maybe, BinExprLens Identity)]
ineqOps = [ (opEq,  opEq)
          , (opLt,  opLt)
          , (opLeq, opLeq)
          , (opGt,  opGt)
          , (opGeq, opGeq)
          ]

-- | Iterate slightly faster over a domain if generating two distinct variables.
fasterIteration :: (MonadFail m, MonadLog m)
                => Model       -- ^ Model as context.
                -> Declaration -- ^ Ignored declaration.
                -> m Model     -- ^ Possibly updated model.
fasterIteration m _
  = let matches = findInUncondForAllZ (isJust . doubleDistinctIter . zipper) m
        -- Pair up matches with the updated constraint
        pairs = zip matches (map changeIterator $ mapMaybe doubleDistinctIter matches)
        newConstraints = map (fromZipper *** fromZipper) $ foldr fromMaybePairs [] pairs
        -- Remove the old constraint
        in return $ flip removeConstraints (map fst newConstraints) $
                    -- Add the new constraints
                    mergeConstraints m (map snd newConstraints)
  where
    -- Match the elemenents of interest in the constraint
    doubleDistinctIter z
      = case hole z of
             [essence| forAll &x, &y in &v, &x' != &y' . &_ |]
               | areDiffed x y x' y' -> Just ((x, x'), (y, y'), v, down z >>= down)
             [essence| forAll &x, &y : &d, &x' != &y' . &_ |]
               | areDiffed x y x' y' -> Just ((x, x'), (y, y'), Domain d, down z >>= down)
             _ -> Nothing
    -- Are the two generated variables constrained to be different
    -- in the condition on the forAll?
    areDiffed x y (Reference nx' _) (Reference ny' _)
      = case map namesFromAbstractPattern [x, y] of
             [[nx], [ny]] -> nx == nx' && ny == ny'
             _            -> False
    areDiffed _ _ _ _ = False
    -- Change the iterator to use the new, faster notation
    changeIterator ((x, x'), (y, y'), v, Just z)
      = let e = hole z
            in case v of
                    r@Reference{}
                      -> case domainOf r of
                              Left _ -> Nothing
                              Right DomainSet{}
                                -> replaceHole [essence| forAll {&x, &y} subsetEq &v . &e |] <$>
                                   (up z >>= up)
                              Right _
                                -> replaceHole [essence| forAll &x, &y in &v, &y' > &x' . &e |] <$>
                                   (up z >>= up)
                    Op MkOpDefined{}
                      -> replaceHole [essence| forAll &x, &y in &v, &y' > &x' . &e |] <$>
                         (up z >>= up)
                    Domain d
                      -> replaceHole [essence| forAll &x, &y : &d, &y' > &x' . &e |] <$>
                         (up z >>= up)
                    _ -> Nothing
    changeIterator _ = Nothing
    -- Only keep pairs that have a Just on the right hand side
    fromMaybePairs (x, Just y) ps = (x, y) : ps
    fromMaybePairs _ ps = ps
