{-
 - Module      : Conjure.Process.ModelStrengthening
 - Description : Strengthen a model using type- and domain-inference.
 - Copyright   : Billy Brown 2017
 - License     : BSD3
 
 Processing step that attempts to strengthen an Essence model, using methods described in the "Reformulating Essence Specifications for Robustness" paper.
-}

{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.ModelStrengthening
  (
    strengthenModel
  ) where

import Data.List ( find, union )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M ( (!?), empty, union )
import Data.Maybe ( mapMaybe )

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.Domain.AddAttributes
import Conjure.Language.NameResolution ( resolveNames )
-- These two are needed together
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.DomainSizeOf ( domainSizeOf )
import Conjure.Compute.DomainOf ( domainOf )
import Conjure.UI.VarSymBreaking ( outputVarSymBreaking )

-- aeson
import qualified Data.Aeson as JSON ( decodeStrict )
-- shelly
import Shelly ( run )
-- directory
import System.Directory ( removeFile )
-- text
import qualified Data.Text.Encoding as T ( encodeUtf8 )
-- uniplate zipper
import Data.Generics.Uniplate.Zipper ( Zipper, zipper, down, fromZipper, hole, replaceHole, right, up )

type ExpressionZ = Zipper Expression Expression
type FindVar     = (Name, Domain () Expression)
type AttrPair    = (AttrName, Maybe Expression)
type ToAddToRem  = ([ExpressionZ], [ExpressionZ])

-- | Strengthen a model using type- and domain-inference.
strengthenModel :: (MonadFail m, MonadIO m, MonadLog m, MonadUserError m)
                => LogLevel -- ^ Log level to use.
                -> Bool     -- ^ Generate logs for rule applications.
                -> Model    -- ^ Model to strengthen.
                -> m Model  -- ^ Strengthened model.
strengthenModel logLevel logRuleSuccesses model = runNameGen model $ (resolveNames >=> core . fixRelationProj) model
  where
    core :: (MonadFail m, MonadIO m, MonadLog m, MonadUserError m, NameGen m) => Model -> m Model
    core model1 = do
      -- Apply attribute rules to each decision (find) variable
      (model2, toAddToRem) <- foldM (\modelAndToKeep findAndCstrs@((n, d), _) ->
          foldM (\(m1, tatr1) (rule, name) -> do
                  (attrs, tatr2) <- nested rule m1 findAndCstrs
                  let m2 = foldr (uncurry3 addAttrsToModel) m1 attrs
                  when (((not (null attrs) && m1 /= m2) ||
                         (tatr2 /= mempty && toAddRem tatr2 tatr1 /= tatr1)) &&
                        logRuleSuccesses)
                       (log logLevel $ name <+> if null attrs
                                                   then vcat $ map (pretty . hole) (fst tatr2)
                                                   else pretty n <+> ":" <+> pretty d)
                  return (m2, toAddRem tatr2 tatr1))
              modelAndToKeep [ (surjectiveIsTotalBijective, "function marked total and bijective")
                             , (totalInjectiveIsBijective,  "function marked bijective")
                             , (definedForAllIsTotal,       "function marked total")
                             , (diffArgResultIsInjective,   "function marked injective")
                             , (varSize,                    "added or refined domain size attribute")
                             , (setSize,                    "added or refined set domain size attribute")
                             , (mSetSizeOccur,              "added or refined multiset occurrence attribute")
                             , (mSetOccur,                  "added or refined multiset occurrence attribute")
                             , (partRegular,                "marked partition regular")
                             , (numPartsToAttr,             "added or refined partition domain numParts attribute")
                             , (partSizeToAttr,             "added or refined partition domain partSize attribute")
                             , (funcRangeEqSet,             "equated function range and set")
                             , (forAllIneqToIneqSum,        "lifted arithmetic relation from two forAlls to a sum")
                             , (fasterIteration,            "refined distinctness condition on forAll")
                             ])
          (model1, ([], []))
          (zip (collectFindVariables model1)
               (repeat $ map zipper $ collectConstraints model1))

      -- Apply constraint additions and removals
      model3 <- resolveNames $
                addConstraints (fst toAddToRem) $
                remConstraints (snd toAddToRem) model2

      -- Apply type change rules to each decision (find) variable
      (model4, toAddToRem') <- foldM (\modelAndToKeep findAndCstrs@((n, d), _) ->
          foldM (\(m1, tatr1) (rule, name) -> do
                  (dom, tatr2) <- rule m1 findAndCstrs
                  when ((dom /= d || toAddRem tatr2 tatr1 /= tatr1) &&
                        logRuleSuccesses)
                       (log logLevel $ name <+> pretty n <+> ":" <+> pretty d)
                  return (updateDecl (n, dom) m1, toAddRem tatr2 tatr1))
              modelAndToKeep [ (mSetToSet, "multiset changed to set")
                             ])
          (model3, ([], []))
          (zip (collectFindVariables model3)
               (repeat $ map zipper $ collectConstraints model3))

      -- Apply constraint additions and removals
      model5 <- resolveNames $
                addConstraints (fst toAddToRem') $
                remConstraints (snd toAddToRem') model4

      -- Make another pass if the model was updated, but stop if it contains machine names
      if model1 == model5 || any containsMachineName (collectConstraints model5)
         then return model5
         else core model5
    -- Does an expression contain a reference with a machine name?
    containsMachineName = any isMachineName . universe
    isMachineName (Reference MachineName{} _) = True
    isMachineName _                           = False

-- | 'uncurry' for functions of three arguments and triples.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

-- | Collect decision (find) variables from a model, returning their name and domain.
collectFindVariables :: Model -> [FindVar]
collectFindVariables = mapMaybe collectFind . mStatements
  where
    collectFind (Declaration (FindOrGiven Find n d)) = Just (n, d)
    collectFind _                                    = Nothing

-- | Collect the constraints in a model.
collectConstraints :: Model -> [Expression]
collectConstraints = concatMap getSuchThat . mStatements
  where
    getSuchThat (SuchThat cs) = cs
    getSuchThat _             = []

-- | Add constraints to a model.
addConstraints :: [ExpressionZ] -> Model -> Model
addConstraints [] m = m
addConstraints cs m@Model { mStatements = stmts }
  = m { mStatements = addConstraints' stmts }
  where
    addConstraints' (SuchThat cs':ss) = SuchThat (cs' `union` map fromZipper cs) : ss
    addConstraints' (s:ss)            = s : addConstraints' ss
    addConstraints' []                = [SuchThat (map fromZipper cs)]

-- | Remove a list of constraints from a model.
remConstraints :: [ExpressionZ] -> Model -> Model
remConstraints cs m@Model { mStatements = stmts }
  = m { mStatements = filter (not . emptySuchThat) $ map remConstraints' stmts }
  where
    remConstraints' (SuchThat cs') = SuchThat $ filter (`notElem` map fromZipper cs) cs'
    remConstraints' s              = s
    emptySuchThat (SuchThat []) = True
    emptySuchThat _             = False

-- | Update the domain of a declaration in a model.
updateDecl :: FindVar -> Model -> Model
updateDecl (n, d) m@Model { mStatements = stmts } = m { mStatements = map updateDecl' stmts }
  where
    updateDecl' (Declaration (FindOrGiven Find n' _))
      | n == n' = Declaration (FindOrGiven Find n d)
    updateDecl' decl = decl

-- | Try adding an attribute at a given depth of a variable's domain, in a model.
addAttrsToModel :: FindVar -> Int -> [AttrPair] -> Model -> Model
addAttrsToModel (n, _) depth attrs m
  = let d = snd <$> find (\(n', _) -> n == n') (collectFindVariables m)
        in case d >>= flip (addAttrsToDomain depth) attrs of
                Just d' -> updateDecl (n, d') m
                Nothing -> m
  where
    addAttrsToDomain :: (MonadFail m) => Int -> Domain () Expression -> [AttrPair] -> m (Domain () Expression)
    addAttrsToDomain 0 dom = addAttributesToDomain dom . map mkAttr
    addAttrsToDomain level (DomainSet r as inner)           = addAttrsToDomain (level - 1) inner >=> (pure . DomainSet r as)
    addAttrsToDomain level (DomainMSet r as inner)          = addAttrsToDomain (level - 1) inner >=> (pure . DomainMSet r as)
    addAttrsToDomain level (DomainMatrix index inner)       = addAttrsToDomain (level - 1) inner >=> (pure . DomainMatrix index)
    addAttrsToDomain level (DomainFunction r as from inner) = addAttrsToDomain (level - 1) inner >=> (pure . DomainFunction r as from)
    addAttrsToDomain level (DomainPartition r as inner)     = addAttrsToDomain (level - 1) inner >=> (pure . DomainPartition r as)
    addAttrsToDomain _ _ = const (fail "[addAttrsToDomain] not a supported nested domain")
    -- Special treatment for functions
    mkAttr (attr, Just [essence| image(&f, &_) |])     = (attr, Just [essence| max(range(&f)) |])
    mkAttr (attr, Just [essence| image(&f, &_) - 1 |]) = (attr, Just [essence| max(range(&f)) - 1 |])
    mkAttr (attr, Just [essence| image(&f, &_) + 1 |]) = (attr, Just [essence| max(range(&f)) + 1 |])
    mkAttr (attr, e')                                  = (attr, e')

-- | Does an expression directly reference a given name variable?
nameExpEq :: Name -> Expression -> Bool
nameExpEq n (Reference n' _)           = n == n'
nameExpEq n [essence| image(&f, &x) |] = nameExpEq n f || nameExpEq n x
nameExpEq n [essence| &f(&x) |]        = nameExpEq n f || nameExpEq n x
nameExpEq n [essence| defined(&f) |]   = nameExpEq n f
nameExpEq n [essence| range(&f) |]     = nameExpEq n f
nameExpEq n [essence| |&x| |]          = nameExpEq n x
nameExpEq _ _                          = False

-- | Does a reference refer to an abstract pattern?
refersTo :: Expression -> AbstractPattern -> Bool
refersTo (Reference n _) a = n `elem` namesFromAbstractPattern a
refersTo _ _               = False

-- | Get a single name from an abstract pattern.
nameFromAbstractPattern :: (MonadFail m) => AbstractPattern -> m Name
nameFromAbstractPattern a = case namesFromAbstractPattern a of
                                 [n] -> pure n
                                 []  -> fail "[nameFromAbstractPattern] no names in abstract pattern"
                                 _   -> fail "[nameFromAbstractPattern] more than one name in abstract pattern"

-- | Get the list of names from an abstract pattern.
namesFromAbstractPattern :: AbstractPattern -> [Name]
namesFromAbstractPattern (Single n)        = [n]
namesFromAbstractPattern (AbsPatTuple ns)  = concatMap namesFromAbstractPattern ns
namesFromAbstractPattern (AbsPatMatrix ns) = concatMap namesFromAbstractPattern ns
namesFromAbstractPattern (AbsPatSet ns)    = concatMap namesFromAbstractPattern ns
namesFromAbstractPattern _                 = []

-- | Get the list of names from a generator.
namesFromGenerator :: Generator -> [Name]
namesFromGenerator (GenDomainNoRepr a _)  = namesFromAbstractPattern a
namesFromGenerator (GenDomainHasRepr n _) = [n]
namesFromGenerator (GenInExpr a _)        = namesFromAbstractPattern a

-- | Find an expression at any depth of unconditional forAll expressions.
findInUncondForAll :: (Expression -> Bool) -> [ExpressionZ] -> [Expression]
findInUncondForAll p = map hole . findInUncondForAllZ p

-- | Find an expression at any depth of unconditional forAll expressions,
--   returning a Zipper containing the expression's context.
findInUncondForAllZ :: (Expression -> Bool) -> [ExpressionZ] -> [ExpressionZ]
findInUncondForAllZ p = concatMap findInForAll
  where
    findInForAll z | p (hole z) = [z]
    findInForAll z
      = case hole z of
             [essence| forAll &_ in defined(&_) . &_ |]
                 -> []
             [essence| forAll &x, &y : &_, &x' != &y' . &_ |]
               | x' `refersTo` x && y' `refersTo` y
                 -> maybe [] findInForAll (down z >>= down)
             [essence| forAll &x, &y in &_, &x' != &y' . &_ |]
               | x' `refersTo` x && y' `refersTo` y
                 -> maybe [] findInForAll (down z >>= down)
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

-- | Lens function over a binary expression.
type BinExprLens m = Proxy m -> (Expression -> Expression -> Expression,
                                 Expression -> m (Expression, Expression))

-- | Get the lens for an expression and the values it matches.
matching :: Expression
         -> [(BinExprLens Maybe, a)]
         -> Maybe (a, (Expression, Expression))
matching e ops = case mapMaybe (\(f1, f2) -> (,) f2 <$> match f1 e) ops of
                      [x] -> pure x
                      _   -> fail $ "no matching operator for expression: " <+> pretty e

-- | (In)equality operator lens pairs.
ineqOps :: [(BinExprLens Maybe, BinExprLens Identity)]
ineqOps = [ (opEq,  opEq)
          , (opLt,  opLt)
          , (opLeq, opLeq)
          , (opGt,  opGt)
          , (opGeq, opGeq)
          ]

-- | Opposites of (in)equality operator lens pairs.
oppIneqOps :: [(BinExprLens Maybe, BinExprLens Identity)]
oppIneqOps = [ (opEq, opEq)
             , (opLt, opGt)
             , (opLeq, opGeq)
             , (opGt, opLt)
             , (opGeq, opLeq)
             ]

-- | (In)equality operator to size attribute modifier pairs.
ineqSizeAttrs :: [(BinExprLens Maybe, (AttrName, Expression -> Maybe Expression))]
ineqSizeAttrs = [ (opEq,  ("size",    Just))
                , (opLt,  ("maxSize", Just . \x -> x - 1))
                , (opLeq, ("maxSize", Just))
                , (opGt,  ("minSize", Just . (+ 1)))
                , (opGeq, ("minSize", Just))
                ]

-- | (In)equality operator to size attribute modifier pairs.
ineqOccurAttrs :: [(BinExprLens Maybe, [(AttrName, Expression -> Maybe Expression)])]
ineqOccurAttrs = [ (opEq,  [ ("minOccur", Just), ("maxOccur", Just) ])
                 , (opLt,  [ ("maxOccur", Just . \x -> x - 1) ])
                 , (opLeq, [ ("maxOccur", Just) ])
                 , (opGt,  [ ("minOccur", Just . (+ 1)) ])
                 , (opGeq, [ ("minOccur", Just) ])
                 ]

-- | Unzip where the key is a 'Maybe' but the values should all be combined.
unzipMaybeK :: Monoid m => [(Maybe a, m)] -> ([a], m)
unzipMaybeK = foldr (\(mx, y) (xs, z) ->
                     case mx of
                          Just x  -> (x:xs, y `mappend` z)
                          Nothing -> (  xs, y `mappend` z))
              ([], mempty)

-- | Does an expression contain a find variable?
isFind :: Expression -> Bool
isFind (Reference _ (Just (DeclNoRepr  Find _ _ _))) = True
isFind (Reference _ (Just (DeclHasRepr Find _ _)))   = True
isFind Reference{}                                   = False
isFind Constant{}                                    = False
isFind [essence| &f(&_) |]                           = isFind f
isFind [essence| image(&f, &_) |]                    = isFind f
isFind e                                             = any isFind $ children e

-- | Add expressions to the ToAdd list.
toAdd :: [ExpressionZ] -> ToAddToRem -> ToAddToRem
toAdd e = first (`union` e)

-- | Add expressions to the ToRemove list.
toRem :: [ExpressionZ] -> ToAddToRem -> ToAddToRem
toRem e = second (`union` e)

-- | Combine two 'ToAddToRem' values.
toAddRem :: ToAddToRem -> ToAddToRem -> ToAddToRem
toAddRem (ta, tr) = toAdd ta . toRem tr

-- | Apply a rule to arbitrary levels of nested domains.
nested :: (MonadFail m, MonadLog m, NameGen m)
       => (Model -> (FindVar, [ExpressionZ])
                 -> m ([AttrPair], ToAddToRem))
       -> Model
       -> (FindVar, [ExpressionZ])
       -> m ([(FindVar, Int, [AttrPair])], ToAddToRem)
nested rule m fc@(fv, cs) = do
  -- Apply the rule at the top level
  (attrs, toAddToRem) <- rule m fc
  -- Look deeper into the domain if possible, for forAll constraints involving it
  nestedResults <- fmap mconcat $ forM cs $ \c ->
    case hole c of
         [essence| forAll &x in &gen . &_ |] | nameExpEq (fst fv) gen ->
           -- Create the new decision variable at this level
           case (,) <$> nameFromAbstractPattern x
                    <*> (domainOf gen >>= innerDomainOf) of
                Left  _   -> return mempty
                Right fv' -> do
                            -- Apply the rule from here
                            out <- nested rule m (fv', mapMaybe (down >=> down) [c])
                            case out of
                                 ([], _)     -> return mempty
                                 -- The rule was applied, so unwrap the variable and increase the depth
                                 (vs, tatr') -> return ( [ (fv, d + 1, as) | (_, d, as) <- vs ]
                                                       , tatr')
         _ -> return mempty
  -- Do not add a modification if there are no attributes
  let attrs' = if null attrs then [] else [(fv, 0, attrs)]
  return $ mappend nestedResults (attrs', toAddToRem)

-- | If a function is surjective or bijective, and its domain and codomain
--   are of equal size, then it is total and bijective.
surjectiveIsTotalBijective :: (MonadFail m, MonadLog m)
                           => Model
                           -> (FindVar, [ExpressionZ])
                           -> m ([AttrPair], ToAddToRem)
surjectiveIsTotalBijective _ ((_, dom), _)
  = case dom of
         DomainFunction _ (FunctionAttr _ p j) from to
           | (p == PartialityAttr_Partial && j == JectivityAttr_Bijective) ||
             j == JectivityAttr_Surjective -> do
               (fromSize, toSize) <- functionDomainSizes from to
               if fromSize == toSize
                  then return ([("total", Nothing), ("bijective", Nothing)], mempty)
                  else return mempty
         _ -> return mempty

-- | Calculate the sizes of the domain and codomain of a function.
functionDomainSizes :: (MonadFail m)
                    => Domain () Expression       -- ^ The function's domain.
                    -> Domain () Expression       -- ^ The function's codomain.
                    -> m (Expression, Expression) -- ^ The sizes of the two.
functionDomainSizes from to = (,) <$> domainSizeOf from <*> domainSizeOf to

-- | If a function is total and injective, and its domain and codomain
--   are of equal size, then it is bijective.
totalInjectiveIsBijective :: (MonadFail m, MonadLog m)
                          => Model
                          -> (FindVar, [ExpressionZ])
                          -> m ([AttrPair], ToAddToRem)
totalInjectiveIsBijective _ ((_, dom), _)
  = case dom of
         DomainFunction _ (FunctionAttr _ PartialityAttr_Total JectivityAttr_Injective) from to -> do
           (fromSize, toSize) <- functionDomainSizes from to
           if fromSize == toSize
              then return ([("bijective", Nothing)], mempty)
              else return mempty
         _ -> return mempty

-- | If a function is defined for all values in its domain, then it is total.
definedForAllIsTotal :: (MonadFail m, MonadLog m)
                     => Model
                     -> (FindVar, [ExpressionZ])
                     -> m ([AttrPair], ToAddToRem)
definedForAllIsTotal _ ((n, dom), cs)
  -- Is the function called with parameters generated from its domain in an expression?
  = let definedIn from e = any (funcCalledWithGenParams from) (children e)
        in case dom of
                DomainFunction _ (FunctionAttr _ PartialityAttr_Partial _) from _
                  | any (definedIn from) $ findInUncondForAll isOp cs
                    -> return ([("total", Nothing)], mempty)
                _ -> return mempty
  where
    -- Look for operator expressions but leave comprehensions, ANDs and ORs up to findInUncondForAll
    isOp (Op (MkOpAnd (OpAnd Comprehension{}))) = False
    isOp [essence| &_ /\ &_ |]                  = False
    isOp [essence| &_ \/ &_ |]                  = False
    -- Disallow implications which may remove some cases
    isOp [essence| &_ -> &_ |]                  = False
    isOp Op{}                                   = True
    isOp _                                      = False
    -- Determine whether a function is called with values generated from its domain
    funcCalledWithGenParams d [essence| image(&f, &param) |]
      = nameExpEq n f && case domainOf param of
                              Right d' -> d' == d
                              Left _   -> False
    funcCalledWithGenParams _ _ = False

-- | If all distinct inputs to a function have distinct results, then it is injective.
--   It will also be total if there are no conditions other than the disequality between
--   the two inputs.
diffArgResultIsInjective :: (MonadFail m, MonadLog m)
                         => Model
                         -> (FindVar, [ExpressionZ])
                         -> m ([AttrPair], ToAddToRem)
diffArgResultIsInjective _ ((n, DomainFunction _ (FunctionAttr _ _ ject) from _), cs)
  | (ject == JectivityAttr_None || ject == JectivityAttr_Surjective) &&
    not (null $ findInUncondForAll isDistinctDisequality cs)
    -- It is known that no inputs are ignored
    = return ([("injective", Nothing), ("total", Nothing)], mempty)
  where
    -- Match a very specific pattern, which will also add the total attribute
    isDistinctDisequality [essence| &i != &j -> image(&f, &i') != image(&f', &j') |]
      = f == f' && i == i' && j == j' &&
        nameExpEq n f &&          -- the function is the one under consideration
        domIsGen i && domIsGen j  -- the values are generated from the function's domain
    isDistinctDisequality _ = False
    domIsGen x = case domainOf x of
                      Right dom -> dom == from
                      Left _    -> False
diffArgResultIsInjective _ _ = return mempty

-- | Set a size attribute on a variable.
varSize :: (MonadFail m, MonadLog m)
        => Model
        -> (FindVar, [ExpressionZ])
        -> m ([AttrPair], ToAddToRem)
varSize _ ((n, _), cs) = do
  results <- forM cs $ \c ->
    case matching (hole c) ineqSizeAttrs of
         -- Do not allow find variables to be put in attributes
         Just ((attr, f), ([essence| |&x| |], e)) | nameExpEq n x && not (isFind e)
           -> pure (Just (attr, f e), ([], [c]))
         _ -> pure (Nothing, mempty)
  return $ unzipMaybeK results

-- | Set the minimum size of a set based on it being a superset of another.
setSize :: (MonadFail m, MonadLog m, NameGen m)
        => Model
        -> (FindVar, [ExpressionZ])
        -> m ([AttrPair], ToAddToRem)
setSize _ ((n, DomainSet{}), cs)
  = fmap mconcat $ forM (findInUncondForAllZ isSubSupSet cs) $ \c ->
    case hole c of
         -- subset(Eq)
         [essence| &l subset   &r |] | nameExpEq n r -> return (minSize l (+ 1), mempty)
         [essence| &l subset   &r |] | nameExpEq n l -> return (maxSize r (\x -> x - 1), mempty)
         [essence| &l subsetEq &r |] | nameExpEq n r -> return (minSize l id, mempty)
         [essence| &l subsetEq &r |] | nameExpEq n l -> return (maxSize r id, mempty)
         -- supset(Eq)
         [essence| &l supset   &r |] | nameExpEq n l -> return (minSize r (+ 1), mempty)
         [essence| &l supset   &r |] | nameExpEq n r -> return (maxSize l (\x -> x - 1), mempty)
         [essence| &l supsetEq &r |] | nameExpEq n l -> return (minSize r id, mempty)
         [essence| &l supsetEq &r |] | nameExpEq n r -> return (maxSize l id, mempty)
         _                                           -> return mempty
  where
    isSubSupSet [essence| &_ subset   &_ |] = True
    isSubSupSet [essence| &_ subsetEq &_ |] = True
    isSubSupSet [essence| &_ supset   &_ |] = True
    isSubSupSet [essence| &_ supsetEq &_ |] = True
    isSubSupSet _                           = False
    minSize [essence| defined(&g) |] f
      = case domainOf g of
             Right (DomainFunction _ (FunctionAttr _ PartialityAttr_Total _) from _) ->
               case domainSizeOf from of
                    Just s  -> [("minSize", Just (f s))]
                    Nothing -> mempty
             _ -> mempty
    minSize [essence| range(&g) |] f
      = case domainOf g of
             Right (DomainFunction _ (FunctionAttr _ PartialityAttr_Total j) from to)
               | j == JectivityAttr_Bijective || j == JectivityAttr_Surjective ->
                 case domainSizeOf to of
                      Just s  -> [("minSize", Just (f s))]
                      Nothing -> mempty
               | j == JectivityAttr_Injective ->
                 case domainSizeOf from of
                      Just s  -> [("minSize", Just (f s))]
                      Nothing -> mempty
               | otherwise    -> [("minSize", Just (f 1))]
             _ -> mempty
    minSize e f = case domainOf e of
                       Right (DomainSet _ (SetAttr (SizeAttr_Size mn)) _) ->
                         [("minSize", Just (f mn))]
                       Right (DomainSet _ (SetAttr (SizeAttr_MinSize mn)) _) ->
                         [("minSize", Just (f mn))]
                       Right (DomainSet _ (SetAttr (SizeAttr_MinMaxSize mn _)) _) ->
                         [("minSize", Just (f mn))]
                       _ -> mempty
                       -- TODO: extend for Matrix, MSet, Partition and Sequence
    maxSize [essence| defined(&g) |] f
      = case domainOf g >>= innerDomainOf of
             Right (DomainTuple [d, _]) ->
               case domainSizeOf d of
                    Just s  -> [("maxSize", Just (f s))]
                    Nothing -> mempty
             _ -> mempty
    maxSize [essence| range(&g) |] f
      = case domainOf g >>= innerDomainOf of
             Right (DomainTuple [_, d]) ->
               case domainSizeOf d of
                    Just s  -> [("maxSize", Just (f s))]
                    Nothing -> mempty
             _ -> mempty
    maxSize e f = case domainOf e of
                       Right (DomainSet _ (SetAttr (SizeAttr_Size mx)) _) ->
                         [("maxSize", Just (f mx))]
                       Right (DomainSet _ (SetAttr (SizeAttr_MaxSize mx)) _) ->
                         [("maxSize", Just (f mx))]
                       Right (DomainSet _ (SetAttr (SizeAttr_MinMaxSize _ mx)) _) ->
                         [("maxSize", Just (f mx))]
                       Right d@(DomainSet _ (SetAttr SizeAttr_None) _) ->
                         case domainSizeOf d of
                              Just mx -> [("maxSize", Just (f mx))]
                              Nothing -> mempty
                       _ -> mempty
                       -- TODO: extend for Matrix, MSet, Partition and Sequence
setSize _ _ = return mempty

-- | The maxSize, and minOccur attributes of an mset affect its maxOccur and minSize attributes.
mSetSizeOccur :: (MonadFail m, MonadLog m)
              => Model
              -> (FindVar, [ExpressionZ])
              -> m ([AttrPair], ToAddToRem)
mSetSizeOccur _ ((_, d), _)
  = case d of
         -- Ordering is important here, as there is a rule that applies
         -- to maxSize and minOccur, but none that applies to minSize
         -- and maxOccur. size uses the maxSize rule, but can ignore a
         -- minOccur because it cannot have its minSize changed.
         -- size -> maxOccur
         DomainMSet _ (MSetAttr (SizeAttr_Size mx) _) _
           -> return ([("maxOccur", Just mx)], mempty)
         -- minOccur -> minSize
         DomainMSet _ (MSetAttr _ (OccurAttr_MinOccur mn)) _
           -> return ([("minSize", Just mn)], mempty)
         DomainMSet _ (MSetAttr _ (OccurAttr_MinMaxOccur mn _)) _
           -> return ([("minSize", Just mn)], mempty)
         -- maxSize -> maxOccur
         DomainMSet _ (MSetAttr (SizeAttr_MaxSize mx) _) _
           -> return ([("maxOccur", Just mx)], mempty)
         DomainMSet _ (MSetAttr (SizeAttr_MinMaxSize _ mx) _) _
           -> return ([("maxOccur", Just mx)], mempty)
         _ -> return mempty

-- | Infer multiset occurrence attributes from constraints.
mSetOccur :: (MonadFail m, MonadLog m)
          => Model
          -> (FindVar, [ExpressionZ])
          -> m ([AttrPair], ToAddToRem)
mSetOccur _ ((n, DomainMSet _ _ d), cs)
  = return $ mconcat $ flip mapMaybe (findInUncondForAllZ (not . null . isFreq) cs) $ \e ->
      case isFreq (hole e) of
           [] -> Nothing
           -- Only remove constraints if they are all used up.
           -- Because freq(a, b) = c adds two attributes, removing constraints
           -- in an AND expression cannot work, in the case of freq(a, b) = c /\ e
           -- because there are two attributes and two terms, but term e may not
           -- be removed.
           as -> let tattr = case hole e of
                                  AbstractLiteral AbsLitMatrix{} -> mempty
                                  _                              -> ([], [e])
                     in Just (as, tattr)
  where
    isFreq :: Expression -> [AttrPair]
    isFreq (AbstractLiteral (AbsLitMatrix _ es)) = concatMap isFreq es
    isFreq e = case matching e oppIneqOps of
                    Just (_, ([essence| freq(&x, &v) |], e'))
                      | valid x v e' -> case matching e ineqOccurAttrs of
                                             Just (as, _) -> map (second ($ e')) as
                                             Nothing      -> []
                    -- Flip the terms
                    Just (oper, (l, r@[essence| freq(&x, &v) |]))
                      | valid x v l -> isFreq $ make oper r l
                    _               -> []
    -- Make sure that the expression's components are valid
    valid :: Expression -> Expression -> Expression -> Bool
    valid x v e = nameExpEq n x && isGen v && isConst e
    -- Make sure that the value is generated from the mset's domain
    isGen (Reference _ (Just (InComprehension (GenDomainNoRepr _ d')))) = d == d'
    isGen (Reference _ (Just (DeclNoRepr Quantified _ d' _)))           = d == d'
    isGen (Reference _ (Just (InComprehension (GenInExpr _ e ))))       = nameExpEq n e
    isGen _                                                             = False
    -- Make sure that the mset is being equated to a constant
    isConst (Reference _ (Just (DeclNoRepr Given _ _ _))) = True
    isConst (Constant ConstantInt{})                      = True
    isConst _                                             = False
mSetOccur _ _ = return mempty

-- | Mark a partition regular if its numParts * partSize = |domain|, or if there
--   is a constraint on its parts constraining them to be of equal size.
partRegular :: (MonadFail m, MonadLog m)
            => Model
            -> (FindVar, [ExpressionZ])
            -> m ([AttrPair], ToAddToRem)
partRegular _ ((n, d@DomainPartition{}), cs)
  = if inferFromDomain /= mempty
       then return inferFromDomain
       else do
            attrs <- forM cs $ \c ->
              case hole c of
                   [essence| forAll &x in parts(&p) . forAll &y in parts(&p') . &e |]
                     | nameExpEq n p && p == p'
                       -> case e of
                               [essence| |&x'| = |&y'| |] | x' `refersTo` x && y' `refersTo` y
                                   -> pure (Just ("regular", Nothing), ([], [c]))
                               [essence| &x'' != &y'' -> |&x'| = |&y'| |]
                                 | x' `refersTo` x && y' `refersTo` y &&
                                   ((x' == x'' && y' == y'') || (x' == y'' && y' == x''))
                                   -> pure (Just ("regular", Nothing), ([], [c]))
                               _ -> pure (Nothing, mempty)
                   _   -> pure (Nothing, mempty)
            return $ unzipMaybeK attrs
  where
    inferFromDomain :: ([AttrPair], ToAddToRem)
    inferFromDomain = case d of
                           DomainPartition _ PartitionAttr { partsNum = SizeAttr_Size pNum@Constant{}
                                                           , partsSize = SizeAttr_Size pSize@Constant{}
                                                           } dom
                             | Just n1 <- domainSizeOf dom >>= e2c
                             , Just pNum' <- e2c pNum
                             , Just pSize' <- e2c pSize
                             , Just n2 <- evaluateOp $ MkOpProduct $ OpProduct $ fromList [ pNum', pSize' ]
                             , n1 == n2
                               -> ([("regular", Nothing)], mempty)
                           _   -> mempty
partRegular _ _ = return mempty

-- | Convert constraints acting on the number of parts in a partition to an attribute.
numPartsToAttr :: (MonadFail m, MonadLog m)
               => Model
               -> (FindVar, [ExpressionZ])
               -> m ([AttrPair], ToAddToRem)
numPartsToAttr _ ((n, DomainPartition{}), cs) = do
  attrs <- forM cs $ \c ->
    case matching (hole c) ineqSizeAttrs of
         -- Do not allow find variables to be put in attributes
         Just ((attr, f), ([essence| |parts(&x)| |], e)) | nameExpEq n x && not (isFind e)
           -> pure (Just (changeAttr attr, f e), ([], [c]))
         _ -> pure (Nothing, mempty)
  return $ unzipMaybeK attrs
  where
    -- Change a size attribute name to a numParts attribute name
    changeAttr "size"    = "numParts"
    changeAttr "minSize" = "minNumParts"
    changeAttr "maxSize" = "maxNumParts"
    changeAttr a         = a
numPartsToAttr _ _ = return mempty

-- | Convert constraints acting on the sizes of parts in a partition to an attribute.
partSizeToAttr :: (MonadFail m, MonadLog m)
               => Model
               -> (FindVar, [ExpressionZ])
               -> m ([AttrPair], ToAddToRem)
partSizeToAttr _ ((n, DomainPartition{}), cs) = do
  attrs <- forM cs $ \c ->
    case hole c of
         [essence| forAll &x in parts(&p) . |&x'| =  &e |] | valid p x x' e
           -> pure (Just ("partSize", Just e), ([], [c]))
         [essence| forAll &x in parts(&p) . |&x'| <  &e |] | valid p x x' e
           -> pure (Just ("maxPartSize", Just (e - 1)), ([], [c]))
         [essence| forAll &x in parts(&p) . |&x'| <= &e |] | valid p x x' e
           -> pure (Just ("maxPartSize", Just e), ([], [c]))
         [essence| forAll &x in parts(&p) . |&x'| >  &e |] | valid p x x' e
           -> pure (Just ("minPartSize", Just (e + 1)), ([], [c]))
         [essence| forAll &x in parts(&p) . |&x'| >= &e |] | valid p x x' e
           -> pure (Just ("minPartSize", Just e), ([], [c]))
         _ -> pure (Nothing, mempty)
  return $ unzipMaybeK attrs
  where
    -- Make sure that the expression's components are valid
    valid :: Expression -> AbstractPattern -> Expression -> Expression -> Bool
    valid p x v e = nameExpEq n p && v `refersTo` x && not (isFind e)
partSizeToAttr _ _ = return mempty

-- | Equate the range of a function to a set of the former is a subset of the latter
--   and all values in the set are results of the function.
funcRangeEqSet :: (MonadFail m, MonadLog m)
               => Model
               -> (FindVar, [ExpressionZ])
               -> m ([AttrPair], ToAddToRem)
funcRangeEqSet _ ((n, DomainSet{}), cs)
  -- Get references to the set and the function whose range it is a superset of
  = let funcSubsets = mapMaybe funcSubsetEqOf $
                      findInUncondForAllZ (isJust . funcSubsetEqOf . zipper) cs
        -- Reduce the functions to those whose values are equated to the values in the set
        fsToUse = flip filter funcSubsets $ \(_, f) ->
                  not $ null $ findInUncondForAll (funcValEqSetVal (hole f)) cs
        -- Transform the functions into new constraints, preserving structure
        csToAdd = flip mapMaybe fsToUse $ \(s, f) ->
                  let f' = hole f
                      in replaceHole [essence| range(&f') = &s |] <$>
                         (up f >>= up)
        in return ([], (csToAdd, []))
  where
    -- Get the function whose range is a subsetEq of the set
    funcSubsetEqOf z = case hole z of
                            [essence| range(&_) subsetEq &s |] | nameExpEq n s
                              -> (,) s <$> (down z >>= down)
                            [essence| &s supsetEq range(&_) |] | nameExpEq n s
                              -> (,) s <$> (down z >>= right >>= down)
                            _ -> Nothing
    -- Are the values of the function equal to the values of the set?
    funcValEqSetVal f [essence| forAll &x in &s . image(&f', &_) = &x' |]
      = nameExpEq n s && f == f' && x' `refersTo` x
    funcValEqSetVal f [essence| forAll &x in &s . &x' = image(&f', &_) |]
      = nameExpEq n s && f == f' && x' `refersTo` x
    funcValEqSetVal _ _ = False
funcRangeEqSet _ _ = return mempty


-- | An (in)equality in a forAll implies that the (in)equality also applies to
--   the sums of both terms.
forAllIneqToIneqSum :: (MonadFail m, MonadLog m, NameGen m)
                    => Model
                    -> (FindVar, [ExpressionZ])
                    -> m ([AttrPair], ToAddToRem)
forAllIneqToIneqSum _ (_, cs) = do
  let matches = mapMaybe matchParts $ findInUncondForAllZ (isJust . matchParts . zipper) cs
  csToAdd <- mapMaybe mkConstraint <$> filterM partsAreNumeric matches
  return ([], (csToAdd, []))
  where
    -- Match and extract the desired parts of the expression
    matchParts :: ExpressionZ -> Maybe (Generator, Maybe ExpressionZ, Expression, Expression)
    matchParts z = case hole z of
                        Op (MkOpAnd (OpAnd (Comprehension e [Generator g])))
                          -> matching e ineqOps >>=
                             uncurry (matchComponents g z) . snd
                        _ -> Nothing
    -- Match the components of the expression of interest
    matchComponents :: Generator -> ExpressionZ -> Expression -> Expression
                    -> Maybe (Generator, Maybe ExpressionZ, Expression, Expression)
    matchComponents g z e1 e2
      | refInExpr (namesFromGenerator g) e1 && refInExpr (namesFromGenerator g) e2
        = Just (g, down z >>= down, e1, e2)
    matchComponents _ _ _ _ = Nothing
    -- Is a name referred to in an expression?
    refInExpr names = any (\e -> any (`nameExpEq` e) names) . universe
    -- Are the parts of the matched expression numeric?
    partsAreNumeric (_, _, e1, e2) = (&&) <$> domainIsNumeric e1 <*> domainIsNumeric e2
    domainIsNumeric e = case domainOf e of
                             Right DomainInt{}           -> return True
                             Right (DomainAny _ TypeInt) -> return True
                             _                           -> return False
    -- Replace the forAll with the (in)equality between sums
    mkConstraint :: (Generator, Maybe ExpressionZ, Expression, Expression) -> Maybe ExpressionZ
    mkConstraint (gen, Just z, _, _)
      -- Use matching with ineqOps to get the operation that is used on the two expressions
      = case matching (hole z) ineqOps of
             Just (f, (e1, e2))
               -> let mkSumOf = Op . MkOpSum . OpSum . flip Comprehension [Generator gen]
                      -- Two steps to get out of the forAll, and replace it with the constraint
                      in replaceHole (make f (mkSumOf e1) (mkSumOf e2)) <$> (up z >>= up)
             _ -> Nothing
    mkConstraint _ = Nothing

-- | Iterate slightly faster over a domain if generating two distinct variables.
fasterIteration :: (MonadFail m, MonadIO m, MonadLog m)
                => Model
                -> (FindVar, [ExpressionZ])
                -> m ([AttrPair], ToAddToRem)
fasterIteration m (_, cs) = do
  let iters = findInUncondForAllZ (isJust . doubleDistinctIter . zipper) cs
  fmap ((,) [] . mconcat) $ forM iters $ \z -> do
    -- Find the equivalent variables
    [equivs] <- sequence [ findEquivVars (hole z) ]
    -- Only apply to equivalent variables and make the new constraint
    case doubleDistinctIter z >>= onlyEquivalent equivs >>= changeIterator of
         Nothing -> return mempty
         -- Remove the old constraint
         Just z' -> return ([z'], [z])
  where
    -- Match the elemenents of interest in the constraint
    doubleDistinctIter z
      = case hole z of
             Op (MkOpAnd (OpAnd (Comprehension _ [ Generator (GenInExpr x v)
                                                 , Generator (GenInExpr y v')
                                                 , Condition [essence| &x' != &y' |]
                                                 ])))
               | v == v' && x' `refersTo` x && y' `refersTo` y
                 -> Just ((x, x'), (y, y'), v, down z >>= down)
             Op (MkOpAnd (OpAnd (Comprehension _ [ Generator (GenDomainNoRepr x d)
                                                 , Generator (GenDomainNoRepr y d')
                                                 , Condition [essence| &x' != &y' |]
                                                 ])))
               | d == d' && x' `refersTo` x && y' `refersTo` y
                 -> Just ((x, x'), (y, y'), Domain d, down z >>= down)
             _ -> Nothing
    -- Find which variables are equivalent in an expression
    findEquivVars :: (MonadIO m) => Expression -> m (Map Text Text)
    findEquivVars e = case e of
                           [essence| forAll &_, &_ : &_, &_ . &e' |]  -> liftIO $ findSyms e'
                           [essence| forAll &_, &_ in &_, &_ . &e' |] -> liftIO $ findSyms e'
                           _ -> return M.empty
    -- Find the symmetries in an expression
    findSyms :: Expression -> IO (Map Text Text)
    findSyms e = do
      let m' = addConstraints [zipper e] $ remConstraints cs m
      let filename = ".tmp-variable-strengthening.json"
      outputVarSymBreaking filename m'
      symmetries <- ferret $ stringToText filename
      removeFile filename
      case (JSON.decodeStrict $ T.encodeUtf8 symmetries) :: Maybe [Map Text Text] of
           Nothing -> return M.empty
           Just ss -> return $ foldr M.union M.empty ss
    -- Only perform the modification if the variables are equivalent in the expression
    onlyEquivalent es v@((x, _), (y, _), _, _)
      = case namesFromAbstractPattern x of
             [Name nx] -> case namesFromAbstractPattern y of
                               [Name ny] -> case es M.!? nx of
                                                 Just ny' | ny == ny' -> Just v
                                                 _ -> Nothing
                               _         -> Nothing
             _         -> Nothing
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

-- | Call ferret's symmetry detection on a JSON file
ferret :: Text -> IO Text
ferret path = sh (run "symmetry_detect" [ "--json", path ]) `catch`
              (\(_ :: SomeException) -> return "{}")

-- | Change the type of a multiset with `maxOccur 1` to set.
mSetToSet :: (MonadFail m, MonadLog m)
          => Model
          -> (FindVar, [ExpressionZ])
          -> m (Domain () Expression, ToAddToRem)
mSetToSet _ ((n, DomainMSet r (MSetAttr sa oa) d), cs) | maxOccur1 oa = do
  let dom'  = DomainSet r (SetAttr sa) d
  let torem = filter (any (nameExpEq n) . universe . hole) cs
  let toadd = map (zipper . transform (\e -> if nameExpEq n e
                                                then [essence| toMSet(&e) |]
                                                else e)
                          . hole)
                  cs
  return (dom', (toadd, torem))
  where
    maxOccur1 (OccurAttr_MaxOccur 1)      = True
    maxOccur1 (OccurAttr_MinMaxOccur _ 1) = True
    maxOccur1 _                           = False
mSetToSet _ ((_, dom), _) = return (dom, mempty)
