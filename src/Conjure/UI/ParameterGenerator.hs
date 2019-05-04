{-# LANGUAGE QuasiQuotes #-}

module Conjure.UI.ParameterGenerator where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Language.Instantiate ( trySimplify )
import Conjure.Process.Enumerate ( EnumerateDomain )
-- import Conjure.Language.Expression.DomainSizeOf ( domainSizeOf )

-- text
import Data.Text ( pack )


-- | This doesn't do anything to do with correcting categories at the moment, it should.
--   An example:
--      given n : int(1..10)
--      given s : set (size n) of int(1..10)
--   Should output:
--      find n : int(1..10)
--      find s : set (minSize 1, maxSize 10) of int(1..10)
--      such that n = |s|
--   (Just dropping wrong category stuff from attribute list isn't acceptable, because mset.)
parameterGenerator ::
    MonadLog m =>
    MonadFail m =>
    MonadUserError m =>
    EnumerateDomain m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Integer ->      -- MININT
    Integer ->      -- MAXINT
    Model -> m Model
parameterGenerator minIntValue maxIntValue model = runNameGen () (resolveNames model) >>= core >>= evaluateBounds
    where
        core m = do
            outStatements <- forM (mStatements m) $ \ st -> case st of
                Declaration (FindOrGiven Given nm dom) -> do
                    (dom', decls, cons) <- pgOnDomain (Reference nm Nothing) nm dom
                    return $ decls ++ [Declaration (FindOrGiven Find nm dom'), SuchThat cons]
                Declaration (FindOrGiven Find  _  _  ) -> return []
                Declaration (Letting _ _)              -> return []
                Declaration       {}                   -> return [st]
                SearchOrder       {}                   -> return []
                SearchHeuristic   {}                   -> return []
                Where             xs                   -> return [SuchThat xs]
                Objective         {}                   -> return []
                SuchThat          {}                   -> return []
            return m { mStatements = concat outStatements }

        evaluateBounds m = do
            let symbols = [("MININT", fromInt minIntValue), ("MAXINT", fromInt maxIntValue)]
            let eval = transformBiM (trySimplify symbols)
            stmtsEvaluated <- mapM eval (mStatements m)
            return m { mStatements = stmtsEvaluated }


pgOnDomain ::
    MonadUserError m =>
    NameGen m =>
    Expression ->                       -- how do we refer to this top level variable
    Name ->                             -- its name
    Domain () Expression ->             -- its domain
    m ( Domain () Expression            -- its modified domain for the find version
      , [Statement]                     -- statements that define the necessary givens
      , [Expression]                    -- constraints
      )
pgOnDomain x nm dom =

    case dom of

        DomainBool -> return3 DomainBool [] []

        DomainInt t _ -> do
            lbX <- minOfIntDomain dom
            ubX <- maxOfIntDomain dom
            lb  <- lowerBoundOfIntExpr lbX
            ub  <- upperBoundOfIntExpr ubX
            let nmMiddle = nm `mappend` "_middle"
            let nmDelta  = nm `mappend` "_delta"
            let middle = Reference nmMiddle Nothing
            let delta = Reference nmDelta Nothing
            return3
                (DomainInt t [RangeBounded lb ub])
                [ Declaration (FindOrGiven Given nmMiddle (DomainInt t [RangeBounded lb ub]))
                , Declaration (FindOrGiven Given nmDelta  (DomainInt t [RangeBounded 0 [essence| (&ub - &lb) / 2 |]]))
                ]
                $ [ [essence| &x >= &middle - &delta |]
                  , [essence| &x <= &middle + &delta |]
                  ] ++
                  [ [essence| &x >= &lbX |]
                  | lb /= lbX
                  ] ++
                  [ [essence| &x <= &ubX |]
                  | ub /= ubX
                  ]

        DomainTuple ds -> do
            inners <- forM (zip [1..] ds) $ \ (i, d) -> do
                let iE = fromInt i
                let ref = [essence| &x[&iE] |]
                pgOnDomain ref (nm `mappend` (Name $ pack $ "_tuple" ++ show i)) d
            return3
                (DomainTuple (map fst3 inners))
                (concatMap snd3 inners)
                (concatMap thd3 inners)

        DomainMatrix indexDomain innerDomain -> do
            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat : &indexDomain . &c |]
            let ref = [essence| &x[&i] |]
            (innerDomain', declInner, consInner) <- pgOnDomain ref (nm `mappend` "_inner") innerDomain
            return3
                (DomainMatrix indexDomain innerDomain')
                declInner
                (map liftCons consInner)

        DomainSequence r attr innerDomain -> do

            let nmCardMiddle = nm `mappend` "_cardMiddle"
            let nmCardDelta  = nm `mappend` "_cardDelta"
            let cardMiddle = Reference nmCardMiddle Nothing
            let cardDelta = Reference nmCardDelta Nothing

            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]
            (domInner, declInner, consInner) <- pgOnDomain i (nm `mappend` "_inner") innerDomain

            (attrOut, sizeLb, sizeUb, cardDomain) <-
                    case attr of
                        SequenceAttr size jectivity -> do
                            (sizeOut, lb, ub, cardDomain) <-
                                case size of
                                    SizeAttr_None ->
                                        return ( SizeAttr_None, Nothing, Nothing
                                               , DomainInt TagInt [RangeBounded minInt maxInt]
                                               )
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just a
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb maxInt, Just a, Nothing
                                               , DomainInt TagInt [RangeBounded lb maxInt]
                                               )
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MaxSize ub, Nothing, Just a
                                               , DomainInt TagInt [RangeBounded 0 ub]
                                               )
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just b
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                            return (SequenceAttr sizeOut jectivity, lb, ub, cardDomain)

            let
                deltaDomain = DomainInt TagInt [RangeBounded 0 3]
                newDecl =
                    [ Declaration (FindOrGiven Given nmCardMiddle cardDomain)
                    , Declaration (FindOrGiven Given nmCardDelta deltaDomain)
                    ]

            let
                cardinalityCons = return $ return
                    [essence|
                        |&x| >= &cardMiddle - &cardDelta /\
                        |&x| <= &cardMiddle + &cardDelta
                    |]

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

            newCons <- concat <$> sequence [cardinalityCons, sizeLbCons, sizeUbCons]

            return3
                (DomainSequence r attrOut domInner)
                (newDecl ++ declInner)
                (newCons ++ map liftCons consInner)

        DomainSet r attr innerDomain -> do

            let nmCardMiddle = nm `mappend` "_cardMiddle"
            let nmCardDelta  = nm `mappend` "_cardDelta"
            let cardMiddle = Reference nmCardMiddle Nothing
            let cardDelta = Reference nmCardDelta Nothing

            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]
            (domInner, declInner, consInner) <- pgOnDomain i (nm `mappend` "_inner") innerDomain

            (attrOut, sizeLb, sizeUb, cardDomain) <-
                    case attr of
                        SetAttr size -> do
                            (sizeOut, lb, ub, cardDomain) <-
                                case size of
                                    SizeAttr_None ->
                                        return ( SizeAttr_None, Nothing, Nothing
                                               , DomainInt TagInt [RangeBounded minInt maxInt]
                                               )
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just a
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb maxInt, Just a, Nothing
                                               , DomainInt TagInt [RangeBounded lb maxInt]
                                               )
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MaxSize ub, Nothing, Just a
                                               , DomainInt TagInt [RangeBounded 0 ub]
                                               )
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just b
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                            return (SetAttr sizeOut, lb, ub, cardDomain)

            let
                deltaDomain = DomainInt TagInt [RangeBounded 0 3]
                newDecl =
                    [ Declaration (FindOrGiven Given nmCardMiddle cardDomain)
                    , Declaration (FindOrGiven Given nmCardDelta deltaDomain)
                    ]

            let
                cardinalityCons = return $ return
                    [essence|
                        |&x| >= &cardMiddle - &cardDelta /\
                        |&x| <= &cardMiddle + &cardDelta
                    |]

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

            newCons <- concat <$> sequence [cardinalityCons, sizeLbCons, sizeUbCons]

            return3
                (DomainSet r attrOut domInner)
                (newDecl ++ declInner)
                (newCons ++ map liftCons consInner)

        DomainMSet r attr innerDomain -> do

            let nmCardMiddle = nm `mappend` "_cardMiddle"
            let nmCardDelta  = nm `mappend` "_cardDelta"
            let cardMiddle = Reference nmCardMiddle Nothing
            let cardDelta = Reference nmCardDelta Nothing

            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]
            (domInner, declInner, consInner) <- pgOnDomain i (nm `mappend` "_inner") innerDomain

            (attrOut, sizeLb, sizeUb, cardDomain, occurLb, occurUb) <-
                    case attr of
                        MSetAttr sizeAttr occurAttr -> do
                            (sizeAttrOut, sizeLb, sizeUb, cardDomain) <-
                                case sizeAttr of
                                    SizeAttr_None ->
                                        return ( SizeAttr_None, Nothing, Nothing
                                               , DomainInt TagInt [RangeBounded minInt maxInt]
                                               )
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just a
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb maxInt, Just a, Nothing
                                               , DomainInt TagInt [RangeBounded lb maxInt]
                                               )
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MaxSize ub, Nothing, Just a
                                               , DomainInt TagInt [RangeBounded 0 ub]
                                               )
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just b
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                            (occurAttrOut, occurLb, occurUb) <-
                                case occurAttr of
                                    OccurAttr_None ->
                                        return (OccurAttr_MaxOccur maxInt, Nothing, Nothing)
                                    OccurAttr_MinOccur a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return (OccurAttr_MinMaxOccur lb maxInt, Just a, Nothing)
                                    OccurAttr_MaxOccur a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return (OccurAttr_MaxOccur ub, Nothing, Just a)
                                    OccurAttr_MinMaxOccur a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return (OccurAttr_MinMaxOccur lb ub, Just a, Just b)
                            return (MSetAttr sizeAttrOut occurAttrOut, sizeLb, sizeUb, cardDomain, occurLb, occurUb)

            let
                deltaDomain = DomainInt TagInt [RangeBounded 0 3]
                newDecl =
                    [ Declaration (FindOrGiven Given nmCardMiddle cardDomain)
                    , Declaration (FindOrGiven Given nmCardDelta deltaDomain)
                    ]

            let
                cardinalityCons = return $ return
                    [essence|
                        |&x| >= &cardMiddle - &cardDelta /\
                        |&x| <= &cardMiddle + &cardDelta
                    |]

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

                occurLbCons =
                    case occurLb of
                        Nothing -> return []
                        Just bound -> return $ return $ liftCons [essence| freq(&x, &i) >= &bound |]

                occurUbCons =
                    case occurUb of
                        Nothing -> return []
                        Just bound -> return $ return $ liftCons [essence| freq(&x, &i) <= &bound |]

            newCons <- concat <$> sequence [cardinalityCons, sizeLbCons, sizeUbCons, occurLbCons, occurUbCons]

            return3
                (DomainMSet r attrOut domInner)
                (newDecl ++ declInner)
                (newCons ++ map liftCons consInner)

        DomainFunction r attr innerDomainFr innerDomainTo -> do

            let nmCardMiddle = nm `mappend` "_cardMiddle"
            let nmCardDelta  = nm `mappend` "_cardDelta"
            let cardMiddle = Reference nmCardMiddle Nothing
            let cardDelta = Reference nmCardDelta Nothing

            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]
            (domFr, declFr, consFr) <- pgOnDomain [essence| &i[1] |] (nm `mappend` "_defined") innerDomainFr
            (domTo, declTo, consTo) <- pgOnDomain [essence| &i[2] |] (nm `mappend` "_range") innerDomainTo

            -- drop total, post constraint instead
            (attrOut, sizeLb, sizeUb, cardDomain) <-
                    case attr of
                        FunctionAttr size _totality jectivity -> do
                            (sizeOut, lb, ub, cardDomain) <-
                                case size of
                                    SizeAttr_None ->
                                        return ( SizeAttr_None, Nothing, Nothing
                                               , DomainInt TagInt [RangeBounded minInt maxInt]
                                               )
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just a
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb maxInt, Just a, Nothing
                                               , DomainInt TagInt [RangeBounded lb maxInt]
                                               )
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MaxSize ub, Nothing, Just a
                                               , DomainInt TagInt [RangeBounded 0 ub]
                                               )
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just b
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                            return (FunctionAttr sizeOut PartialityAttr_Partial jectivity, lb, ub, cardDomain)

            let isTotal = case attr of
                                FunctionAttr _ PartialityAttr_Total _ -> True
                                _ -> False
                isPartial = not isTotal

            let
                deltaDomain = DomainInt TagInt [RangeBounded 0 3]
                newDecl | isTotal = []
                        | otherwise =
                            [ Declaration (FindOrGiven Given nmCardMiddle cardDomain)
                            , Declaration (FindOrGiven Given nmCardDelta deltaDomain)
                            ]

            let
                cardinalityCons | isTotal = return []
                                | otherwise = return $ return
                    [essence|
                        |&x| >= &cardMiddle - &cardDelta /\
                        |&x| <= &cardMiddle + &cardDelta
                    |]

                totalityCons | isPartial = return []
                             | otherwise = do

                    let
                        go d = case d of
                            DomainInt{} -> do
                                dMin <- minOfIntDomain d
                                dMax <- maxOfIntDomain d
                                return (\ k -> [essence| (&k >= &dMin /\ &k <= &dMax) |] )
                            DomainTuple ds -> do
                                makers <- mapM go ds
                                return $ \ k -> make opAnd $ fromList
                                    [ mk [essence| &k[&nExpr] |]
                                    | (mk, n) <- zip makers [1..]
                                    , let nExpr = fromInt n
                                    ]
                            _ -> bug $ "Unhandled domain (in function defined):" <+> vcat [pretty d, pretty (show d)]

                    mkCondition <- go innerDomainFr

                    let iCondition = mkCondition i

                    return $ return [essence|
                        forAll &iPat : &domFr .
                            &iCondition
                            <->
                            &i in defined(&x)
                        |]

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

            newCons <- concat <$> sequence [cardinalityCons, totalityCons, sizeLbCons, sizeUbCons]
            let innerCons = concat $ concat
                    [ [consFr | isPartial ] -- only if the function is not total
                    , [consTo]
                    ]

            return3
                (DomainFunction r attrOut domFr domTo)
                (newDecl ++ concat [ declFr | isPartial ] ++ declTo)
                (newCons ++ map liftCons innerCons)

        DomainRelation r attr innerDomains -> do

            let nmCardMiddle = nm `mappend` "_cardMiddle"
            let nmCardDelta  = nm `mappend` "_cardDelta"
            let cardMiddle = Reference nmCardMiddle Nothing
            let cardDelta = Reference nmCardDelta Nothing

            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]

            inners <- forM (zip [1..] innerDomains) $ \ (n, d) -> do
                let nE = fromInt n
                let ref = [essence| &i[&nE] |]
                (d', decl, cons) <- pgOnDomain ref (nm `mappend` (Name $ pack $ "_relation" ++ show i)) d
                return (d', decl, map liftCons cons)

            (attrOut, sizeLb, sizeUb, cardDomain) <-
                    case attr of
                        RelationAttr size binRelAttr -> do
                            (sizeOut, lb, ub, cardDomain) <-
                                case size of
                                    SizeAttr_None ->
                                        return ( SizeAttr_None, Nothing, Nothing
                                               , DomainInt TagInt [RangeBounded minInt maxInt]
                                               )
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just a
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb maxInt, Just a, Nothing
                                               , DomainInt TagInt [RangeBounded lb maxInt]
                                               )
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MaxSize ub, Nothing, Just a
                                               , DomainInt TagInt [RangeBounded 0 ub]
                                               )
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just b
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                            return (RelationAttr sizeOut binRelAttr, lb, ub, cardDomain)

            let
                deltaDomain = DomainInt TagInt [RangeBounded 0 3]
                newDecl =
                    [ Declaration (FindOrGiven Given nmCardMiddle cardDomain)
                    , Declaration (FindOrGiven Given nmCardDelta deltaDomain)
                    ]

            let
                cardinalityCons = return $ return
                    [essence|
                        |&x| >= &cardMiddle - &cardDelta /\
                        |&x| <= &cardMiddle + &cardDelta
                    |]

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

            newCons <- concat <$> sequence [cardinalityCons, sizeLbCons, sizeUbCons]

            return3
                (DomainRelation r attrOut (map fst3 inners))
                (newDecl ++ concatMap snd3 inners)
                (newCons ++ concatMap thd3 inners)

        _ -> userErr1 $ "Unhandled domain:" <++> vcat [ pretty dom
                                                      , pretty (show dom)
                                                      ]


-- helper functions

return3 :: Monad m => a -> b -> c -> m (a,b,c)
return3 x y z = return (x,y,z)

minInt :: Expression
minInt = Reference "MININT" Nothing

maxInt :: Expression
maxInt = Reference "MAXINT" Nothing


minOfIntDomain :: MonadUserError m => Domain () Expression -> m Expression
minOfIntDomain (DomainInt _ rs) = do
    xs <- sortNub <$> mapM minOfIntRange rs
    case xs of
        []  -> return minInt
        [x] -> return x
        _   -> return $ make opMax $ fromList xs
minOfIntDomain d = userErr1 $ "Expected integer domain, but got:" <++> vcat [pretty d, pretty (show d)]

minOfIntRange :: Monad m => Range Expression -> m Expression
minOfIntRange (RangeSingle lb) = return lb
minOfIntRange (RangeLowerBounded lb) = return lb
minOfIntRange (RangeBounded lb _) = return lb
minOfIntRange _ = return minInt

lowerBoundOfIntExpr :: MonadUserError m => Expression -> m Expression
lowerBoundOfIntExpr x@Constant{} = return x
lowerBoundOfIntExpr x | x == minInt = return minInt
lowerBoundOfIntExpr x | x == maxInt = return maxInt
lowerBoundOfIntExpr (Reference _ (Just (DeclNoRepr Given _ dom _))) = minOfIntDomain dom
lowerBoundOfIntExpr (Reference _ (Just (Alias x))) = lowerBoundOfIntExpr x
lowerBoundOfIntExpr (Op (MkOpSum (OpSum x))) | Just xs <- listOut x = do
    bounds <- mapM lowerBoundOfIntExpr xs
    return $ make opSum $ fromList bounds
-- TODO: check for negatives
lowerBoundOfIntExpr (Op (MkOpProduct (OpProduct x))) | Just xs <- listOut x = do
    bounds <- mapM lowerBoundOfIntExpr xs
    return $ make opProduct $ fromList bounds
lowerBoundOfIntExpr (Op (MkOpNegate (OpNegate x))) = do
    bound <- upperBoundOfIntExpr x
    return (make opNegate bound)
lowerBoundOfIntExpr x = userErr1 $ "Cannot compute lower bound of integer expression:" <++> vcat [pretty x, pretty (show x)]


maxOfIntDomain :: MonadUserError m => Domain () Expression -> m Expression
maxOfIntDomain (DomainInt _ rs) = do
    xs <- sortNub <$> mapM maxOfIntRange rs
    case xs of
        []  -> return maxInt
        [x] -> return x
        _   -> return $ make opMin $ fromList xs
maxOfIntDomain d = userErr1 $ "Expected integer domain, but got:" <++> pretty d

maxOfIntRange :: Monad m => Range Expression -> m Expression
maxOfIntRange (RangeSingle ub) = return ub
maxOfIntRange (RangeUpperBounded ub) = return ub
maxOfIntRange (RangeBounded _ ub) = return ub
maxOfIntRange _ = return maxInt

upperBoundOfIntExpr :: MonadUserError m => Expression -> m Expression
upperBoundOfIntExpr x@Constant{} = return x
upperBoundOfIntExpr x | x == minInt = return minInt
upperBoundOfIntExpr x | x == maxInt = return maxInt
upperBoundOfIntExpr (Reference _ (Just (DeclNoRepr Given _ dom _))) = maxOfIntDomain dom
upperBoundOfIntExpr (Reference _ (Just (Alias x))) = upperBoundOfIntExpr x
upperBoundOfIntExpr (Op (MkOpSum (OpSum x))) | Just xs <- listOut x = do
    bounds <- mapM upperBoundOfIntExpr xs
    return $ make opSum $ fromList bounds
-- TODO: check for negatives
upperBoundOfIntExpr (Op (MkOpProduct (OpProduct x))) | Just xs <- listOut x = do
    bounds <- mapM upperBoundOfIntExpr xs
    return $ make opProduct $ fromList bounds
upperBoundOfIntExpr (Op (MkOpNegate (OpNegate x))) = do
    bound <- lowerBoundOfIntExpr x
    return (make opNegate bound)
upperBoundOfIntExpr x = userErr1 $ "Cannot compute lower bound of integer expression:" <++> vcat [pretty x, pretty (show x)]
