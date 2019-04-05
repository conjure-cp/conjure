{-# LANGUAGE QuasiQuotes #-}

module Conjure.UI.ParameterGenerator where

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Language.Instantiate ( instantiateDomain )
import Conjure.Process.Enumerate ( EnumerateDomain )
-- import Conjure.Language.Expression.DomainSizeOf ( domainSizeOf )


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
            let eval = instantiateDomain [("MININT", fromInt minIntValue), ("MAXINT", fromInt maxIntValue)]
            stmtsEvaluated <- forM (mStatements m) $ \ case
                Declaration (FindOrGiven forg nm dom) -> do
                    -- traceM $ show $ "EVAL nm :" <+> pretty nm
                    -- traceM $ show $ "EVAL bef:" <+> pretty dom
                    dom' <- eval dom
                    -- traceM $ show $ "EVAL aft:" <+> pretty dom'
                    -- traceM $ show $ "EVAL aft:" <+> pretty (show dom')
                    -- traceM ""
                    return $ Declaration (FindOrGiven forg nm (fmap Constant dom'))
                st -> return st
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
        DomainFunction r attr innerDomainFr innerDomainTo -> do
            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]
            (domFr, declFr, consFr) <- pgOnDomain [essence| &i[1] |] (nm `mappend` "_1") innerDomainFr
            (domTo, declTo, consTo) <- pgOnDomain [essence| &i[2] |] (nm `mappend` "_2") innerDomainTo

            -- drop total, post constraint instead
            (attrOut, sizeLb, sizeUb) <-
                    case attr of
                        FunctionAttr size _totality jectivity -> do
                            (sizeOut, lb, ub) <-
                                case size of
                                    SizeAttr_None ->
                                        return (SizeAttr_None, Nothing, Nothing)
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return (SizeAttr_MinMaxSize lb ub, Just a, Just a)
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return (SizeAttr_MinSize lb, Just a, Nothing)
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return (SizeAttr_MaxSize ub, Nothing, Just a)
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return (SizeAttr_MinMaxSize lb ub, Just a, Just b)
                            return (FunctionAttr sizeOut PartialityAttr_Partial jectivity, lb, ub)

            let
                totalityCons =
                    case attr of
                        FunctionAttr _ PartialityAttr_Total _ -> do
                            innerDomainFrMin <- minOfIntDomain innerDomainFr
                            innerDomainFrMax <- maxOfIntDomain innerDomainFr
                            return $ return [essence|
                                forAll &iPat : &domFr .
                                    (&i >= &innerDomainFrMin /\ &i <= &innerDomainFrMax)
                                    <->
                                    &i in defined(&x)
                                |]
                        _ -> return []

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

            newCons <- concat <$> sequence [totalityCons, sizeLbCons, sizeUbCons]

            return3
                (DomainFunction r attrOut domFr domTo)
                (declFr ++ declTo)
                (newCons ++ map liftCons (consFr ++ consTo))
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
minOfIntDomain d = userErr1 $ "Expected integer domain, but got:" <++> pretty d

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
upperBoundOfIntExpr x = userErr1 $ "Cannot compute lower bound of integer expression:" <++> vcat [pretty x, pretty (show x)]
