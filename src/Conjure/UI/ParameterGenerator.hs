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
    (?typeCheckerMode :: TypeCheckerMode) =>
    Integer ->      -- MININT
    Integer ->      -- MAXINT
    Model -> m Model
parameterGenerator minIntValue maxIntValue model = runNameGen () (resolveNames model) >>= core >>= evaluateBounds
    where
        core m = do
            outStatements <- forM (mStatements m) $ \ st -> case st of
                Declaration (FindOrGiven Given nm dom) -> pgOnDomain nm dom
                Declaration (FindOrGiven Find  _  _  ) -> return []
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
                    -- traceM $ show $ "nm :" <+> pretty nm
                    -- traceM $ show $ "bef:" <+> pretty dom
                    dom' <- eval dom
                    -- traceM $ show $ "aft:" <+> pretty dom'
                    -- traceM $ show $ "aft:" <+> pretty (show dom')
                    -- traceM ""
                    return $ Declaration (FindOrGiven forg nm (fmap Constant dom'))
                st -> return st
            return m { mStatements = stmtsEvaluated }


pgOnDomain ::
    MonadUserError m =>
    Name ->
    Domain () Expression ->
    m [Statement]
pgOnDomain nm dom =
    case dom of
        DomainInt t _ -> do
            lbX <- minOfIntDomain dom
            ubX <- maxOfIntDomain dom
            lb  <- lowerBoundOfIntExpr lbX
            ub  <- upperBoundOfIntExpr ubX
            let nmMiddle = nm `mappend` "_middle"
            let nmDelta  = nm `mappend` "_delta"
            let x = Reference nm Nothing
            let middle = Reference nmMiddle Nothing
            let delta = Reference nmDelta Nothing
            return
                [ Declaration (FindOrGiven Given nmMiddle (DomainInt t [RangeBounded lb ub]))
                , Declaration (FindOrGiven Given nmDelta  (DomainInt t [RangeBounded 0 [essence| (&ub - &lb) / 2 |]]))
                , Declaration (FindOrGiven Find  nm       (DomainInt t [RangeBounded lb ub]))
                , SuchThat $ [ [essence| &x >= &middle - &delta |]
                             , [essence| &x <= &middle + &delta |]
                             ] ++
                             [ [essence| &x >= &lbX |]
                             | lb /= lbX
                             ] ++
                             [ [essence| &x <= &ubX |]
                             | ub /= ubX
                             ]
                ]
        _ -> userErr1 $ "Unhandled domain:" <++> vcat [ pretty dom
                                                      , pretty (show dom)
                                                      ]


-- helper functions

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
