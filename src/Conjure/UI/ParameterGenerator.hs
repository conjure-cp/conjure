{-# LANGUAGE QuasiQuotes #-}

module Conjure.UI.ParameterGenerator where

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )
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
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m Model
parameterGenerator model = runNameGen () (resolveNames model) >>= core
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
            let
                usesMININT = not $ null [ () | Reference "MININT" _ <- universeBi outStatements ]
                usesMAXINT = not $ null [ () | Reference "MAXINT" _ <- universeBi outStatements ]
                addMinMaxInt stmts =
                    [ Declaration (FindOrGiven Given "MININT" (DomainInt TagInt [])) | usesMININT ] ++
                    [ Declaration (FindOrGiven Given "MAXINT" (DomainInt TagInt [])) | usesMAXINT ] ++
                    stmts
            return m { mStatements = addMinMaxInt (concat outStatements) }


pgOnDomain ::
    MonadUserError m =>
    Name ->
    Domain () Expression ->
    m [Statement]
pgOnDomain nm dom =
    case dom of
        DomainInt t _ -> do
            lb <- minOfIntDomain dom
            ub <- maxOfIntDomain dom
            let nmMiddle = nm `mappend` "_middle"
            let nmDelta  = nm `mappend` "_delta"
            let x = Reference nm Nothing
            let middle = Reference nmMiddle Nothing
            let delta = Reference nmDelta Nothing
            return
                [ Declaration (FindOrGiven Given nmMiddle (DomainInt t [RangeBounded lb ub]))
                , Declaration (FindOrGiven Given nmDelta  (DomainInt t [RangeBounded 0 [essence| (&ub - &lb) / 2 |]]))
                , Declaration (FindOrGiven Find  nm       (DomainInt t [RangeBounded lb ub]))
                , SuchThat [ [essence| &x >= &middle - &delta |]
                           , [essence| &x <= &middle + &delta |]
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
minOfIntDomain (DomainInt _ []) = return minInt
minOfIntDomain (DomainInt _ [r]) = minOfIntRange r
minOfIntDomain (DomainInt _ rs) = do
    xs <- mapM minOfIntRange rs
    return $ make opMax $ fromList xs
minOfIntDomain d = userErr1 $ "Expected integer domain, but got:" <+> pretty d

minOfIntRange :: Monad m => Range Expression -> m Expression
minOfIntRange (RangeSingle lb) = return lb
minOfIntRange (RangeLowerBounded lb) = return lb
minOfIntRange (RangeBounded lb _) = return lb
minOfIntRange _ = return minInt


maxOfIntDomain :: MonadUserError m => Domain () Expression -> m Expression
maxOfIntDomain (DomainInt _ []) = return maxInt
maxOfIntDomain (DomainInt _ [r]) = maxOfIntRange r
maxOfIntDomain (DomainInt _ rs) = do
    xs <- mapM maxOfIntRange rs
    return $ make opMin $ fromList xs
maxOfIntDomain d = userErr1 $ "Expected integer domain, but got:" <+> pretty d

maxOfIntRange :: Monad m => Range Expression -> m Expression
maxOfIntRange (RangeSingle ub) = return ub
maxOfIntRange (RangeUpperBounded ub) = return ub
maxOfIntRange (RangeBounded _ ub) = return ub
maxOfIntRange _ = return maxInt

