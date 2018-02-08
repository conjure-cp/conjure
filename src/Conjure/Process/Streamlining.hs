{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.Streamlining where

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.TypeOf ( typeOf )


streamlining :: (MonadFail m, MonadLog m, MonadUserError m) => Model -> m ()
streamlining model = do

    decisionVariables <- concatForM (mStatements model) $ \ statement ->
        case statement of
            Declaration (FindOrGiven Find nm domain) ->
                return [Reference nm (Just (DeclNoRepr Find nm domain NoRegion))]
            _ -> return []

    traceM $ show $ vcat [ "Number of decision variables:" <+> pretty (length decisionVariables)
                         , "Their names are:" <+> prettyList id "," decisionVariables
                         ]

    streamliners <- streamlinersForAllVariables decisionVariables

    traceM $ show $ "Number of streamliners:" <+> pretty (length streamliners)

    forM_ (concat streamliners) $ \ s ->
        traceM $ show $ pretty s



type Streamliner = [Statement]

streamlinersForAllVariables :: MonadFail m => [Expression] -> m [Streamliner]
streamlinersForAllVariables xs = concatMapM streamlinersForSingleVariable xs

-- given a reference to a top level variable, produce a list of all applicable streamliners
streamlinersForSingleVariable :: MonadFail m => Expression -> m [Streamliner]
streamlinersForSingleVariable x = concatMapM ($ x) [intOdd, intEven]


-- given an integer expression (which can be a reference to a decision variable),
-- generate a constraint forcing it to be odd
intOdd :: MonadFail m => Expression -> m [Streamliner]
intOdd x = do
    ty <- typeOf x
    if typeUnify ty TypeInt
        then return [ [essenceStmts| such that &x % 2 = 1 |] ]
        else return []

intEven :: MonadFail m => Expression -> m [Streamliner]
intEven x = do
    ty <- typeOf x
    if typeUnify ty TypeInt
        then return [ [essenceStmts| such that &x % 2 = 0 |] ]
        else return []

-- intEven :: Expression -> Streamliner
-- intEven x = [essenceStmts| such that &x % 2 = 0 |]
--
--
--
-- --filter
--
-- a       b    c
-- odd     -    -
-- even    -    -
-- odd    odd   even
--
