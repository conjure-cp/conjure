{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.Sanity ( sanityChecks, isInfinite ) where

import Conjure.Prelude
import Conjure.UserError
import Conjure.Language
import Conjure.Language.CategoryOf


sanityChecks :: (MonadFail m, MonadUserError m) => Model -> m Model
sanityChecks model = do
    let
        recordErr :: MonadWriter [Doc] m => [Doc] -> m ()
        recordErr = tell . return . vcat

        check :: (MonadFail m, MonadWriter [Doc] m) => Model -> m Model
        check m = do
            upToOneObjective m
            upToOneHeuristic m
            upToOneBranchingOn m
            upToOneDominance m
            forM_ (mStatements m) $ \ st -> case st of
                Declaration (FindOrGiven Given _ _) -> return () -- skip
                Declaration FindOrGiven{}           -> mapM_ (checkDomain True  (Just st)) (universeBi (forgetRefs st))
                _                                   -> mapM_ (checkDomain False (Just st)) (universeBi (forgetRefs st))
            forM_ (mStatements m) $ \ st -> case st of
                SuchThat{} -> 
                    forM_ (universeBi st) $ \case
                        x@Comprehension{} ->
                            forM_ (universeBi x) $ \case
                                GenDomainNoRepr _ dom -> checkDomain True (Just st) dom
                                _                     -> return ()
                        _ -> return ()
                _ -> return ()
            mapM_ checkFactorial (universeBi $ mStatements m)
            statements2 <- transformBiM checkLit (mStatements m)
            return m { mStatements = statements2 }

        -- check for mset attributes
        -- check for binary relation attrobutes
        checkDomain :: MonadWriter [Doc] m => Bool -> Maybe Statement -> Domain () Expression -> m ()
        checkDomain checkForInfinity mstmt domain = case domain of
            DomainInt _ rs | checkForInfinity && isInfinite rs -> recordErr
                        [ "Infinite integer domain."
                        , "Context:" <++> maybe (pretty domain) pretty mstmt
                        ]
            DomainMatrix index _
                | domainCanIndexMatrix index -> return ()
                | otherwise -> recordErr
                        [ "A matrix cannot be indexed with this domain:" <++> pretty index
                        , "Context:" <++> maybe (pretty domain) pretty mstmt
                        ]
            DomainSequence _ (SequenceAttr size _) _ ->
                case size of
                    SizeAttr_Size{} -> return ()
                    SizeAttr_MaxSize{} -> return ()
                    SizeAttr_MinMaxSize{} -> return ()
                    _ -> recordErr
                        [ "sequence requires (at least) one of the following attributes: size, maxSize"
                        , "Context:" <++> maybe (pretty domain) pretty mstmt
                        ]
            DomainMSet _ (MSetAttr size occur) _ ->
                case (size, occur) of
                    (SizeAttr_Size{}, _) -> return ()
                    (SizeAttr_MaxSize{}, _) -> return ()
                    (SizeAttr_MinMaxSize{}, _) -> return ()
                    (_, OccurAttr_MaxOccur{}) -> return ()
                    (_, OccurAttr_MinMaxOccur{}) -> return ()
                    _ -> recordErr
                        [ "mset requires (at least) one of the following attributes: size, maxSize, maxOccur"
                        , "Context:" <++> maybe (pretty domain) pretty mstmt
                        ]
            DomainRelation _ (RelationAttr _ binRelAttr) [a,b]
                | binRelAttr /= def && a /= b
                -> recordErr
                        [ "Binary relation attributes can only be used for binary relation between identical domains."
                        , "Either remove these attributes:" <+> pretty binRelAttr
                        , "Or make sure that the relation is between identical domains."
                        , "Context:" <++> maybe (pretty domain) pretty mstmt
                        ]
            DomainRelation _ (RelationAttr _ binRelAttr) innerDoms
                | binRelAttr /= def && length innerDoms /= 2
                -> recordErr
                        [ "Binary relation attributes can only be used on binary relations."
                        , "Either remove these attributes:" <+> pretty binRelAttr
                        , "Or make sure that the relation is binary."
                        , "Context:" <++> maybe (pretty domain) pretty mstmt
                        ]
            _ -> return ()

        -- check for function literals
        --     they cannot contain anything > CatParameter
        --     they cannot map the same element to multiple range elemnets
        -- check for partition literals
        --     they cannot contain anything > CatParameter
        --     the parts have to be disjoint
        -- TODO: Generate where clauses for when they contain parameters.
        checkLit :: MonadFail m => Expression -> m Expression
        checkLit lit = case lit of
            AbstractLiteral (AbsLitFunction mappings) -> do
                let defineds = fromList $ map fst mappings
                return $ WithLocals lit (DefinednessConstraints [ [essence| allDiff(&defineds) |] ])
            AbstractLiteral (AbsLitPartition parts) -> do
                let disjoint = concat
                        [ checks
                        | (part1, after) <- withAfter parts
                        , part2 <- after
                        , let checks = [ [essence| &el1 != &el2 |]
                                       | el1 <- part1
                                       , el2 <- part2
                                       ]
                        ]
                return $
                    if null disjoint
                        then lit
                        else WithLocals lit (DefinednessConstraints disjoint)
            _ -> return lit

        checkFactorial :: MonadWriter [Doc] m => Expression -> m ()
        checkFactorial p@[essence| factorial(&x) |]
            | categoryOf x >= CatDecision
            = recordErr
                [ "The factorial function does not work on decision expressions."
                , "Context:" <++> pretty p
                ]
        checkFactorial _ = return ()

        upToOne :: MonadWriter [Doc] m => (Statement -> Bool) -> Doc -> Model -> m ()
        upToOne f message m = do
            let found = [ st | st <- mStatements m, f st ]
            unless (length found <= 1) $ recordErr
                [ "Expected at most one \'" <> message <> "\' statement, but got" <+> pretty (length found) <> "."
                , vcat $ map (nest 4 . ("-" <+>) . pretty) found
                ]

        upToOneObjective :: MonadWriter [Doc] m => Model -> m ()
        upToOneObjective = upToOne (\ st -> case st of Objective{} -> True; _ -> False) "objective"

        upToOneHeuristic :: MonadWriter [Doc] m => Model -> m ()
        upToOneHeuristic = upToOne (\ st -> case st of SearchHeuristic{} -> True; _ -> False) "heuristic"

        upToOneBranchingOn :: MonadWriter [Doc] m => Model -> m ()
        upToOneBranchingOn = upToOne (\ st -> case st of SearchOrder{} -> True; _ -> False) "branching on"

        upToOneDominance :: MonadWriter [Doc] m => Model -> m ()
        upToOneDominance m = do
            upToOne (\ st -> case st of DominanceRelation{} -> True; _ -> False) "dominance_relation" m
            upToOne (\ st -> case st of IncomparabilityFunction{} -> True; _ -> False) "incomparability_function" m

    (model', errs) <- runWriterT (check model)
    if null errs
        then return model'
        else userErr errs

-- | return True if a bunch of ranges represent an infinite domain.
--   return False if finite. also, return false if unsure.
isInfinite :: [Range a] -> Bool
isInfinite [] = True
isInfinite [RangeOpen{}] = True
isInfinite [RangeLowerBounded{}] = True
isInfinite [RangeUpperBounded{}] = True
isInfinite _ = False

forgetRefs :: Statement -> Statement
forgetRefs = transformBi f
    where
        f (Reference nm _) = Reference nm Nothing
        f x = x
