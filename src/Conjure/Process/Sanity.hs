{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.Sanity ( sanityChecks ) where

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
            forM_ (mStatements m) $ \ st -> case st of
                Declaration FindOrGiven{} -> mapM_ (checkDomain (Just st)) (universeBi st)
                _                         -> mapM_ (checkDomain Nothing  ) (universeBi st)
            mapM_ checkFactorial (universeBi $ mStatements m)
            statements2 <- transformBiM checkLit (mStatements m)
            return m { mStatements = statements2 }

        -- check for mset attributes
        -- check for binary relation attrobutes
        checkDomain :: MonadWriter [Doc] m => (Maybe Statement) -> Domain () Expression -> m ()
        checkDomain mstmt domain = case domain of
            DomainMSet _ (MSetAttr size occur) _ ->
                case (size, occur) of
                    (SizeAttr_Size{}, _) -> return ()
                    (SizeAttr_MaxSize{}, _) -> return ()
                    (SizeAttr_MinMaxSize{}, _) -> return ()
                    (_, OccurAttr_MaxOccur{}) -> return ()
                    (_, OccurAttr_MinMaxOccur{}) -> return ()
                    _ -> recordErr
                        [ "mset requires (at least) one of the following attributes: size, maxSize, maxOccur"
                        , "When working on:" <++> maybe (pretty domain) pretty mstmt
                        ]
            DomainRelation _ (RelationAttr _ binRelAttr) [a,b]
                | binRelAttr /= def && a /= b
                -> recordErr
                        [ "Binary relation attributes can only be used for binary relation between identical domains."
                        , "Either remove these attributes:" <+> pretty binRelAttr
                        , "Or make sure that the relation is between identical domains."
                        , "When working on:" <++> maybe (pretty domain) pretty mstmt
                        ]
            DomainRelation _ (RelationAttr _ binRelAttr) innerDoms
                | binRelAttr /= def && length innerDoms /= 2
                -> recordErr
                        [ "Binary relation attributes can only be used on binary relations."
                        , "Either remove these attributes:" <+> pretty binRelAttr
                        , "Or make sure that the relation is binary."
                        , "When working on:" <++> maybe (pretty domain) pretty mstmt
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
                return $ WithLocals lit (Right [ [essence| allDiff(&defineds) |] ])
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
                return $ WithLocals lit (Right disjoint)
            _ -> return lit

        checkFactorial :: MonadWriter [Doc] m => Expression -> m ()
        checkFactorial p@[essence| factorial(&x) |]
            | categoryOf x >= CatDecision
            = recordErr
                [ "The factorial function does not work on decision expressions."
                , "When working on:" <++> pretty p
                ]
        checkFactorial _ = return ()

    (model', errs) <- runWriterT (check model)
    if null errs
        then return model'
        else userErr errs
