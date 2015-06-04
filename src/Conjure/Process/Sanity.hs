module Conjure.Process.Sanity ( sanityChecks ) where

import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.CategoryOf


sanityChecks :: MonadUserError m => Model -> m Model
sanityChecks model = do
    let
        recordErr :: MonadWriter [Doc] m => [Doc] -> m ()
        recordErr = tell . return . vcat

        check :: MonadWriter [Doc] m => Model -> m ()
        check m = do
            -- check for mset attributes
            forM_ (mStatements m) $ \ st -> case st of
                Declaration (FindOrGiven _ _ domain) ->
                    case domain of
                        DomainMSet _ (MSetAttr size occur) _ ->
                            case (size, occur) of
                                (SizeAttr_Size{}, _) -> return ()
                                (SizeAttr_MaxSize{}, _) -> return ()
                                (SizeAttr_MinMaxSize{}, _) -> return ()
                                (_, OccurAttr_MaxOccur{}) -> return ()
                                (_, OccurAttr_MinMaxOccur{}) -> return ()
                                _ -> recordErr
                                    [ "mset requires (at least) one of the following attributes: size, maxSize, maxOccur"
                                    , "When working on:" <++> pretty st
                                    ]
                        _ -> return ()
                _ -> return ()

            -- check for function literals
            --     they cannot contain anything > CatParameter
            --     they cannot map the same element to multiple range elemnets
            -- check for partition literals
            --     they cannot contain anything > CatParameter
            --     the parts have to be disjoint
            -- TODO: Generate where clauses for when they contain parameters.
            forM_ (universeBi m) $ \ lit -> case lit of
                AbstractLiteral (AbsLitFunction mappings) -> do
                    when (categoryOf lit > CatParameter) $ recordErr
                        [ "A function literal may only contain constants or parameters."
                        , "When working on:" <++> pretty lit
                        ]
                    let definedSet = map fst (nub mappings)
                    let definedSetNoDups = nub definedSet
                    when (length definedSet /= length definedSetNoDups) $ recordErr
                        [ "A function literal can not map one element to multiple range elements."
                        , "When working on:" <++> pretty lit
                        ]
                AbstractLiteral (AbsLitPartition parts) -> do
                    when (categoryOf lit > CatParameter) $ recordErr
                        [ "A partition literal may only contain constants or parameters."
                        , "When working on:" <++> pretty lit
                        ]
                    let disjoint = and [ null (intersect part1 part2)
                                       | (part1, after) <- withAfter parts
                                       , part2 <- after
                                       ]
                    unless disjoint $ recordErr
                        [ "A partition literal has to contain disjoint parts."
                        , "When working on:" <++> pretty lit
                        ]
                _ -> return ()

    errs <- execWriterT $ check model
    if null errs
        then return model
        else userErr errs
