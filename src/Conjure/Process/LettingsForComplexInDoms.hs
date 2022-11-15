module Conjure.Process.LettingsForComplexInDoms
    ( lettingsForComplexInDoms
    , inlineLettingDomainsForDecls
    , removeDomainLettings
    ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


-- | if the domain of a declaration contains a reference to another declaration (a given)
--   that needs a representation, hell breaks loose.
--   specifically, name-resolution needs to be rerun after representation selection.
--   however, this transformation (as part of `Conjure.UI.Model.prologue`) cleans this up by introducing
--   extra letting statements.
lettingsForComplexInDoms ::
    MonadFailDoc m =>
    NameGen m =>
    Model -> m Model
lettingsForComplexInDoms m = do
    let
        expressionExtract expr@Constant{}        = return expr
        expressionExtract expr@AbstractLiteral{} = return expr
        expressionExtract expr@Reference{}       = return expr
        expressionExtract expr = do
            newLetting <- nextName "let"
            tell [Declaration (Letting newLetting expr)]        -- new declarations
            return (Reference newLetting Nothing)               -- the replacement expression

    statements <- forM (mStatements m) $ \ st ->
        case st of
            Declaration (FindOrGiven forg name domain) -> do
                (domain', newLettings) <- runWriterT (mapM expressionExtract domain)
                return (newLettings ++ [Declaration (FindOrGiven forg name domain')])
            Declaration (LettingDomainDefnUnnamed name expr) -> do
                (expr', newLettings) <- runWriterT (expressionExtract expr)
                return (newLettings ++ [Declaration (LettingDomainDefnUnnamed name expr')])
            _ -> return [st]
    return m { mStatements = concat statements }


-- | inline letting domains for declarations, before saving the original domain in the logs
inlineLettingDomainsForDecls :: MonadFailDoc m => Model -> m Model
inlineLettingDomainsForDecls m = do
    let
        f (DomainReference name Nothing) = do
            (ctxt, unnameds) <- gets id
            case name `lookup` ctxt of
                Just d -> transformM f d
                _ -> if name `elem` unnameds
                        then return (DomainReference name Nothing)
                        else failDoc $ vcat
                                $ ("No value for:" <+> pretty name)
                                : "Bindings in context:"
                                : prettyContext ctxt
        f d = return d

    flip evalStateT ( []    -- name, domain pairs for letting domains.
                    , []    -- names for unnamed types. so they can be skipped.
                    ) $ do
        statements <- forM (mStatements m) $ \ st ->
            case st of
                Declaration (Letting name (Domain domain)) -> do
                    modify (([(name, domain)], []) `mappend`)
                    return st
                Declaration (LettingDomainDefnUnnamed name _) -> do
                    modify (([], [name]) `mappend`)
                    return st
                Declaration (FindOrGiven forg name domain) -> do
                    domain' <- transformM f domain
                    return (Declaration (FindOrGiven forg name domain'))
                _ -> return st
        return m { mStatements = statements }


-- | remove domain lettings, only after name resolution
removeDomainLettings :: Monad m => Model -> m Model
removeDomainLettings m =
    return m { mStatements = concat [ case st of
                                        Declaration (Letting _ (Domain _)) -> []
                                        _ -> [st]
                                    | st <- mStatements m
                                    ] }
