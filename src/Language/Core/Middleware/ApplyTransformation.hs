{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Middleware.ApplyTransformation ( worker ) where

import Language.Core

type RulesDB m = [Core -> CompT m [Core]]

worker :: (Functor m, Monad m)
    => RulesDB m
    -> Spec
    -> CompT m Spec
-- worker :: (Functor m, Monad m) => RuleBase (CompT m) -> Middleware (CompT m) Spec (Maybe [Spec])
-- worker fs (Spec lang statements) = trace (show $ "worker" <+> pretty (Spec lang statements)) $ do
worker fs spec = do
    mkLog "ApplyTransformation.worker" $ pretty spec
    (spec', Any flag) <- runWriterT $ onSpec fs spec
    if flag
         then tryAgain fs spec'
         else returns []


tryAgain :: (Functor m, Monad m)
    => RulesDB m
    -> Spec
    -> CompT m Spec
-- tryAgain :: (Functor m, Monad m) => RuleBase (CompT m) -> Middleware (CompT m) Spec [Spec]
-- tryAgain _ spec = return [spec]
-- tryAgain fs spec = trace (show $ "tryAgain" <+> pretty spec) $ do
tryAgain fs spec = do
    mkLog "ApplyTransformation.tryAgain" $ pretty spec
    (result, Any flag) <- runWriterT $ onSpec fs spec
    if flag
        then tryAgain fs result
        else return spec


onSpec :: (Functor m, Monad m)
    => RulesDB m
    -> Spec
    -> WriterT Any (CompT m) Spec
onSpec fs (Spec lang statements) = do
    lift $ mapM_ processStatement statements
    statements' <- sequence <$> mapM (onCore fs) statements
    lift $ returns [ Spec lang s | s <- statements' ]


onCore :: (Functor m, Monad m)
    => RulesDB m
    -> Core
    -> WriterT Any (CompT m) [Core]
-- onCore :: (Functor m, Monad m) => RuleBase (CompT m) -> Middleware (WriterT Any (CompT m)) Core [Core]
-- onCore _ x | trace (show $ "onCore" <+> pretty x) False = undefined
onCore fs x@(L {}) = tryApply fs x
onCore fs x@(R {}) = tryApply fs x
onCore fs x@( viewDeep [":expr-quantified"] -> Just xs ) = 
    case lookUpInExpr ":expr-quantified-quanVar" xs of
        Just [Expr ":structural-single" [R r]] -> do
            lift $ addBinder r
                    $ Expr ":quanVar" [ Expr ":quanVar-name"   [R r]
                                      , Expr ":quanVar-within" [x]
                                      ]
            result <- onCoreGeneric fs x ":expr-quantified" xs
            lift $ removeLastBinder
            return result
        _ -> lift $ err ErrInvariant
                    $ singletonNested
                    $ "Invariant violation in ApplyTransformation.onCore" <+> pretty x
onCore fs x@(Expr t xs) = onCoreGeneric fs x t xs


onCoreGeneric :: (Functor m, Monad m)
    => RulesDB m
    -> Core
    -> Tag
    -> [Core]
    -> WriterT Any (CompT m) [Core]
-- onCoreGeneric :: (Functor m, Monad m) => RuleBase (CompT m)
--                                       -> Core -> Tag -> [Core]
--                                       -> WriterT Any (CompT m) [Core]
onCoreGeneric fs x t xs = do
    (results, Any flag) <- listen $ do
        yss <- sequence <$> mapM (onCore fs) xs
        let zs = map (Expr t) yss
        concat <$> mapM (tryApply fs) zs
    if flag
        then concat <$> mapM (onCore fs) results
        else return [x]

    -- (result, Any flag) <- listen $ do
    --     ys <- mapM (onCore fs) xs
    --     let z = Expr t ys
    --     tryApply fs z
    -- if flag
    --     then return result
    --     else return [x]


tryApply :: (Functor m, Monad m)
    => RulesDB m
    -> Core
    -> WriterT Any (CompT m) [Core]
-- tryApply :: (Functor m, Monad m) => RuleBase (CompT m) -> Middleware (WriterT Any (CompT m)) Core [Core]
tryApply fs x = do
    let
        go [] = return (False, [x])
        go (g:gs) = do
            let
                doIt = do
                    ys <- g x
                    mkLog "applied" $ vcat $ pretty x
                                           : map (("~~>" <+>) . pretty) ys
                    -- mkLog "applied" $ vcat [pretty x, "~~>", pretty y]
                    return (True, ys)
            catchIf doIt
                    (ErrNoRuleApplications==)
                    (\ _ -> go gs )
    (x', Any flag) <- listen (simplify x)
    if flag
        then do
            lift $ mkLog "simplified" $ vcat [pretty x, "~~>", pretty x']
            return [x']
        else do
            (b,z) <- lift $ go fs
            tell (Any b)
            return z
