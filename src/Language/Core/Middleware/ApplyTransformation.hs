{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Middleware.ApplyTransformation ( worker ) where

import Language.Core
import Language.Core.Middleware

type RuleBase m = [Middleware m Core (Maybe [Core])]

worker :: (Functor m, Monad m) => RuleBase (CompT m) -> Middleware (CompT m) Spec (Maybe [Spec])
-- worker fs (Spec lang statements) = trace (show $ "worker" <+> pretty (Spec lang statements)) $ do
worker fs spec@(Spec lang statements) = do
    mkLog "ApplyTransformation.worker" $ pretty spec
    mapM_ processStatement statements
    (specs, Any flag) <- runWriterT $ do
        xss <- sequence <$> mapM (onCore fs) statements
        return $ map (Spec lang) xss
    if flag
         then do
             ys <- mapM (tryAgain fs) specs
             return $ Just $ concat ys
         else return Nothing

tryAgain :: (Functor m, Monad m) => RuleBase (CompT m) -> Middleware (CompT m) Spec [Spec]
-- tryAgain _ spec = return [spec]
-- tryAgain fs spec = trace (show $ "tryAgain" <+> pretty spec) $ do
tryAgain fs spec = do
    mkLog "ApplyTransformation.tryAgain" $ pretty spec
    mresult <- worker fs spec
    case mresult of
        Nothing     -> return [spec]
        Just result -> return result

onCore :: (Functor m, Monad m) => RuleBase (CompT m) -> Middleware (WriterT Any (CompT m)) Core [Core]
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
        _ -> err $ "Invariant violation in ApplyTransformation.onCore" <+> pretty x
onCore fs x@(Expr t xs) = onCoreGeneric fs x t xs


onCoreGeneric :: (Functor m, Monad m) => RuleBase (CompT m)
                                      -> Core -> Tag -> [Core]
                                      -> WriterT Any (CompT m) [Core]
onCoreGeneric fs x t xs = do
    (results, Any flag) <- listen $ do
        yss <- sequence <$> mapM (onCore fs) xs
        let zs = map (Expr t) yss
        concat <$> mapM (tryApply fs) zs
    if flag
        then concat <$> mapM (onCore fs) results
        -- then return results
        else return [x]


tryApply :: (Functor m, Monad m) => RuleBase (CompT m) -> Middleware (WriterT Any (CompT m)) Core [Core]
tryApply fs x = do
    let
        go [] = return [x]
        go (g:gs) = do
            mys <- lift (g x)
            case mys of
                Nothing -> go gs
                -- Just [] -> return [x]
                Just ys -> do
                    lift $ mkLog "applied" $ vcat (pretty x : map (("~~>" <+>) . pretty) ys)
                    tell (Any True)
                    return ys
    (x',Any flag) <- listen (simplify x)
    if flag
        then do
            lift $ mkLog "simplified" $ vcat [pretty x, "~~>", pretty x']
            return [x']
        else go fs
