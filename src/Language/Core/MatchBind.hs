{-# LANGUAGE OverloadedStrings #-}

module Language.Core.MatchBind where

import Language.Core.Definition
import Language.Core.Imports
import Language.Core.Properties.ShowAST
import Language.Core.Properties.Pretty
import Language.Core.Parser
import Language.EssenceLexerP

import Text.PrettyPrint as Pr ( comma )
import Data.Set as S ( fromList, toList, isSubsetOf, unions )
import Data.Generics.Uniplate.Data ( universe )

match :: Monad m => Core -> Core -> CompT m Bool
match p@(Expr ":metavar" [R r]) c = do
    mkLog "match" $ showAST p <+> "~~" <+> showAST c
    addBinder ("@" `mappend` r) c
    return True
match x@(L {}) y@(L {}) =
    if x == y
        then do
            mkLog "match" $ "same literal" <+> showAST x <+> "~~" <+> showAST y
            return True
        else do
            mkLog "match" $ "literals not equal" <+> showAST x <+> "~~" <+> showAST y
            return False
match x@(R {}) y@(R {}) =
    if x == y
        then do
            mkLog "match" $ "same identifier" <+> showAST x <+> "~~" <+> showAST y
            return True
        else do
            mkLog "match" $ "identifiers not equal" <+> showAST x <+> "~~" <+> showAST y
            return False
match (Expr ":attributes" xArgs') (Expr ":attributes" yArgs) = do
    let dontcare    = Expr ":attribute" [ Expr ":attribute-dontcare" [] ]
    let xArgs       = filter (/= dontcare) xArgs'
    let hasDontcare = xArgs /= xArgs'
    if not hasDontcare
        then match (Expr ":nowordered" xArgs) (Expr ":nowordered" yArgs)
        else do
            let foo _ [] = return False
                foo j (i:is) = do
                    b <- match j i
                    if b then return True else foo j is
            bs <- forM xArgs $ \ x -> foo x yArgs
            if and bs
                then do
                    return True
                else do
                    return False
match _x@(Expr xTag xArgs) _y@(Expr yTag yArgs) = do
    case (xTag == yTag, length xArgs == length yArgs) of
        (True, True) -> do
            -- mkLog "match" $ "same tree label & equal number of subtrees. great" <+> showAST xTag
            bs <- zipWithM match xArgs yArgs
            if and bs
                then do
                    -- mkLog "match" $ "all subtrees matched fine" <+> showAST x <+> "~~" <+> showAST y
                    return True
                else do
                    -- mkLog "match" $ "some subtrees didn't match" <+> showAST x <+> "~~" <+> showAST y
                    return False
        (False, _) -> do
            -- mkLog "match" $ "different tree labels" <+> showAST xTag <+> "~~" <+> showAST yTag
            return False
        (_, False) -> do
            -- mkLog "match" $ "different number of subtrees" <+> showAST xTag <+> "~~" <+> showAST yTag
            return False
match _x _y = do
    -- mkLog "match" $ "this just fails" <+> showAST x <+> "~~" <+> showAST y
    return False


bind :: (Functor m, Monad m) => Core -> MaybeT (CompT m) Core
bind p@(Expr (Tag ":metavar") [R r']) = do
    let r = "@" `mappend` r'
    lift $ mkLog "bind" $ showAST p
    mx <- lift $ (Just <$> lookUpRef r) `catchError` \ _ -> return Nothing
    case mx of
        Nothing -> do
            lift $ mkLog "bind-fail" $ showAST p
            mzero
        Just x  -> return x
bind (Expr xTag xArgs) = Expr xTag <$> mapM bind xArgs
bind x = return x


mkFunction :: (Functor m, Monad m) => Core -> [Core] -> CompT m (Core -> MaybeT (CompT m) [Core])
mkFunction pattern templates = do
    let patternMetaVars  = S.fromList [ r | Expr ":metavar" [R r] <- universe pattern  ]
    let templateMetaVars = S.unions [ S.fromList [ r | Expr ":metavar" [R r] <- universe template ]
                                    | template <- templates
                                    ]
    unless (templateMetaVars `S.isSubsetOf` patternMetaVars)
        $ err undefined
        $ singletonNested
        $ vcat [ "Pattern meta variables:"  <+> prettyListDoc id Pr.comma (map showAST $ S.toList patternMetaVars)
                     , "Template meta variables:" <+> prettyListDoc id Pr.comma (map showAST $ S.toList templateMetaVars)
                     ]
    return $ \ x -> do
        bindersBefore <- gets binders
        b <- lift $ match pattern x
        if b
            then do
                ys <- mapM bind templates
                modify $ \ st -> st { binders = bindersBefore }
                return ys
            else do
                modify $ \ st -> st { binders = bindersBefore }
                mzero

testAsAFunc :: Text -> Text -> Text -> IO ()
testAsAFunc pattern' template' x' = do
    pattern  <- headNote "parsing pattern"  <$> lexAndParseIO (parseExpr <* eof) pattern'
    template <- headNote "parsing template" <$> lexAndParseIO (parseExpr <* eof) template'
    x        <- headNote "parsing x"        <$> lexAndParseIO (parseExpr <* eof) x'
    void $ runCompIO def def $ do
        f  <- mkFunction pattern [template]
        my <- runMaybeT (f x)
        case my of
            Nothing -> do
                error "function returns Nothing"
            Just ys -> forM_ ys $ \ y -> do
                liftIO $ print $ pretty y
                -- mkLog "testAsAFunc" (showAST y)
                -- return $ showAST y
                -- return $ pretty y






-- [x] <- lexAndParseIO (parseDomain <* eof) "set of int(1..9)" 
-- [p] <- lexAndParseIO (parseDomain <* eof) "set of int(@a..9)" 
-- [t] <- lexAndParseIO (parseExpr <* eof) "a*2**a" 
