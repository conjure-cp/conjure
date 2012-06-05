{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.ToLit where


import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ShowAST
import Language.Core.Properties.Simplify
import Language.Core.Properties.Pretty


class ToLit a where
    toLit :: (Functor m, Monad m) => a -> CompT m (Maybe Literal)

instance ToLit Core where
    toLit (L x) = toLit x
    toLit (R x) = toLit x
    toLit (Expr ":value" [Expr ":value-literal" [L x]]) = return $ Just x
    toLit p     = do
        mkLog "toLit" $ pretty p
        -- mkLog "toLit" $ showAST p
        (p',Any flag) <- runWriterT (simplify p)
        if flag
            then do
                mkLog "toLit" $ "simplified to:" <+> pretty p'
                -- mkLog "toLit" $ "simplified to:" <+> showAST p'
                toLit p'
            else do
                mkLog "toLit" "not simplified."
                return Nothing -- err $ "Core.toLit:" <+> showAST p

instance ToLit Literal where
    toLit = return . Just

instance ToLit Reference where
    toLit r = core <?> "Reference.toLit:" <+> showAST r
        where
            core = do
                val <- lookUpRef r
                toLit val
