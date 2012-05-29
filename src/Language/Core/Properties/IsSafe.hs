{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.IsSafe where


import Language.Core
import Language.Core.Properties.Pretty
import Language.Core.Properties.ToInt


class IsSafe a where
    isSafe :: Monad m => a -> CompT m Bool

instance IsSafe Core where
    isSafe c = multiMatch "Core.isSafe"
        [ case c of
            L {} -> matchSuccess $ return True
            _    -> matchFail
        , case c of
            R x  -> matchSuccess $ isSafe x
            _    -> matchFail
        , case c of
            Expr "div" [_,possiblyZero] -> do
                case possiblyZero of
                    L (I 0) -> matchSuccess $ return False
                    L (I _) -> matchSuccess $ return True
                    R x -> do
                        val <- lift $ lookUpRef x
                        case val of
                            Expr "int-domain-from-to" [Expr "from" [L (I fr)], Expr "to" [L (I to)]]
                                | 0 `elem` [fr..to] -> matchSuccess $ return False
                                | otherwise         -> matchSuccess $ return True
                            Expr "int-domain-list" xs -> do
                                is <- lift (mapM toInt xs)
                                matchSuccess $ return $ 0 `elem` is
                            _ -> matchFail
                    _ -> matchFail
            _ -> matchFail
        ]
    -- isSafe (Expr "div" [_,L (I 0)]) = return False
    -- isSafe (Expr "div" [_,possiblyZero]) = do
    -- isSafe p@(Expr {}) = do
    --     tell [ "Assumed to be safe:" <+> pretty p ]
    --     return True
    -- isSafe p@(Let r _ x) = do
    --     tell [ "Passing through a let expression:" <+> Pr.parens (pretty r) <+> pretty p ]
    --     isSafe x

instance IsSafe Reference where
    isSafe r = core <?> "safety check for reference" <+> pretty r
        where
            core = do
                val <- lookUpRef r
                isSafe val

