{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Properties.IsSafe where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ToInt

-- testing: runCompIO def def $ do a <- runP Nothing parseExpr "(1+(3/x)+foo @ find x,y : int(1..8) letting foo be 3/x)"; isSafe a


class IsSafe a where
    isSafe :: (Functor m, Monad m) => a -> CompT m Bool

instance IsSafe Core where
    isSafe (L {}) = return True
    isSafe (R x ) = isSafe x
    isSafe (Expr ":withlocals" [Expr ":actual" [a], Expr ":locals" bs]) = localState $ do
        mapM_ processStatement bs
        isSafe a
    isSafe (Expr ":/" [_,a]) = do
        let
            canBeZero (L (I 0)) = return True
            canBeZero (R r) = do
                val <- lookUpRef r
                canBeZero val
            canBeZero ( view -> ( [":value", ":value-literal"]
                                , [x]
                                ) ) = canBeZero x
            canBeZero ( view -> ( [":domain", ":domain-int", ":range", ":range-fromto"]
                                , [fr,to]
                                ) ) = do
                                    mfrI <- toInt fr
                                    mtoI <- toInt to
                                    case (mfrI, mtoI) of
                                        (Just frI, Just toI) -> return $ 0 `elem` [frI .. toI]
                                        _                    -> return False
            canBeZero _ = return False
        not <$> canBeZero a
    isSafe (Expr _ xs) = do
        flags <- mapM isSafe xs
        return $ and flags
    -- isSafe _ = return False

    -- isSafe c = multiMatch "Core.isSafe"
    --     [ case c of
    --         L {} -> matchSuccess $ return True
    --         _    -> matchFail
    --     , case c of
    --         R x  -> matchSuccess $ isSafe x
    --         _    -> matchFail
    --     , case c of
    --         -- Expr ":/" [_,a] -> do
    --             -- let
    --             --     canBeZero (L (I 0)) = return True
    --             --     canBeZero (R r) = do
    --             --         val <- lookUpRef r
    --             --         
    --             --     canBeZero (Expr ":value-literal" [x]) = canBeZero x
    --             --     canBeZero (Expr ":value"         [x]) = canBeZero x
    --             --     canBeZero _ = return False
    --             -- not <$> canBeZero a
    --             -- matchSuccess $ return $ not $ canBeZero a
    --             -- case possiblyZero of
    --             --     L (I 0) -> matchSuccess $ return False
    --             --     L (I _) -> matchSuccess $ return True
    --             --     R x -> do
    --             --         val <- lift $ lookUpRef x
    --             --         case val of
    --             --             Expr "int-domain-from-to" [Expr "from" [L (I fr)], Expr "to" [L (I to)]]
    --             --                 | 0 `elem` [fr..to] -> matchSuccess $ return False
    --             --                 | otherwise         -> matchSuccess $ return True
    --             --             Expr "int-domain-list" xs -> do
    --             --                 is <- lift (mapM toInt xs)
    --             --                 matchSuccess $ return $ 0 `elem` is
    --             --             _ -> matchFail
    --             --     _ -> matchFail
    --         _ -> matchFail
    --     -- , case c of
    --     --     Expr 
    --     , matchSuccess $ return False
    --     ]
    -- isSafe (Expr "div" [_,L (I 0)]) = return False
    -- isSafe (Expr "div" [_,possiblyZero]) = do
    -- isSafe p@(Expr {}) = do
    --     tell [ "Assumed to be safe:" <+> showAST p ]
    --     return True
    -- isSafe p@(Let r _ x) = do
    --     tell [ "Passing through a let expression:" <+> Pr.parens (showAST r) <+> showAST p ]
    --     isSafe x

instance IsSafe Reference where
    isSafe r = do
        val <- lookUpRef r
        isSafe val

