{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Properties.Simplify where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.TypeOf
import Language.Core.Properties.ShowAST
import Language.Core.Properties.Pretty


class Simplify a where
    simplify :: (Functor m, Monad m) => a -> WriterT Any (CompT m) Core

instance Simplify Core where
    -- simplify (Expr ":negate" [Expr ":value" [Expr ":value-literal" [L (I i)]]])
    --     = do
    --         tell $ Any True
    --         return $ Expr ":value" [Expr ":value-literal" [L $ I $ negate i]]
    simplify _p@( viewDeep [":operator-hastype"] -> Just [a,b] ) = do
        -- lift $ mkLog "simplify" $ pretty p
        ta   <- lift $ typeOf a
        tb   <- lift $ typeOf b
        flag <- lift $ typeUnify ta tb
        tell $ Any True
        return $ L $ B flag
    simplify x = do
        lift $ mkLog "simplify" $ "default case:" <+> pretty x
        return x

instance Simplify Reference where
    simplify r = core <?> "domain check for reference" <+> showAST r
        where
            core = do
                val <- lift $ lookUpRef r
                simplify val

typeUnify :: (Functor m, Monad m) => Core -> Core -> CompT m Bool
typeUnify (viewDeep [":type-unknown"] -> Just []) _ = return True
typeUnify _ (viewDeep [":type-unknown"] -> Just []) = return True
typeUnify (Expr t1 xs1) (Expr t2 xs2)
    | t1 == t2
    , length xs1 == length xs2
    = and <$> zipWithM typeUnify xs1 xs2
typeUnify x y = do
    -- mkLog "typeUnify" $ vcat [ pretty x
    --                          , "~~"
    --                          , pretty y
    --                          ]
    return $ x == y
