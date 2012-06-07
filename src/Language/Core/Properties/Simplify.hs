{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Properties.Simplify where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.MatchBind ( match )
import Language.Core.Properties.TypeOf
import Language.Core.Properties.DomainOf
import Language.Core.Properties.ShowAST
import Language.Core.Properties.Pretty


class Simplify a where
    simplify :: (Functor m, Monad m) => a -> WriterT Any (CompT m) Core

instance Simplify Core where
    -- simplify (Expr ":negate" [Expr ":value" [Expr ":value-literal" [L (I i)]]])
    --     = do
    --         tell $ Any True
    --         return $ Expr ":value" [Expr ":value-literal" [L $ I $ negate i]]

    simplify ( viewDeep [":metavar"] -> Just [R x] ) = simplify ("@" `mappend` x)

    simplify  p@( viewDeep [":operator-hastype"] -> Just [a,b] ) = do
        lift $ mkLog "simplify" $ pretty p
        ta   <- lift $ typeOf a
        tb   <- lift $ typeOf b
        flag <- lift $ typeUnify ta tb
        tell $ Any True
        return $ L $ B flag
    simplify  p@( viewDeep [":operator-hasdomain"] -> Just [a,b] ) = do
        lift $ mkLog "simplify" $ pretty p
        da   <- lift $ domainOf a
        db   <- lift $ domainOf b
        flag <- lift $ domainUnify da db
        tell $ Any True
        return $ L $ B flag

    simplify _p@( viewDeep [":operator-\\/"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B True)]]
                           , _
                           ]
                 ) = returnTrue
    simplify _p@( viewDeep [":operator-\\/"]
                   -> Just [ _
                           , Expr ":value" [Expr ":value-literal" [L (B True)]]
                           ]
                 ) = returnTrue

    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ Expr ":empty-guard" []
                           , x
                           ]
                 ) = do tell $ Any True
                        return x
    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ x
                           , Expr ":empty-guard" []
                           ]
                 ) = do tell $ Any True
                        return x

    -- simplify _p@( viewDeep [":operator-\\/"] -> Just [a,b] ) = do
    --     a' <- simplify a
    --     b' <- simplify b
    --     return $ Expr ":operator-\\/" [a',b']
    simplify _p@( viewDeep [":operator-="] -> Just [R a,R b] ) | a == b = do
        tell $ Any True
        returnTrue
    -- simplify  p@( viewDeep [":operator-="] -> Just [a,b] ) = do
    --     a' <- simplify a
    --     b' <- simplify b
    --             return $ Expr ":operator-=" [a',b']
    simplify p@(Expr t xs) = do
        ys <- mapM simplify xs
        let result = Expr t ys
        lift $ mkLog "simplify" $ "generic case:" <++> vcat [ pretty p
                                                            , pretty result
                                                            ]
        return result
    simplify x = do
        lift $ mkLog "simplify" $ "default case:" <++>
                                vcat [ pretty x
                                     , showAST x
                                     ]
        return x

instance Simplify Reference where
    simplify r = core <?> "domain check for reference" <+> showAST r
        where
            core = do
                val <- lift $ lookUpRef r
                tell $ Any True
                simplify val


returnTrue :: (Functor m, Monad m) => WriterT Any (CompT m) Core
returnTrue  = do tell $ Any True; return $ Expr ":value" [Expr ":value-literal" [L (B True )]]

returnFalse :: (Functor m, Monad m) => WriterT Any (CompT m) Core
returnFalse = do tell $ Any True; return $ Expr ":value" [Expr ":value-literal" [L (B False)]]

domainUnify :: Monad m => Core -> Core -> CompT m Bool
domainUnify y x = do
    mkLog "domainUnify" $ pretty x <+> "~~" <++> pretty y
    match x y
