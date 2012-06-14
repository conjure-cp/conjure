{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Properties.DomainOf where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ShowAST
import Language.Core.Properties.Pretty


class DomainOf a where
    domainOf :: Monad m => a -> CompT m Core

instance DomainOf Core where
    domainOf (L x) = domainOf x
    domainOf (R x) = domainOf x

    domainOf _p@( viewDeep [":domain-in-expr"] -> Just [d] ) = domainOf d
    domainOf  p@( viewDeep [":domain"]         -> Just [_] ) = return p

    domainOf ( viewDeep [":metavar"] -> Just [R x] ) = domainOf ("@" `mappend` x)

    domainOf ( viewDeep [":toplevel",":declaration",":find"]
              -> Just [ Expr ":find-name" _
                      , Expr ":find-domain" [domain]
                      ]
             ) = return domain

    domainOf x@( viewDeep [":value",":value-set"] -> Just xs ) = do
        dxs <- mapM domainOf xs
        domainUnions (pretty x) dxs

    domainOf x = err ErrDomainOf
                    $ singletonNested
                    $ "Unknown domain." <++> vcat [ pretty x
                                                  , showAST x
                                                  ]

instance DomainOf Reference where
    domainOf r = do
        val <- lookUpRef r
        domainOf val

instance DomainOf Literal where
    domainOf (B {}) = return $ Expr ":domain" [Expr ":domain-bool" []]
    domainOf (I i )
        = return $ Expr ":domain"
                 [ Expr ":domain-int"
                 [ Expr ":domain-int-ranges"
                 [ Expr ":range"
                 [ Expr ":range-single"
                 [ Expr ":value"
                 [ Expr ":value-literal" [L $ I i]
                 ]]]]]]


domainUnions :: Monad m => Doc -> [Core] -> CompT m Core
domainUnions msg [] = err ErrDomainOf
                    $ singletonNested
                    $ "Unknown domain." <++> msg
domainUnions _ [x] = return x
domainUnions _ (x:xs)   = foldM domainUnion x xs

domainUnion :: Monad m => Core -> Core -> CompT m Core
domainUnion a b = err ErrDomainOf
                    $ singletonNested
                    $ "Cannot calculate the union of two domains" $$ vcat [ pretty a, pretty b ]
