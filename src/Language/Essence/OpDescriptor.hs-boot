module Language.Essence.OpDescriptor where

import Language.EssenceLexerP ( Parser )
import qualified PrintUtils as Pr

import {-# SOURCE #-} Language.Essence.Expr
import Language.Essence.Op


data Fixity = InfixL | InfixN | InfixR

instance Eq Fixity

data OpDescriptor
    = OpLispy
            (Parser Expr)
            ([Expr] -> Pr.Doc)
    | OpInfix
            (Int, Fixity, Parser (Expr -> Expr -> Expr))
            ((Int -> Expr -> Pr.Doc) -> Int -> Expr -> Expr -> Pr.Doc)
    | OpPrefix
            (Parser (Expr -> Expr))
            (Expr -> Pr.Doc)
    | OpPostfix
            (Parser (Expr -> Expr))
            (Expr -> Pr.Doc)
    | OpSpecial
            (Parser Expr)
            (Expr -> Pr.Doc)

opDescriptor :: Op -> OpDescriptor
