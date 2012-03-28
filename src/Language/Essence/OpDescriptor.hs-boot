module Language.Essence.OpDescriptor where

import Control.Monad.Identity ( Identity )

import ParsecUtils ( Parser, Operator )
import qualified PrintUtils as Pr

import {-# SOURCE #-} Language.Essence.Expr
import Language.Essence.Op



type OperatorParser = Operator String () Identity Expr

data Fixity = InfixL | InfixN | InfixR

data OpDescriptor
    = OpLispy
            (Parser Expr)
            ([Expr] -> Pr.Doc)
    | OpInfix
            (Int, OperatorParser)
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
