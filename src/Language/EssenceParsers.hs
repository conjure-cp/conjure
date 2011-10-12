module Language.EssenceParsers ( pExpr, pValue ) where

import Control.Applicative
import Language.Essence
import ParsecUtils


pExpr :: Parser Expr
pExpr = choice $ map try [ pIdentifier <?> "identifier"
                         , pValue <?> "inline value"
                         ]

pIdentifier :: Parser Expr
pIdentifier = Identifier <$> identifier


--------------------------------------------------------------------------------
-- Parsers for inline values ---------------------------------------------------
--------------------------------------------------------------------------------

pValue :: Parser Expr
pValue = choice $ map try [ pBool, pInteger, pMatrix, pTuple, pSet, pMSet, pFunction, pRelation, pPartition ]

pBool :: Parser Expr
pBool = ValueBoolean False <$ reserved "false"
        <|>
        ValueBoolean True  <$ reserved "true"

pInteger :: Parser Expr
pInteger = ValueInteger <$> integer

pMatrix :: Parser Expr
pMatrix = ValueMatrix <$> brackets (sepBy pExpr comma)

pTuple :: Parser Expr
pTuple = ValueTuple <$> parens (countSepAtLeast 2 pExpr comma)

pSet :: Parser Expr
pSet = ValueSet <$> (reserved "set" *> braces (sepBy pExpr comma))

pMSet :: Parser Expr
pMSet = ValueMSet <$> (reserved "mset" *> braces (sepBy pExpr comma))

pFunction :: Parser Expr
pFunction = ValueFunction <$> (reserved "function" *> braces (sepBy pTuple2 comma))
    where
        pTuple2 :: Parser (Expr, Expr)
        pTuple2 = (,) <$> pExpr <*> (reservedOp "->" *> pExpr)

pRelation :: Parser Expr
pRelation = ValueRelation <$> (reserved "relation" *> braces (sepBy pTuple comma))

pPartition :: Parser Expr
pPartition = ValuePartition <$> (reserved "partition" *> braces (sepBy aPart comma))
    where
        aPart :: Parser [Expr]
        aPart = braces (sepBy pExpr comma)
