{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Parser.EssenceFile.Value ( parseValue ) where

import Language.E.Parser.Imports
import {-# SOURCE #-} Language.E.Parser.EssenceFile.Domain ( parseDomain )
import Language.E.Parser.EssenceFile.Expr ( parseExpr )

import Stuff.Generic
import Language.E.Imports
import Language.E.Definition
import Language.E.Lexer ( Lexeme(..) )

import Text.Parsec ( (<?>), try )
import Text.Parsec.Combinator ( sepBy )



parseValue :: Parser E
parseValue = msum ( map try
    [ pBool, pInt
    , pMatrix, pMatrix', pTupleWith, pTupleWithout
    , pSet, pMSet
    , pFunction, pRelation, pPartition
    ] ) <?> "value"
    where
        pBool = do
            x <- Prim (B False) <$ lexeme L_false
                 <|>
                 Prim (B True)  <$ lexeme L_true
            return [xMake| value.literal := [x] |]

        pInt = do
            x <- Prim . I <$> integer
            return [xMake| value.literal := [x] |]

        pMatrix = do
            xs <- brackets (sepBy parseExpr comma)
            return [xMake| value.matrix.values := xs |]

        pMatrix' = brackets $ do
            xs <- sepBy parseExpr comma
            lexeme L_SemiColon
            r <- parseDomain
            return [xMake| value.matrix.values     := xs
                         | value.matrix.indexrange := [r]
                         |]
        pTupleWith = do
            lexeme L_tuple
            xs <- parens $ sepBy parseExpr comma
            return [xMake| value.tuple.values := xs |]

        pTupleWithout = do
            xs <- parens $ countSepAtLeast 2 parseExpr comma
            return [xMake| value.tuple.values := xs |]

        pSet = do
            xs <- braces (sepBy parseExpr comma)
            return [xMake| value.set.values := xs |]

        pMSet = do
            lexeme L_mset
            xs <- parens (sepBy parseExpr comma)
            return [xMake| value.mset.values := xs |]

        pFunction = do
            lexeme L_function
            xs <- parens (sepBy inner comma)
            return [xMake| value.function.values := xs |]
            where
                inner = do
                    (i,j) <- arrowedPair parseExpr
                    return [xMake| mapping := [i,j] |]

        pRelation = do
            lexeme L_relation
            xs <- parens (sepBy (try pTupleWith <|> pTupleWithout) comma)
            return [xMake| value.relation.values := xs |]

        pPartition = do
            lexeme L_partition
            xs <- parens (sepBy inner comma)
            return [xMake| value.partition.values := xs|]
            where
                inner = do
                    is <- braces (sepBy parseExpr comma)
                    return [xMake| part := is |]
