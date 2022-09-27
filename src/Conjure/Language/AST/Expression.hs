{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Conjure.Language.AST.Expression where

import Conjure.Language.AST.Helpers
import Conjure.Language.AST.Syntax
import Conjure.Language.NewLexer hiding (Parser)
import Conjure.Prelude hiding (many)
import Text.Megaparsec

parseExpression :: Parser ExpressionNode
parseExpression = do
    choice [
        Literal <$> parseLiteral
        ]
    

parseExpressionStrict :: Parser ExpressionNode -- can fail
parseExpressionStrict = do
    expr <- try parseExpression
    case expr of
        MissingExpressionNode _ -> empty
        _ -> return expr

parseLiteral :: Parser LiteralNode
parseLiteral =
    choice
        [ parseIntLiteral
        , parseBoolLiteral
        , parseMatrixLiteral
        , parseTupleLiteral
        , parseRecordLiteral
        , parseSetLiteral
        , parseMSetLiteral
        , parseFunctionLiteral
        , parseSequenceLiteral
        , parseRelationLiteral
        , parsePartitionLiteral
        ]

parseIntLiteral :: Parser LiteralNode
parseIntLiteral = IntLiteral . RealToken <$> intLiteral

parseBoolLiteral :: Parser LiteralNode
parseBoolLiteral = BoolLiteral <$> (need L_true <|> need L_false)

parseMatrixLiteral :: Parser LiteralNode
parseMatrixLiteral = do empty

parseTupleLiteral :: Parser LiteralNode
parseTupleLiteral = do
    lTuple <- need L_tuple
    members <- parenList $ commaList parseExpression
    return $ TupleLiteralNode $ LongTuple lTuple members

parseRecordLiteral :: Parser LiteralNode
parseRecordLiteral = do
    lRecord <- need L_record
    members <- curlyBracketList (commaList parseRecordMember)
    return $ RecordLiteral lRecord members

parseVariantLiteral :: Parser LiteralNode
parseVariantLiteral = do
    lVariant <- need L_variant
    members <- curlyBracketList (commaList parseRecordMember)
    return $ RecordLiteral lVariant members

parseRecordMember :: Parser RecordMemberNode
parseRecordMember = do
    name <- parseIdentifier
    lEqual <- want L_Eq
    val <- parseExpression
    return $ RecordMemberNode name lEqual val

parseSetLiteral :: Parser LiteralNode
parseSetLiteral = SetLiteral <$> curlyBracketList (commaList parseExpression)
    

parseMSetLiteral :: Parser LiteralNode
parseMSetLiteral = do 
    lMSet <- need L_mset 
    members <- parenList (commaList parseExpression)
    return $ MSetLiteral lMSet members 

parseFunctionLiteral :: Parser LiteralNode
parseFunctionLiteral = do
    lFunc <- need L_function
    args <- parenList (commaList parseArrowPair)
    return $ FunctionLiteral lFunc args

parseArrowPair :: Parser ArrowPairNode
parseArrowPair = try $ do 
    lhs <- parseExpression
    arrow <- want L_LongArrow
    rhs <- parseExpression
    return $ ArrowPairNode lhs arrow rhs

parseSequenceLiteral :: Parser LiteralNode
parseSequenceLiteral = do 
    lSeq <- need L_sequence
    members <- parenList (commaList parseExpression)
    return $ SequenceLiteral lSeq members 

parseRelationLiteral :: Parser LiteralNode
parseRelationLiteral = do 
    lRel <- need L_relation
    members <- parenList (commaList parseRelationMember)
    return $ RelationLiteral lRel members

parseRelationMember :: Parser RelationElemNode
parseRelationMember = do
    f <- optional $ need L_tuple
    members <- parenList $ commaList parseExpression
    return $ case f of 
        Nothing -> RelationElemNodeShort $ ShortTuple members
        Just lTup -> RelationElemNodeLabeled $ LongTuple lTup members

parsePartitionLiteral :: Parser LiteralNode
parsePartitionLiteral = do 
    lPartition <- need L_partition
    members <- parenList (commaList parsePartitionElem)
    return $ PartitionLiteral lPartition members

parsePartitionElem :: Parser PartitionElemNode
parsePartitionElem = PartitionElemNode <$> parenList (commaList parseExpression)
