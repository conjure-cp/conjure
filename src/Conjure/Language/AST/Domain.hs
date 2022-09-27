{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Conjure.Language.AST.Domain where

import Conjure.Language.AST.Expression
import Conjure.Language.AST.Helpers
import Conjure.Language.AST.Syntax
import Conjure.Language.NewLexer hiding (Parser)
import Conjure.Prelude
import Text.Megaparsec

parseDomain :: Parser DomainNode
parseDomain = do
    choice
        [ BoolDomainNode <$> need L_bool
        , parseIntDomain
        , parseTuple
        , parseRecord
        , parseVariant
        , parseMatrix
        , parseSet
        , parseMSet
        , parseFunction
        , parseSequenceDomain
        , parseRelation
        , parsePartition
        , parseEnumDomain
        , parseMissingDomain
        ]

parseIntDomain :: Parser DomainNode
parseIntDomain = do
    lInt <- need L_int
    ranges <- parenList $ commaList parseRange
    return $ RangedIntDomainNode lInt ranges

parseTuple :: Parser DomainNode
parseTuple = do
    lTuple <- need L_tuple
    members <- parenList $ commaList parseDomain
    return $ TupleDomainNode lTuple members

parseRecord :: Parser DomainNode
parseRecord = do
    lRecord <- need L_record
    members <- curlyBracketList $ commaList parseNameDomain
    return $ RecordDomainNode lRecord members

parseVariant :: Parser DomainNode
parseVariant = do
    lVariant <- need L_variant
    members <- curlyBracketList $ commaList parseNameDomain
    return $ VariantDomainNode lVariant members

parseMatrix :: Parser DomainNode
parseMatrix = do
    lMatrix <- need L_matrix
    lIndexed <- want L_indexed
    lBy <- want L_by
    members <- squareBracketList $ commaList parseDomain
    lOf <- want L_of
    domain <- parseDomain
    return $ MatrixDomainNode lMatrix lIndexed lBy members lOf domain

parseSet :: Parser DomainNode
parseSet = do
    lSet <- need L_set
    attributes <- parenList $ commaList parseAttribute
    lOf <- want L_of
    domain <- parseDomain
    return $ SetDomainNode lSet attributes lOf domain

parseMSet :: Parser DomainNode
parseMSet = do
    lMSet <- need L_mset
    attributes <- parenList $ commaList parseAttribute
    lOf <- want L_of
    domain <- parseDomain
    return $ MSetDomainNode lMSet attributes lOf domain

parseFunction :: Parser DomainNode
parseFunction = do
    lFunction <- need L_function
    attributes <- parenList $ commaList parseAttribute
    fromDomain <- parseDomain
    arrow <- want L_LongArrow
    toDomain <- parseDomain
    return $ FunctionDomainNode lFunction attributes fromDomain arrow toDomain

parseSequenceDomain :: Parser DomainNode
parseSequenceDomain = do
    lSequence <- need L_sequence
    attributes <- parenList $ commaList parseAttribute
    lOf <- want L_of
    domain <- parseDomain
    return $ SequenceDomainNode lSequence attributes lOf domain

parseRelation :: Parser DomainNode
parseRelation = do
    lRelation <- need L_relation
    attributes <- parenList $ commaList parseAttribute
    lOf <- want L_of
    domains <- parenList $ parseSequence L_Times parseDomain
    return $ RelationDomainNode lRelation attributes lOf domains

parsePartition :: Parser DomainNode
parsePartition = do
    lPartition <- need L_partition
    attributes <- parenList $ commaList parseAttribute
    lFrom <- want L_from
    domain <- parseDomain
    return $ PartitionDomainNode lPartition attributes lFrom domain

parseEnumDomain :: Parser DomainNode
parseEnumDomain = do
    name <- parseIdentifierStrict
    (RangedEnumNode name <$> try (parenList (commaList parseRange)))
        <|> return (EnumDomainNode name)

-- Util
parseNameDomain :: Parser NamedDomainNode
parseNameDomain = do
    name <- parseIdentifierStrict
    lColon <- want L_Colon
    domain <- parseDomain
    return $ NameDomainNode name lColon domain

parseRange :: Parser RangeNode
parseRange =
    do
        lExpr <- optional parseExpressionStrict
        dots <- parseDoubleDot
        rExpr <- optional parseExpressionStrict
        case (lExpr, rExpr) of
            (Nothing, Nothing) -> return $ OpenRangeNode dots
            (Just l, Nothing) -> return $ RightUnboundedRangeNode l dots
            (Nothing, Just r) -> return $ LeftUnboundedRangeNode dots r
            (Just l, Just r) -> return $ BoundedRangeNode l dots r
        <|> SingleRangeNode <$> parseExpression

parseDoubleDot :: Parser DoubleDotNode
parseDoubleDot = do
    a <- need L_Dot
    b <- want L_Dot
    return $ DoubleDotNode a b

parseAttribute :: Parser AttributeNode
parseAttribute = do
    name <- parseIdentifier -- TODO This is wrong
    NamedExpressionAttribute name <$> parseExpression
        <|> return (NamedAttributeNode name)

parseMissingDomain :: Parser DomainNode
parseMissingDomain = do
    m <- makeMissing L_domain
    return $ MissingDomainNode m