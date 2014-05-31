{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Parser.EssenceFile.Domain ( parseDomain ) where

import Language.E.Parser.Imports
import Language.E.Parser.Shunt ( shuntingYardDomain )
import Language.E.Parser.EssenceFile.Expr ( parseExpr, parseMetaVariable )

import Language.E.Imports
import Language.E.Definition
import Language.E.Lexer ( Lexeme(..) )

import Text.Parsec ( (<?>), try )
import Text.Parsec.Combinator ( optionMaybe, sepBy, sepBy1 )



parseRange :: Parser Range
parseRange = msum [try pRange, pSingle]
    where
        pRange = do
            fr <- optionMaybe parseExpr
            dot; dot
            to <- optionMaybe parseExpr
            return $ case (fr,to) of
                (Nothing, Nothing) -> RangeOpen
                (Just x , Nothing) -> RangeLowerBounded x
                (Nothing, Just y ) -> RangeUpperBounded y
                (Just x , Just y ) -> RangeBounded x y
        pSingle = do
            x <- parseExpr
            return (RangeSingle x)

parseDomain :: Parser Domain
parseDomain
    = shuntingYardDomain
    $ some
    $ msum [ Right <$> try pDomainAtom
           , Left  <$> parseOp'
           ]
    where
        parseOp' = msum [ do lexeme x; return x | x <- [L_Minus, L_union, L_intersect] ] <?> "operator"
        pDomainAtom = msum $ map try
            [ pBool, pInt, pEnum
            , pMatrix, pTupleWithout, pTupleWith
            , pSet, pMSet, pFunction, pFunction'
            , pRelation, pRelation'
            , pPartition
            , DomainHack <$> parseMetaVariable, pParens
            ]

        pParens = parens parseDomain

        pBool = do
            lexeme L_bool
            return DomainBool

        pInt = do
            lexeme L_int
            mxs <- optionMaybe $ parens $ parseRange `sepBy` comma
            let xs = fromMaybe [] mxs
            return $ DomainInt xs

        pEnum = do
            r <- identifierText
            xs <- optionMaybe $ parens $ parseRange `sepBy` comma
            case xs of
                Nothing -> return $ DomainHack [xMake| reference := [Prim (S r)] |]
                Just ys -> return $ DomainEnum (DomainDefnEnum (Name r) (error "we need state in the parser")) ys

        pMatrix = do
            lexeme L_matrix
            lexeme L_indexed
            lexeme L_by
            xs <- brackets (parseDomain `sepBy1` comma)
            lexeme L_of
            y  <- parseDomain
            return $ foldr DomainMatrix y xs

        pTupleWith = do
            lexeme L_tuple
            xs <- parens $ parseDomain `sepBy` comma
            return $ DomainTuple xs

        pTupleWithout = do
            xs <- parens $ countSepAtLeast 2 parseDomain comma
            return $ DomainTuple xs

        pSet = do
            lexeme L_set
            x <- parseAttributes
            y <- lexeme L_of >> parseDomain
            return $ DomainSet x y

        pMSet = do
            lexeme L_mset
            x <- parseAttributes
            y <- lexeme L_of >> parseDomain
            return $ DomainMSet x y

        pFunction = do
            lexeme L_function
            (y,z) <- arrowedPair parseDomain
            return $ DomainFunction (DomainAttributes []) y z

        pFunction' = do
            lexeme L_function
            x <- parseAttributes
            y <- parseDomain
            lexeme L_LongArrow
            z <- parseDomain
            return $ DomainFunction x y z

        pRelation' = do
            lexeme L_relation
            return $ DomainHack [xMake| type.relation.inners.type.unknown := [] |]

        pRelation = do
            lexeme L_relation
            x  <- parseAttributes
            lexeme L_of
            ys <- parens (parseDomain `sepBy` lexeme L_Times)
            return $ DomainRelation x ys

        pPartition = do
            lexeme L_partition
            x <- parseAttributes
            lexeme L_from
            y <- parseDomain
            return $ DomainPartition x y

parseAttributes :: Parser DomainAttributes
parseAttributes = do
    xs <- parens (parseAttribute `sepBy` comma) <|> return []
    return $ DomainAttributes xs
    where
        parseAttribute = msum [try parseNameValue, try parseName, parseDontCare]
        parseNameValue = DANameValue <$> (Name <$> identifierText) <*> parseExpr
        parseName = DAName <$> (Name <$> identifierText)
        parseDontCare = do dot; dot ; return DADotDot

