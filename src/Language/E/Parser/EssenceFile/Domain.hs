{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Parser.EssenceFile.Domain ( parseDomain ) where

import Language.E.Parser.Imports
import Language.E.Parser.Shunt ( shuntingYardDomain )
import Language.E.Parser.EssenceFile.Expr ( parseExpr, parseMetaVariable, parseReference )

import Language.E.Imports
import Language.E.Definition
import Language.E.Lexer ( Lexeme(..) )

import Text.Parsec ( (<?>), try )
import Text.Parsec.Combinator ( optionMaybe, sepBy, sepBy1 )



parseRange :: Parser E
parseRange = msum [try pRange, pSingle]
    where
        pRange = do
            fr <- optionMaybe parseExpr
            dot; dot
            to <- optionMaybe parseExpr
            return $ case (fr,to) of
                (Nothing, Nothing) -> [xMake| range.open   := []    |]
                (Just x , Nothing) -> [xMake| range.from   := [x]   |]
                (Nothing, Just y ) -> [xMake| range.to     := [y]   |]
                (Just x , Just y ) -> [xMake| range.fromTo := [x,y] |]
        pSingle = do
            x <- parseExpr
            return [xMake| range.single := [x] |]

parseDomain :: Parser E
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
            , parseMetaVariable, pParens
            ]

        pParens = parens parseDomain

        pBool = do
            lexeme L_bool
            return [xMake| domain.bool := [] |]

        pInt = do
            lexeme L_int
            mxs <- optionMaybe $ parens $ parseRange `sepBy` comma
            let xs = fromMaybe [] mxs
            return [xMake| domain.int.ranges := xs |]

        pEnum = do
            r <- parseReference
            xs <- optionMaybe $ parens $ parseRange `sepBy` comma
            case xs of
                Nothing -> return r
                Just ys -> return [xMake| domain.enum.name   := [r]
                                        | domain.enum.ranges := ys
                                        |]

        pMatrix = do
            lexeme L_matrix
            lexeme L_indexed
            lexeme L_by
            xs <- brackets (parseDomain `sepBy1` comma)
            lexeme L_of
            y  <- parseDomain
            return $
                foldr (\ i j -> [xMake| domain.matrix.index := [i]
                                      | domain.matrix.inner := [j]
                                      |]
                      ) y xs

        pTupleWith = do
            lexeme L_tuple
            xs <- parens $ parseDomain `sepBy` comma
            return [xMake| domain.tuple.inners := xs |]

        pTupleWithout = do
            xs <- parens $ countSepAtLeast 2 parseDomain comma
            return [xMake| domain.tuple.inners := xs |]

        pSet = do
            lexeme L_set
            x <- parseAttributes
            y <- lexeme L_of >> parseDomain
            return [xMake| domain.set.attributes := [x]
                         | domain.set.inner      := [y]
                         |]

        pMSet = do
            lexeme L_mset
            x <- parseAttributes
            y <- lexeme L_of >> parseDomain
            return [xMake| domain.mset.attributes := [x]
                         | domain.mset.inner      := [y]
                         |]

        pFunction = do
            lexeme L_function
            (y,z) <- arrowedPair parseDomain
            return [xMake| domain.function.attributes.attrCollection := []
                         | domain.function.innerFrom  := [y]
                         | domain.function.innerTo    := [z]
                         |]

        pFunction' = do
            lexeme L_function
            x <- parseAttributes
            y <- parseDomain
            lexeme L_LongArrow
            z <- parseDomain
            return [xMake| domain.function.attributes := [x]
                         | domain.function.innerFrom  := [y]
                         | domain.function.innerTo    := [z]
                         |]

        pRelation' = do
            lexeme L_relation
            return [xMake| type.relation.inners.type.unknown := [] |]

        pRelation = do
            lexeme L_relation
            x  <- parseAttributes
            lexeme L_of
            ys <- parens (parseDomain `sepBy` lexeme L_Times)
            return [xMake| domain.relation.attributes := [x]
                         | domain.relation.inners     := ys
                         |]

        pPartition = do
            lexeme L_partition
            x <- parseAttributes
            lexeme L_from
            y <- parseDomain
            return [xMake| domain.partition.attributes := [x]
                         | domain.partition.inner      := [y]
                         |]

parseAttributes :: Parser E
parseAttributes = do
    xs <- parens (parseAttribute `sepBy` comma) <|> return []
    return [xMake| attrCollection := map snd xs
                 |]
    -- return [xMake| attrCollection := map snd $ sortBy (comparing fst) xs
    --              |]
    where
        parseAttribute = msum [try parseNameValue, try parseName, parseDontCare]
        parseNameValue = do
            n <- parseReference
            v <- parseExpr
            return
                ( Just n
                , [xMake| attribute.nameValue.name  := [n]
                        | attribute.nameValue.value := [v]
                        |]
                )
        parseName = do
            n <- parseReference
            return
                ( Just n
                , [xMake| attribute.name := [n]
                        |]
                )
        parseDontCare = do
            dot; dot
            return
                ( Nothing
                , [xMake| attribute.dontCare := []
                        |]
                )
