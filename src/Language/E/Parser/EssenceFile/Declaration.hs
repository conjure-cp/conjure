{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Parser.EssenceFile.Declaration ( parseTopLevels ) where

import Conjure.Prelude
import Language.E.Parser.Imports
import Language.E.Parser.EssenceFile.Domain ( parseDomain )
import Language.E.Parser.EssenceFile.Expr ( parseExpr, parseQuantifiedExpr, parseReference, parseMetaVariable )

import Language.E.Definition
import Language.E.Lexer ( Lexeme(..) )

import Text.Parsec ( (<?>), try )
import Text.Parsec.Combinator ( sepBy, sepEndBy1 )



parseTopLevels :: Parser [E]
parseTopLevels = do
    let one = msum $ map try
                [ do
                    lexeme L_find
                    decls <- flip sepEndBy1 comma $ do
                        is <- parseReference `sepEndBy1` comma
                        j  <- colon >> parseDomain
                        return [ [xMake| topLevel.declaration.find.name   := [i]
                                       | topLevel.declaration.find.domain := [D j]
                                       |]
                               | i <- is ]
                    return $ concat decls
                    <?> "find statement"
                , do
                    lexeme L_given
                    decls <- flip sepEndBy1 comma $ do
                        is <- parseReference `sepEndBy1` comma
                        msum
                            [ do
                                colon
                                j <- parseDomain
                                return [ [xMake| topLevel.declaration.given.name   := [i]
                                               | topLevel.declaration.given.domain := [D j]
                                               |]
                                       | i <- is ]
                            , do
                                lexeme L_new
                                msum
                                    [ do
                                        lexeme L_type
                                        lexeme L_enum
                                        return [ [xMake| topLevel.declaration.given.name     := [i]
                                                       | topLevel.declaration.given.typeEnum := []
                                                       |]
                                               | i <- is ]
                                    , do
                                        lexeme L_domain
                                        lexeme L_int
                                        return [ [xMake| topLevel.declaration.given.name    := [i]
                                                       | topLevel.declaration.given.typeInt := []
                                                       |]
                                               | i <- is ]
                                    ]
                            ]
                    return $ concat decls
                    <?> "given statement"
                , do
                    lexeme L_letting
                    decls <- flip sepEndBy1 comma $ do
                        is <- (try parseMetaVariable <|> parseReference) `sepEndBy1` comma
                        lexeme L_be
                        msum
                            [ do
                                lexeme L_new
                                lexeme L_type
                                msum
                                    [ do
                                        lexeme L_of
                                        lexeme $ LIdentifier "size"
                                        j <- parseExpr
                                        return [ [xMake| topLevel.letting.name := [i]
                                                       | topLevel.letting.typeUnnamed := [j]
                                                       |]
                                               | i <- is
                                               ]
                                    , do
                                        lexeme L_enum
                                        ys <- braces (parseReference `sepBy` comma) <|> return []
                                        return [ [xMake| topLevel.letting.name := [i]
                                                       | topLevel.letting.typeEnum.values := ys
                                                       |]
                                               | i <- is
                                               ]
                                    ]
                            , do
                                lexeme L_domain
                                j <- parseDomain
                                return [ [xMake| topLevel.letting.name   := [i]
                                               | topLevel.letting.domain := [D j]
                                               |]
                                       | i <- is
                                       ]
                            , do
                                j <- parseExpr
                                return [ [xMake| topLevel.letting.name := [i]
                                               | topLevel.letting.expr := [j]
                                               |]
                                       | i <- is
                                       ]
                            , do
                                j <- parseLambda L_lambda
                                return [ [xMake| topLevel.letting.name   := [i]
                                               | topLevel.letting.lambda := [j]
                                               |]
                                       | i <- is
                                       ]
                            , do
                                j <- parseQuanDecl
                                return [ [xMake| topLevel.letting.name       := [i]
                                               | topLevel.letting.quantifier := [j]
                                               |]
                                       | i <- is
                                       ]
                            ]
                    return $ concat decls
                    <?> "letting statement"
                , do
                    lexeme L_dim
                    is <- parseReference `sepEndBy1` comma
                    j  <- colon >> parseDomain
                    return [ [xMake| topLevel.declaration.dim.name   := [i]
                                   | topLevel.declaration.dim.domain := [D j]
                                   |]
                           | i <- is
                           ]
                    <?> "dim statement"
                , do
                    let dimfind = do
                            lexeme L_find
                            i <- parseExpr
                            colon
                            j <- parseDomain
                            return [xMake| dimFind.name   := [i]
                                         | dimFind.domain := [D j]
                                         |]
                    let nested = try dimfind <|> try (parseQuantifiedExpr nested) <|> parens nested
                    i <- nested
                    return [ [xMake| topLevel.declaration.nestedDimFind := [i]
                                   |]
                           ]
                    <?> "find statement"
                , do
                    lexeme L_where
                    xs <- parseExpr `sepEndBy1` comma
                    return [ [xMake| topLevel.where := [x] |]
                           | x <- xs ]
                    <?> "where statement"
                , do
                    lexeme L_such
                    lexeme L_that
                    xs <- parseExpr `sepEndBy1` comma
                    return [ [xMake| topLevel.suchThat := [x] |]
                           | x <- xs ]
                    <?> "such that statement"
                , do
                    lexeme L_minimising
                    x <- parseExpr
                    return [ [xMake| topLevel.objective.minimising := [x]
                                   |]
                           ]
                    <?> "objective"
                , do
                    lexeme L_maximising
                    x <- parseExpr
                    return [ [xMake| topLevel.objective.maximising := [x]
                                   |]
                           ]
                    <?> "objective"
                , do
                    lexeme L_branching
                    lexeme L_on
                    x <- parseExpr
                    return [ [xMake| topLevel.branchingOn := [x]
                                   |]
                           ]
                    <?> "branching on"
                ]
    concat <$> some one

parseLambda :: Lexeme -> Parser E
parseLambda l = do
    lexeme l
    braces $ do
        param <- parseExpr
        lexeme L_LongArrow
        body  <- parseExpr
        return [xMake| lambda.param := [param]
                     | lambda.body  := [body]
                     |]

parseQuanDecl :: Parser E
parseQuanDecl = do
    lexeme L_quantifier
    braces $ do
        append   <- parseLambda $ LIdentifier "append"
        guardE   <- parseLambda $ LIdentifier "guard"
        identity <- lexeme (LIdentifier "identity") *> parseExpr
        return [xMake| quantifierDecl.append   := [append]
                     | quantifierDecl.guard    := [guardE]
                     | quantifierDecl.identity := [identity]
                     |]
