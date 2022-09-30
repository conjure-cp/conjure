{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Conjure.Language.AST.ASTParser where

import Conjure.Prelude hiding (many)

import Conjure.Language.AST.Helpers
import Conjure.Language.AST.Syntax
import Conjure.Language.NewLexer hiding (Parser)

import Conjure.Language.AST.Expression
import Conjure.Language.Lexemes
import Text.Megaparsec

import Data.Text (pack)

parseTopLevel :: Parser StatementNode
parseTopLevel =
    do
        parseDeclaration
        <|> parseBranching
        <|> parseSuchThat
        <|> parseWhere
        <|> parseObjective

parseBranching :: Parser StatementNode
parseBranching = do
    lBranching <- need L_branching
    lOn <- want L_on
    statements <- squareBracketList (commaList parseBranchingPart)
    return $ Branching $ BranchingStatementNode lBranching lOn statements

parseBranchingPart :: Parser BranchingOnNode
parseBranchingPart =
    do
        BranchingOnName <$> parseIdentifier
        <|> (BranchingOnExpression <$> parseExpression)

parseSuchThat :: Parser StatementNode
parseSuchThat = do
    lSuch <- need L_such
    lThat <- want L_that
    exprs <- commaList parseExpression
    return $ SuchThat $ SuchThatStatementNode lSuch lThat exprs

parseWhere :: Parser StatementNode
parseWhere = do
    lWhere <- need L_where
    exprs <- commaList parseExpression
    return $ Where $ WhereStatementNode lWhere exprs

parseObjective :: Parser StatementNode
parseObjective = do
    Objective <$> parseObjectiveStatement

parseDeclaration :: Parser StatementNode
parseDeclaration =
    Declaration
        <$> do
            (LettingStatement <$> parseLetting)
                <|> (GivenStatement <$> parseGiven)
                <|> (FindStatement <$> parseFind)

parseLetting :: Parser LettingStatementNode
parseLetting = do
    lLetting <- need L_letting
    names <- commaList parseIdentifier
    lBe <- want L_be
    choice
        [ finishDomain $ LettingDomain lLetting names lBe
        , try $ finishEnum $ LettingEnum lLetting names lBe
        , try $ finishAnon $ LettingAnon lLetting names lBe
        , finishExpression $ LettingExpr lLetting names lBe
        ]
  where
    finishDomain start = do
        lDomain <- need L_domain
        domain <- parseDomain
        return $ start lDomain domain
    finishExpression start = do
        expr <- parseExpression
        return $ start expr
    finishEnum start = do
        lNew <- need L_new
        lType <- need L_type
        lEnum <- need L_enum
        members <- curlyBracketList $ commaList parseIdentifier
        return $ start lNew lType lEnum members
    finishAnon start = do
        lNew <- need L_new
        lType <- want L_type
        lOf <- want L_of
        lSize <- want L_size
        expr <- parseExpression
        return $ start lNew lType lOf lSize expr

parseGiven :: Parser GivenStatementNode
parseGiven = do
    lGiven <- need L_given
    names <- commaList parseIdentifier
    choice
        [ finishEnum (GivenEnumNode lGiven names)
        , finishDomain (GivenStatementNode lGiven names)
        ]
  where
    finishEnum start = do
        lNew <- need L_new
        lType <- want L_type
        lEnum <- want L_enum
        return $ start lNew lType lEnum
    finishDomain start = do
        lColon <- want L_Colon -- want here so that parse cannot fail
        domain <- parseDomain
        return $ start lColon domain

parseFind :: Parser FindStatementNode
parseFind = do
    lFind <- need L_find
    names <- commaList parseIdentifier
    lColon <- want L_Colon
    domain <- parseDomain
    return $ FindStatementNode lFind names lColon domain

parseObjectiveStatement :: Parser ObjectiveStatementNode
parseObjectiveStatement = do
    s <- eSymbol L_minimising <|> eSymbol L_maximising
    e <- parseExpression
    return $ case s of
        (ETok {lexeme=L_minimising}) -> ObjectiveMin (RealToken s) e
        _ -> ObjectiveMax (RealToken s) e

parseProgram :: Parser ProgramTree
parseProgram = do
    tl <- many parseTopLevel
    eof 
    return $ ProgramTree tl

example :: String -> IO ()
example s = do
    let str = s
    let other = [ETok (0, 0, 0, L_EOF) [] L_EOF ""]
    let txt  = pack str
    let lexed = parseMaybe eLex  txt
    let stream = ETokenStream txt $ fromMaybe other lexed
    parseTest parseProgram stream

demoString :: String
demoString =
    intercalate
        "\n"
        [ "letting letters be new type enum {S,E,N,D,M,O,R,Y}"
        , "find f : function (injective) letters --> int(0..9)"
        , "such that"
        , "                   1000 * f(S) + 100 * f(E) + 10 * f(N) + f(D) +"
        , "                   1000 * f(M) + 100 * f(O) + 10 * f(R) + f(E) ="
        , "    10000 * f(M) + 1000 * f(O) + 100 * f(N) + 10 * f(E) + f(Y)"
        , ""
        , "such that f(S) > 0, f(M) > 0"
        ]

demo2 :: String
demo2 = intercalate "\n"[
    "given n : int(1..2)"
    ,"find perm : sequence (size n) of int(1..n)"
    ,"such that"
    ,"    allDiff([perm(k) | k : int(1..n) ]),"
    ,"    and([ max(subs) - min(subs) + 1 != |subs| |"
    ,"        i : int(1..n-1), j : int(2..n),"
    ,"        i < j,"
    ,"        !(i = 1 /\\ j = n),"
    ,"        letting subs be [perm(k) | k : int(i..j)]]"
    ,"    )"
    ]