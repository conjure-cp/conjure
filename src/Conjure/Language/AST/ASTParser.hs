{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Conjure.Language.AST.ASTParser where

import Conjure.Prelude hiding (many)

import Conjure.Language.AST.Helpers
import Conjure.Language.AST.Syntax
import Conjure.Language.NewLexer

import Conjure.Language.AST.Expression
import Conjure.Language.Lexemes
import Text.Megaparsec

import Data.Text (pack)
import Data.Void (Void)
import Conjure.Language.AST.Reformer (Flattenable(..))

data ParserError = ParserError
    deriving (Show)
runASTParser :: Parser a -> ETokenStream -> Either ParserError a
runASTParser p str = case runParser p "Parser" str of
  Left peb -> Left ParserError
  Right res -> Right res

parseProgram :: Parser ProgramTree
parseProgram =  do
    (tl,ending) <- manyTill_ parseTopLevel pEnding
    return $ ProgramTree tl ending
    <?> "Program"

parseTopLevels :: Parser [StatementNode]
parseTopLevels = manyTill parseTopLevel pEnding

parseTopLevel :: Parser StatementNode
parseTopLevel = 
    do
            parseDeclaration
        <|> parseBranching
        <|> parseSuchThat
        <|> parseWhere
        <|> parseObjective
        <|> UnexpectedToken <$> makeUnexpected


parseBranching :: Parser StatementNode
parseBranching = do
    lBranching <- need L_branching
    lOn <- want L_on
    statements <- squareBracketList (commaList parseBranchingPart)
    return $ BranchingStatement $ BranchingStatementNode lBranching lOn statements

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
    return $ SuchThatStatement $ SuchThatStatementNode lSuch lThat exprs

parseWhere :: Parser StatementNode
parseWhere = do
    lWhere <- need L_where
    exprs <- commaList parseExpression
    return $ WhereStatement $ WhereStatementNode lWhere exprs

parseObjective :: Parser StatementNode
parseObjective = do
    ObjectiveStatement <$> parseObjectiveStatement

parseDeclaration :: Parser StatementNode
parseDeclaration =
    DeclarationStatement
        <$> choice [
                    LettingStatement <$> parseLetting,
                    GivenStatement <$> parseGiven,
                    FindStatement <$> parseFind
        ]
                

parseLetting :: Parser LettingStatementNode
parseLetting = do
    lLetting <- need L_letting
    names <- commaList parseIdentifier
    lBe <- want L_be
    let start = LettingStatementNode lLetting names lBe
    start <$> choice
        [ finishDomain 
        , try finishEnum 
        , try finishAnon 
        , LettingExpr <$> parseExpression 
        ]
  where
    finishDomain = do
        lDomain <- need L_domain
        domain <- parseDomain
        return $ LettingDomain lDomain domain
    finishEnum = do
        lNew <- need L_new
        lType <- need L_type
        lEnum <- need L_enum
        members <- curlyBracketList $ commaList parseIdentifier
        return $ LettingEnum lNew lType lEnum members
    finishAnon = do
        lNew <- need L_new
        lType <- want L_type
        lOf <- want L_of
        lSize <- want L_size
        expr <- parseExpression
        return $ LettingAnon lNew lType lOf lSize expr

parseGiven :: Parser GivenStatementNode
parseGiven = do
    lGiven <-  need L_given
    names <- commaList parseIdentifier
    choice
        [ finishEnum (GivenEnumNode lGiven names)
        , finishDomain (GivenStatementNode lGiven names)
        ]
  where
    finishEnum start =  do
        lNew <-  need L_new
        lType <- want L_type
        lEnum <- want L_enum
        return $ start lNew lType lEnum
    finishDomain start =  do
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
    <?> "Find Statement"
parseObjectiveStatement :: Parser ObjectiveStatementNode
parseObjectiveStatement = do
    s <- eSymbol L_minimising <|> eSymbol L_maximising
    e <- parseExpression
    return $ case s of
        (ETok {lexeme=L_minimising}) -> ObjectiveMin (RealToken s) e
        _ -> ObjectiveMax (RealToken s) e
    <?> "Objective Statement"


pEnding :: Parser LToken
pEnding =  do
    t <- lookAhead anySingle
    case t of
        ETok {lexeme=L_EOF} -> return $ RealToken t
        _ -> empty
example :: String -> IO ()
example s = do
    let str = s
    let other = [ETok (0, 0, 0, SourcePos "" (mkPos 0) (mkPos  0)) [] L_EOF ""]
    let txt  = pack str
    let lexed = parseMaybe eLex  txt
    putStrLn "Lexmes"
    putStrLn $ show  lexed
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
    "given n : int"
    ,"find perm : sequence (size n) of int(1..n)"
    ,"such that $comment"
    ,"    allDiff([perm(k) | k : int(1..n) ]),"
    ,"    and([ max(subs) - min(subs) + 1 != |subs| |"
    ,"        i : int(1..n-1), j : int(2..n),"
    ,"        i < j,"
    ,"        !(i = 1 /\\ j = n),"
    ,"        letting subs be [perm(k) | k : int(i..j)]]"
    ,"    )"
    ]

demo3 :: String
demo3 = intercalate "\n" [ "$COMMENT"
    ,"given n : int"
    ,"letting DOMAIN be domain int(1..n)"
    ,"given hints : function (DOMAIN, DOMAIN) --> DOMAIN"
    ,"given less_than : relation of ((DOMAIN, DOMAIN) * (DOMAIN, DOMAIN))"
    ,"find board : matrix indexed by [DOMAIN, DOMAIN] of DOMAIN"
    ,"such that"
    ,"    forAll (hint,num) in hints ."
    ,"        board[hint[1], hint[2]] = num,"
    ,"    forAll i: DOMAIN ."
    ,"        allDiff(board[i,..]),"
    ,"    forAll j: DOMAIN ."
    ,"        allDiff(board[..,j]),"
    ,"    forAll (l,g) in less_than ."
    ,"        board[l[1],l[2]] < board[g[1],g[2]]"
    ]

parsePrint :: String -> IO ()
parsePrint text = do
    toks <- parseAndRevalidate (pack text) eLex (concatMap reform) text
    case toks of
      Left (a,b)-> do
            putStrLn "Lexer wasn't reversible"
            showDiff a b
      Right ets -> putStrLn "Lexer success" >> do
            tree <- parseAndRevalidate (ETokenStream (pack text) ets) parseProgram (\v -> reformList (flatten v :: [ETok]) ) text
            case tree of
              Left (a,b) -> do
                        putStrLn "Parser wasn't reversible:"
                        showDiff a b
              Right _ -> putStrLn "Success"
    where
        showDiff a b = do
            putStrLn "got vvvvvvvvv"
            putStrLn a
            putStrLn "expected vvvvvvvvv"
            putStrLn b


parseAndRevalidate ::(VisualStream a,TraversableStream a,Stream a,Show b) => a -> ParsecT Void a Identity b -> (b -> String) -> String -> IO (Either (String,String) b)
parseAndRevalidate src p f ref = do
                            case runParser p "" src of
                                Left _ -> do 
                                            putStrLn "Parse error"
                                            parseTest p src >> empty
                                Right res -> return  (if f res == ref then Right res else Left (f res,ref))