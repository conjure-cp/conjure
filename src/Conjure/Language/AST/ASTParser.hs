{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Conjure.Language.AST.ASTParser where

import Conjure.Prelude hiding (many,some)

import Conjure.Language.AST.Helpers
import Conjure.Language.AST.Syntax
import Conjure.Language.NewLexer

-- import Conjure.Language.AST.Expression
import Conjure.Language.Lexemes
import Text.Megaparsec

import Data.Text (pack)
import Data.Void (Void)
import Conjure.Language.AST.Reformer (Flattenable(..))
import Conjure.Language.Expression.Op.Internal.Common
import Control.Monad.Combinators.Expr
import Conjure.Language.Domain.AddAttributes (allSupportedAttributes)
import Language.Haskell.TH.PprLib (rparen)
import Conjure.Language.Attributes (allAttributLexemes)

data ParserError = ParserError
    deriving (Show)


runASTParser :: Parser a -> ETokenStream -> Either ParserError a
runASTParser p str = case runParser p "Parser" str of
  Left peb -> Left ParserError
  Right res -> Right res

parseProgram :: Parser ProgramTree
parseProgram =  do
    langV <- optional parseLangVersion
    (tl,ending) <- manyTill_ parseTopLevel pEnding
    return $ ProgramTree langV tl ending
    <?> "Program"

parseLangVersion :: Parser LangVersionNode
parseLangVersion = do
    lLang <- need L_language
    lLName <- parseIdentifier
    nums <- parseSequence L_Dot (RealToken <$> intLiteral)
    return $ LangVersionNode lLang lLName nums


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
        <|> parseHeuristic
        <|> UnexpectedToken <$> makeUnexpected


parseHeuristic :: Parser StatementNode
parseHeuristic = do
    lHeuristic <- need L_heuristic
    expr <- parseExpression
    return $ HeuristicStatement lHeuristic expr

parseBranching :: Parser StatementNode
parseBranching = do
    lBranching <- need L_branching
    lOn <- want L_on
    statements <- squareBracketList (commaList parseExpression)
    return $ BranchingStatement $ BranchingStatementNode lBranching lOn statements


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



---------------------------------------

---------------------------------------

parseExpression :: Parser ExpressionNode
parseExpression = parseSpecialCase <|>
    parseOperator
        <|> parseAtomicExpression

parseExpressionStrict :: Parser ExpressionNode -- can fail
parseExpressionStrict = do
    expr <- try parseExpression
    case expr of
        MissingExpressionNode _ -> empty
        _ -> return expr

parseAtomicExpression :: Parser ExpressionNode
parseAtomicExpression = do
    try $
        choice
            [ Literal <$> parseLiteral
            , parseFunction
            , parseAttributeAsConstraint
            , IdentifierNode <$> parseIdentifierStrict
            , MetaVarExpr <$> parseMetaVar
            , ParenExpression <$> parseParenExpression parensPair
            , AbsExpression <$> parseAbsExpression
            , QuantificationExpr <$> parseQuantificationStatement
            , DomainExpression <$> parseDomainExpression
            , MissingExpressionNode <$> makeMissing (L_Missing "Expression")
            ]



parseDomainExpression :: Parser DomainExpressionNode
parseDomainExpression = try $ do
    lTick <- need L_BackTick
    domain <- parseDomain
    case domain of
        MissingDomainNode _ -> empty
        _ -> pure ()
    rTick <- want L_BackTick
    return $ DomainExpressionNode lTick domain rTick

-- [a,b,c : int (1..2)]
-- [a,b,c : int (1..4) | x < 3,letting x be int]



parseMatrixBasedExpression :: Parser LiteralNode
parseMatrixBasedExpression = do
    openB <- need L_OpenBracket
    exprs <- commaList parseExpression
    range <- optional pOverDomain
    comprehension <- optional pComp
    closeB <- want L_CloseBracket
    let es = exprs
    return $ MatrixLiteral $ MatrixLiteralNode openB es range comprehension closeB
  where
    pOverDomain = OverDomainNode <$> need L_SemiColon <*> parseDomain
    pComp = do
        bar <- need L_Bar
        body <- commaList parseComprehensionCondition
        return $ ComprehensionNode bar body

--TODO look into adding enviorment to the parser to configure forgiveness
parseAbsExpression :: Parser ParenExpressionNode
parseAbsExpression = try $ do
    lParen <- need   L_Bar
    expr <- parseExpression
    rParen <- need  L_Bar
    return $ ParenExpressionNode lParen expr rParen

parseParenExpression :: (Lexeme, Lexeme) -> Parser ParenExpressionNode
parseParenExpression (open, close) = try $ do
    lParen <- need open
    body <- parseExpression
    notFollowedBy $ need L_Comma
    rParen <- want close
    return $ ParenExpressionNode lParen body rParen

parseLiteral :: Parser LiteralNode
parseLiteral =
    choice
        [ parseIntLiteral
        , parseBoolLiteral
        , parseMatrixBasedExpression
        , parseTupleLiteral
        , parseShortTupleLiteral
        , parseRecordLiteral
        , parseVariantLiteral
        , parseSetLiteral
        , parseMSetLiteral
        , parseFunctionLiteral
        , parseSequenceLiteral
        , parseRelationLiteral
        , parsePartitionLiteral
        ]

parseShortTupleLiteral :: Parser LiteralNode
parseShortTupleLiteral = try $ do
    lOpen <- need L_OpenParen
    exprs <- commaList parseExpression
    let Seq xs = exprs
    guard (length xs > 1)
    lClose <- want L_CloseParen
    return $ TupleLiteralNodeShort $ ShortTuple (ListNode lOpen exprs lClose)

parseIntLiteral :: Parser LiteralNode
parseIntLiteral = IntLiteral . RealToken <$> intLiteral

parseBoolLiteral :: Parser LiteralNode
parseBoolLiteral = BoolLiteral <$> (need L_true <|> need L_false)

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
    return $ VariantLiteral lVariant members

parseRecordMember :: Parser RecordMemberNode
parseRecordMember = do
    name <- parseIdentifier
    lEqual <- want L_Eq
    val <- parseExpression
    return $ RecordMemberNode name lEqual val

parseSetLiteral :: Parser LiteralNode
parseSetLiteral = do
    -- cant just recycle list as it does not require first char
    lOpen <- need L_OpenCurly
    members <- commaList parseExpression
    lClose <- want L_CloseCurly
    return $ SetLiteral (ListNode lOpen members lClose)

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
parseRelationMember = try $ do
    f <- optional $ need L_tuple
    members <- parenList $ commaList parseExpression
    case f of
        Just lTup -> return $ RelationElemNodeLabeled $ LongTuple lTup members
        Nothing -> case members of 
            ListNode l c r | (isMissing l || isMissing r) && isMissing c -> empty
            _ -> return $ RelationElemNodeShort $ ShortTuple members
        

parsePartitionLiteral :: Parser LiteralNode
parsePartitionLiteral = do
    lPartition <- need L_partition
    members <- parenList (commaList parsePartitionElem)
    return $ PartitionLiteral lPartition members

parsePartitionElem :: Parser PartitionElemNode
parsePartitionElem = PartitionElemNode <$> parseList L_OpenCurly L_CloseCurly (commaList parseExpression)

parseQuantificationStatement :: Parser QuantificationExpressionNode
parseQuantificationStatement = do
    lType <- choice $ map need quantifiers
    terms <- commaList parseAbstractPattern
    over <- parseQuantificationOver
    qGuard <- optional $ do
        lComma <- need L_Comma
        expr <- parseExpression
        return $ QuanticationGuard lComma expr
    lDot <- want L_Dot
    expr <- parseExpression
    return $ QuantificationExpressionNode lType terms over qGuard lDot expr
  where
    parseQuantificationOver :: Parser QuantificationOverNode
    parseQuantificationOver =
        choice
            [ QuantifiedMemberOfNode <$> need L_in <*> parseExpression
            , QuantifiedSubsetOfNode <$> need L_subsetEq <*> parseExpression
            , QuantifiedDomainNode <$> (OverDomainNode <$> want L_Colon <*> parseDomain)
            ]

parseAbstractPattern :: Parser AbstractPatternNode
parseAbstractPattern = do
    choice
        [ parseAbstractId
        , parseAbstractMetaVar
        , parseAbstractPatternTuple
        , parseAbstractPatternMatrix
        , parseAbstractPatternSet
        ]
  where
    parseAbstractId :: Parser AbstractPatternNode
    parseAbstractId = AbstractIdentifier <$> parseIdentifierStrict
    parseAbstractMetaVar :: Parser AbstractPatternNode
    parseAbstractMetaVar = AbstractMetaVar <$> parseMetaVar
    parseAbstractPatternTuple :: Parser AbstractPatternNode
    parseAbstractPatternTuple = do
        lTuple <- optional $ need L_tuple
        openB <- (if null lTuple then need else want) L_OpenParen
        es <- commaList parseAbstractPattern
        closeB <- want L_CloseParen
        return $ AbstractPatternTuple lTuple (ListNode openB es closeB)
    parseAbstractPatternMatrix :: Parser AbstractPatternNode
    parseAbstractPatternMatrix = do
        openB <- need L_OpenBracket
        es <- commaList parseAbstractPattern
        closeB <- want L_CloseBracket
        return $ AbstractPatternMatrix (ListNode openB es closeB)
    parseAbstractPatternSet :: Parser AbstractPatternNode
    parseAbstractPatternSet = do
        openB <- need L_OpenCurly
        es <- commaList parseAbstractPattern
        closeB <- want L_CloseCurly
        return $ AbstractPatternMatrix (ListNode openB es closeB)

parseComprehensionCondition :: Parser ComprehensionBodyNode
parseComprehensionCondition = do
    letting <|> generator <|> condition
  where
    letting = do
        lLetting <- need L_letting
        v <- parseAbstractPattern
        lBe <- want L_be
        expr <- parseExpression
        return $ CompBodyLettingNode lLetting v lBe expr
    generator = try $ do
        pats <- commaList parseAbstractPattern
        choice
            [ try $ do
                lColon <- need L_Colon
                domain <- parseDomain
                return $ CompBodyDomain pats lColon domain
            , try $ do
                lArrow <- need L_LeftArrow
                expr <- parseExpression
                return $ CompBodyGenExpr pats lArrow expr
            ]

    condition = CompBodyCondition <$> parseExpressionStrict

-- TODO look over this, asignment of domains should happen in next stage
-- Current implementation is hacky

parseOperator :: Parser ExpressionNode
parseOperator = try (makeExprParser parseAtomicExpression operatorTable <?> "Expression")

parseFunction :: Parser ExpressionNode
parseFunction = try $ do
    name <- choice $ map need functionals
    let parenP = if  isOverloaded name then parenListStrict else parenList
    args <-  parenP $ commaList parseExpression
    guard $ argsHasNoLeadingTrivia args
    return $ FunctionalApplicationNode name args
    where
        isOverloaded (RealToken ETok{lexeme=lex}) = lex `elem` overloadedFunctionals
        isOverloaded _ = False
        argsHasNoLeadingTrivia (ListNode (RealToken ETok{trivia=[]}) y z) =  True
        argsHasNoLeadingTrivia _ = False
parseAttributeAsConstraint :: Parser ExpressionNode
parseAttributeAsConstraint = do
    name <- choice $ map need (attributesAsLexemes allSupportedAttributes)
    args <- parenList $ commaList parseExpression
    return $ AttributeAsConstriant name args


parsePostfixOp :: Parser (ExpressionNode -> ExpressionNode)
parsePostfixOp = do
    op <-
        try $
            choice
                [ indexed
                , factorial
                , application
                , explicitDomain
                ]
    return $ \e -> OperatorExpressionNode $ PostfixOpNode e op
  where
    indexed = do
        lBracket <- need L_OpenBracket
        indexer <- commaList parseRange
        rBracket <- want L_CloseBracket
        return $ IndexedNode $ ListNode lBracket indexer rBracket
    factorial = OpFactorial <$> need L_ExclamationMark
    application = do
        lBracket <- need L_OpenParen
        args <- commaList parseExpression
        rBracket <- want L_CloseParen
        return $ ApplicationNode $ ListNode lBracket args rBracket
    explicitDomain = try $ do
        lColon <- need L_Colon
        lTickl <- need L_BackTick
        dom <- parseDomain
        lTickr <- want L_BackTick
        return $ ExplicitDomain lColon lTickl dom lTickr

-- TODO treat funcitonals differently or actually don't but why

operatorTable :: [[Operator Parser ExpressionNode]]
operatorTable =
    let operatorsGrouped =
            operators
                |> sortBy (\(_, a) (_, b) -> compare a b)
                |> groupBy (\(_, a) (_, b) -> a == b)
                |> reverse
     in postfixOps
            : [ [ case descr of
                    BinaryOp op FLeft -> InfixL $ prefixBinary <$> need op
                    BinaryOp op FNone -> InfixN $ prefixBinary <$> need op
                    BinaryOp op FRight -> InfixR $ prefixBinary <$> need op
                    UnaryPrefix op -> Prefix $ prefixUnary <$> need op
                | -- UnaryPrefix L_ExclamationMark -> Prefix $ prefixBinary--foldr1 (.) <$> some parseUnaryNot
                -- UnaryPrefix l                 -> bug ("Unknown UnaryPrefix" <+> pretty (show l))
                (descr, _) <- operatorsInGroup
                ]
              | operatorsInGroup <- operatorsGrouped
              ]

prefixBinary :: LToken -> ExpressionNode -> ExpressionNode -> ExpressionNode
prefixBinary t l = OperatorExpressionNode . BinaryOpNode l t

prefixUnary :: LToken -> ExpressionNode -> ExpressionNode
prefixUnary l = OperatorExpressionNode . PrefixOpNode l

postfixOps :: [Operator Parser ExpressionNode]
postfixOps =
    [ Postfix $ foldr1 (.) . reverse <$> some parsePostfixOp
    ]

-- DOMAINS
parseDomain :: Parser DomainNode
parseDomain =
    do
        choice
            [ BoolDomainNode <$> need L_bool
            , parseIntDomain
            , MetaVarDomain <$> parseMetaVar
            , parseTuple
            , parseRecord
            , parseVariant
            , parseMatrix
            , parseSet
            , parseMSet
            , parseFunctionDomain
            , parseSequenceDomain
            , parseRelation
            , parsePartition
            , parseEnumDomain
            , parseShortTuple
            ]
            <?> "Domain"
        <|> parseMissingDomain
        <?> "missingDomain"

parseSpecialCase :: Parser ExpressionNode
parseSpecialCase = do
    SpecialCase <$> choice [parseWithDecls]
    where
        parseWithDecls = try $
            do
                p1 <- need L_OpenCurly
                exp1 <- parseExpression
                lAt <- need L_At
                (decsl,p2) <- manyTill_ parseTopLevel  (need L_CloseCurly)

                return $ ExprWithDecls p1  exp1  lAt decsl p2



parseIntDomain :: Parser DomainNode
parseIntDomain = do
    lInt <- need L_int
    ranges <- optional $ parenListStrict $ commaList parseRange
    return $ RangedIntDomainNode lInt ranges

parseTuple :: Parser DomainNode
parseTuple = do
    lTuple <- need L_tuple
    members <- parenList $ commaList parseDomain
    return $ TupleDomainNode lTuple members

parseShortTuple :: Parser DomainNode
parseShortTuple = do
    openB <- need L_OpenParen
    lst <- commaList parseDomain
    closeB <- want L_CloseParen
    return $ ShortTupleDomainNode $ ListNode openB lst closeB

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
    attributes <- optional parseAttributes
    lOf <- want L_of
    domain <- parseDomain
    return $ SetDomainNode lSet attributes lOf domain

parseMSet :: Parser DomainNode
parseMSet = do
    lMSet <- need L_mset
    attributes <- optional parseAttributes
    lOf <- want L_of
    domain <- parseDomain
    return $ MSetDomainNode lMSet attributes lOf domain

parseFunctionDomain :: Parser DomainNode
parseFunctionDomain = do
    lFunction <- need L_function
    attributes <- optional parseAttributes
    fromDom <- parseDomain
    arrow <- want L_LongArrow
    toDom <- parseDomain
    return $ FunctionDomainNode lFunction attributes fromDom arrow toDom
--   where
--     parseFunctionAttributes :: Parser (ListNode AttributeNode)
--     parseFunctionAttributes = try $ do
--         openB <- want L_OpenParen
--         lst <- commaList1 parseAttribute
--         closeB <- want L_CloseParen
--         return $ ListNode openB lst closeB

parseSequenceDomain :: Parser DomainNode
parseSequenceDomain = do
    lSequence <- need L_sequence
    attributes <- optional parseAttributes
    lOf <- want L_of
    domain <- parseDomain
    return $ SequenceDomainNode lSequence attributes lOf domain

parseRelation :: Parser DomainNode
parseRelation = do
    lRelation <- need L_relation
    attributes <- optional parseAttributes
    lOf <- want L_of
    domains <- parenList $ parseSequence L_Times parseDomain
    return $ RelationDomainNode lRelation attributes lOf domains

parsePartition :: Parser DomainNode
parsePartition = do
    lPartition <- need L_partition
    attributes <- optional parseAttributes
    lFrom <- want L_from
    domain <- parseDomain
    return $ PartitionDomainNode lPartition attributes lFrom domain

parseEnumDomain :: Parser DomainNode
parseEnumDomain = do
    name <- parseIdentifierStrict
    brackets <- optional $ parenListStrict (commaList parseRange)
    return $ RangedEnumNode name brackets

-- (RangedEnumNode name <$> try (parenList (commaList parseRange)))
--     <|> return (EnumDomainNode name)

-- Util
parseNameDomain :: Parser NamedDomainNode
parseNameDomain = do
    name <- parseIdentifierStrict
    lColon <- want L_Colon
    domain <- parseDomain
    return $ NameDomainNode name lColon domain

parseRange :: Parser RangeNode
parseRange = ranged <|> singleR
  where
    ranged = try $ do
        lExpr <- optional $ try parseExpressionStrict
        dots <- need L_DoubleDot
        rExpr <- optional parseExpressionStrict
        case (lExpr, rExpr) of
            (Nothing, Nothing) -> return $ OpenRangeNode dots
            (Just l, Nothing) -> return $ RightUnboundedRangeNode l dots
            (Nothing, Just r) -> return $ LeftUnboundedRangeNode dots r
            (Just l, Just r) -> return $ BoundedRangeNode l dots r
    singleR = SingleRangeNode <$> parseExpressionStrict

parseAttributes :: Parser (ListNode AttributeNode)
parseAttributes = try $ do
    attrs <- parenList (commaList parseAttribute)
    case attrs of
            ListNode _(Seq xs) _| not (validInterior xs) -> empty
            _ -> return attrs
    where
        validInterior :: [SeqElem AttributeNode] -> Bool
        validInterior members = not $ null  [x |
           (SeqElem (NamedAttributeNode x _) _) <- members,isNonIdentifier x
           ]
        isNonIdentifier :: LToken -> Bool
        isNonIdentifier (RealToken ETok{lexeme=(LIdentifier _)}) = False
        isNonIdentifier _ = True

parseAttribute :: Parser AttributeNode
parseAttribute = do
    name <- (choice (map need allAttributLexemes))  <|> RealToken <$> identifier
    expr <- optional parseExpressionStrict
    return $ NamedAttributeNode name expr

parseMissingDomain :: Parser DomainNode
parseMissingDomain =
    do
        m <- makeMissing (L_Missing "Domain")
        return $ MissingDomainNode m
        <?> "Anything"


attributesAsLexemes :: [(Name,Int)] -> [Lexeme]
attributesAsLexemes xs = do
        let xs' = map fst xs
        let ys = [t | Name t <- xs']
        let lexes = map textToLexeme ys
        catMaybes lexes

---------------------------------------
---EXAMPLES AND TESTING            ----
---------------------------------------
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

exampleFile :: String -> IO ()
exampleFile p = do
    path <- readFileIfExists p
    case path of
      Nothing -> putStrLn "NO such file"
      Just s -> example s
    return ()

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