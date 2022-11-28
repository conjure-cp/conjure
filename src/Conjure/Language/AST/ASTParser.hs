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
import Data.Sequence (Seq)
import Text.PrettyPrint.HughesPJ (text)
import Text.Megaparsec.Debug (dbg)

data ParserError = ParserError Doc
    deriving (Show)


runASTParser :: Parser a -> ETokenStream -> Either ParserError a
runASTParser p str =
    case runParser (evalStateT p def) "parser" str of
        Left peb -> Left $ ParserError . text $ errorBundlePretty peb
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
    nums <- parseSequence L_Dot (RealToken [] <$> intLiteral)
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
parseSuchThat =  do
    lSuch <- need L_such
    lThat <- want L_that
    exprs <- commaList1 parseExpression
    return $ SuchThatStatement $ SuchThatStatementNode lSuch lThat exprs

parseWhere :: Parser StatementNode
parseWhere = do
    lWhere <- need L_where
    exprs <- commaList1 parseExpression
    return $ WhereStatement $ WhereStatementNode lWhere exprs

parseObjective :: Parser StatementNode
parseObjective = do
    ObjectiveStatement <$> parseObjectiveStatement

parseDeclaration :: Parser StatementNode
parseDeclaration =
    DeclarationStatement
        <$> choice [
                    declaration LettingStatement L_letting parseLetting,
                    declaration GivenStatement L_given parseGiven,
                    declaration FindStatement L_find parseFind
        ]
    where
        declaration :: (Null a,Show a) => (LToken -> Sequence a -> b) -> Lexeme -> Parser a -> Parser b
        declaration c t p = do
                            l <- need t
                            seq <- commaList1 p
                            return $ c l seq

parseLetting :: Parser LettingStatementNode
parseLetting = do
    names <- commaList1 parseIdentifier
    lBe <- want L_be
    let start = LettingStatementNode names lBe
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
    names <- commaList1 parseIdentifier
    choice
        [ finishEnum (GivenEnumNode names)
        , finishDomain (GivenStatementNode names)
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
    names <- commaList1 parseIdentifier
    lColon <- want L_Colon
    domain <- parseDomain
    return $ FindStatementNode names lColon domain
    <?> "Find Statement"
parseObjectiveStatement :: Parser ObjectiveStatementNode
parseObjectiveStatement = do
    s <- eSymbol L_minimising <|> eSymbol L_maximising
    e <- parseExpression
    return $ case s of
        (ETok {lexeme=L_minimising}) -> ObjectiveMin (RealToken [] s) e
        _ -> ObjectiveMax (RealToken [] s) e
    <?> "Objective Statement"


pEnding :: Parser LToken
pEnding =  do
    t <- lookAhead anySingle
    case t of
        ETok {lexeme=L_EOF} -> return $ RealToken [] t
        _ -> empty



---------------------------------------

---------------------------------------

guardExpressionOverlap :: Parser ()
guardExpressionOverlap = do
    off <- getOffset
    lastOffset <- gets lastMissingExpOffset
    -- traceC $ "DME query:" ++ show off ++ "," ++ show lastOffset
    guard $ off > lastOffset
    -- traceC $ "DME used:" ++ show off
    modify (\x->x{lastMissingExpOffset=off})

parseExpression :: Parser ExpressionNode
parseExpression =  do
    (parseOperator)
        <|> (parseAtomicExpression) <|> (MissingExpressionNode <$> makeMissing (L_Missing "expression"))

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
            [
               parseSpecialCase
            ,  Literal <$> parseLiteral
            ,  parseFunction
            ,  parseAttributeAsConstraint
            ,  IdentifierNode <$> parseIdentifierStrict
            ,  MetaVarExpr <$> parseMetaVar
            ,  ParenExpression <$> parseParenExpression parensPair
            ,  AbsExpression <$> parseAbsExpression
            ,  QuantificationExpr <$> parseQuantificationStatement
            ,  DomainExpression <$> parseDomainExpression
            ,  guardExpressionOverlap >> MissingExpressionNode <$> (makeMissing $ L_Missing "Expr")
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
parseIntLiteral = IntLiteral . RealToken [] <$> intLiteral

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
parseOperator =  try (makeExprParser parseAtomicExpressionAndFixes operatorTable <?> "Expression")

parseFunction :: Parser ExpressionNode
parseFunction = try $ do
    name <- choice $ map need functionals
    let ol = isOverloaded name
    let parenP = if ol then parenListStrict else parenList
    args <-  parenP $ commaList parseExpression
    guard $ not ol || argsHasNoLeadingTrivia args
    return $ FunctionalApplicationNode name args
    where
        isOverloaded (RealToken _ ETok{lexeme=lex}) = lex `elem` overloadedFunctionals
        isOverloaded _ = False
        argsHasNoLeadingTrivia (ListNode (RealToken [] ETok{trivia=[]}) y z) =  True
        argsHasNoLeadingTrivia _ = False
parseAttributeAsConstraint :: Parser ExpressionNode
parseAttributeAsConstraint = do
    name <- choice $ map need (attributesAsLexemes allSupportedAttributes)
    args <- parenList $ commaList parseExpression
    return $ AttributeAsConstriant name args


parsePostfixOp :: Parser (ExpressionNode -> ExpressionNode)
parsePostfixOp =  do
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
                    UnaryPrefix op -> prefixOps op
                | -- UnaryPrefix L_ExclamationMark -> Prefix $ prefixBinary--foldr1 (.) <$> some parseUnaryNot
                -- UnaryPrefix l                 -> bug ("Unknown UnaryPrefix" <+> pretty (show l))
                (descr, _) <- operatorsInGroup
                ]
              | operatorsInGroup <- operatorsGrouped
              ]

parseAtomicExpressionAndFixes :: Parser ExpressionNode
parseAtomicExpressionAndFixes = try $ do
    let
        prefixes = do
            fs <- some parsePrefixes
            return $ foldr1 (.) fs
        postfixes = do
            fs <- some parsePostfixOp
            return $ foldr1 (.) (reverse fs)
        withPrefix  x = try x <|> do f <- prefixes; i <- x; return $ f i
        withPostfix x = do i <- x; guard $ not $ isMissing i ; mf <- optional postfixes; return $ fromMaybe id mf i
    withPrefix (withPostfix parseAtomicExpression) <?> "expression"


parsePrefixes :: Parser (ExpressionNode -> ExpressionNode)
parsePrefixes = choice [parseUnary L_Minus,parseUnary L_ExclamationMark]
    where
        parseUnary l = (\e -> OperatorExpressionNode . PrefixOpNode e ) <$> need l



prefixBinary :: LToken -> ExpressionNode -> ExpressionNode -> ExpressionNode
prefixBinary t l = OperatorExpressionNode . BinaryOpNode l t

prefixUnary :: LToken -> ExpressionNode -> ExpressionNode
prefixUnary l = OperatorExpressionNode . PrefixOpNode l

prefixOps ::Lexeme -> Operator Parser ExpressionNode
prefixOps l = Prefix $ foldr1 (. ) <$> some (try opBuilder)
    where
        opBuilder :: Parser (ExpressionNode -> ExpressionNode)
        opBuilder = do
            t <- need l
            return (OperatorExpressionNode . PrefixOpNode t)
postfixOps ::  [Operator Parser ExpressionNode]
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
    let indexByNode = case (lIndexed,lBy) of
            (MissingToken _,MissingToken _) -> Nothing
            _ -> Just (IndexedByNode lIndexed lBy)
    members <- squareBracketList $ commaList parseDomain
    lOf <- want L_of
    domain <- parseDomain
    return $ MatrixDomainNode lMatrix indexByNode members lOf domain

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
    name <- parseIdentifier
    lColon <- want L_Colon
    domain <- parseDomain
    let definedDomain = case (lColon,domain) of
            (a,b) | isMissing a && isMissing b -> Nothing
            (a,b) -> Just (a,b)
    return $ NameDomainNode name definedDomain

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
        isNonIdentifier (RealToken _ ETok{lexeme=(LIdentifier _)}) = False
        isNonIdentifier _ = True

parseAttribute :: Parser AttributeNode
parseAttribute = do
    name <- (choice (map need allAttributLexemes))  <|> RealToken [] <$> identifier
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
    let txt  = pack str
    let lexed = runParser eLex "lexer" txt
    case lexed of
      Left peb -> putStrLn "Lexer error:" >> (putStrLn $ errorBundlePretty peb)
      Right ets -> do
        putStrLn "Lexmes"
        putStrLn $ show  ets
        putStrLn $ "reformed"
        putStrLn $ concatMap reform ets
        let stream = ETokenStream txt  ets
        case runParser (evalStateT parseProgram def) "parser" stream  of
          Left peb -> putStrLn "Parser error: " >> (putStrLn $ errorBundlePretty peb)
          Right pt -> do
            putStrLn $show pt
            putStrLn $ "Reforming"
            putStrLn $ show $ flatten pt
            putStrLn $ reformList $ flatten pt

exampleFile :: String -> IO ()
exampleFile p = do
    path <- readFileIfExists p
    case path of
      Nothing -> putStrLn "NO such file"
      Just s -> example s
    return ()

contextRegion :: String -> Parser a -> Parser a
contextRegion l m = do
    prev <- gets context
    modify (\x->x{context=l:prev})
    r <- m
    modify (\x->x{context=prev})
    return r

traceC :: String -> Parser ()
traceC msg = do
    ctx <- gets context
    traceM $ intercalate "." (reverse ctx) ++ msg
-- parsePrint :: String -> IO ()
-- parsePrint text = do
--     toks <- parseAndRevalidate (pack text) eLex (concatMap reform) text
--     case toks of
--       Left (a,b)-> do
--             putStrLn "Lexer wasn't reversible"
--             showDiff a b
--       Right ets -> putStrLn "Lexer success" >> do
--             tree <- parseAndRevalidate (ETokenStream (pack text) ets) parseProgram (\v -> reformList (flatten v :: Seq ETok) ) text
--             case tree of
--               Left (a,b) -> do
--                         putStrLn "Parser wasn't reversible:"
--                         showDiff a b
--               Right _ -> putStrLn "Success"
--     where
--         showDiff a b = do
--             putStrLn "got vvvvvvvvv"
--             putStrLn a
--             putStrLn "expected vvvvvvvvv"
--             putStrLn b


-- parseAndRevalidate ::(VisualStream a,TraversableStream a,Stream a,Show b) => a -> ParsecT Void a (StateT ParserState Identity) b -> (b -> String) -> String -> IO (Either (String,String) b)
-- parseAndRevalidate src p f ref = do
--                             case evalState (runParserT p "" src) def of
--                                 Left _ -> do
--                                             putStrLn "Parse error"
--                                             parseTest p src >> empty
--                                 Right res -> return  (if f res == ref then Right res else Left (f res,ref))