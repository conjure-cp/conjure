-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- {-# HLINT ignore "Use <$>" #-}
module Conjure.Language.AST.Expression where

-- import Conjure.Language.AST.Helpers
-- import Conjure.Language.AST.Syntax
-- import Conjure.Language.Lexemes
-- import Conjure.Prelude hiding (many, some)
-- import Text.Megaparsec

-- import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)

-- import Conjure.Language.Expression.Op.Internal.Common
-- import Conjure.Language.NewLexer (ETok(..))
-- import Conjure.Language.Expression.Op.Internal.Common (overloadedFunctionals)
-- import Conjure.Language.AST.ASTParser (parseTopLevel)

-- import Text.Megaparsec.Debug (dbg)

-- parseExpression :: Parser ExpressionNode
-- parseExpression =
--     parseOperator
--         <|> parseAtomicExpression

-- parseExpressionStrict :: Parser ExpressionNode -- can fail
-- parseExpressionStrict = do
--     expr <- try parseExpression
--     case expr of
--         MissingExpressionNode _ -> empty
--         _ -> return expr

-- parseAtomicExpression :: Parser ExpressionNode
-- parseAtomicExpression = do
--     try $
--         choice
--             [ Literal <$> parseLiteral
--             , parseFunction
--             , IdentifierNode <$> parseIdentifierStrict
--             , MetaVarExpr <$> parseMetaVar
--             , ParenExpression <$> parseParenExpression parensPair
--             , AbsExpression <$> parseParenExpression (L_Bar, L_Bar)
--             , QuantificationExpr <$> parseQuantificationStatement
--             , DomainExpression <$> parseDomainExpression
--             , MissingExpressionNode <$> makeMissing (L_Missing "Expression")
--             ]



-- parseDomainExpression :: Parser DomainExpressionNode
-- parseDomainExpression = do 
--     lTick <- need L_BackTick
--     domain <- parseDomain
--     rTick <- want L_BackTick
--     return $ DomainExpressionNode lTick domain rTick

-- -- [a,b,c : int (1..2)]
-- -- [a,b,c : int (1..4) | x < 3,letting x be int]



-- parseMatrixBasedExpression :: Parser LiteralNode
-- parseMatrixBasedExpression = do
--     openB <- need L_OpenBracket
--     exprs <- commaList parseExpression
--     range <- optional pOverDomain
--     comprehension <- optional pComp
--     closeB <- want L_CloseBracket
--     return $ MatrixLiteral $ MatrixLiteralNode openB exprs range comprehension closeB
--   where
--     pOverDomain = OverDomainNode <$> need L_Colon <*> parseDomain
--     pComp = do
--         bar <- need L_Bar
--         body <- commaList parseComprehensionCondition
--         return $ ComprehensionNode bar body

-- parseParenExpression :: (Lexeme, Lexeme) -> Parser ParenExpressionNode
-- parseParenExpression (open, close) = try $ do
--     lParen <- need open
--     body <- parseExpression
--     notFollowedBy $ need L_Comma
--     rParen <- want close
--     return $ ParenExpressionNode lParen body rParen

-- parseLiteral :: Parser LiteralNode
-- parseLiteral =
--     choice
--         [ parseIntLiteral
--         , parseBoolLiteral
--         , parseMatrixBasedExpression
--         , parseTupleLiteral
--         , parseShortTupleLiteral
--         , parseRecordLiteral
--         , parseSetLiteral
--         , parseMSetLiteral
--         , parseFunctionLiteral
--         , parseSequenceLiteral
--         , parseRelationLiteral
--         , parsePartitionLiteral
--         ]

-- parseShortTupleLiteral :: Parser LiteralNode
-- parseShortTupleLiteral = try $ do
--     lOpen <- need L_OpenParen
--     exprs <- commaList parseExpression
--     let Seq xs = exprs 
--     guard (length xs > 1)
--     lClose <- want L_CloseParen
--     return $ TupleLiteralNodeShort $ ShortTuple (ListNode lOpen exprs lClose)

-- parseIntLiteral :: Parser LiteralNode
-- parseIntLiteral = IntLiteral . RealToken <$> intLiteral

-- parseBoolLiteral :: Parser LiteralNode
-- parseBoolLiteral = BoolLiteral <$> (need L_true <|> need L_false)

-- parseTupleLiteral :: Parser LiteralNode
-- parseTupleLiteral = do
--     lTuple <- need L_tuple
--     members <- parenList $ commaList parseExpression
--     return $ TupleLiteralNode $ LongTuple lTuple members

-- parseRecordLiteral :: Parser LiteralNode
-- parseRecordLiteral = do
--     lRecord <- need L_record
--     members <- curlyBracketList (commaList parseRecordMember)
--     return $ RecordLiteral lRecord members

-- parseVariantLiteral :: Parser LiteralNode
-- parseVariantLiteral = do
--     lVariant <- need L_variant
--     members <- curlyBracketList (commaList parseRecordMember)
--     return $ RecordLiteral lVariant members

-- parseRecordMember :: Parser RecordMemberNode
-- parseRecordMember = do
--     name <- parseIdentifier
--     lEqual <- want L_Eq
--     val <- parseExpression
--     return $ RecordMemberNode name lEqual val

-- parseSetLiteral :: Parser LiteralNode
-- parseSetLiteral = do
--     -- cant just recycle list as it does not require first char
--     lOpen <- need L_OpenCurly
--     members <- commaList parseExpression
--     lClose <- want L_CloseCurly
--     return $ SetLiteral (ListNode lOpen members lClose)

-- parseMSetLiteral :: Parser LiteralNode
-- parseMSetLiteral = do
--     lMSet <- need L_mset
--     members <- parenList (commaList parseExpression)
--     return $ MSetLiteral lMSet members

-- parseFunctionLiteral :: Parser LiteralNode
-- parseFunctionLiteral = do
--     lFunc <- need L_function
--     args <- parenList (commaList parseArrowPair)
--     return $ FunctionLiteral lFunc args

-- parseArrowPair :: Parser ArrowPairNode
-- parseArrowPair = try $ do
--     lhs <- parseExpression
--     arrow <- want L_LongArrow
--     rhs <- parseExpression
--     return $ ArrowPairNode lhs arrow rhs

-- parseSequenceLiteral :: Parser LiteralNode
-- parseSequenceLiteral = do
--     lSeq <- need L_sequence
--     members <- parenList (commaList parseExpression)
--     return $ SequenceLiteral lSeq members

-- parseRelationLiteral :: Parser LiteralNode
-- parseRelationLiteral = do
--     lRel <- need L_relation
--     members <- parenList (commaList parseRelationMember)
--     return $ RelationLiteral lRel members

-- parseRelationMember :: Parser RelationElemNode
-- parseRelationMember = do
--     f <- optional $ need L_tuple
--     members <- parenList $ commaList parseExpression
--     return $ case f of
--         Nothing -> RelationElemNodeShort $ ShortTuple members
--         Just lTup -> RelationElemNodeLabeled $ LongTuple lTup members

-- parsePartitionLiteral :: Parser LiteralNode
-- parsePartitionLiteral = do
--     lPartition <- need L_partition
--     members <- parenList (commaList parsePartitionElem)
--     return $ PartitionLiteral lPartition members

-- parsePartitionElem :: Parser PartitionElemNode
-- parsePartitionElem = PartitionElemNode <$> parenList (commaList parseExpression)

-- parseQuantificationStatement :: Parser QuantificationExpressionNode
-- parseQuantificationStatement = do
--     lType <- choice $ map need quantifiers
--     terms <- commaList parseAbstractPattern
--     over <- parseQuantificationOver
--     qGuard <- optional $ do
--         lComma <- need L_Comma
--         expr <- parseExpression
--         return $ QuanticationGuard lComma expr
--     lDot <- want L_Dot
--     expr <- parseExpression
--     return $ QuantificationExpressionNode lType terms over qGuard lDot expr
--   where
--     parseQuantificationOver :: Parser QuantificationOverNode
--     parseQuantificationOver =
--         choice
--             [ QuantifiedMemberOfNode <$> need L_in <*> parseExpression
--             , QuantifiedSubsetOfNode <$> need L_subsetEq <*> parseExpression
--             , QuantifiedDomainNode <$> (OverDomainNode <$> want L_Colon <*> parseDomain)
--             ]

-- parseAbstractPattern :: Parser AbstractPatternNode
-- parseAbstractPattern = do
--     choice
--         [ parseAbstractId
--         , parseAbstractMetaVar
--         , parseAbstractPatternTuple
--         , parseAbstractPatternMatrix
--         , parseAbstractPatternSet
--         ]
--   where
--     parseAbstractId :: Parser AbstractPatternNode
--     parseAbstractId = AbstractIdentifier <$> parseIdentifierStrict
--     parseAbstractMetaVar :: Parser AbstractPatternNode
--     parseAbstractMetaVar = AbstractMetaVar <$> parseMetaVar
--     parseAbstractPatternTuple :: Parser AbstractPatternNode
--     parseAbstractPatternTuple = do
--         lTuple <- optional $ need L_tuple
--         openB <- (if null lTuple then need else want) L_OpenParen
--         es <- commaList parseAbstractPattern
--         closeB <- want L_CloseParen
--         return $ AbstractPatternTuple lTuple (ListNode openB es closeB)
--     parseAbstractPatternMatrix :: Parser AbstractPatternNode
--     parseAbstractPatternMatrix = do
--         openB <- need L_OpenBracket
--         es <- commaList parseAbstractPattern
--         closeB <- want L_CloseBracket
--         return $ AbstractPatternMatrix (ListNode openB es closeB)
--     parseAbstractPatternSet :: Parser AbstractPatternNode
--     parseAbstractPatternSet = do
--         openB <- need L_OpenCurly
--         es <- commaList parseAbstractPattern
--         closeB <- want L_CloseCurly
--         return $ AbstractPatternMatrix (ListNode openB es closeB)

-- parseComprehensionCondition :: Parser ComprehensionBodyNode
-- parseComprehensionCondition = do
--     letting <|> generator <|> condition
--   where
--     letting = do
--         lLetting <- need L_letting
--         v <- parseAbstractPattern
--         lBe <- want L_be
--         expr <- parseExpression
--         return $ CompBodyLettingNode lLetting v lBe expr
--     generator = try $ do
--         pats <- commaList parseAbstractPattern
--         choice
--             [ try $ do
--                 lColon <- need L_Colon
--                 domain <- parseDomain
--                 return $ CompBodyDomain pats lColon domain
--             , try $ do
--                 lArrow <- need L_LeftArrow
--                 expr <- parseExpression
--                 return $ CompBodyGenExpr pats lArrow expr
--             ]

--     condition = CompBodyCondition <$> parseExpressionStrict

-- -- TODO look over this, asignment of domains should happen in next stage
-- -- Current implementation is hacky

-- parseOperator :: Parser ExpressionNode
-- parseOperator = try (makeExprParser parseAtomicExpression operatorTable <?> "Expression")

-- parseFunction :: Parser ExpressionNode
-- parseFunction = try $ do
--     name <- choice $ map need functionals
--     let parenP = if  isOverloaded name then parenListStrict else parenList
--     args <-  parenP $ commaList parseExpression
--     return $ FunctionalApplicationNode name args
--     where
--         isOverloaded (RealToken ETok{lexeme=lex}) = lex `elem` overloadedFunctionals
--         isOverloaded _ = False
-- parsePostfixOp :: Parser (ExpressionNode -> ExpressionNode)
-- parsePostfixOp = do
--     op <-
--         try $
--             choice
--                 [ indexed
--                 , factorial
--                 , application
--                 ]
--     return $ \e -> OperatorExpressionNode $ PostfixOpNode e op
--   where
--     indexed = do
--         lBracket <- need L_OpenBracket
--         indexer <- commaList parseRange
--         rBracket <- want L_CloseBracket
--         return $ IndexedNode $ ListNode lBracket indexer rBracket
--     factorial = OpFactorial <$> need L_ExclamationMark
--     application = do
--         lBracket <- need L_OpenParen
--         args <- commaList parseExpression
--         rBracket <- want L_CloseParen
--         return $ ApplicationNode $ ListNode lBracket args rBracket

-- -- TODO treat funcitonals differently or actually don't but why

-- operatorTable :: [[Operator Parser ExpressionNode]]
-- operatorTable =
--     let operatorsGrouped =
--             operators
--                 |> sortBy (\(_, a) (_, b) -> compare a b)
--                 |> groupBy (\(_, a) (_, b) -> a == b)
--                 |> reverse
--      in postfixOps
--             : [ [ case descr of
--                     BinaryOp op FLeft -> InfixL $ prefixBinary <$> need op
--                     BinaryOp op FNone -> InfixN $ prefixBinary <$> need op
--                     BinaryOp op FRight -> InfixR $ prefixBinary <$> need op
--                     UnaryPrefix op -> Prefix $ prefixUnary <$> need op
--                 | -- UnaryPrefix L_ExclamationMark -> Prefix $ prefixBinary--foldr1 (.) <$> some parseUnaryNot
--                 -- UnaryPrefix l                 -> bug ("Unknown UnaryPrefix" <+> pretty (show l))
--                 (descr, _) <- operatorsInGroup
--                 ]
--               | operatorsInGroup <- operatorsGrouped
--               ]

-- prefixBinary :: LToken -> ExpressionNode -> ExpressionNode -> ExpressionNode
-- prefixBinary t l = OperatorExpressionNode . BinaryOpNode l t

-- prefixUnary :: LToken -> ExpressionNode -> ExpressionNode
-- prefixUnary l = OperatorExpressionNode . PrefixOpNode l

-- postfixOps :: [Operator Parser ExpressionNode]
-- postfixOps =
--     [ Postfix $ foldr1 (.) <$> some parsePostfixOp
--     ]

-- -- DOMAINS
-- parseDomain :: Parser DomainNode
-- parseDomain =
--     do
--         choice
--             [ BoolDomainNode <$> need L_bool
--             , parseIntDomain
--             , MetaVarDomain <$> parseMetaVar
--             , parseTuple
--             , parseRecord
--             , parseVariant
--             , parseMatrix
--             , parseSet
--             , parseMSet
--             , parseFunctionDomain
--             , parseSequenceDomain
--             , parseRelation
--             , parsePartition
--             , parseEnumDomain
--             , parseShortTuple
--             ]
--             <?> "Domain"
--         <|> parseMissingDomain
--         <?> "missingDomain"

-- parseSpecialCase :: Parser ExpressionNode
-- parseSpecialCase = do
--     lt <- need L_SpecialCase
--     SpecialCase lt <$> choice [parseWithDecls]
    
--     where
--         parseWithDecls = 
--             do 
--                 p1 <- need L_OpenBracket
--                 exp1 <- parseExpression
--                 at <- need L_At
--                 decsl <- many parseTopLevel
--                 p2 <- need L_CloseBracket
--                 return $ ExprWithDecls p1  exp1  at dels p2



-- parseIntDomain :: Parser DomainNode
-- parseIntDomain = do
--     lInt <- need L_int
--     ranges <- parenList $ commaList parseRange
--     let range = case ranges of
--             ListNode (MissingToken _) (Seq []) (MissingToken _) -> Nothing
--             _ -> Just ranges
--     return $ RangedIntDomainNode lInt range

-- parseTuple :: Parser DomainNode
-- parseTuple = do
--     lTuple <- need L_tuple
--     members <- parenList $ commaList parseDomain
--     return $ TupleDomainNode lTuple members

-- parseShortTuple :: Parser DomainNode
-- parseShortTuple = do
--     openB <- need L_OpenParen
--     lst <- commaList parseDomain
--     closeB <- want L_CloseParen
--     return $ ShortTupleDomainNode $ ListNode openB lst closeB

-- parseRecord :: Parser DomainNode
-- parseRecord = do
--     lRecord <- need L_record
--     members <- curlyBracketList $ commaList parseNameDomain
--     return $ RecordDomainNode lRecord members

-- parseVariant :: Parser DomainNode
-- parseVariant = do
--     lVariant <- need L_variant
--     members <- curlyBracketList $ commaList parseNameDomain
--     return $ VariantDomainNode lVariant members

-- parseMatrix :: Parser DomainNode
-- parseMatrix = do
--     lMatrix <- need L_matrix
--     lIndexed <- want L_indexed
--     lBy <- want L_by
--     members <- squareBracketList $ commaList parseDomain
--     lOf <- want L_of
--     domain <- parseDomain
--     return $ MatrixDomainNode lMatrix lIndexed lBy members lOf domain

-- parseSet :: Parser DomainNode
-- parseSet = do
--     lSet <- need L_set
--     attributes <- parenList $ commaList parseAttribute
--     lOf <- want L_of
--     domain <- parseDomain
--     return $ SetDomainNode lSet attributes lOf domain

-- parseMSet :: Parser DomainNode
-- parseMSet = do
--     lMSet <- need L_mset
--     attributes <- parenList $ commaList parseAttribute
--     lOf <- want L_of
--     domain <- parseDomain
--     return $ MSetDomainNode lMSet attributes lOf domain

-- parseFunctionDomain :: Parser DomainNode
-- parseFunctionDomain = do
--     lFunction <- need L_function
--     attributes <- optional parseFunctionAttributes
--     fromDom <- parseDomain
--     arrow <- want L_LongArrow
--     toDom <- parseDomain
--     return $ FunctionDomainNode lFunction attributes fromDom arrow toDom
--   where
--     parseFunctionAttributes :: Parser (ListNode AttributeNode)
--     parseFunctionAttributes = try $ do
--         openB <- want L_OpenParen
--         lst <- commaList1 parseAttribute
--         closeB <- want L_CloseBracket
--         return $ ListNode openB lst closeB

-- parseSequenceDomain :: Parser DomainNode
-- parseSequenceDomain = do
--     lSequence <- need L_sequence
--     attributes <- parenList $ commaList parseAttribute
--     lOf <- want L_of
--     domain <- parseDomain
--     return $ SequenceDomainNode lSequence attributes lOf domain

-- parseRelation :: Parser DomainNode
-- parseRelation = do
--     lRelation <- need L_relation
--     attributes <- parenList $ commaList parseAttribute
--     lOf <- want L_of
--     domains <- parenList $ parseSequence L_Times parseDomain
--     return $ RelationDomainNode lRelation attributes lOf domains

-- parsePartition :: Parser DomainNode
-- parsePartition = do
--     lPartition <- need L_partition
--     attributes <- parenList $ commaList parseAttribute
--     lFrom <- want L_from
--     domain <- parseDomain
--     return $ PartitionDomainNode lPartition attributes lFrom domain

-- parseEnumDomain :: Parser DomainNode
-- parseEnumDomain = do
--     name <- parseIdentifierStrict
--     brackets <- optional $ parenListStrict (commaList parseRange)
--     case brackets of
--         Nothing -> return $ EnumDomainNode name
--         Just parens -> return $ RangedEnumNode name parens

-- -- (RangedEnumNode name <$> try (parenList (commaList parseRange)))
-- --     <|> return (EnumDomainNode name)

-- -- Util
-- parseNameDomain :: Parser NamedDomainNode
-- parseNameDomain = do
--     name <- parseIdentifierStrict
--     lColon <- want L_Colon
--     domain <- parseDomain
--     return $ NameDomainNode name lColon domain

-- parseRange :: Parser RangeNode
-- parseRange = ranged <|> singleR
--   where
--     ranged = try $ do
--         lExpr <- optional $ try parseExpressionStrict
--         dots <- need L_DoubleDot
--         rExpr <- optional parseExpressionStrict
--         case (lExpr, rExpr) of
--             (Nothing, Nothing) -> return $ OpenRangeNode dots
--             (Just l, Nothing) -> return $ RightUnboundedRangeNode l dots
--             (Nothing, Just r) -> return $ LeftUnboundedRangeNode dots r
--             (Just l, Just r) -> return $ BoundedRangeNode l dots r
--     singleR = SingleRangeNode <$> parseExpressionStrict

-- parseAttribute :: Parser AttributeNode
-- parseAttribute = do
--     name <- choice $ map need functionAttributes -- TODO This is wrong
--     expr <- optional parseExpressionStrict
--     case expr of
--         Nothing -> return $ NamedAttributeNode (NameNode name)
--         Just en -> return $ NamedExpressionAttribute (NameNode name) en

-- parseMissingDomain :: Parser DomainNode
-- parseMissingDomain =
--     do
--         m <- makeMissing (L_Missing "Domain")
--         return $ MissingDomainNode m
--         <?> "Anything"