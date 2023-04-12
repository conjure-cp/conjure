module Conjure.Language.AST.Helpers (
    Parser,
    makeUnexpected,
    makeMissing,
    eSymbol,
    identifier,
    need,
    want,
    needWeak,
    commaList,
    commaList1,
    parseIdentifier,
    parseIdentifierStrict,
    parseMetaVar,
    intLiteral,
    parseSequence,
    parseSequence1,
    parseList,
    curlyBracketList,
    squareBracketList,
    parenListStrict,
    parenList,
    parensPair,
    parseAttributeLexeme
) where

import Conjure.Language.AST.Syntax
import Conjure.Language.Attributes (allAttributLexemes)
import Conjure.Language.Lexemes
import Conjure.Language.Lexer
import Conjure.Prelude hiding (many)
import qualified Data.Set as Set
import Data.Void
import Text.Megaparsec

type Parser = (Parsec Void ETokenStream)

eSymbol :: Lexeme -> Parser ETok
eSymbol lx = token test Set.empty <?> "Symbol " ++ show lx
  where
    test x
        | lexeme x == lx = Just x
        | otherwise = Nothing

identifier :: Parser ETok
identifier = token test Set.empty <?> "Identifier"
  where
    test x = case x of
        ETok{lexeme = (LIdentifier _)} -> Just x
        ETok{} -> Nothing

metaVar :: Parser ETok
metaVar = token test Set.empty <?> "Metavar"
  where
    test x = case x of
        ETok{lexeme = (LMetaVar _)} -> Just x
        ETok{} -> Nothing

anIdent :: Lexeme
anIdent = LIdentifier ""


intLiteral :: Parser ETok
intLiteral = token test Set.empty <?> "Int Literal"
  where
    test x = case x of
        ETok{lexeme = (LIntLiteral _)} -> Just x
        ETok{} -> Nothing

makeMissing :: Lexeme -> Parser LToken
makeMissing l = do
    ETok (Offsets st _ _ _) _ _ _ <- lookAhead anySingle
    spos <- getSourcePos
    return (MissingToken (ETok (Offsets st st 0 spos) [] l ""))

makeUnexpected :: Parser LToken
makeUnexpected = SkippedToken <$> anySingle

-- try to get a token from the stream but allow failiure
want :: Lexeme -> Parser LToken
want (LIdentifier _) = do
    (ETok o t lex _) <- lookAhead anySingle
    case lex of
        (LIdentifier _) -> makeStrict <$> anySingle
        _ -> return $ MissingToken $ ETok o{oTLength = 0} t LMissingIdentifier ""
want a = do
    (ETok o t lex _) <- lookAhead anySingle
    if lex == a
        then makeStrict <$> anySingle
        else return $ MissingToken $ ETok o{oTLength = 0} t a ""

-- get a symbol from the stream with no fallback
need :: Lexeme -> Parser SToken
need a = StrictToken [] <$> eSymbol a <?> "\"" ++ lexemeFace a ++ "\""

-- get a symbol from the stream where it is required but the underlying
-- structure does not (e.g. disambiguating a list)
needWeak :: Lexeme -> Parser LToken
needWeak a = RealToken <$> need a



parseIdentifier :: Parser NameNode
parseIdentifier = do NameNode <$> parseIdentifierStrict <|> MissingNameNode <$> want anIdent

parseAttributeLexeme :: Parser SToken
parseAttributeLexeme = StrictToken [] <$> token isValid Set.empty
  where
    isValid t@(lexeme -> l)
        | l `elem` allAttributLexemes = Just t
        | otherwise = Nothing

parseMetaVar :: Parser SToken
parseMetaVar = StrictToken [] <$> metaVar

parseIdentifierStrict :: Parser NameNodeS
parseIdentifierStrict = do
    NameNodeS . StrictToken [] <$> identifier

-- List helpers

commaList :: (Null a, Show a) => Parser a -> Parser (Sequence a)
commaList = parseSequence L_Comma

commaList1 :: (Null a, Show a) => Parser a -> Parser (Sequence a)
commaList1 = parseSequence1 L_Comma

squareBracketList :: Parser (Sequence a) -> Parser (ListNode a)
squareBracketList = parseList L_OpenBracket L_CloseBracket

curlyBracketList :: Parser (Sequence a) -> Parser (ListNode a)
curlyBracketList = parseList L_OpenCurly L_CloseCurly

parenList :: Parser (Sequence a) -> Parser (ListNode a)
parenList = parseList L_OpenParen L_CloseParen

parenListStrict :: Parser (Sequence a) -> Parser (ListNode a)
parenListStrict = parseListStrict L_OpenParen L_CloseParen

parseList :: Lexeme -> Lexeme -> Parser (Sequence a) -> Parser (ListNode a)
parseList startB endB seq = do
    startB' <- want startB
    vals <- seq
    endB' <- want endB
    return $ ListNode startB' vals endB'

parseListStrict :: Lexeme -> Lexeme -> Parser (Sequence a) -> Parser (ListNode a)
parseListStrict startB endB seq = do
    startB' <- need startB
    vals <- seq
    endB' <- want endB
    return $ ListNode (RealToken startB') vals endB'

parseSequence1 :: (Null a, Show a) => Lexeme -> Parser a -> Parser (Sequence a)
parseSequence1 divider pElem = do
    s <- parseSequence divider pElem
    case s of
        Seq [] -> try $ do
            q <- pElem
            return $ Seq [SeqElem q Nothing]
        Seq _ -> return s

parseSequence :: (Null a, Show a) => Lexeme -> Parser a -> Parser (Sequence a)
parseSequence divider pElem = try $ do
    missingPlaceholder <- makeMissing $ L_Missing "SequenceElem"
    sElem <- optional pElem
    sep <- want divider
    case (sElem, isMissing sep) of
        (a, True) | isMissing a -> return $ Seq []
        _ -> do
            Seq rest <- parseSequence divider pElem
            makeElem rest sElem sep missingPlaceholder
  where
    makeElem rest el sep plc = do
        let newElem = case (el, isMissing sep) of
                (Just a, True) -> [SeqElem a $ if null rest then Nothing else Just sep]
                (a, False) | isMissing a -> [MissingSeqElem plc sep]
                (Just a, _) -> [SeqElem a $ Just sep]
                _ -> []
        return $ Seq $ newElem ++ rest



parensPair :: (Lexeme, Lexeme)
parensPair = (L_OpenParen, L_CloseParen)

