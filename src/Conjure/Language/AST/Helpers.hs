module Conjure.Language.AST.Helpers where

import Conjure.Language.AST.Syntax
import Conjure.Language.Lexemes
import Conjure.Language.NewLexer
import Conjure.Prelude hiding (many)
import qualified Data.Set as Set
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void ETokenStream

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
        ETok {lexeme=(LIdentifier _) } -> Just x
        ETok{} -> Nothing

metaVar :: Parser ETok
metaVar = token test Set.empty <?> "Metavar"
  where
    test x = case x of
        ETok {lexeme=(LMetaVar _) } -> Just x
        ETok{} -> Nothing


anIdent :: Lexeme
anIdent = LIdentifier ""

aMetaVar :: Lexeme
aMetaVar = LMetaVar ""
intLiteral :: Parser ETok
intLiteral = token test Set.empty <?> "Int Literal"
  where
    test x = case x of
        ETok {lexeme=(LIntLiteral _)} -> Just x
        ETok{} -> Nothing

makeMissing :: Lexeme -> Parser LToken
makeMissing l = do
    spos <- getSourcePos
    s <- getOffset
    return (MissingToken (ETok ( Offsets s s 0 spos) [] l ""))

makeUnexpected :: Parser LToken
makeUnexpected = SkippedToken <$> anySingle

-- try to get a token from the stream but allow failiure
want :: Lexeme -> Parser LToken
want (LIdentifier _) = do
    (ETok o t lex _ ) <- lookAhead anySingle
    case lex of
        (LIdentifier _) -> RealToken <$> anySingle
        _ -> return $ MissingToken $ ETok o{oTLength=0} t LMissingIdentifier ""
want a = do
    (ETok o t lex _) <- lookAhead anySingle
    if lex == a
        then RealToken <$> anySingle
        else return $ MissingToken $ ETok o{oTLength=0} t a ""

-- get a symbol from the stream with no fallback
need :: Lexeme -> Parser LToken
need a = RealToken <$> eSymbol a <?> "\"" ++ lexemeFace a ++ "\""

adjacent :: Parser a -> Parser a
adjacent p = do 
    next <- lookAhead anySingle
    guard $ null $ trivia next
    p

parseIdentifier :: Parser NameNode
parseIdentifier = do
    x <- want anIdent
    return $ NameNode x

parseMetaVar :: Parser LToken
parseMetaVar = RealToken <$> metaVar

parseIdentifierStrict :: Parser NameNode
parseIdentifierStrict = do
    NameNode . RealToken <$> identifier

-- List helpers

commaList ::(Null a) => Parser a -> Parser (Sequence a)
commaList = parseSequence L_Comma

commaList1 ::(Null a) => Parser a -> Parser (Sequence a)
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
    return $ ListNode startB' vals endB'

-- parseSequence :: Lexeme -> Parser a -> Parser (Sequence a)
-- parseSequence divider pElem = do
--     start <- optional seqElemNoSep
--     rest <- many $ try seqElemSep
--     case start of
--       Nothing -> return $ Seq rest
--       Just se -> return $ Seq (se:rest)
--     where
--     seqElemNoSep = do
--         SeqElem Nothing <$> pElem
--     seqElemSep = do
--         s <- need divider
--         SeqElem (Just s) <$> pElem

parseSequence1 :: (Null a) => Lexeme -> Parser a -> Parser (Sequence a)
parseSequence1 divider pElem = do
    s <- parseSequence divider pElem
    case s of 
        Seq [] -> do
                q <- pElem
                return $ Seq [SeqElem q Nothing]
        Seq _ -> return s

           

parseSequence :: (Null a) => Lexeme -> Parser a -> Parser (Sequence a)
parseSequence divider pElem = do
    missingPlaceholder <- makeMissing $ L_Missing "SequenceElem"
    elem <- optional pElem
    sep <- want divider
    case (elem, isMissing sep) of
        (a,True) | isMissing a -> return $ Seq []
        _ -> do 
            Seq rest <- parseSequence divider pElem
            makeElem rest elem sep missingPlaceholder

    where
    makeElem rest el sep plc = do
            let newElem = case (el, isMissing sep) of 
                    (Just a, True) -> [SeqElem a $ if null rest then Nothing else Just sep ]
                    (a,False) | isMissing a -> [MissingSeqElem plc sep]
                    (Just a,_) -> [SeqElem a $ Just sep]
                    _ -> [] 
            return $ Seq $ newElem++rest 

                    




parseNESequence :: (Null a) => Lexeme -> Parser a -> Parser (Sequence a)
parseNESequence divider pElem = do
    lst <- try $ parseSequence divider pElem
    case lst of
        Seq {elems=[]} -> empty
        Seq _ -> return lst




parensPair :: (Lexeme,Lexeme)
parensPair = (L_OpenParen,L_CloseParen)

squarBracketPair :: (Lexeme,Lexeme)
squarBracketPair = (L_OpenBracket,L_CloseBracket)

curlyPair :: (Lexeme,Lexeme)
curlyPair = (L_OpenCurly,L_CloseCurly)