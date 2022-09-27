module Conjure.Language.AST.Helpers where
import Conjure.Prelude hiding (many)
import Conjure.Language.NewLexer hiding (Parser)
import Text.Megaparsec
import Data.Void
import Conjure.Language.AST.Syntax
import qualified Data.Set as Set

type Parser = Parsec Void ETokenStream



eSymbol :: Lexeme -> Parser ETok
eSymbol lx = token test Set.empty <?> "Symbol " ++ show lx
                where test x
                        | lexeme x == lx = Just x
                        | otherwise = Nothing

identifier :: Parser ETok
identifier = token test Set.empty <?> "Identifier"
                where test x = case x of
                                    ETok _ _ (LIdentifier _) -> Just x
                                    ETok {} -> Nothing

lIdent :: Lexeme
lIdent = LIdentifier ""

intLiteral :: Parser ETok
intLiteral = token test Set.empty <?> "Int Literal"
                where test x = case x of
                                    ETok _ _ (LIntLiteral _) -> Just x
                                    ETok {} -> Nothing

makeMissing :: Lexeme -> Parser LToken
makeMissing l = do
    (ETok (s,_,_,_) _ _) <- lookAhead anySingle
    return (MissingToken (ETok (s,s,0,l) [] l))

--try to get a token from the stream but allow failiure
want :: Lexeme -> Parser LToken
want (LIdentifier _) = do
        (ETok (s,ts,_,_) t lex) <- lookAhead anySingle
        case lex of
            (LIdentifier _) -> RealToken <$> anySingle
            _ -> return $ MissingToken $ ETok (s,ts,0,LMissingIdentifier) t LMissingIdentifier
want a = do
        (ETok (s,ts,_,_) t lex) <- lookAhead anySingle
        if lex == a then
            RealToken <$> anySingle
            else
            return $ MissingToken $ ETok (s,ts,0,a) t a
--get a symbol from the stream with no fallback
need :: Lexeme -> Parser LToken
need a = RealToken <$> eSymbol a

parseIdentifier :: Parser NameNode
parseIdentifier = do
    x <- want lIdent
    return $ NameNode x

parseIdentifierStrict :: Parser NameNode
parseIdentifierStrict = do
    NameNode . RealToken <$> identifier


--List helpers

commaList :: Parser a -> Parser (Sequence a)
commaList = parseSequence L_Comma

squareBracketList :: Parser (Sequence a) -> Parser (ListNode a)
squareBracketList = parseList L_OpenBracket L_CloseBracket

curlyBracketList :: Parser (Sequence a) -> Parser (ListNode a)
curlyBracketList = parseList L_OpenCurly L_CloseCurly

parenList :: Parser (Sequence a) -> Parser (ListNode a)
parenList = parseList L_OpenParen L_CloseParen

parseList :: Lexeme -> Lexeme -> Parser (Sequence a) -> Parser (ListNode a)
parseList startB endB seq = do
    startB' <- want startB
    vals <- seq
    endB' <- want endB
    return $ ListNode startB' vals endB'

parseSequence :: Lexeme -> Parser a -> Parser (Sequence a)
parseSequence divider pElem = do
    (a,b) <- manyTill_ (try seqElemSep) (try seqElemNoSep)
    return $ Seq (a ++ [b])
    where
        seqElemNoSep = do
                            v <- pElem
                            notFollowedBy $ eSymbol divider
                            return $ SeqElem v Nothing
        seqElemSep = do
            v <-pElem
            s <- need divider
            return $ SeqElem v (Just s)