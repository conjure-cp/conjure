{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Conjure.Language.Lexer
    ( Lexeme(..)
    , LexemePos(..)
    , textToLexeme
    , lexemeText
    , lexemeFace
    ) where
import Conjure.Prelude hiding (some,many)
import Conjure.Language.Lexemes hiding (lexemeFace)
import Data.Char ( isAlpha, isAlphaNum )
import Data.Void
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Conjure.Language.Pretty
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char

import Text.Megaparsec --( SourcePos, initialPos, incSourceLine, incSourceColumn, setSourceColumn )
import Text.Megaparsec.Stream ()


data LexemePos = LexemePos
                    Lexeme          -- the lexeme
                    SourcePos       -- source position, the beginning of this lexeme
                    SourcePos       -- source position, just after this lexeme, including whitespace after the lexeme
    deriving (Show,Eq, Ord)


lexemeFace :: Lexeme -> Doc
lexemeFace L_Newline = "new line"
lexemeFace L_Carriage = "\\r"
lexemeFace L_Space   = "space character"
lexemeFace L_Tab     = "tab character"
lexemeFace (LIntLiteral i) = pretty i
lexemeFace (LIdentifier i) = pretty (T.unpack i)
-- lexemeFace (LComment    i) = Pr.text (T.unpack i)
lexemeFace l =
    case M.lookup l mapLexemeToText of
        Nothing -> pretty (show l)
        Just t  -> pretty . T.unpack $ t

isLexemeSpace :: Lexeme -> Bool
isLexemeSpace L_Newline {} = True
isLexemeSpace L_Carriage{} = True
isLexemeSpace L_Tab     {} = True
isLexemeSpace L_Space   {} = True
-- isLexemeSpace LComment  {} = True
isLexemeSpace _            = False

tryLex :: T.Text -> (T.Text, Lexeme) -> Maybe (T.Text, Lexeme)
tryLex running (face,lexeme) = do
    rest <- T.stripPrefix face running
    if T.all isIdentifierLetter face
        then
            case T.uncons rest of
                Just (ch, _) | isIdentifierLetter ch -> Nothing
                _                                    -> Just (rest, lexeme)
        else Just (rest, lexeme)

-- tryLexIntLiteral :: T.Text -> Maybe (T.Text, Lexeme)
-- tryLexIntLiteral t =
--     case T.decimal t of
--         Left _ -> Nothing
--         Right (x, rest) -> Just (rest, LIntLiteral x)


emojis :: [Char]
emojis = concat [['\x1f600'..'\x1F64F'],
                 ['\x1f300'..'\x1f5ff'],
                 ['\x1f680'..'\x1f999'],
                 ['\x1f1e0'..'\x1f1ff']]


isIdentifierFirstLetter :: Char -> Bool
isIdentifierFirstLetter ch = isAlpha ch || ch `elem` ("_" :: String) || ch `elem` emojis

isIdentifierLetter :: Char -> Bool
isIdentifierLetter ch = isAlphaNum ch || ch `elem` ("_'" :: String) || ch `elem` emojis

tryLexMetaVar :: T.Text -> Maybe (T.Text, Lexeme)
tryLexMetaVar running = do
    ('&', rest) <- T.uncons running
    (rest2, LIdentifier iden) <- tryLexIden rest
    return (rest2, LMetaVar iden)

tryLexIden :: T.Text -> Maybe (T.Text, Lexeme)
tryLexIden running = do
    let (iden,rest) = T.span isIdentifierLetter running
    (ch, _) <- T.uncons running
    if isIdentifierFirstLetter ch
        then
            if T.null iden
                then Nothing
                else Just (rest, LIdentifier iden)
        else Nothing

tryLexQuotedIden :: T.Text -> Maybe (T.Text, Lexeme)
tryLexQuotedIden running = do
    let
        go inp = do
            ('\"', rest) <- T.uncons inp
            go2 "\"" rest

        -- after the first "
        go2 sofar inp = do
            (ch, rest) <- T.uncons inp
            case ch of
                -- end
                '\"'
                    | sofar /= "\""         -- so we don't allow empty strings
                    -> Just (rest, LIdentifier (T.pack (reverse ('\"' : sofar))))
                -- escaped
                '\\' -> do
                    (ch2, rest2) <- T.uncons rest
                    case ch2 of
                        '\"' -> go2 ('\"':sofar) rest2
                        '\\' -> go2 ('\\':sofar) rest2
                        _ -> Nothing
                _ -> go2 (ch:sofar) rest
    go running

-- tryLexComment :: T.Text -> Maybe (T.Text, Lexeme)
-- tryLexComment running = let (dollar,rest1) = T.span (=='$') running
--                         in  if T.null dollar
--                                 then Nothing
--                                 else let (commentLine,rest2) = T.span (/='\n') rest1
--                                      in  Just (rest2, LComment commentLine)


-- instance ShowToken [LexemePos] where
--     showToken = intercalate ", " . map showToken

-- instance ShowToken LexemePos where
--     showToken (LexemePos tok _ _) = showToken tok

-- instance ShowToken Lexeme where
--     showToken = show . lexemeFace

--Generic

-- instance Hashable Lexeme

type Offsets = (Int,Int,Int,Lexeme)
type Parser = Parsec Void T.Text
data Trivia = WhiteSpace T.Text | LineComment T.Text | BlockComment  T.Text
    deriving (Show,Eq,Ord)
data ETok = ETok {
            offsets :: Offsets,
            trivia :: [Trivia],
            lexeme :: Lexeme
}
            deriving (Eq,Ord)


makeToken :: Offsets -> [Trivia] -> Lexeme -> ETok
makeToken = ETok

eLex :: Parser [ETok]
eLex = manyTill aToken eof

aToken :: Parser ETok
aToken = do
            start <- getOffset
            whiteSpace <- pTrivia
            wse <- getOffset
            tok <- aLexeme
            tokenEnd <- getOffset
            return $ makeToken (start,wse,tokenEnd-start,tok) whiteSpace tok

aLexeme :: Parser Lexeme
aLexeme  = try pEOF
    <|> try pNumber
    <|> try (choice $ map pLexeme lexemes)
    <|> try pIdentifier
    <|> try pMetaVar

pEOF :: Parser Lexeme
pEOF = do
        eof
        return L_EOF

pNumber :: Parser Lexeme
pNumber = do
            LIntLiteral <$> L.decimal

pMetaVar :: Parser Lexeme
pMetaVar = do
            empty
            return $ LMetaVar "TODO"


pIdentifier :: Parser Lexeme
pIdentifier = do
                firstLetter <- takeWhile1P Nothing isIdentifierFirstLetter
                rest <- takeWhileP Nothing isIdentifierLetter
                return $ LIdentifier (T.append firstLetter rest)


pLexeme :: (T.Text,Lexeme) -> Parser Lexeme
pLexeme (s,l) = do
                    tok <- string s
                    return l

pTrivia :: Parser [Trivia]
pTrivia = many (whiteSpace <|> lineComment <|> blockComment)

whiteSpace :: Parser Trivia
whiteSpace = do
                s <- some spaceChar
                return $  WhiteSpace $ T.pack s

lineEnd :: Parser ()
lineEnd = do
            _ <- optional eol
            _ <- optional eof
            return ()

lineComment :: Parser Trivia
lineComment = do
                _<-try (chunk "$")
                text <- manyTill L.charLiteral lineEnd
                return $ LineComment $ T.pack text

blockComment :: Parser Trivia
blockComment = do
                _ <- try (chunk "/*")
                text <- manyTill L.charLiteral (chunk "*/")
                return $ BlockComment $ T.pack text

instance Show ETok where
    show (ETok _ _ q) = show q

newtype ETokenStream = ETokenStream [ETok]
instance Stream ETokenStream where
    type Token ETokenStream= ETok
    type Tokens ETokenStream= [ETok]
    tokenToChunk proxy x = [x]
    tokensToChunk proxy xs= xs
    chunkToTokens proxy = id
    chunkLength proxy = length
    chunkEmpty proxy xs = False
    take1_ (ETokenStream (x:xs)) = Just (x, ETokenStream xs)
    take1_ (ETokenStream []) = Nothing
    takeN_ n xs | n<=0 = Just([],xs)
    takeN_ n (ETokenStream []) = Nothing
    takeN_ n (ETokenStream xs) = Just (take n xs,ETokenStream $ drop n xs)
    takeWhile_ p (ETokenStream xs) = (takeWhile p xs,ETokenStream $ dropWhile p xs)
instance VisualStream ETokenStream where
    showTokens p q = concat $ show <$> q
    tokensLength p ls = sum $ len <$> ls
                        where len (ETok (_,_,x,_) _ _) = x

instance TraversableStream ETokenStream where
    reachOffset i s = (Nothing, s)
    reachOffsetNoLine i s = s 