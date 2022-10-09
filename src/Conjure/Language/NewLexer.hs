{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Conjure.Language.NewLexer where

import Conjure.Language.Lexemes
import Conjure.Prelude hiding (many, some)
import Data.Char (isAlpha, isAlphaNum)
import Data.Void

import qualified Data.Text as T

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Data.List (splitAt)
import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec.Char.Lexer as L

sourcePos0 :: SourcePos
sourcePos0 = SourcePos "" (mkPos  0) (mkPos 0)

class Reformable a where
    reform :: a -> String

instance Reformable ETok where
    reform (ETok{capture=cap,trivia=triv}) = concatMap showTrivia triv ++ T.unpack cap
        where
            showTrivia :: Trivia -> String
            showTrivia x = case x of
              WhiteSpace txt -> T.unpack txt
              LineComment txt -> T.unpack txt
              BlockComment txt -> T.unpack txt

reformList :: Reformable a => [a] -> String
reformList = concatMap reform

emojis :: [Char]
emojis =
    concat
        [ ['\x1f600' .. '\x1F64F']
        , ['\x1f300' .. '\x1f5ff']
        , ['\x1f680' .. '\x1f999']
        , ['\x1f1e0' .. '\x1f1ff']
        ]

isIdentifierFirstLetter :: Char -> Bool
isIdentifierFirstLetter ch = isAlpha ch || ch `elem` ("_" :: String) || ch `elem` emojis

isIdentifierLetter :: Char -> Bool
isIdentifierLetter ch = isAlphaNum ch || ch `elem` ("_'" :: String) || ch `elem` emojis

type Offsets = (Int, Int, Int, SourcePos)
type Parser = Parsec Void T.Text


data Trivia = WhiteSpace T.Text | LineComment T.Text | BlockComment T.Text
    deriving (Show, Eq, Ord)

data ETok = ETok
    { offsets :: Offsets
    , trivia :: [Trivia]
    , lexeme :: Lexeme
    , capture :: Text
    }
    deriving (Eq, Ord)


totalLength :: ETok -> Int
totalLength (ETok{offsets = (_, _, l, _)}) = l

tokenStart :: ETok -> Int
tokenStart (ETok{offsets = (_, s, _, _)}) = s

tokenSourcePos :: ETok -> SourcePos
tokenSourcePos ETok{offsets=(_,_,_,s)} = s
makeToken :: Offsets -> [Trivia] -> Lexeme -> Text -> ETok
makeToken = ETok

eLex :: Parser [ETok]
eLex = 
    do 
        main <- many $ try aToken 
        end <- pEOF
        return $ main ++ [end]

aToken :: Parser ETok
aToken = do
    start <- getOffset
    whiteSpace <- pTrivia
    wse <- getOffset
    spos <- getSourcePos
    (tok,cap) <- aLexeme
    tokenEnd <- getOffset
    return $ makeToken (start, wse, tokenEnd - start, spos) whiteSpace tok cap

pEOF :: Parser ETok
pEOF = do
    start <- getOffset
    whiteSpace <- pTrivia
    wse <- getOffset
    spos <- getSourcePos
    eof
    tokenEnd <- getOffset
    return $ makeToken (start, wse, tokenEnd - start, spos) whiteSpace L_EOF ""


aLexeme :: Parser (Lexeme,Text)
aLexeme = aLexemeStrict <|> pFallback

aLexemeStrict :: Parser (Lexeme,Text)
aLexemeStrict =
    try
        pNumber
        <|> try  (choice (map pLexeme lexemes) <?> "Lexeme")
        <|> try pIdentifier
        <|> try pMetaVar


pNumber :: Parser (Lexeme,Text)
pNumber = do
    v <- L.decimal
    return (LIntLiteral v,T.pack $ show  v)
    <?> "Numeric Literal"

pMetaVar :: Parser (Lexeme,Text)
pMetaVar = do
    amp <- chunk "&"
    (_,cap) <- pIdentifier
    return (LMetaVar cap,amp `T.append` cap)

pIdentifier :: Parser (Lexeme,Text)
pIdentifier = do
    firstLetter <- takeWhile1P Nothing isIdentifierFirstLetter
    rest <- takeWhileP Nothing isIdentifierLetter
    let ident = T.append firstLetter rest
    return ( LIdentifier ident, ident)
    <?> "Identifier"
pFallback :: Parser (Lexeme,Text)
pFallback = do
    q <- T.pack <$> someTill anySingle (lookAhead $ try somethingValid)
    return (LUnexpected  q,q)
  where
    somethingValid :: Parser ()
    somethingValid = void pTrivia <|> void aLexemeStrict <|> eof

pLexeme :: (T.Text, Lexeme) -> Parser (Lexeme,Text)
pLexeme (s, l) = do
    tok <- string s
    notFollowedBy $ if isIdentifierLetter $ T.last tok then nonIden else empty
    return (l,tok)
    <?> "Lexeme :" ++ show l
    where
        nonIden = takeWhile1P Nothing isIdentifierLetter

pTrivia :: Parser [Trivia]
pTrivia = many (whiteSpace <|> lineComment <|> blockComment)

whiteSpace :: Parser Trivia
whiteSpace = do
    s <- some spaceChar
    return $ WhiteSpace $ T.pack s

lineEnd :: Parser [Char]
lineEnd = T.unpack <$> eol <|> ( eof >> return [])

lineComment :: Parser Trivia
lineComment = do
    _ <- try (chunk "$")
    (text,end) <- manyTill_ L.charLiteral lineEnd
    return $ LineComment $ T.pack ('$' : text++end)

blockComment :: Parser Trivia
blockComment = do
    _ <- try (chunk "/*")
    text <- manyTill L.charLiteral (chunk "*/")
    return $ BlockComment $ T.pack text

instance Show ETok where
    show (ETok _ _ q _) = show q

data ETokenStream = ETokenStream
    { streamSourceText :: Text
    , streamTokens :: [ETok]
    }
instance Stream ETokenStream where
    type Token ETokenStream = ETok
    type Tokens ETokenStream = [ETok]
    tokenToChunk _ x = [x]
    tokensToChunk _ xs = xs
    chunkToTokens _ = id
    chunkLength _ = length
    chunkEmpty _ [] = True
    chunkEmpty _ _ = False
    take1_ :: ETokenStream -> Maybe (Token ETokenStream, ETokenStream)
    take1_ (ETokenStream _ (x : xs)) = Just (x, buildStream xs)
    take1_ (ETokenStream _ []) = Nothing
    takeN_ :: Int -> ETokenStream -> Maybe (Tokens ETokenStream, ETokenStream)
    takeN_ n xs | n <= 0 = Just ([], xs)
    takeN_ _ (ETokenStream _ []) = Nothing
    takeN_ n (ETokenStream s xs) = Just (take n xs, buildStream $ drop n xs)
    takeWhile_ :: (Token ETokenStream -> Bool) -> ETokenStream -> (Tokens ETokenStream, ETokenStream)
    takeWhile_ p (ETokenStream _ xs) =
        (a, buildStream b)
      where
        (a, b) = span p xs

-- (takeWhile p xs,ETokenStream $ dropWhile p xs)

buildStream :: [ETok] -> ETokenStream
buildStream xs = case NE.nonEmpty xs of
    Nothing -> ETokenStream "" xs
    Just s -> ETokenStream (T.pack $ showTokens pxy s) xs

instance VisualStream ETokenStream where
    showTokens p =  concatMap reform
    tokensLength p ls = sum $ len <$> ls
      where
        len ETok{offsets = (_, _, x, _)} = x

-- https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams
instance TraversableStream ETokenStream where
    reachOffset o PosState{..} =
        ( Just (prefix ++ restOfLine)
        , PosState
            { pstateInput = buildStream post
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = newSourcePos
            , pstateTabWidth = pstateTabWidth
            , pstateLinePrefix = prefix
            }
        )
      where
        prefix =
            if sameLine
                then pstateLinePrefix ++ preLine
                else preLine
        sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
        newSourcePos =
            case post of
                [] -> pstateSourcePos
                (x : _) -> tokenSourcePos x
        (pre, post) :: ([ETok], [ETok]) = splitAt (o - pstateOffset) (streamTokens pstateInput)
        (preStr, postStr) = (maybe "" (showTokens pxy) (NE.nonEmpty pre), maybe "" (showTokens pxy) (NE.nonEmpty post))
        preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
        restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy ETokenStream
pxy = Proxy