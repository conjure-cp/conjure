{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Conjure.Language.NewLexer (
    ETok(..),
    Offsets(..),
    prettySplitComments,
    eLex,
    reformList,
    tokenSourcePos,
    totalLength,
    trueLength,
    tokenStart,
    sourcePos0,
    LexerError(..),
    runLexer,
    ETokenStream(..)
    ) where

import Conjure.Language.Lexemes
import Conjure.Prelude hiding (many, some,Text)
import Data.Char (isAlpha, isAlphaNum)
import Data.Void

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text (Text)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Data.List (splitAt)
import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec as L
import Prelude (read)
import  Prettyprinter as Pr


sourcePos0 :: SourcePos
sourcePos0 = SourcePos "" (mkPos  1) (mkPos 1)

class Reformable a where
    reform :: a -> L.Text

instance Reformable ETok where
    reform e | oTLength (offsets e) == 0 = ""
    reform (ETok{capture=cap,trivia=triv}) = L.append  (L.concat $ map showTrivia triv) (L.fromStrict cap)
        where
            showTrivia :: Trivia -> L.Text
            showTrivia x = case x of
              WhiteSpace txt    -> L.fromStrict txt
              LineComment txt   -> L.fromStrict txt
              BlockComment txt  -> L.fromStrict txt

reformList :: (Traversable t ,Reformable a) => t a -> L.Text
reformList =  L.concat  . map reform . toList

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

data Offsets =Offsets {oStart::Int,oTrueStart :: Int,oTLength::Int,oSourcePos::SourcePos}
    deriving (Show, Eq, Ord)
type Lexer = Parsec Void Text

-- type Lexer = Parsec Void Text ETokenStream

data Trivia = WhiteSpace Text | LineComment Text | BlockComment Text
    deriving (Show, Eq, Ord)

data ETok = ETok
    { offsets :: Offsets
    , trivia :: [Trivia]
    , lexeme :: Lexeme
    , capture :: Text
    }
    deriving (Eq, Ord)

instance Pr.Pretty ETok where
    pretty = Pr.unAnnotate . uncurry (<>) .  prettySplitComments

prettySplitComments :: ETok -> (Pr.Doc ann, Pr.Doc ann)
prettySplitComments (ETok _ tr _ capture) = (Pr.hcat [Pr.pretty t <> Pr.hardline | LineComment t <- tr],Pr.pretty capture)


totalLength :: ETok -> Int
totalLength = oTLength . offsets

trueLength :: ETok -> Int
trueLength (ETok{offsets = (Offsets o d l _)}) = max 0 (l + (o-d))

tokenStart :: ETok -> Int
tokenStart (ETok{offsets = (Offsets _  s _ _)}) = s

tokenSourcePos :: ETok -> SourcePos
tokenSourcePos = oSourcePos . offsets
sourcePosAfter :: ETok -> SourcePos
sourcePosAfter ETok {offsets=(Offsets _ _ l (SourcePos a b (unPos->c)))} = SourcePos a b (mkPos (c + l))

makeToken :: Offsets -> [Trivia] -> Lexeme -> Text -> ETok
makeToken = ETok

data LexerError = LexerError String
    deriving (Show)

runLexer :: Text -> Either LexerError ETokenStream
runLexer txt = case runParser eLex "Lexer" txt of
  Left peb -> Left $ LexerError $ errorBundlePretty peb
  Right ets -> Right $ ETokenStream txt ets


eLex :: Lexer [ETok]
eLex = 
    do 
        main <- many $ try aToken 
        end <- pEOF
        return $ main ++ [end]

aToken :: Lexer ETok
aToken = do
    start <- getOffset
    whiteSpace <- pTrivia
    wse <- getOffset
    spos <- getSourcePos
    (tok,cap) <- aLexeme
    tokenEnd <- getOffset
    return $ makeToken (Offsets start wse (tokenEnd - start) spos) whiteSpace tok cap

pEOF :: Lexer ETok
pEOF = do
    start <- getOffset
    whiteSpace <- pTrivia
    wse <- getOffset
    spos <- getSourcePos
    eof
    tokenEnd <- getOffset
    return $ makeToken (Offsets start wse (tokenEnd - start) spos) whiteSpace L_EOF ""


aLexeme :: Lexer (Lexeme,Text)
aLexeme = aLexemeStrict <|> pFallback

aLexemeStrict :: Lexer (Lexeme,Text)
aLexemeStrict =
    try
        pNumber
        <|> try  (choice (map pLexeme lexemes) <?> "Lexeme")
        <|> try pIdentifier
        <|> try pQuotedIdentifier
        <|> try pMetaVar


pNumber :: Lexer (Lexeme,Text)
pNumber = do
    v <- takeWhile1P Nothing (`elem` ['1','2','3','4','5','6','7','8','9','0']) 
    let n = read $ T.unpack v
    return (LIntLiteral n,v)
    <?> "Numeric Literal"

pMetaVar :: Lexer (Lexeme,Text)
pMetaVar = do
    amp <- chunk "&"
    (_,cap) <- pIdentifier
    return (LMetaVar cap,amp `T.append` cap)

pIdentifier :: Lexer (Lexeme,Text)
pIdentifier = do
    firstLetter <- takeWhile1P Nothing isIdentifierFirstLetter
    rest <- takeWhileP Nothing isIdentifierLetter
    let ident = T.append firstLetter rest
    return ( LIdentifier ident, ident)
    <?> "Identifier"

pQuotedIdentifier :: Lexer (Lexeme,Text)
pQuotedIdentifier = do 
    l <- quoted 
    return (LIdentifier l,l)

pFallback :: Lexer (Lexeme,Text)
pFallback = do
    q <- T.pack <$> someTill anySingle (lookAhead $ try somethingValid)
    return (LUnexpected  q,q)
  where
    somethingValid :: Lexer ()
    somethingValid = void pTrivia <|> void aLexemeStrict <|> eof

pLexeme :: (Text, Lexeme) -> Lexer (Lexeme,Text)
pLexeme (s, l) = do
    tok <- string s
    notFollowedBy $ if isIdentifierLetter $ T.last tok then nonIden else empty
    return (l,tok)
    <?> "Lexeme :" ++ show l
    where
        nonIden = takeWhile1P Nothing isIdentifierLetter

pTrivia :: Lexer [Trivia]
pTrivia = many (whiteSpace <|> lineComment <|> blockComment)

whiteSpace :: Lexer Trivia
whiteSpace = do
    s <- some spaceChar
    return $ WhiteSpace $ T.pack s

quoted :: Lexer Text
quoted = do
    open <- char '\"'
    (body,end) <- manyTill_ anySingle $ char '\"' 
    return $ T.pack  $ open:body++[end]

lineEnd :: Lexer [Char]
lineEnd = T.unpack <$> eol <|> ( eof >> return [])

lineComment :: Lexer Trivia
lineComment = do
    _ <- try (chunk "$")
    (text,end) <- manyTill_ anySingle lineEnd
    return $ LineComment $ T.pack ('$' : text++end)

blockComment :: Lexer Trivia
blockComment = do
    _ <- try (chunk "/*")
    text <- manyTill L.anySingle (lookAhead (void(chunk "*/") <|>eof))
    cl <- optional $ chunk "*/"
    let cl' = fromMaybe "" cl
    return $ BlockComment $ T.concat ["/*",T.pack text ,cl' ]

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
    Just _ -> ETokenStream (T.pack "showTokens pxy s") xs

instance VisualStream ETokenStream where
    showTokens _ =  L.unpack . reformList
    tokensLength _ = sum . fmap ( oTLength . offsets )

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