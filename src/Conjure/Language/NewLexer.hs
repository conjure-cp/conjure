
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}


module Conjure.Language.NewLexer  where
import Conjure.Prelude hiding (some,many)
import Conjure.Language.Lexemes
import Data.Char ( isAlpha, isAlphaNum )
import Data.Void

import qualified Data.Text as T

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.List.NonEmpty as NE
import Data.List (splitAt)




emojis :: [Char]
emojis = concat [['\x1f600'..'\x1F64F'],
                 ['\x1f300'..'\x1f5ff'],
                 ['\x1f680'..'\x1f999'],
                 ['\x1f1e0'..'\x1f1ff']]


isIdentifierFirstLetter :: Char -> Bool
isIdentifierFirstLetter ch = isAlpha ch || ch `elem` ("_" :: String) || ch `elem` emojis

isIdentifierLetter :: Char -> Bool
isIdentifierLetter ch = isAlphaNum ch || ch `elem` ("_'" :: String) || ch `elem` emojis


type Offsets = (Int,Int,Int,Lexeme)
type Parser = Parsec Void T.Text

class Reconstructable a where
    unLex :: a -> String

data Trivia = WhiteSpace T.Text | LineComment T.Text | BlockComment  T.Text
    deriving (Show,Eq,Ord)

instance Reconstructable Trivia where
    unLex (WhiteSpace s) = T.unpack s
    unLex (BlockComment s) = T.unpack s
    unLex (LineComment s) = "$" ++ T.unpack s
data ETok = ETok {
            offsets :: Offsets,
            trivia :: [Trivia],
            lexeme :: Lexeme,
            capture :: String
}
            deriving (Eq,Ord)

instance Reconstructable ETok where
    unLex (ETok _ t _ v) = concatMap unLex t ++ v


totalLength :: ETok -> Int
totalLength (ETok {offsets=(_,_,l,_)}) = l

tokenStart :: ETok -> Int
tokenStart (ETok {offsets=(_,s,_,_)}) = s

makeToken :: Offsets -> [Trivia] -> Lexeme -> String -> ETok
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
            return $ makeToken (start,wse,tokenEnd-start,tok) whiteSpace tok (lexemeFace tok)

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
    show (ETok _ _ q _) = show q

data ETokenStream = ETokenStream {
    streamSourceText :: Text
    ,streamTokens ::  [ETok]
}
instance Stream ETokenStream where
    type Token ETokenStream= ETok
    type Tokens ETokenStream= [ETok]
    tokenToChunk proxy x = [x]
    tokensToChunk proxy xs= xs
    chunkToTokens proxy = id
    chunkLength proxy = length
    chunkEmpty proxy xs = False
    take1_ :: ETokenStream -> Maybe (Token ETokenStream, ETokenStream)
    take1_ (ETokenStream _ (x:xs)) = Just (x, buildStream xs)
    take1_ (ETokenStream _ []) = Nothing
    takeN_ :: Int -> ETokenStream -> Maybe (Tokens ETokenStream, ETokenStream)
    takeN_ n xs | n<=0 = Just([],xs)
    takeN_ _ (ETokenStream _ []) = Nothing
    takeN_ n (ETokenStream s xs) = Just (take n xs,buildStream $ drop n xs)
    takeWhile_ :: (Token ETokenStream -> Bool)-> ETokenStream -> (Tokens ETokenStream, ETokenStream)
    takeWhile_ p (ETokenStream _ xs) =
        (a,buildStream b)
        where
            (a,b) = span p xs
        -- (takeWhile p xs,ETokenStream $ dropWhile p xs)

buildStream :: [ETok] -> ETokenStream
buildStream xs = case NE.nonEmpty xs of
                        Nothing -> ETokenStream "" xs
                        Just s -> ETokenStream (T.pack $ showTokens pxy s) xs

instance VisualStream ETokenStream where
    showTokens p q = concat $ unLex <$> q
    tokensLength p ls = sum $ len <$> ls
                        where len ETok{offsets =(_,_,x,_)} = x

--https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams
instance TraversableStream ETokenStream where
  reachOffset o PosState {..} =
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
          (x:_) -> pstateSourcePos { sourceLine = mkPos $ tokenStart x , sourceColumn = mkPos $ tokenStart x}
      (pre, post) :: ([ETok],[ETok]) = splitAt (o - pstateOffset) (streamTokens pstateInput)
      (preStr, postStr) =  (maybe "" (showTokens pxy) (NE.nonEmpty pre),maybe "" (showTokens pxy) (NE.nonEmpty post))
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      tokensConsumed = o
      restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy ETokenStream
pxy = Proxy