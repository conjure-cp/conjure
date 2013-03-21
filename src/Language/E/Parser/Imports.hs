{-# LANGUAGE FlexibleContexts #-}

module Language.E.Parser.Imports where


import Stuff.Generic
import Language.E.Lexer ( Lexeme(..), LexemePos, lexemeFace, runLexer )
import Language.E.Imports
import Language.E.Pretty ( Pretty, pretty )

import Text.Parsec ( ParsecT, parse, tokenPrim, (<?>) )
import Text.Parsec.Combinator ( between )

import qualified Data.Text as T
import qualified Text.PrettyPrint as Pr


type Parser a = ParsecT [LexemePos] () Identity a

_testParsePrint' ::
    ( Pretty primitive
    , Pretty (Generic primitive)
    ) => Parser (Generic primitive) -> T.Text -> IO ()
_testParsePrint' p t = do
    let res = runLexerAndParser p "" t
    case res of
        Left  e -> print e
        Right x -> do
            -- print x
            -- print $ prettyAsTree x
            print $ prettyAsPaths x
            print $ pretty x

_testParsePrint'' ::
    ( Pretty primitive
    , Pretty (Generic primitive)
    ) => Parser [Generic primitive] -> T.Text -> IO ()
_testParsePrint'' p t = do
    let res = runLexerAndParser p "" t
    case res of
        Left  e  -> print e
        Right xs -> forM_ xs $ \ x -> do
            -- print x
            -- print $ prettyAsTree x
            print $ prettyAsPaths x
            print $ pretty x

runLexerAndParser :: (MonadError Pr.Doc m, Applicative m) => Parser a -> String -> T.Text -> m a
runLexerAndParser p s = runLexer >=> runParser p s

runParser :: (MonadError Pr.Doc m) => Parser a -> String -> [LexemePos] -> m a
runParser p s ls =
    -- error $ unlines $ map show ls
    case parse p s ls of
        Left  e -> let eDoc = pretty $ show e
                   in  throwError $ pretty s <+> eDoc
        Right x -> return x

identifierText :: Parser T.Text
identifierText = do
    LIdentifier i <- satisfyT isIdentifier
    return i
    where isIdentifier LIdentifier {} = True
          isIdentifier _ = False

satisfyT :: (Lexeme -> Bool) -> Parser Lexeme
satisfyT predicate = tokenPrim showTok nextPos testTok
    where
        showTok              = show . lexemeFace . fst
        testTok (tok, _)     = if predicate tok then Just tok else Nothing
        nextPos _ (_, pos) _ = pos

integer :: Parser Integer
integer = do
    LIntLiteral i <- satisfyT isInt
    return i
    where isInt LIntLiteral {} = True
          isInt _ = False

comma :: Parser ()
comma = lexeme L_Comma <?> "comma"

dot :: Parser ()
dot = lexeme L_Dot <?> "dot"

colon :: Parser ()
colon = lexeme L_Colon <?> "colon"


-- parses a specified number of elements separated by the given separator
countSep :: Int -> Parser a -> Parser sep -> Parser [a]
countSep 1 p _ = (:[]) <$> p
countSep i p separator | i > 1 = (:) <$> (p <* separator) <*> countSep (i-1) p separator
countSep _ _ _ = return []

-- parses at least a given number of elements separated by the given separator
countSepAtLeast :: Int -> Parser a -> Parser sep -> Parser [a]
countSepAtLeast i p separator = (++) <$> countSep i p separator <*> many (separator *> p)

betweenTicks :: Parser a -> Parser a
betweenTicks = between (lexeme L_BackTick) (lexeme L_BackTick)

parens :: Parser a -> Parser a
parens = between (lexeme L_OpenParen) (lexeme L_CloseParen)

braces :: Parser a -> Parser a
braces = between (lexeme L_OpenCurly) (lexeme L_CloseCurly)

brackets :: Parser a -> Parser a
brackets = between (lexeme L_OpenBracket) (lexeme L_CloseBracket)

lexeme :: Lexeme -> Parser ()
lexeme l = void (satisfyT (l==)) <?> show (lexemeFace l)

