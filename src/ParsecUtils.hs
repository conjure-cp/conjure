{-# LANGUAGE TemplateHaskell #-}

module ParsecUtils (
    Assoc(..), Operator(..), OperatorTable, Parser, (<?>),

    parseFromFile, parseIO, parseMaybe, parseEither, unsafeParse,

    buildExpressionParser,

    choice, choiceTry, try, optionMaybe,
    angles, braces, brackets,
    count, countSep, countSepAtLeast,
    sepBy, sepBy1, sepEndBy, sepEndBy1,
    many, many1,

    colon, comma, dot, parens, between,

    identifier, integer, reserved, reservedOp,
    eof, whiteSpace, symbol
    ) where


import Control.Applicative
import Control.Monad.Identity
import Data.FileEmbed ( embedFile )
import Data.Maybe
import Data.ByteString.Char8 ( unpack)

import Text.Parsec ( ParsecT, alphaNum, char, letter, parse )
import Text.Parsec.Combinator ( between, choice, eof, many1, optionMaybe, sepBy, sepBy1, sepEndBy, sepEndBy1 )
import Text.Parsec.Expr ( Assoc(..), Operator(..), OperatorTable, buildExpressionParser )
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.Prim ( (<?>), try )
import Text.Parsec.Token ( GenTokenParser, LanguageDef, caseSensitive, commentLine, identLetter, identStart, makeTokenParser, natural, reservedNames, reservedOpNames )
import qualified Text.Parsec.Token as T ( angles, braces, brackets, colon, comma, dot, identifier, parens, reserved, reservedOp, symbol, whiteSpace )


type Parser a = ParsecT String () Identity a

ldef :: LanguageDef st
ldef  = emptyDef { commentLine     = "$"
                 , identStart      = char '_' <|> letter
                 , identLetter     = char '_' <|> char '#' <|> alphaNum
                 , reservedNames   = lines reservedNamesTxt
                 , reservedOpNames = lines reservedOpNamesTxt
                 , caseSensitive   = True
                 }

lexer :: GenTokenParser String () Identity
lexer = makeTokenParser ldef

angles     :: Parser a -> Parser a;  angles     = T.angles     lexer
braces     :: Parser a -> Parser a;  braces     = T.braces     lexer
brackets   :: Parser a -> Parser a;  brackets   = T.brackets   lexer
colon      :: Parser ();             colon      = T.colon      lexer >> return ()
comma      :: Parser ();             comma      = T.comma      lexer >> return ()
dot        :: Parser ();             dot        = T.dot        lexer >> return ()
identifier :: Parser String;         identifier = T.identifier lexer
integer    :: Parser Integer;        integer    = natural      lexer
parens     :: Parser a -> Parser a;  parens     = T.parens     lexer
reserved   :: String -> Parser ();   reserved   = T.reserved   lexer
reservedOp :: String -> Parser ();   reservedOp = T.reservedOp lexer
symbol     :: String -> Parser ();   symbol s   = T.symbol     lexer s >> return ()
whiteSpace :: Parser ();             whiteSpace = T.whiteSpace lexer


-- choice with try applied to every parser in the list but not the last
choiceTry :: [Parser a] -> Parser a
choiceTry []     = error "choiceTry []"
choiceTry [x]    = x
choiceTry (x:xs) = try x <|> choiceTry xs


-- parses a specified number of elements
count :: Int -> Parser a -> Parser [a]
count i p | i > 0 = (:) <$> p <*> count (i-1) p
count _ _ = return []


-- parses a specified number of elements separated by the given separator
countSep :: Int -> Parser a -> Parser sep -> Parser [a]
countSep 1 p _   = (:[]) <$> p
countSep i p sep | i > 1 = (:) <$> (p <* sep) <*> countSep (i-1) p sep
countSep _ _ _   = return []


-- parses at least a given number of elements separated by the given separator
countSepAtLeast :: Int -> Parser a -> Parser sep -> Parser [a]
countSepAtLeast i p sep = (++) <$> countSep i p sep <*> many (sep *> p)


-- parse in the IO monad. throw an error in case of a parsing error
parseIO :: Parser a -> String -> IO a
parseIO p s = case parse p "" s of
    Left err -> error $ "parser error: " ++ show err
    Right a  -> return a


-- parse in the Maybe monad. returns Nothing if parsing fails
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case parse p "" s of
    Left  _ -> Nothing
    Right a -> return a


parseEither :: Parser a -> String -> Either String a
parseEither p s = case parse p "" s of
    Left msg -> Left (show msg)
    Right a  -> Right a


-- reads the file,
-- aplies a preprocessor to its contents,
-- runs the parser,
-- applies a postprocessor to the result of parsing and returns the result
parseFromFile :: Parser a -> (String -> String) -> FilePath -> (a -> a) -> IO a
parseFromFile parser preProcess filepath postProcess = do
    s <- readFile filepath
    case parse parser filepath (preProcess s) of
        Left err -> error (show err)
        Right  a -> return (postProcess a)


-- to be used while trying things in ghci
unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
    Left err -> error $ "parser error: " ++ show err
    Right a  -> a


-- reservedNames are loaded from `reservedNames.txt` at compile time
reservedNamesTxt :: String
reservedNamesTxt = unpack $(embedFile "datafiles/reservedNames.txt")

-- reservedOpNames are loaded from `reservedOpNames.txt` at compile time
reservedOpNamesTxt :: String
reservedOpNamesTxt = unpack $(embedFile "datafiles/reservedOpNames.txt")
