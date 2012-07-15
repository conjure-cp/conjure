module Stuff.Generic.PatParser where

import Stuff.Generic.Definition
import Stuff.Generic.PatTokeniser

import Control.Applicative ( (<$>), (<*>), (<*), (*>), (<|>) )
import Data.String ( IsString, fromString )
import Data.Generics ( Data )

-- parsing
import Text.Parsec ( between, sepBy, parse, eof )

    
parsePat :: String -> String -> Either String GenericPat
parsePat loc s = do
    tokens <- tokenise s
    case parse (pPat <* eof) loc tokens of
        Left  e -> Left $ "parsing: " ++ show e
        Right x -> Right x
    where
        pPat     = pNamed <|> pList <|> pObject <|> pMetaVar
        pMetaVar = MetaVar <$> pMeta
        pNamed   = NamedPat <$> pIdentifier
                            <*> (satisfyT (PatColon==) *> pPat)
        pList    = ListPat <$>
                      between (satisfyT (PatOpenBracket ==))
                              (satisfyT (PatCloseBracket==))
                              (pPat `sepBy` satisfyT (PatComma==))
        pObject  = ObjectPat <$>
                     between (satisfyT (PatOpenBrace ==))
                             (satisfyT (PatCloseBrace==))
                             (((,) <$> (fromString <$> pIdentifier)
                                   <*> (satisfyT (PatColon==) *> pPat)) `sepBy` satisfyT (PatComma==))
