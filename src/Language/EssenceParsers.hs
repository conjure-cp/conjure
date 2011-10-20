module Language.EssenceParsers ( pExpr ) where


import Control.Applicative
import Data.Either ( lefts, rights )

import Language.Essence
import ParsecUtils


pExpr :: Parser Expr
pExpr = pExprCore <?> "expression"

pExprCore :: Parser Expr
pExprCore = choiceTry $ (pIdentifier <?> "identifier") : pValue ++ pDomain

pIdentifier :: Parser Expr
pIdentifier = Identifier <$> identifier


--------------------------------------------------------------------------------
-- Parsers for inline values ---------------------------------------------------
--------------------------------------------------------------------------------

pValue :: [Parser Expr]
pValue = [ pBool, pInteger, pMatrix, pTuple, pSet, pMSet, pFunction, pRelation, pPartition ]
    where
        pBool :: Parser Expr
        pBool = ValueBoolean False <$ reserved "false"
                <|>
                ValueBoolean True  <$ reserved "true"

        pInteger :: Parser Expr
        pInteger = ValueInteger <$> integer

        pMatrix :: Parser Expr
        pMatrix = ValueMatrix <$> brackets (sepBy pExpr comma)

        pTuple :: Parser Expr
        pTuple = ValueTuple <$> parens (countSepAtLeast 2 pExpr comma)

        pSet :: Parser Expr
        pSet = ValueSet <$> (reserved "set" *> braces (sepBy pExpr comma))

        pMSet :: Parser Expr
        pMSet = ValueMSet <$> (reserved "mset" *> braces (sepBy pExpr comma))

        pFunction :: Parser Expr
        pFunction = ValueFunction <$> (reserved "function" *> braces (sepBy pTuple2 comma))
            where
                pTuple2 :: Parser (Expr, Expr)
                pTuple2 = (,) <$> pExpr <*> (reservedOp "->" *> pExpr)

        pRelation :: Parser Expr
        pRelation = ValueRelation <$> (reserved "relation" *> braces (sepBy pTuple comma))

        pPartition :: Parser Expr
        pPartition = ValuePartition <$> (reserved "partition" *> braces (sepBy aPart comma))
            where
                aPart :: Parser [Expr]
                aPart = braces (sepBy pExpr comma)


--------------------------------------------------------------------------------
-- Parsers for domains ---------------------------------------------------------
--------------------------------------------------------------------------------

pDomain :: [Parser Expr]
pDomain = [ pBool, pIntegerList, pIntegerFromTo, pIntegerOnly, pUnnamed, pEnum, pMatrix, pTuple
          , pSet False, pSet True, pMSet False, pMSet True
          ]
    where
        pBool :: Parser Expr
        pBool = DomainBoolean <$ reserved "bool"

        pIntegerOnly :: Parser Expr
        pIntegerOnly = DomainIntegerList [] <$ reserved "int"

        pIntegerFromTo :: Parser Expr
        pIntegerFromTo = do
            reserved "int"
            (i,j) <- parens ( do
                i <- optionMaybe pExpr
                dot
                dot
                j <- optionMaybe pExpr
                return (i,j) )
            return (DomainIntegerFromTo i j)

        pIntegerList :: Parser Expr
        pIntegerList = DomainIntegerList <$> (reserved "int" *> parens (sepBy pExpr comma))

        pUnnamed :: Parser Expr
        pUnnamed = DomainUnnamed <$> (reserved "new" *> reserved "type" *> reserved "of" *> reserved "size" *> pExpr) <*> pure Nothing

        pEnum :: Parser Expr
        pEnum = DomainEnum <$> (reserved "enum" *> braces (sepBy1 identifier comma)) <*> pure Nothing

        pMatrix :: Parser Expr
        pMatrix = helper <$>
                (reserved "matrix"  *> 
                 reserved "indexed" *> 
                 reserved "by"      *> 
                 brackets (sepBy1 pExpr comma)
                ) <*>
                (reserved "of" *> pExpr)
            where
                helper :: [Expr] -> Expr -> Expr
                helper is e = foldr DomainMatrix e is

        pTuple :: Parser Expr
        pTuple = do
            reserved "tuple"
            DomainTuple <$> parens (countSepAtLeast 2 pExpr comma) <*> pure Nothing

        pSet :: Bool -> Parser Expr
        pSet b = case b of
            False -> helper <$> pure [] <*> (reserved "set" *> reserved "of" *> pExpr)
            True  -> helper <$> (reserved "set" *> parens (keyValuePairOrAttibuteList ["size","minSize","maxSize"] []))
                            <*> (reserved "of"  *> pExpr)
            where
                helper :: [Either String (String,Expr)] -> Expr -> Expr
                helper attrs e = DomainSet
                    {    size        = "size"         `lookup` rights attrs
                    , minSize        = "minSize"      `lookup` rights attrs
                    , maxSize        = "maxSize"      `lookup` rights attrs
                    , attrDontCare   = "attrDontCare" `elem`   lefts  attrs
                    , representation = lookupRepresentation (rights attrs)
                    , element        = e
                    }

        pMSet :: Bool -> Parser Expr
        pMSet b = case b of
            False -> helper <$> pure [] <*> (reserved "mset" *> reserved "of" *> pExpr)
            True  -> helper <$> (reserved "mset" *> parens (keyValuePairOrAttibuteList [ "size", "minSize", "maxSize", "occr", "minOccr", "maxOccr" ] []))
                            <*> (reserved "of"   *> pExpr)
            where
                helper :: [Either String (String,Expr)] -> Expr -> Expr
                helper attrs e = DomainMSet
                    {    size        = "size"         `lookup` rights attrs
                    , minSize        = "minSize"      `lookup` rights attrs
                    , maxSize        = "maxSize"      `lookup` rights attrs
                    ,    occr        = "occr"         `lookup` rights attrs
                    , minOccr        = "minOccr"      `lookup` rights attrs
                    , maxOccr        = "maxOccr"      `lookup` rights attrs
                    , attrDontCare   = "attrDontCare" `elem`   lefts  attrs
                    , representation = lookupRepresentation (rights attrs)
                    , element        = e
                    }


-- | Parser for a comma separated attribute of key-value pair list. First
-- parameter is possible key (in a key value pair), and the second parameter
-- is possible attributes.
keyValuePairOrAttibuteList :: [String] -> [String] -> Parser [Either String (String,Expr)]
keyValuePairOrAttibuteList kws ats = sepBy1 (try (Left <$> anyAttr) <|> (Right <$> anyKV)) comma
    where
        reprP :: Parser (String, Expr)
        reprP = (,) "representation" <$> (reserved "representation" *> (Identifier <$> identifier))

        attrDontCareP :: Parser String
        attrDontCareP = "attrDontCare" <$ reserved "_"

        anyKV :: Parser (String, Expr)
        anyKV = choiceTry $ reprP : [ (,) kw <$> (reserved kw *> pExpr) | kw <- kws ]

        anyAttr :: Parser String
        anyAttr = choiceTry $ attrDontCareP : [ s <$ reserved s | s <- ats ]


lookupRepresentation :: [(String,Expr)] -> Maybe String
lookupRepresentation lu = case lookup "representation" lu of
    Just (Identifier nm) -> Just nm
    _                    -> Nothing
