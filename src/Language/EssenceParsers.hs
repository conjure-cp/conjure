{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Language.EssenceParsers ( pSpec, pExpr, pType, pKind
                               , pTopLevels, pObjective
                               , pRuleRepr, pRuleRefn
                               , pExprPattern
                               ) where

import Control.Applicative
import Control.Monad.Identity ( Identity )
import Data.Char ( isLetter, isNumber )
import Data.Either ( lefts, rights )
import Data.Function ( on )
import Data.Generics.Uniplate.Direct ( transform, transformBi )
import Data.List ( groupBy, sortBy )
import Data.Maybe ( mapMaybe )
import Data.Monoid ( mappend )
import Data.Ord ( comparing )

import Language.Essence
import ParsecUtils
import Utils ( allValues )


pIdentifier :: Parser Expr
pIdentifier = Identifier <$> identifier
    <?> "identifier"

-- pUnderscore :: Parser Expr
-- pUnderscore = Underscore <$ symbol "_"
--     <?> "underscore"

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

pDomain :: Parser Expr
pDomain = choiceTry (pIdentifier : pDomains) <?> "domain"

pDomains :: [Parser Expr]
pDomains = map (<?> "domain")
            [ pBool, pIntegerList, pIntegerFromTo, pIntegerOnly, pEnum, pMatrix
            , pTuple     False, pTuple     True
            , pUnnamed   False, pUnnamed   True
            , pSet       False, pSet       True
            , pMSet      False, pMSet      True
            , pFunction  False, pFunction  True
            , pRelation  False, pRelation  True
            , pPartition False, pPartition True
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

        pUnnamed :: Bool -> Parser Expr
        pUnnamed b = case b of
            False -> helper <$> pure [] <*> (reserved "new" *> reserved "type" *> reserved "of" *> reserved "size" *> pExpr)
            True  -> helper <$> (reserved "new" *> reserved "type" *> parens (keyValuePairOrAttibuteList [] []))
                            <*> (reserved "of"  *> reserved "size" *> pExpr)
            where
                helper :: [Either String (String,Expr)] -> Expr -> Expr
                helper attrs e = DomainUnnamed e (lookupRepresentation attrs)

        pEnum :: Parser Expr
        pEnum = DomainEnum <$> (reserved "enum" *> braces (sepBy1 identifier comma)) <*> pure Nothing

        pMatrix :: Parser Expr
        pMatrix = helper <$>
                (reserved "matrix"  *> 
                 reserved "indexed" *> 
                 reserved "by"      *> 
                 brackets (sepBy1 pDomain comma)
                ) <*>
                (reserved "of" *> pDomain)
            where
                helper :: [Expr] -> Expr -> Expr
                helper is e = foldr DomainMatrix e is

        pTuple :: Bool -> Parser Expr
        pTuple b = case b of
            False -> helper <$> (reserved "tuple" *> reserved "of" *> pure [])
                            <*> parens (countSepAtLeast 1 pDomain comma)
            True  -> helper <$> (reserved "tuple" *> parens (keyValuePairOrAttibuteList [] []) <* reserved "of")
                            <*> parens (countSepAtLeast 1 pDomain comma)
            where
                helper :: [Either String (String,Expr)] -> [Expr] -> Expr
                helper attrs xs = DomainTuple xs (lookupRepresentation attrs)

        pSet :: Bool -> Parser Expr
        pSet b = case b of
            False -> helper <$> (reserved "set" *> pure [])
                            <*> (reserved "of"  *> pDomain)
            True  -> helper <$> (reserved "set" *> parens (keyValuePairOrAttibuteList ["size","minSize","maxSize"] []))
                            <*> (reserved "of"  *> pDomain)
            where
                helper :: [Either String (String,Expr)] -> Expr -> Expr
                helper attrs e = DomainSet
                    {    size        = "size"         `lookup` rights attrs
                    , minSize        = "minSize"      `lookup` rights attrs
                    , maxSize        = "maxSize"      `lookup` rights attrs
                    , attrDontCare   = "attrDontCare" `elem`   lefts  attrs
                    , representation = lookupRepresentation attrs
                    , element        = e
                    }

        pMSet :: Bool -> Parser Expr
        pMSet b = case b of
            False -> helper <$> (reserved "mset" *> pure [])
                            <*> (reserved "of"   *> pDomain)
            True  -> helper <$> (reserved "mset" *> parens (keyValuePairOrAttibuteList [ "size", "minSize", "maxSize", "occr", "minOccr", "maxOccr" ] []))
                            <*> (reserved "of"   *> pDomain)
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
                    , representation = lookupRepresentation attrs
                    , element        = e
                    }

        pFunction :: Bool -> Parser Expr
        pFunction b = case b of
            False -> helper <$> (reserved "function" *> pure [])
                            <*> pDomain <*> (reservedOp "->" *> pDomain)
            True  -> helper <$> (reserved "function" *> parens (keyValuePairOrAttibuteList [] [ "total", "partial", "injective", "bijective", "surjective" ]))
                            <*> pDomain <*> (reservedOp "->" *> pDomain)
            where
                helper :: [Either String (String,Expr)] -> Expr -> Expr -> Expr
                helper attrs fr to = DomainFunction
                    { functionFrom   = fr
                    , functionTo     = to
                    , isTotal        = "total"        `elem` lefts attrs
                    , isPartial      = "partial"      `elem` lefts attrs
                    , isInjective    = "injective"    `elem` lefts attrs
                    , isBijective    = "bijective"    `elem` lefts attrs
                    , isSurjective   = "surjective"   `elem` lefts attrs
                    , attrDontCare   = "attrDontCare" `elem` lefts attrs
                    , representation = lookupRepresentation attrs
                    }

        pRelation :: Bool -> Parser Expr
        pRelation b = case b of
            False -> helper <$> (reserved "relation" *> pure [])
                            <*> (reserved "of" *> parens (sepBy pDomain (reservedOp "*")))
            True  -> helper <$> (reserved "relation" *> parens (keyValuePairOrAttibuteList [] []))
                            <*> (reserved "of" *> parens (sepBy pDomain (reservedOp "*")))
            where
                helper :: [Either String (String,Expr)] -> [Expr] -> Expr
                helper attrs ds = DomainRelation
                    { components     = ds
                    , attrDontCare   = "attrDontCare" `elem` lefts attrs
                    , representation = lookupRepresentation attrs
                    }

        pPartition :: Bool -> Parser Expr
        pPartition b = case b of
            False -> helper <$> (reserved "partition" *> pure [])
                            <*> (reserved "from"      *> pDomain)
            True  -> helper <$> (reserved "partition" *> parens (keyValuePairOrAttibuteList [ "size", "minSize", "maxSize"
                                                                                            , "partSize", "minPartSize", "maxPartSize"
                                                                                            , "numParts", "minNumParts", "maxNumParts" ]
                                                                                            [ "regular", "complete" ]) )
                            <*> (reserved "from"      *> pDomain)
            where
                helper :: [Either String (String,Expr)] -> Expr -> Expr
                helper attrs e = DomainPartition
                    { element        = e
                    , isRegular      = "regular"      `elem`   lefts attrs
                    , isComplete     = "complete"     `elem`   lefts attrs
                    , size           = "size"         `lookup` rights attrs
                    , minSize        = "minSize"      `lookup` rights attrs
                    , maxSize        = "maxSize"      `lookup` rights attrs
                    , partSize       = "partSize"     `lookup` rights attrs
                    , minPartSize    = "minPartSize"  `lookup` rights attrs
                    , maxPartSize    = "maxPartSize"  `lookup` rights attrs
                    , numParts       = "numParts"     `lookup` rights attrs
                    , minNumParts    = "minNumParts"  `lookup` rights attrs
                    , maxNumParts    = "maxNumParts"  `lookup` rights attrs
                    , attrDontCare   = "attrDontCare" `elem`   lefts attrs
                    , representation = lookupRepresentation attrs
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


lookupRepresentation :: [Either String (String,Expr)] -> Maybe String
lookupRepresentation lu = case lookup "representation" (rights lu) of
    Just (Identifier nm) -> Just nm
    _                    -> Nothing


--------------------------------------------------------------------------------
-- The expression parser -------------------------------------------------------
--------------------------------------------------------------------------------

pExpr :: Parser Expr
pExpr = buildExpressionParser table core
    where
        f :: Op -> Maybe (Either (Parser Expr) (Int, Operator String () Identity Expr))
        f op = case opDescriptor op of
            OpLispy opFace card -> return $ Left $ do
                reserved opFace
                is <- parens (countSep card pExpr comma)
                return $ GenericNode op is
            OpInfixL opFace prec -> return $ Right (prec, Infix  (pFace opFace >> return (toBinOp op)) AssocLeft )
            OpInfixN opFace prec -> return $ Right (prec, Infix  (pFace opFace >> return (toBinOp op)) AssocNone )
            OpInfixR opFace prec -> return $ Right (prec, Infix  (pFace opFace >> return (toBinOp op)) AssocRight)
            OpPrefix opFace prec -> return $ Right (prec, Prefix (pFace opFace >> return (toUnOp  op))           )
            OpSpecial          -> Nothing
        
        toBinOp :: Op -> Expr -> Expr -> Expr
        toBinOp op i j = GenericNode op [i,j]

        toUnOp :: Op -> Expr -> Expr
        toUnOp op i = GenericNode op [i]

        pFace :: String -> Parser ()
        pFace s 
            | all (\ i -> isLetter i || isNumber i || i == '_' ) s = reserved s <?> "operator"
            | otherwise = reservedOp s <?> "operator"

        pOps :: [(Int, Operator String () Identity Expr)]
        pOps = rights (mapMaybe f (allValues :: [Op]))

        table :: OperatorTable String () Identity Expr
        table = [Postfix pPostfix] :
            ( reverse
            . map (map snd)
            . groupBy ((==) `on` fst)
            . sortBy (comparing fst)
            ) pOps

        core :: Parser Expr
        core = choiceTry ( pExprCore
                        ++ lefts (mapMaybe f (allValues :: [Op]))
                         )
            <?> "expression"

pExprCore :: [Parser Expr]
pExprCore =  pExprQuantifier
          :  pIdentifier
          :  pValue
          ++ pDomains
          ++ [ pExprTwoBars, parens pExpr ]

pExprQuantifier :: Parser Expr
pExprQuantifier = do
    qName  <- pIdentifier
    qVars  <- pIdentifier `sepBy1` comma
    qOver  <- colon *> pExpr
    qGuard <- optionMaybe (comma *> pExpr)
    qBody  <- dot *> pExpr
    let
        helper []     = error "impossible has happenned: pExprQuantifier.helper []"
        helper [i]    = ExprQuantifier qName i qOver qGuard qBody
        helper (i:is) = ExprQuantifier qName i qOver Nothing (helper is)
    return $ helper qVars

pExprTwoBars :: Parser Expr
pExprTwoBars = do
    x <- between (symbol "|") (symbol "|") pExpr
    return $ GenericNode Abs [x]

pPostfix :: Parser (Expr -> Expr)
pPostfix = do
   fs <- many1 $ choiceTry [pIndexed, pFuncApp, pInvFuncApp, pReplace]
   return $ \ i -> foldr1 (.) (reverse fs) i

pIndexed :: Parser (Expr -> Expr)
pIndexed = do
    is <- brackets $ pIndexer `sepBy1` comma
    return (`helper` is)
    where
        helper :: Expr -> [Expr] -> Expr
        helper m = foldl (\ m' i -> GenericNode Index [m', i]) m
        -- helper m []     = m
        -- helper m (i:is) = helper (GenericNode Index [m,i]) is

        pIndexer :: Parser Expr
        pIndexer = try pSlice <|> pExpr

        pSlice :: Parser Expr
        pSlice = do
            i <- optionMaybe pExpr
            dot
            dot
            j <- optionMaybe pExpr
            return $ MatrixSlice i j

pFuncApp :: Parser (Expr -> Expr)
pFuncApp = do
    is <- parens (pExpr `sepBy1` comma)
    let i = case is of [t] -> t
                       _   -> ValueTuple is
    return $ \ x -> GenericNode Image [x,i]

pInvFuncApp :: Parser (Expr -> Expr)
pInvFuncApp = do
    reservedOp "'"
    i <- parens pExpr
    return $ \ x -> GenericNode PreImage [x,i]

pReplace :: Parser (Expr -> Expr)
pReplace = braces $ do
    fr <- pExpr
    reservedOp "->"
    to <- pExpr
    return $ \ x -> GenericNode Replace [x,fr,to]

-- pExprTypeExpr :: Parser Expr
-- pExprTypeExpr = TypeExpr <$> pType


--------------------------------------------------------------------------------
-- Type parser -----------------------------------------------------------------
--------------------------------------------------------------------------------

pType :: Parser Type
pType = choiceTry [ pTypeUnknown, pTypeIdentifier
                  , pTypeBool, pTypeInteger, pTypeUnnamed, pTypeEnum
                  , pTypeMatrix, pTypeTuple, pTypeSet, pTypeMSet
                  , pTypeFunction, pTypeRelation, pTypePartition
                  , pTypeLambda
                  ]
    where
        pTypeUnknown = TypeUnknown <$ reservedOp "?"
        pTypeIdentifier = TypeIdentifier <$> identifier
        pTypeBool = TypeBoolean <$ reserved "bool"
        pTypeInteger = TypeInteger <$ reserved "int"
        pTypeUnnamed = TypeUnnamed <$ reserved "unnamed"
        pTypeEnum = TypeEnum <$ reserved "enum"
        pTypeMatrix = TypeMatrix <$> (reserved "matrix" *> reserved "of" *> pType)
        pTypeTuple = TypeTuple <$> (reserved "tuple" *> reserved "of" *> parens (sepBy1 pType comma))
        pTypeSet = TypeSet <$> (reserved "set" *> reserved "of" *> pType)
        pTypeMSet = TypeMSet <$> (reserved "mset" *> reserved "of" *> pType)
        pTypeFunction = TypeFunction <$> pType <*> (reservedOp "->" *> pType)
        pTypeRelation = TypeRelation <$> (reserved "relation" *> reserved "of" *> parens (sepBy1 pType comma))
        pTypePartition = TypePartition <$> (reserved "partition" *> reserved "from" *> pType)
        pTypeLambda = do
            reserved "lambda"
            braces $ do
                args <- sepBy1 pType comma
                reservedOp "->"
                x <- pType
                return $ TypeLambda args x


--------------------------------------------------------------------------------
-- Kind parser -----------------------------------------------------------------
--------------------------------------------------------------------------------

pKind :: Parser Kind
pKind = choiceTry [ KindUnknown <$ reservedOp "?"
                  , KindDomain  <$ reserved "domain"
                  , KindValue   <$ reserved "value"
                  , KindExpr    <$ reserved "expression"
                  , KindLambda  <$ reserved "lambda"
                  , KindFind    <$ reserved "find"
                  , KindGiven   <$ reserved "given"
                  ]


--------------------------------------------------------------------------------
-- The Spec parser -------------------------------------------------------------
--------------------------------------------------------------------------------

knownQuans :: String
knownQuans = "letting forall be quantifier                "
          ++ "    {                                       "
          ++ "        lambda { x:bool, y:bool -> x /\\ y } "
          ++ "        lambda { x:bool, y:bool -> x => y } "
          ++ "        true                                "
          ++ "    }                                       "
          ++ "                                            "
          ++ "letting exists be quantifier                "
          ++ "    {                                       "
          ++ "        lambda { x:bool, y:bool -> x \\/ y } "
          ++ "        lambda { x:bool, y:bool -> x /\\ y } "
          ++ "        false                               "
          ++ "    }                                       "
          ++ "                                            "
          ++ "letting sum be quantifier                   "
          ++ "    {                                       "
          ++ "        lambda { x:int , y:int -> x + y }   "
          ++ "        lambda { x:bool, y:int -> x * y }   "
          ++ "        0                                   "
          ++ "    }                                       "


pSpec :: Parser Spec
pSpec = do
    let knowns = unsafeParse pTopLevels knownQuans
    whiteSpace
    (lang,ver)        <- pLanguage
    (bindings,wheres) <- mappend knowns <$> pTopLevels
    obj               <- optionMaybe pObjective
    cons              <- pConstraints
    eof
    return $ Spec lang ver bindings wheres obj cons []


pLanguage :: Parser (String,[Int])
pLanguage = do
    l  <- reserved "language" *> identifier
    is <- sepBy1 integer dot
    return (l, map fromInteger is)


pTopLevels :: Parser ([Binding],[Where])
pTopLevels = do
    let
        pOne :: Parser ([Binding],[Where])
        pOne = choiceTry [ do i <- pBinding; return (i,[])
                         , do i <- pWhere  ; return ([],i)
                         ]
    is <- many pOne
    return (concatMap fst is, concatMap snd is)


pBinding :: Parser [Binding]
pBinding = choiceTry [ do reserved "given"
                          idens <- sepBy1 identifier comma
                          colon
                          rhs <- pDomain
                          return [ (Given, i, rhs) | i <- idens ]
                     , do reserved "find"
                          idens <- sepBy1 identifier comma
                          colon
                          rhs <- pDomain
                          return [ (Find, i, rhs) | i <- idens ]
                     , do reserved "letting"
                          idens <- sepBy1 identifier comma
                          reserved "be"
                          rhs <- pExpr
                          return [ (Letting, i, rhs) | i <- idens ]
                     , do reserved "letting"
                          idens <- sepBy1 identifier comma
                          reserved "be"
                          reserved "domain"
                          rhs <- pDomain
                          return [ (Letting, i, rhs) | i <- idens ]
                     , do reserved "letting"
                          idens <- sepBy1 identifier comma
                          reserved "be"
                          rhs <- pDeclLambda
                          return [ (Letting, i, rhs) | i <- idens ]
                     , do reserved "letting"
                          iden <- identifier
                          reserved "be"
                          reserved "quantifier"
                          rhs <- pDeclQuantifier iden
                          return [ (Letting, iden, rhs) ]
                     ]


--------------------------------------------------------------------------------
-- DeclLambda parser -----------------------------------------------------------
--------------------------------------------------------------------------------

pDeclLambda :: Parser Expr
pDeclLambda = do
    reserved "lambda"
    braces $ do
        args <- sepBy1 nameType comma
        reservedOp "->"
        x <- pExpr
        return $ DeclLambda args x
    where
        nameType :: Parser (String, Type)
        nameType = (,) <$> identifier <*> (colon *> pType)


--------------------------------------------------------------------------------
-- DeclQuantifier parser -------------------------------------------------------
--------------------------------------------------------------------------------

pDeclQuantifier :: String -> Parser Expr
pDeclQuantifier qnName = do
    result <- braces $ DeclQuantifier <$> pDeclLambda
                                      <*> pDeclLambda
                                      <*> pExpr
    let pre = "__" ++ qnName ++ "_"
    return $ transformBi (reprVarsNaming pre)
           $ transformBi (reprOneMoreRenaming pre)
           $ result


--------------------------------------------------------------------------------
-- other top-level statements --------------------------------------------------
--------------------------------------------------------------------------------

pWhere :: Parser [Where]
pWhere = reserved "where" *> sepBy1 pExpr comma


pObjective :: Parser Objective
pObjective = choiceTry [ reserved "minimising" *> ((Minimising,) <$> pExpr)
                       , reserved "minimizing" *> ((Minimising,) <$> pExpr)
                       , reserved "maximising" *> ((Maximising,) <$> pExpr)
                       , reserved "maximizing" *> ((Maximising,) <$> pExpr)
                       ]


pConstraints :: Parser [Expr]
pConstraints = choiceTry [ reserved "such" *> reserved "that" *> sepEndBy pExpr comma
                         , return []
                         ]


-- forall i : s . k ~~> forall i : s, _ . k
quantifiedPatternGuards :: Expr -> Expr
quantifiedPatternGuards p@(ExprQuantifier {quanGuard=Nothing}) = p {quanGuard=Just (Identifier "_")}
quantifiedPatternGuards p = p

pExprPattern :: Parser Expr
pExprPattern = transform quantifiedPatternGuards <$> pExpr

--------------------------------------------------------------------------------
-- parser for RuleRepr ---------------------------------------------------------
--------------------------------------------------------------------------------

pRuleRepr :: String -> Parser RuleRepr
pRuleRepr filename = do
    whiteSpace
    name       <- leadsto *> identifier
    template   <- leadsto *> pExpr
    structural <- optionMaybe (leadsto *> pExpr)
    (bs, ws)   <- pTopLevels
    cases      <- many1 pRuleReprCase
    eof
    let pre = "__" ++ safeStr filename ++ "_"
    return $ transformBi (reprVarsNaming     pre)
           $ transformBi (reprBindingsNaming pre)
           $ RuleRepr filename name template structural ws bs cases

pRuleReprCase :: Parser RuleReprCase
pRuleReprCase = do
    reservedOp "***"
    pattern    <- pExprPattern
    structural <- optionMaybe (leadsto *> pExpr)
    (bs, ws)   <- pTopLevels
    return $ RuleReprCase pattern structural ws bs


--------------------------------------------------------------------------------
-- parser for RuleRefn ---------------------------------------------------------
--------------------------------------------------------------------------------

pRuleRefn :: String -> Parser RuleRefn
pRuleRefn filename = do
    whiteSpace
    i        <- optionMaybe (brackets (fromInteger <$> integer))
    pattern  <- pExprPattern
    template <- leadsto *> pExpr
    (bs, ws) <- pTopLevels
    eof
    let pre = "__" ++ safeStr filename ++ "_"
    return $ transformBi (reprVarsNaming     pre)
           $ transformBi (reprBindingsNaming pre)
           $ RuleRefn i filename pattern [template] ws bs


renameIdentifier :: String -> String -> String
renameIdentifier pre i | i `elem` ruleLangOps = i
                       | otherwise            = pre ++ i
    where
        ruleLangOps :: [String]
        ruleLangOps = words "_ tau refn repr indices domSize forall sum exists glueOp skipOp quanID"

reprVarsNaming :: String -> Expr -> Expr
reprVarsNaming pre (Identifier i) = Identifier $ renameIdentifier pre i
reprVarsNaming _ x = x

reprBindingsNaming :: String -> Binding -> Binding
reprBindingsNaming pre (e,i,x) = (e,renameIdentifier pre i,x)

reprOneMoreRenaming :: String -> (String,Type) -> (String,Type)
reprOneMoreRenaming pre (i,t) = (renameIdentifier pre i,t)

safeStr :: String -> String
safeStr = map (\ ch -> if ch `elem` "/." then '_' else ch)

leadsto :: Parser ()
leadsto = reservedOp "~~>"

