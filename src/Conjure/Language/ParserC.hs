{-# LANGUAGE RecordWildCards #-}

module Conjure.Language.ParserC ( parseModel ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Pretty
import Conjure.Language.Lexer ( Lexeme(..), LexemePos(..), lexemeFace )
import Conjure.Language.Parser ( Parser, ParserState(..) )

-- megaparsec
import Text.Megaparsec.Prim ( (<?>), label, token, try, eof, getPosition, setPosition )
import Text.Megaparsec.Error ( Message(..) )
import Text.Megaparsec.Pos ( SourcePos(..) )
import Text.Megaparsec.Combinator ( between, sepBy, sepBy1, sepEndBy, sepEndBy1 )
import Text.Megaparsec.ShowToken ( showToken )

-- text
import qualified Data.Text as T

-- containers
import qualified Data.Set as S ( null, fromList, toList )


parseModel :: Parser Model
parseModel = inCompleteFile $ do
    let
        pLanguage :: Parser LanguageVersion
        pLanguage = do
            lexeme L_language
            pos1 <- getPosition
            l <- identifierText
            -- ESSENCE' is accepted, just for convenience
            unless (l `elem` ["Essence", "ESSENCE", "ESSENCE'"]) $ do
                setPosition pos1
                fail $ "language name has to be Essence, but given:" <+> pretty l
            pos2 <- getPosition
            is <- sepBy1 integer dot
            unless (is >= [1]) $ do
                setPosition pos2
                fail $ "language version expected to be at least 1.0, but given:" <+>
                            pretty (intercalate "." (map show is))
            return (LanguageVersion (Name l) (map fromInteger is))
    l  <- optional pLanguage
    xs <- many parseTopLevels
    return Model
        { mLanguage = fromMaybe def l
        , mStatements = concat xs
        , mInfo = def
        }


--------------------------------------------------------------------------------
-- Actual parsers --------------------------------------------------------------
--------------------------------------------------------------------------------

parseTopLevels :: Parser [Statement]
parseTopLevels = do
    let one = do
                lexeme L_letting
                i <- parseName
                lexeme L_be
                msum
                    [ do
                        lexeme L_new
                        lexeme L_type
                        msum
                            [ do
                                lexeme L_of
                                lexeme $ LIdentifier "size"
                                j <- parseExpr
                                return $ Declaration (LettingDomainDefnUnnamed i j)
                            , do
                                lexeme L_enum
                                ys <- braces (commaSeparated parseName) <|> return []
                                modify (\ st -> st { enumDomains = [i] ++ enumDomains st } )
                                return $ Declaration (LettingDomainDefnEnum i ys)
                            ]
                    , do
                        lexeme L_domain
                        j <- parseDomain
                        return $ Declaration (Letting i (Domain j))
                    , do
                        j <- parseExpr
                        return $ Declaration (Letting i j)
                    ]
                <?> "letting statement"
    some one

parseRange :: Parser a -> Parser (Range a)
parseRange p = msum [try pRange, pSingle] <?> "range"
    where
        pRange = do
            fr <- optional p
            dotdot
            to <- optional p
            return $ case (fr,to) of
                (Nothing, Nothing) -> RangeOpen
                (Just x , Nothing) -> RangeLowerBounded x
                (Nothing, Just y ) -> RangeUpperBounded y
                (Just x , Just y ) -> RangeBounded x y
        pSingle = do
            x <- p
            return (RangeSingle x)

parseDomain :: Parser (Domain () Expression)
parseDomain = (forgetRepr <$> parseDomainWithRepr) <?> "domain"

parseDomainWithRepr :: Parser (Domain HasRepresentation Expression)
parseDomainWithRepr = pDomainAtom

    where

        pDomainAtom = msum
            [ pBool, try pIntFromExpr, pInt, try pEnum, try pReference
            , pMatrix, try pTupleWithout, pTupleWith
            , pRecord, pVariant
            , pSet
            , pMSet
            , try pFunction', pFunction
            , pSequence
            , pRelation
            , pPartition
            , DomainMetaVar <$> parseMetaVariable, parens parseDomainWithRepr
            ]

        pBool = do
            lexeme L_bool
            -- parse and discard, compatibility with SR
            _ <- optional $ parens $ commaSeparated0 $ parseRange parseExpr
            return DomainBool

        pIntFromExpr = do
            lexeme L_int
            x <- parens parseExpr
            case (let ?typeCheckerMode = StronglyTyped in typeOf x) of
                Just (TypeInt TagInt) -> return $ DomainInt TagInt [RangeSingle x]
                _ -> return $ DomainIntE x

        pInt = do
            lexeme L_int
            mxs <- optional $ parens $ commaSeparated0 $ parseRange parseExpr
            let xs = fromMaybe [] mxs
            return $ DomainInt TagInt xs

        pReference = do
            r  <- identifierText
            return $ DomainReference (Name r) Nothing

        pEnum = do
            r  <- identifierText
            xs <- optional $ parens $ commaSeparated0 $ parseRange parseExpr
            st <- get
            guard (Name r `elem` enumDomains st)
            return $ DomainEnum (Name r) xs Nothing

        pMatrix = do
            lexeme L_matrix
            lexeme L_indexed
            lexeme L_by
            xs <- brackets (commaSeparated parseDomain)
            lexeme L_of
            y  <- parseDomainWithRepr
            return $ foldr DomainMatrix y xs

        pTupleWith = do
            lexeme L_tuple
            xs <- parens $ commaSeparated0 parseDomainWithRepr
            return $ DomainTuple xs

        pTupleWithout = do
            xs <- parens $ countSepAtLeast 2 parseDomainWithRepr comma
            return $ DomainTuple xs

        pRecord = do
            lexeme L_record
            let one = do n <- parseName
                         lexeme L_Colon
                         d <- parseDomainWithRepr
                         return (n,d)
            xs <- braces $ commaSeparated0 one
            return $ DomainRecord xs

        pVariant = do
            lexeme L_variant
            let one = do n <- parseName
                         lexeme L_Colon
                         d <- parseDomainWithRepr
                         return (n,d)
            xs <- braces $ commaSeparated0 one
            return $ DomainVariant xs

        pSet = do
            lexeme L_set
            x <- parseSetAttr
            y <- lexeme L_of >> parseDomainWithRepr
            return $ DomainSet NoRepresentation x y

        pMSet = do
            lexeme L_mset
            x <- parseMSetAttr
            y <- lexeme L_of >> parseDomainWithRepr
            return $ DomainMSet NoRepresentation x y

        pFunction' = do
            lexeme L_function
            (y,z) <- arrowedPair parseDomainWithRepr
            return $ DomainFunction NoRepresentation def y z

        pFunction = do
            lexeme L_function
            x <- parseFunctionAttr
            (y,z) <- arrowedPair parseDomainWithRepr
            return $ DomainFunction NoRepresentation x y z

        pSequence = do
            lexeme L_sequence
            x <- parseSequenceAttr
            y <- lexeme L_of >> parseDomainWithRepr
            return $ DomainSequence NoRepresentation x y

        pRelation = do
            lexeme L_relation
            pos <- getPosition
            x  <- parseRelationAttr
            lexeme L_of
            ys <- parens (parseDomainWithRepr `sepBy` lexeme L_Times)
            let RelationAttr _ (BinaryRelationAttrs binAttrs) = x
            when (length ys /= 2 && not (S.null binAttrs)) $ do
                setPosition pos
                fail $ "Only binary relations can have these attributes:" <+>
                            prettyList id "," (S.toList binAttrs)
            return $ DomainRelation NoRepresentation x ys

        pPartition = do
            lexeme L_partition
            x <- parsePartitionAttr
            lexeme L_from
            y <- parseDomainWithRepr
            return $ DomainPartition NoRepresentation x y

parseAttributes :: Parser (DomainAttributes Expression)
parseAttributes = do
    xs <- parens (commaSeparated0 parseAttribute) <|> return []
    return $ DomainAttributes xs
    where
        parseAttribute = msum [parseDontCare, try parseNameValue, parseDAName]
        parseNameValue = DANameValue <$> (Name <$> identifierText) <*> parseExpr
        parseDAName = DAName <$> (Name <$> identifierText)
        parseDontCare = do dotdot ; return DADotDot

parseSetAttr :: Parser (SetAttr Expression)
parseSetAttr = do
    pos <- getPosition
    DomainAttributes attrs <- parseAttributes
    checkExtraAttributes pos "set" attrs
        ["size", "minSize", "maxSize"]
    SetAttr <$> case filterSizey attrs of
        [] -> return SizeAttr_None
        [DANameValue "size"    a] -> return (SizeAttr_Size a)
        [DANameValue "minSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxSize" b, DANameValue "minSize" a] -> return (SizeAttr_MinMaxSize a b)
        as -> do
            setPosition pos
            fail ("incompatible attributes:" <+> stringToDoc (show as))

parseMSetAttr :: Parser (MSetAttr Expression)
parseMSetAttr = do
    pos <- getPosition
    DomainAttributes attrs <- parseAttributes
    checkExtraAttributes pos "mset" attrs
        [ "size", "minSize", "maxSize"
        , "minOccur", "maxOccur"
        ]
    size <- case filterSizey attrs of
        [] -> return SizeAttr_None
        [DANameValue "size"    a] -> return (SizeAttr_Size a)
        [DANameValue "minSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxSize" b, DANameValue "minSize" a] -> return (SizeAttr_MinMaxSize a b)
        as -> do
            setPosition pos
            fail ("incompatible attributes:" <+> stringToDoc (show as))
    occur <- case filterAttrName ["minOccur", "maxOccur"] attrs of
        [] -> return OccurAttr_None
        [DANameValue "minOccur" a] -> return (OccurAttr_MinOccur a)
        [DANameValue "maxOccur" a] -> return (OccurAttr_MaxOccur a)
        [DANameValue "maxOccur" b, DANameValue "minOccur" a] -> return (OccurAttr_MinMaxOccur a b)
        as -> do
            setPosition pos
            fail ("incompatible attributes:" <+> stringToDoc (show as))
    return (MSetAttr size occur)

parseFunctionAttr :: Parser (FunctionAttr Expression)
parseFunctionAttr = do
    pos <- getPosition
    DomainAttributes attrs <- parseAttributes
    checkExtraAttributes pos "function" attrs
        [ "size", "minSize", "maxSize"
        , "injective", "surjective", "bijective"
        , "total"
        ]
    size <- case filterSizey attrs of
        [DANameValue "size"    a] -> return (SizeAttr_Size a)
        [DANameValue "minSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxSize" b, DANameValue "minSize" a] -> return (SizeAttr_MinMaxSize a b)
        [] -> return SizeAttr_None
        as -> do
            setPosition pos
            fail ("incompatible attributes:" <+> stringToDoc (show as))
    let partiality = if DAName "total" `elem` attrs
                        then PartialityAttr_Total
                        else PartialityAttr_Partial
    jectivity  <- case filterJectivity attrs of
        [] -> return JectivityAttr_None
        [DAName "bijective" ] -> return JectivityAttr_Bijective
        [DAName "injective" ] -> return JectivityAttr_Injective
        [DAName "surjective"] -> return JectivityAttr_Surjective
        [DAName "injective", DAName "surjective"] -> return JectivityAttr_Bijective
        as -> do
            setPosition pos
            fail ("incompatible attributes:" <+> stringToDoc (show as))
    return (FunctionAttr size partiality jectivity)

parseSequenceAttr :: Parser (SequenceAttr Expression)
parseSequenceAttr = do
    pos <- getPosition
    DomainAttributes attrs <- parseAttributes
    checkExtraAttributes pos "sequence" attrs
        [ "size", "minSize", "maxSize"
        , "injective", "surjective", "bijective"
        ]
    size <- case filterSizey attrs of
        [DANameValue "size"    a] -> return (SizeAttr_Size a)
        [DANameValue "minSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxSize" b, DANameValue "minSize" a] -> return (SizeAttr_MinMaxSize a b)
        [] -> return SizeAttr_None
        as -> do
            setPosition pos
            fail ("incompatible attributes:" <+> stringToDoc (show as))
    jectivity  <- case filterJectivity attrs of
        [] -> return JectivityAttr_None
        [DAName "bijective" ] -> return JectivityAttr_Bijective
        [DAName "injective" ] -> return JectivityAttr_Injective
        [DAName "surjective"] -> return JectivityAttr_Surjective
        [DAName "injective", DAName "surjective"] -> return JectivityAttr_Bijective
        as -> do
            setPosition pos
            fail ("incompatible attributes:" <+> stringToDoc (show as))
    return (SequenceAttr size jectivity)

parseRelationAttr :: Parser (RelationAttr Expression)
parseRelationAttr = do
    pos <- getPosition
    DomainAttributes attrs <- parseAttributes
    checkExtraAttributes pos "relation" attrs
        [ "size", "minSize", "maxSize"
        , "reflexive", "irreflexive", "coreflexive"
        , "symmetric", "antiSymmetric", "aSymmetric"
        , "transitive", "total", "connex", "Euclidean"
        , "serial", "equivalence", "partialOrder"
        ]
    size <- case filterSizey attrs of
        [] -> return SizeAttr_None
        [DANameValue "size"    a] -> return (SizeAttr_Size a)
        [DANameValue "minSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxSize" b, DANameValue "minSize" a] -> return (SizeAttr_MinMaxSize a b)
        as -> do
            setPosition pos
            fail ("incompatible attributes:" <+> stringToDoc (show as))
    let readBinRel' (DAName (Name a)) = readBinRel (fromString (textToString a))
        readBinRel' a = do
            setPosition pos
            fail $ "Not a binary relation attribute:" <+> pretty a
    binRels <- mapM readBinRel' (filterBinRel attrs)
    return (RelationAttr size (BinaryRelationAttrs (S.fromList binRels)))

parsePartitionAttr :: Parser (PartitionAttr Expression)
parsePartitionAttr = do
    pos <- getPosition
    DomainAttributes attrs <- parseAttributes
    checkExtraAttributes pos "partition" attrs
        [ "size", "minSize", "maxSize"
        , "regular"
        , "numParts", "minNumParts", "maxNumParts"
        , "partSize", "minPartSize", "maxPartSize"
        ]
    unless (null $ filterAttrName ["complete"] attrs) $ do
        setPosition pos
        fail $ vcat [ "Partitions do not support the 'complete' attribute."
                    , "They are complete by default."
                    ]
    unless (null $ filterSizey attrs) $ do
        setPosition pos
        fail $ vcat [ "Partitions do not support these attributes:" <+> prettyList id "," (filterSizey attrs)
                    , "This is because partitions are complete by default."
                    ]
    partsNum         <- case filterAttrName ["numParts", "minNumParts", "maxNumParts"] attrs of
        [] -> return SizeAttr_None
        [DANameValue "numParts"    a] -> return (SizeAttr_Size a)
        [DANameValue "minNumParts" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxNumParts" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxNumParts" b, DANameValue "minNumParts" a] -> return (SizeAttr_MinMaxSize a b)
        as -> do
            setPosition pos
            fail ("incompatible attributes:" <+> stringToDoc (show as))
    partsSize        <- case filterAttrName ["partSize", "minPartSize", "maxPartSize"] attrs of
        [] -> return SizeAttr_None
        [DANameValue "partSize"    a] -> return (SizeAttr_Size a)
        [DANameValue "minPartSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxPartSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxPartSize" b, DANameValue "minPartSize" a] -> return (SizeAttr_MinMaxSize a b)
        as -> do
            setPosition pos
            fail ("incompatible attributes:" <+> stringToDoc (show as))
    let isRegular  = DAName "regular"  `elem` attrs
    return PartitionAttr {..}


checkExtraAttributes :: SourcePos -> Doc -> [DomainAttribute a] -> [Name] -> Parser ()
checkExtraAttributes pos ty attrs supported = do
    let extras = mapMaybe f attrs
    unless (null extras) $ do
        setPosition pos
        fail $ vcat [ "Unsupported attributes for" <+> ty <> ":" <+> prettyList id "," extras
                    , "Only these are supported:" <+> prettyList id "," supported
                    ]
    where
        f (DANameValue nm _) | nm `notElem` supported = Just nm
        f (DAName      nm  ) | nm `notElem` supported = Just nm
        f _ = Nothing

filterAttrName :: Ord a => [Name] -> [DomainAttribute a] -> [DomainAttribute a]
filterAttrName keep = sort . filter f
    where
        f (DANameValue nm _) | nm `elem` keep = True
        f (DAName      nm  ) | nm `elem` keep = True
        f _ = False

filterSizey :: Ord a => [DomainAttribute a] -> [DomainAttribute a]
filterSizey = filterAttrName ["size", "minSize", "maxSize"]

filterJectivity :: Ord a => [DomainAttribute a] -> [DomainAttribute a]
filterJectivity = filterAttrName ["injective", "surjective", "bijective"]

filterBinRel :: Ord a => [DomainAttribute a] -> [DomainAttribute a]
filterBinRel = filterAttrName
    [ "reflexive"
    , "irreflexive"
    , "coreflexive"
    , "symmetric"
    , "antiSymmetric"
    , "aSymmetric"
    , "transitive"
    , "total"
    , "connex"
    , "Euclidean"
    , "serial"
    , "equivalence"
    , "partialOrder"
    ]

parseMetaVariable :: Parser String
parseMetaVariable = do
    let isMeta LMetaVar{} = True
        isMeta _          = False
    LMetaVar iden <- satisfyT isMeta
    return (T.unpack iden)

parseExpr :: Parser Expression
-- parseExpr | trace "parseExpr" True = parseAtomicExpr <?> "expression"
parseExpr = parseAtomicExpr <?> "expression"

parseAtomicExpr :: Parser Expression
-- parseAtomicExpr | trace "parseAtomicExpr" True = parseAtomicExprNoPrePost <?> "expression"
parseAtomicExpr = parseAtomicExprNoPrePost <?> "expression"

parseAtomicExprNoPrePost :: Parser Expression
-- parseAtomicExprNoPrePost | trace "parseAtomicExprNoPrePost" True = msum [try parseLiteral, parseTyped]
parseAtomicExprNoPrePost = msum [try parseLiteral, parseReference, parseTyped]

parseTyped :: Parser Expression
-- parseTyped | trace "parseTyped" True = parens $ do
parseTyped = parens $ do
    x  <- parseExpr
    lexeme L_Colon
    d  <- betweenTicks parseDomain
    ty <- let ?typeCheckerMode = StronglyTyped in typeOfDomain d
    return (Typed x ty)

parseName :: Parser Name
parseName = Name <$> identifierText

parseReference :: Parser Expression
parseReference = Reference <$> parseName <*> pure Nothing

parseLiteral :: Parser Expression
parseLiteral = label "value" (do p <- pCore ; p)

    where

        -- convert x to a constant if possible
        -- might save us from evaluating it again and again later
        mkAbstractLiteral x =
            case e2c (AbstractLiteral x) of
                Nothing -> AbstractLiteral x
                Just c  -> Constant c

        pCore :: Parser (Parser Expression)
        pCore = satisfyL $ \case
                L_false       -> Just $ return $ Constant $ ConstantBool False
                L_true        -> Just $ return $ Constant $ ConstantBool True
                LIntLiteral i -> Just $ return $ Constant $ ConstantInt TagInt (fromInteger i)
                L_OpenBracket -> Just pMatrix
                L_tuple       -> Just pTupleWith
                L_OpenParen   -> Just pTupleWithout
                L_record      -> Just pRecord
                L_variant     -> Just pVariant
                L_OpenCurly   -> Just pSet
                L_mset        -> Just pMSet
                L_function    -> Just pFunction
                L_sequence    -> Just pSequence
                L_relation    -> Just pRelation
                L_partition   -> Just pPartition
                L_Minus       -> Just $ do
                    p <- pCore
                    res <- p
                    return (negate res)
                _ -> Nothing

        pMatrix = mkAbstractLiteral <$> do
            -- lexeme L_OpenBracket
            xs <- commaSeparated0 parseExpr
            msum
                [ do
                    let r = mkDomainIntB 1 (fromInt (genericLength xs))
                    lexeme L_CloseBracket
                    return (AbsLitMatrix r xs)
                , do
                    lexeme L_SemiColon
                    r <- parseDomain
                    lexeme L_CloseBracket
                    return (AbsLitMatrix r xs)
                ]

        pTupleWith = mkAbstractLiteral <$> do
            -- lexeme L_tuple
            xs <- parens $ commaSeparated0 parseExpr
            return (AbsLitTuple xs)

        pTupleWithout = mkAbstractLiteral <$> do
            -- xs <- parens $ countSepAtLeast 2 parseExpr comma
            xs <- countSepAtLeast 2 parseExpr comma
            lexeme L_CloseParen
            return (AbsLitTuple xs)

        pRecord = mkAbstractLiteral <$> do
            -- lexeme L_record
            let one = do n <- parseName
                         lexeme L_Eq
                         x <- parseExpr
                         return (n,x)
            xs <- braces $ commaSeparated0 one
            return $ AbsLitRecord xs

        pVariant = mkAbstractLiteral <$> do
            -- lexeme L_variant
            let one = do n <- parseName
                         lexeme L_Eq
                         x <- parseExpr
                         return (n,x)
            (n,x) <- braces one
            return $ AbsLitVariant Nothing n x

        pSet = mkAbstractLiteral <$> do
            -- xs <- braces (commaSeparated0 parseExpr)
            xs <- commaSeparated0 parseExpr
            lexeme L_CloseCurly
            return (AbsLitSet xs)

        pMSet = mkAbstractLiteral <$> do
            -- lexeme L_mset
            xs <- parens (commaSeparated0 parseExpr)
            return (AbsLitMSet xs)

        pFunction = mkAbstractLiteral <$> do
            -- lexeme L_function
            xs <- parens (commaSeparated0 inner)
            return (AbsLitFunction xs)
            where
                inner = arrowedPair parseExpr

        pSequence = mkAbstractLiteral <$> do
            -- lexeme L_sequence
            xs <- parens (commaSeparated0 parseExpr)
            return (AbsLitSequence xs)

        pRelation = mkAbstractLiteral <$> do
            -- lexeme L_relation
            xs <- parens (commaSeparated0 (pTupleWith <|> pTupleWithout))
            xsFiltered <- forM xs $ \case
                Constant (ConstantAbstract (AbsLitTuple is)) -> return (map Constant is)
                AbstractLiteral (AbsLitTuple is) -> return is
                x -> fail ("Cannot parse as part of relation literal:" <+> vcat [pretty x, pretty (show x)])
            return (AbsLitRelation xsFiltered)

        pPartition = mkAbstractLiteral <$> do
            -- lexeme L_partition
            xs <- parens (commaSeparated0 inner)
            return (AbsLitPartition xs)
            where
                inner = braces (commaSeparated0 parseExpr)


identifierText :: Parser T.Text
identifierText = do
    LIdentifier i <- satisfyT isIdentifier
    return i
    where isIdentifier LIdentifier {} = True
          isIdentifier _ = False

satisfyT :: (Lexeme -> Bool) -> Parser Lexeme
satisfyT predicate = token nextPos testTok
    where
        testTok :: LexemePos -> Either [Message] Lexeme
        testTok (LexemePos tok _ _) =
            -- trace ("satisfyT: " ++ show pos ++ "\t" ++ show tok) $
            if predicate tok
                then Right tok
                else Left [Unexpected (showToken tok)]

        nextPos :: Int -> SourcePos -> LexemePos -> SourcePos
        nextPos _ _ (LexemePos _ _ pos) = pos

satisfyL :: forall a . (Lexeme -> Maybe a) -> Parser a
satisfyL predicate = token nextPos testTok
    where
        testTok :: LexemePos -> Either [Message] a
        testTok (LexemePos tok _ _) =
            -- trace ("satisfyL: " ++ show pos ++ "\t" ++ show tok) $
            case predicate tok of
                Nothing  -> Left [Unexpected (showToken tok)]
                Just res -> Right res

        nextPos :: Int -> SourcePos -> LexemePos -> SourcePos
        nextPos _ _ (LexemePos _ _ pos) = pos

integer :: Parser Integer
integer = do
    LIntLiteral i <- satisfyT isInt
    return i
    where isInt LIntLiteral {} = True
          isInt _ = False

-- parse a comma separated list of things. can be 0 things.
commaSeparated0 :: Parser a -> Parser [a]
commaSeparated0 p = sepEndBy p comma

-- parse a comma separated list of things. has to be at least 1 thing.
commaSeparated :: Parser a -> Parser [a]
commaSeparated p = sepEndBy1 p comma

comma :: Parser ()
comma = lexeme L_Comma <?> "comma"

dot :: Parser ()
dot = lexeme L_Dot <?> "dot"

dotdot :: Parser ()
dotdot = (dot >> dot) <?> ".."


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
-- lexeme l = trace ("lexeme: " ++ show l) (void (satisfyT (l==)) <?> show (lexemeFace l))
lexeme l = void (satisfyT (l==)) <?> show (lexemeFace l)

arrowedPair :: Parser a -> Parser (a,a)
arrowedPair p = do
    i <- p
    lexeme L_LongArrow
    j <- p
    return (i,j)

inCompleteFile :: Parser a -> Parser a
inCompleteFile parser = do
    result <- parser
    eof
    return result
