{-# LANGUAGE RecordWildCards #-}

module Conjure.Language.Parser
    ( runLexerAndParser
    , parseIO
    , parseModel
    , parseTopLevels
    , parseExpr
    , parseDomain
    , parseDomainWithRepr
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Domain.AddAttributes
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Expression.Op
import Conjure.Language.Pretty
import Conjure.Language.Lexer ( Lexeme(..), LexemePos(..), lexemeFace, lexemeText, runLexer )

-- megaparsec
import Text.Megaparsec.Prim ( (<?>), label, token, try, eof, ParsecT, getPosition, setPosition )
import Text.Megaparsec.Error ( ParseError(..), Message(..), errorPos )
import Text.Megaparsec.Pos ( SourcePos(..), sourceLine, sourceColumn )
import Text.Megaparsec.Combinator ( between, sepBy, sepBy1, sepEndBy, sepEndBy1 )
import Text.Megaparsec.ShowToken ( showToken )
import qualified Text.Megaparsec.Prim as P ( runParser )

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
        { mLanguage = fromMaybe (LanguageVersion "Essence" [0]) l
        , mStatements = concat xs
        , mInfo = def
        }


parseIO :: MonadFail m => Parser a -> String -> m a
parseIO p s =
    case runLexerAndParser (inCompleteFile p) "" (T.pack s) of
        Left err -> fail err
        Right x  -> return x


translateQnName :: Text -> Text
translateQnName qnName = case qnName of
    "forAll" -> "and"
    "exists" -> "or"
    _        -> qnName





--------------------------------------------------------------------------------
-- Actual parsers --------------------------------------------------------------
--------------------------------------------------------------------------------

parseTopLevels :: Parser [Statement]
parseTopLevels = do
    let one = msum
                [ do
                    lexeme L_find
                    decls <- flip sepEndBy1 comma $ do
                        is <- commaSeparated parseName
                        j  <- colon >> parseDomain
                        return [ Declaration (FindOrGiven Find i j)
                               | i <- is ]
                    return $ concat decls
                    <?> "find statement"
                , do
                    lexeme L_given
                    decls <- commaSeparated $ do
                        is <- commaSeparated parseName
                        msum
                            [ do
                                colon
                                j <- parseDomain
                                return [ Declaration (FindOrGiven Given i j)
                                       | i <- is ]
                            , do
                                lexeme L_new
                                msum
                                    [ do
                                        lexeme L_type
                                        lexeme L_enum
                                        modify (\ st -> st { enumDomains = is ++ enumDomains st } )
                                        return [ Declaration (GivenDomainDefnEnum i)
                                               | i <- is ]
                                    ]
                            ]
                    return $ concat decls
                    <?> "given statement"
                , do
                    lexeme L_letting
                    decls <- commaSeparated $ do
                        is <- commaSeparated parseName
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
                                        return [ Declaration (LettingDomainDefnUnnamed i j)
                                               | i <- is
                                               ]
                                    , do
                                        lexeme L_enum
                                        ys <- braces (commaSeparated parseName) <|> return []
                                        modify (\ st -> st { enumDomains = is ++ enumDomains st } )
                                        return [ Declaration (LettingDomainDefnEnum i ys)
                                               | i <- is
                                               ]
                                    ]
                            , do
                                lexeme L_domain
                                j <- parseDomain
                                return [ Declaration (Letting i (Domain j))
                                       | i <- is
                                       ]
                            , do
                                j <- parseExpr
                                return [ Declaration (Letting i j)
                                       | i <- is
                                       ]
                            ]
                    return $ concat decls
                    <?> "letting statement"
                , do
                    lexeme L_where
                    xs <- commaSeparated parseExpr
                    return [Where xs]
                    <?> "where statement"
                , do
                    lexeme L_such
                    lexeme L_that
                    xs <- commaSeparated parseExpr
                    return [SuchThat xs]
                    <?> "such that statement"
                , do
                    lexeme L_minimising
                    x <- parseExpr
                    return [ Objective Minimising x ]
                    <?> "objective"
                , do
                    lexeme L_maximising
                    x <- parseExpr
                    return [ Objective Maximising x ]
                    <?> "objective"
                , do
                    lexeme L_branching
                    lexeme L_on
                    xs <- brackets $ commaSeparated parseSearchOrder
                    return [ SearchOrder xs ]
                    <?> "branching on"
                , do
                    lexeme L_heuristic
                    nm <- parseName
                    return [ SearchHeuristic nm ]
                    <?> "branching on"
                ] <?> "statement"
    concat <$> some one

parseSearchOrder :: Parser SearchOrder
parseSearchOrder = msum [try pBranchingOn, pCut]
    where
        pBranchingOn = BranchingOn <$> parseName
        pCut = Cut <$> parseExpr

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
parseDomainWithRepr = shuntingYardDomain parseBeforeShuntD1

    where

        -- either parse an domain and continue to parseBeforeShuntO
        -- or return []
        parseBeforeShuntD :: Parser [Either Lexeme (Domain HasRepresentation Expression)]
        parseBeforeShuntD =
            try ((:) <$> (Right <$> pDomainAtom <?> "domain")
                     <*> parseBeforeShuntO)

        parseBeforeShuntD1 :: Parser [Either Lexeme (Domain HasRepresentation Expression)]
        parseBeforeShuntD1 =
            (:) <$> (Right <$> pDomainAtom <?> "domain")
                <*> parseBeforeShuntO

        -- either parse an operator and continue to parseBeforeShuntD
        -- or return []
        parseBeforeShuntO :: Parser [Either Lexeme (Domain HasRepresentation Expression)]
        parseBeforeShuntO =
            try ((:) <$> (Left <$> parseOp')
                     <*> parseBeforeShuntD)
            <|>
            return []

        parseOp' = msum [ do lexeme x; return x | x <- [L_Minus, L_union, L_intersect] ] <?> "operator"
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

        parseRepr = msum [ braces parseReprInner
                         , return NoRepresentation
                         ]

        parseReprInner = do
            pos    <- getPosition
            nm     <- identifierText
            inners <- fromMaybe [] <$> optional (brackets (commaSeparated parseReprInner))
            case textToRepresentation nm inners of
                Nothing -> do
                    setPosition pos
                    fail ("Not a valid representation:" <+> pretty nm)
                Just r  -> return r

        pBool = do
            lexeme L_bool
            -- parse and discard, compatibility with SR
            _ <- optional $ parens $ commaSeparated0 $ parseRange parseExpr
            return DomainBool

        pIntFromExpr = do
            lexeme L_int
            x <- parens parseExpr
            case typeOf x of
                Just TypeInt -> return $ DomainInt [RangeSingle x]
                _ -> return $ DomainIntE x

        pInt = do
            lexeme L_int
            mxs <- optional $ parens $ commaSeparated0 $ parseRange parseExpr
            let xs = fromMaybe [] mxs
            return $ DomainInt xs

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
            r <- parseRepr
            x <- parseSetAttr
            y <- lexeme L_of >> parseDomainWithRepr
            return $ DomainSet r x y

        pMSet = do
            lexeme L_mset
            r <- parseRepr
            x <- parseMSetAttr
            y <- lexeme L_of >> parseDomainWithRepr
            return $ DomainMSet r x y

        pFunction' = do
            lexeme L_function
            r <- parseRepr
            (y,z) <- arrowedPair parseDomainWithRepr
            return $ DomainFunction r def y z

        pFunction = do
            lexeme L_function
            r <- parseRepr
            x <- parseFunctionAttr
            (y,z) <- arrowedPair parseDomainWithRepr
            return $ DomainFunction r x y z

        pSequence = do
            lexeme L_sequence
            r <- parseRepr
            x <- parseSequenceAttr
            y <- lexeme L_of >> parseDomainWithRepr
            return $ DomainSequence r x y

        pRelation = do
            lexeme L_relation
            r  <- parseRepr
            pos <- getPosition
            x  <- parseRelationAttr
            lexeme L_of
            ys <- parens (parseDomainWithRepr `sepBy` lexeme L_Times)
            let RelationAttr _ (BinaryRelationAttrs binAttrs) = x
            when (length ys /= 2 && not (S.null binAttrs)) $ do
                setPosition pos
                fail $ "Only binary relations can have these attributes:" <+>
                            prettyList id "," (S.toList binAttrs)
            return $ DomainRelation r x ys

        pPartition = do
            lexeme L_partition
            r <- parseRepr
            x <- parsePartitionAttr
            lexeme L_from
            y <- parseDomainWithRepr
            return $ DomainPartition r x y

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

metaVarInE :: String -> Expression
metaVarInE = ExpressionMetaVar

parseExpr :: Parser Expression
parseExpr = shuntingYardExpr parseBeforeShuntE1
    where
        -- either parse an expression and continue to parseBeforeShuntO
        -- or return []
        parseBeforeShuntE :: Parser [Either Lexeme Expression]
        parseBeforeShuntE =
            (:) <$> (Right <$> parseAtomicExpr)
                <*> parseBeforeShuntO

        parseBeforeShuntE1 :: Parser [Either Lexeme Expression]
        parseBeforeShuntE1 =
            (:) <$> (Right <$> parseAtomicExpr)
                <*> parseBeforeShuntO

        -- either parse an operator and continue to parseBeforeShuntE
        -- or return []
        parseBeforeShuntO :: Parser [Either Lexeme Expression]
        parseBeforeShuntO =
            (:) <$> (Left <$> parseOp)
                <*> parseBeforeShuntE
            <|>
            return []

parseAtomicExpr :: Parser Expression
parseAtomicExpr = do
    let
        prefixes = do
            fs <- some $ msum parsePrefixes
            return $ foldr1 (.) fs
        postfixes = do
            fs <- some $ msum parsePostfixes
            return $ foldr1 (.) (reverse fs)
        withPrefix  x = try x <|> do f <- prefixes; i <- x; return $ f i
        withPostfix x = do i <- x; mf <- optional postfixes; return $ case mf of Nothing -> i
                                                                                 Just f  -> f i
    withPrefix (withPostfix parseAtomicExprNoPrePost) <?> "expression"

parseAtomicExprNoPrePost :: Parser Expression
parseAtomicExprNoPrePost = msum $ map try $ concat
    [ [parseQuantifiedExpr]
    , parseOthers
    , [metaVarInE <$> parseMetaVariable]
    , [parseAAC]
    , [parseReference]
    , [parseLiteral]
    , [parseDomainAsExpr]
    , [parseWithLocals]
    , [parseComprehension]
    , [parens parseExpr]
    ]

parseComprehension :: Parser Expression
parseComprehension = brackets $ do
    x   <- parseExpr
    lexeme L_Bar
    gens <- commaSeparated (letting <|> try generator <|> condition)
    return (Comprehension x (concat gens))
    where
        generator :: Parser [GeneratorOrCondition]
        generator = do
            pats <- commaSeparated parseAbstractPattern
            msum
                [ do
                    lexeme L_Colon
                    domain <- parseDomain
                    return [Generator (GenDomainNoRepr pat domain) | pat <- pats]
                , do
                    lexeme L_LeftArrow
                    expr <- parseExpr
                    return [Generator (GenInExpr       pat expr)   | pat <- pats]
                ]
        condition :: Parser [GeneratorOrCondition]
        condition = return . Condition <$> parseExpr
        letting :: Parser [GeneratorOrCondition]
        letting = do
            lexeme L_letting
            nm <- parseName
            lexeme L_be
            x  <- parseExpr
            return [ComprehensionLetting nm x]

parseDomainAsExpr :: Parser Expression
parseDomainAsExpr = Domain <$> betweenTicks parseDomain

parsePrefixes :: [Parser (Expression -> Expression)]
parsePrefixes = [parseUnaryMinus, parseUnaryNot]
    where
        parseUnaryMinus = do
            lexeme L_Minus
            return $ \ x -> mkOp "negate" [x]
        parseUnaryNot = do
            lexeme L_ExclamationMark
            return $ \ x -> mkOp "not" [x]

parsePostfixes :: [Parser (Expression -> Expression)]
parsePostfixes = [parseIndexed,parseFactorial,parseFuncApply]
    where
        parseIndexed :: Parser (Expression -> Expression)
        parseIndexed = do
            let
                pIndexer = try pRList <|> (do i <- parseExpr ; return $ \ m -> Op (MkOpIndexing (OpIndexing m i)))
                pRList   = do
                    i <- optional parseExpr
                    dotdot
                    j <- optional parseExpr
                    return $ \ m -> Op (MkOpSlicing (OpSlicing m i j))
            is <- brackets $ commaSeparated pIndexer
            return $ \ x -> foldl (\ m f -> f m ) x is
        parseFactorial :: Parser (Expression -> Expression)
        parseFactorial = do
            lexeme L_ExclamationMark
            return $ \ x -> mkOp "factorial" [x]
        parseFuncApply :: Parser (Expression -> Expression)
        parseFuncApply = parens $ do
            xs <- commaSeparated parseExpr
            let underscore = Reference "_" Nothing
            let ys = [ if underscore == x then Nothing else Just x | x <- xs ]
            return $ \ x -> Op $ MkOpRelationProj $ OpRelationProj x ys

parseAAC :: Parser Expression
parseAAC = do
    let
        isAttr (LIdentifier txt) | Just _ <- Name txt `lookup` allSupportedAttributes = True
        isAttr _ = False
    LIdentifier attr <- satisfyT isAttr
    let n = fromMaybe (bug "parseAAC") (lookup (Name attr) allSupportedAttributes)
    args <- parens $ countSep (n+1) parseExpr comma
    case (n, args) of
        (0, [e  ]) -> return $ Op $ MkOpAttributeAsConstraint $ OpAttributeAsConstraint e
                                        (fromString (textToString attr)) Nothing
        (1, [e,v]) -> return $ Op $ MkOpAttributeAsConstraint $ OpAttributeAsConstraint e
                                        (fromString (textToString attr)) (Just v)
        _ -> fail "parseAAC"

parseOthers :: [Parser Expression]
parseOthers = [ parseFunctional l
              | l <- functionals
              ] ++ [parseTyped, parseTwoBars]
    where

        parseTwoBars :: Parser Expression
        parseTwoBars = do
            x <- between (lexeme L_Bar) (lexeme L_Bar) parseExpr
            return (mkOp "twoBars" [x])

        parseTyped :: Parser Expression
        parseTyped = parens $ do
            x  <- parseExpr
            lexeme L_Colon
            d  <- betweenTicks parseDomain
            ty <- typeOfDomain d
            return (Typed x ty)

        parseFunctional :: Lexeme -> Parser Expression
        parseFunctional l = do
            lexeme l
            xs <- parens $ commaSeparated parseExpr
            return $ case (l,xs) of
                (L_image, [y,z]) -> Op $ MkOpImage $ OpImage y z
                _ -> mkOp (fromString $ show $ lexemeFace l) xs

parseWithLocals :: Parser Expression
parseWithLocals = braces $ do
    i  <- parseExpr
    lexeme L_At
    js <- parseTopLevels
    let decls =
            [ Declaration (FindOrGiven LocalFind nm dom)
            | Declaration (FindOrGiven Find nm dom) <- js ]
    let cons = concat
            [ xs
            | SuchThat xs <- js
            ]
    let locals = if null decls
                    then DefinednessConstraints cons
                    else AuxiliaryVars (decls ++ [SuchThat cons])
    return (WithLocals i locals)

parseName :: Parser Name
parseName = Name <$> identifierText

parseReference :: Parser Expression
parseReference = Reference <$> parseName <*> pure Nothing

parseOp :: Parser Lexeme
parseOp = msum [ do lexeme x; return x | (x,_,_) <- operators ]
    <?> "operator"

parseQuantifiedExpr :: Parser Expression
parseQuantifiedExpr = do
    Name qnName <- parseName
    qnPats      <- commaSeparated parseAbstractPattern
    qnOver      <- msum [ Left  <$> (colon *> parseDomain)
                        , Right <$> do
                            lexeme L_in
                            over <- parseExpr
                            return (\ pat -> GenInExpr pat over )
                        , Right <$> do
                            lexeme L_subsetEq
                            over <- parseExpr
                            return (\ pat -> GenInExpr pat (Op $ MkOpPowerSet $ OpPowerSet over) )
                        ]
    qnGuard     <- optional (comma *> parseExpr)
    qnBody      <- dot *> parseExpr <?> "body of a quantified expression"

    let qnMap pat = case qnOver of
            Left dom -> GenDomainNoRepr pat dom
            Right op -> op pat

    return $ mkOp (translateQnName qnName)
           $ return
           $ Comprehension qnBody
           $ [ Generator (qnMap pat) | pat    <- qnPats    ] ++
             [ Condition g           | Just g <- [qnGuard] ]


parseAbstractPattern :: Parser AbstractPattern
parseAbstractPattern = label "pattern" $ msum $ map try
    [ AbstractPatternMetaVar <$> parseMetaVariable
    , Single <$> parseName
    , do
        void $ optional $ lexeme L_tuple
        xs <- parens $ commaSeparated parseAbstractPattern
        return (AbsPatTuple xs)
    , do
        xs <- brackets $ commaSeparated parseAbstractPattern
        return (AbsPatMatrix xs)
    , do
        xs <- braces $ commaSeparated parseAbstractPattern
        return (AbsPatSet xs)
    ]

parseLiteral :: Parser Expression
parseLiteral = label "value" $ msum
    [ Constant <$> pBool
    , Constant <$> pInt
    , AbstractLiteral <$> pMatrix
    , AbstractLiteral <$> pTupleWith
    , AbstractLiteral <$> pTupleWithout
    , AbstractLiteral <$> pRecord
    , AbstractLiteral <$> pVariant
    , AbstractLiteral <$> pSet
    , AbstractLiteral <$> pMSet
    , AbstractLiteral <$> pFunction
    , AbstractLiteral <$> pSequence
    , AbstractLiteral <$> pRelation
    , AbstractLiteral <$> pPartition
    ]
    where
        pBool = do
            x <- False <$ lexeme L_false
                 <|>
                 True  <$ lexeme L_true
            return (ConstantBool x)

        pInt = ConstantInt . fromInteger <$> integer

        pMatrix = do
            lexeme L_OpenBracket
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

        pTupleWith = do
            lexeme L_tuple
            xs <- parens $ commaSeparated0 parseExpr
            return (AbsLitTuple xs)

        pTupleWithout = do
            xs <- parens $ countSepAtLeast 2 parseExpr comma
            return (AbsLitTuple xs)

        pRecord = do
            lexeme L_record
            let one = do n <- parseName
                         lexeme L_Eq
                         x <- parseExpr
                         return (n,x)
            xs <- braces $ commaSeparated0 one
            return $ AbsLitRecord xs

        pVariant = do
            lexeme L_variant
            let one = do n <- parseName
                         lexeme L_Eq
                         x <- parseExpr
                         return (n,x)
            (n,x) <- braces one
            return $ AbsLitVariant Nothing n x

        pSet = do
            xs <- braces (commaSeparated0 parseExpr)
            return (AbsLitSet xs)

        pMSet = do
            lexeme L_mset
            xs <- parens (commaSeparated0 parseExpr)
            return (AbsLitMSet xs)

        pFunction = do
            lexeme L_function
            xs <- parens (commaSeparated0 inner)
            return (AbsLitFunction xs)
            where
                inner = arrowedPair parseExpr

        pSequence = do
            lexeme L_sequence
            xs <- parens (commaSeparated0 parseExpr)
            return (AbsLitSequence xs)

        pRelation = do
            lexeme L_relation
            xs <- parens (commaSeparated0 (try pTupleWith <|> pTupleWithout))
            return (AbsLitRelation [is | AbsLitTuple is <- xs])
            -- return [xMake| value.relation.values := xs |]

        pPartition = do
            lexeme L_partition
            xs <- parens (commaSeparated0 inner)
            return (AbsLitPartition xs)
            where
                inner = braces (commaSeparated0 parseExpr)

shuntingYardExpr :: Parser [Either Lexeme Expression] -> Parser Expression
shuntingYardExpr p = do
    let mergeOp = mkBinOp . lexemeText
    beforeShunt <- fixNegate <$> p
    checkAlternating beforeShunt
    shunt mergeOp beforeShunt

shuntingYardDomain :: (Eq a, Eq r) => Parser [Either Lexeme (Domain r a)] -> Parser (Domain r a)
shuntingYardDomain p = do
    let mergeOp op before after = DomainOp (Name (lexemeText op)) [before,after]
    beforeShunt <- p
    checkAlternating beforeShunt
    shunt mergeOp beforeShunt

fixNegate :: [Either Lexeme Expression] -> [Either Lexeme Expression]
fixNegate ( Right a
          : Right (Op (MkOpNegate (OpNegate b)))
          : cs
          ) = fixNegate $ Right a : Left L_Minus : Right b : cs
fixNegate (a:bs) = a : fixNegate bs
fixNegate [] = []

-- "Left"s are operators, "Right"s are expressions
checkAlternating :: MonadFail m => [Either a b] -> m ()
checkAlternating (Right{} : Right{} : _) = fail "Malformed expression."
checkAlternating (Left{}  : Left{}  : _) = fail "Malformed expression."
checkAlternating (_:xs) = checkAlternating xs
checkAlternating [] = return ()

shunt :: Eq a => (Lexeme -> a -> a -> a) -> [Either Lexeme a] -> Parser a
shunt mergeOp xs = do
    result <- findPivotOp xs
    case result of
        Left x -> return x
        Right (before, op, after) -> do
            b <- shunt mergeOp before
            a <- shunt mergeOp after
            return (mergeOp op b a)

findPivotOp :: Eq a => [Either Lexeme a] -> Parser (Either a ([Either Lexeme a], Lexeme, [Either Lexeme a]))
findPivotOp [Right x] = return $ Left x
findPivotOp xs = do
    let
        pivotPrec :: Int
        pivotFixity :: Fixity
        (pivotPrec,pivotFixity) = minimumBy (comparing fst)
                        [ (p, f) | Left l <- xs, (l',f,p) <- operators, l == l' ]

        chck op = case [ p | (l,_,p) <- operators, l == op ] of
                    [p] -> p == pivotPrec
                    _ -> False

        findFirst :: Eq a => [Either Lexeme a] -> Parser ([Either Lexeme a], Lexeme, [Either Lexeme a])
        findFirst [] = fail "findPivotOp.findFirst"
        findFirst (Left i:is) | chck i = return ([], i, is)
        findFirst (i:is) = do
            (before, op, after) <- findFirst is
            return (i:before, op, after)

        findLast :: Eq a => [Either Lexeme a] -> Parser ([Either Lexeme a], Lexeme, [Either Lexeme a])
        findLast is = do
            (before, op, after) <- findFirst (reverse is)
            return (reverse after, op, reverse before)

        findOnly :: Eq a => [Either Lexeme a] -> Parser ([Either Lexeme a], Lexeme, [Either Lexeme a])
        findOnly is = do
            f <- findFirst is
            l <- findLast  is
            if f == l
                then return f
                else fail "Ambiguous use of non-associative operator."

    let
        finder = case pivotFixity of
                    FLeft  -> findLast
                    FNone  -> findOnly
                    FRight -> findFirst
    Right <$> finder xs




data ParserState = ParserState { enumDomains :: [Name] }
type Parser a = StateT ParserState (ParsecT [LexemePos] Identity) a

runLexerAndParser :: MonadFail m => Parser a -> String -> T.Text -> m a
runLexerAndParser p file inp = do
    ls <- runLexer inp
    case runParser p file ls of
        Left (msg, line, col) ->
            let theLine = T.lines inp |> drop (line-1) |> take 1
            in  fail $ vcat
                    [ msg
                    , vcat (map pretty theLine)
                    , pretty $ replicate (col-1) ' ' ++ "^"
                    ]
        Right x -> return x

runParser :: Parser a -> String -> [LexemePos] -> Either (Doc, Int, Int) a
runParser p file ls = either modifyErr Right (P.runParser (evalStateT p (ParserState [])) file ls)
    where
        modifyErr :: ParseError -> Either (Doc, Int, Int) a
        modifyErr e = Left $
            let pos  = errorPos e
            in  ( if file `isPrefixOf` show e
                    then                       pretty (show e)
                    else pretty file <> ":" <> pretty (show e)
            -- in  ( pretty file <> ":" <> pretty (show e)
                , sourceLine   pos
                , sourceColumn pos
                )

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
        testTok (LexemePos tok _ _) = if predicate tok then Right tok else Left [Unexpected (showToken tok)]
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
