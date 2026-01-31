module Conjure.Language.ParserCPrime
  ( parseModel
  )
where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type ( IntTag(..), Type(..) )
import Conjure.Language.Pretty ( pretty, vcat, (<+>) )

import qualified Data.Text as T
import Data.Char ( isAlpha, isAlphaNum, isDigit )


-- | Fast parser for Essence Prime solution lines.
-- Supports only: bools, ints, and matrices (nested).
parseModel :: Text -> Either Doc Model
parseModel input = do
  lettings <- collectLettings (T.strip input) []
  let stmts = [ Declaration (Letting nm (Constant c)) | (nm, c) <- reverse lettings ]
  return (languageEprime def) { mStatements = stmts }


collectLettings :: Text -> [(Name, Constant)] -> Either Doc [(Name, Constant)]
collectLettings txt acc =
  case findLetting txt of
    Nothing -> Right acc
    Just rest -> do
      (nm, c, rest') <- parseLetting rest
      collectLettings rest' ((nm, c) : acc)


findLetting :: Text -> Maybe Text
findLetting txt =
  case T.breakOn "letting" txt of
    (_, rest) | T.null rest -> Nothing
    (prefix, rest) ->
      let beforeOk = T.null prefix || isSpace (T.last prefix)
          after = T.drop (T.length ("letting" :: Text)) rest
          afterOk = case T.uncons after of
            Just (c, _) -> isSpace c
            Nothing -> False
      in if beforeOk && afterOk
            then Just (T.dropWhile isSpace after)
            else findLetting (T.drop 1 rest)


parseLetting :: Text -> Either Doc (Name, Constant, Text)
parseLetting txt = do
  (nmTxt, rest1) <- parseIdentifier txt
  rest2 <- parseKeyword "be" rest1
  (c, rest3) <- parseConstant rest2
  return (Name nmTxt, c, rest3)


parseKeyword :: Text -> Text -> Either Doc Text
parseKeyword kw txt = do
  (tok, rest) <- parseIdentifier txt
  if tok == kw then Right rest else parseError "Expected keyword" txt


parseIdentifier :: Text -> Either Doc (Text, Text)
parseIdentifier txt =
  let t = skipSpaces txt
  in case T.uncons t of
      Just (c, rest) | isIdentStart c ->
        let (tok, rest') = T.span isIdentChar rest
        in Right (T.cons c tok, rest')
      _ -> parseError "Expected identifier" txt


parseConstant :: Text -> Either Doc (Constant, Text)
parseConstant txt =
  let t = skipSpaces txt
  in case T.uncons t of
      Nothing -> parseError "Unexpected end of input while parsing constant" txt
      Just ('[', _) -> parseMatrix t
      Just ('(', _) -> parseAnnotatedEmptyMatrix t
      Just (c, _) | c == '-' || isDigit c -> parseInt t
      _ -> do
        (tok, rest) <- parseIdentifier t
        case tok of
          "true" -> Right (ConstantBool True, rest)
          "false" -> Right (ConstantBool False, rest)
          _ -> parseError "Expected boolean constant" t


parseInt :: Text -> Either Doc (Constant, Text)
parseInt txt =
  let t = skipSpaces txt
      (sign, t1) =
        case T.uncons t of
          Just ('-', rest1) -> (-1, rest1)
          _ -> (1, t)
      (digits, rest) = T.span isDigit t1
  in if T.null digits
        then parseError "Expected integer constant" txt
        else
          let n = sign * parseInteger digits
          in Right (ConstantInt TagInt n, rest)


parseMatrix :: Text -> Either Doc (Constant, Text)
parseMatrix txt = do
  t1 <- expectChar '[' txt
  let t2 = skipSpaces t1
  case T.uncons t2 of
    Just (']', rest) ->
      let dom = DomainInt TagInt []
      in Right (ConstantAbstract (AbsLitMatrix dom []), rest)
    _ -> do
      (firstVal, rest1) <- parseConstant t2
      (vals, mDom, rest2) <- parseMatrixRest [firstVal] rest1
      let dom =
            case mDom of
              Just d -> d
              Nothing ->
                if null vals
                  then DomainInt TagInt []
                  else DomainInt TagInt [RangeBounded (ConstantInt TagInt 1) (ConstantInt TagInt (genericLength vals))]
      return (ConstantAbstract (AbsLitMatrix dom vals), rest2)


parseMatrixRest :: [Constant] -> Text -> Either Doc ([Constant], Maybe (Domain () Constant), Text)
parseMatrixRest acc txt =
  let t = skipSpaces txt
  in case T.uncons t of
      Just (',', rest) -> do
        (val, rest') <- parseConstant rest
        parseMatrixRest (val : acc) rest'
      Just (';', rest) -> do
        (dom, rest') <- parseDomain rest
        rest'' <- expectChar ']' rest'
        Right (reverse acc, Just dom, rest'')
      Just (']', rest) -> Right (reverse acc, Nothing, rest)
      _ -> parseError "Expected ',', ';' or ']'" txt


parseDomain :: Text -> Either Doc (Domain () Constant, Text)
parseDomain txt = do
  (tok, rest) <- parseIdentifier txt
  case tok of
    "bool" -> Right (DomainBool, rest)
    "int" -> parseIntDomain rest
    _ -> parseError "Expected domain (bool or int(...))" txt


parseIntDomain :: Text -> Either Doc (Domain () Constant, Text)
parseIntDomain txt = do
  t1 <- expectChar '(' txt
  let t1' = skipSpaces t1
  case T.uncons t1' of
    Just (')', rest) -> return (DomainInt TagInt [], rest)
    _ -> do
      (ranges, rest) <- parseRanges [] t1
      rest' <- expectChar ')' rest
      return (DomainInt TagInt (reverse ranges), rest')


parseRanges :: [Range Constant] -> Text -> Either Doc ([Range Constant], Text)
parseRanges acc txt = do
  (r, rest) <- parseRange txt
  let t = skipSpaces rest
  case T.uncons t of
    Just (',', rest') -> parseRanges (r : acc) rest'
    _ -> Right (r : acc, rest)


parseRange :: Text -> Either Doc (Range Constant, Text)
parseRange txt = do
  (c1, rest1) <- parseInt txt
  let t = skipSpaces rest1
  case T.stripPrefix ".." t of
    Just rest2 -> do
      (c2, rest3) <- parseInt rest2
      return (RangeBounded c1 c2, rest3)
    Nothing -> return (RangeSingle c1, rest1)


expectChar :: Char -> Text -> Either Doc Text
expectChar c txt =
  let t = skipSpaces txt
  in case T.uncons t of
      Just (d, rest) | d == c -> Right rest
      _ -> parseError (T.concat ["Expected '", T.singleton c, "'"]) txt


skipSpaces :: Text -> Text
skipSpaces = T.dropWhile isSpace


isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'


isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''


parseError :: Text -> Text -> Either Doc a
parseError msg txt =
  let snippet = T.take 80 (skipSpaces txt)
  in Left $ vcat
      [ "Fast solution parser error:" <+> pretty msg
      , "Near:" <+> pretty snippet
      ]


parseAnnotatedEmptyMatrix :: Text -> Either Doc (Constant, Text)
parseAnnotatedEmptyMatrix txt = do
  t1 <- expectChar '(' txt
  t2 <- expectChar '[' t1
  t3 <- expectChar ']' t2
  t4 <- expectChar ':' t3
  t5 <- expectChar '`' t4
  ((indexDomains, innerDomain), t6) <- parseMatrixTypeInBackticks t5
  t7 <- expectChar '`' t6
  t8 <- expectChar ')' t7
  let indexDomain =
        case indexDomains of
          d : _ -> d
          [] -> DomainInt TagInt []
  let ty = typeFromDomains indexDomains innerDomain
  let c = TypedConstant (ConstantAbstract (AbsLitMatrix indexDomain [])) ty
  return (c, t8)


parseMatrixTypeInBackticks :: Text -> Either Doc (([Domain () Constant], Domain () Constant), Text)
parseMatrixTypeInBackticks txt = do
  t1 <- parseWord "matrix" txt
  t2 <- parseWord "indexed" t1
  t3 <- parseWord "by" t2
  t4 <- expectChar '[' t3
  (indexDomains, t5) <- parseDomainList t4
  t6 <- expectChar ']' t5
  t7 <- parseWord "of" t6
  (innerDomain, t8) <- parseDomain t7
  return ((indexDomains, innerDomain), t8)


parseDomainList :: Text -> Either Doc ([Domain () Constant], Text)
parseDomainList txt = do
  (d, rest) <- parseDomain txt
  let t = skipSpaces rest
  case T.uncons t of
    Just (',', rest') -> do
      (ds, rest'') <- parseDomainList rest'
      return (d : ds, rest'')
    _ -> return ([d], rest)


parseWord :: Text -> Text -> Either Doc Text
parseWord w txt = do
  (tok, rest) <- parseIdentifier txt
  if tok == w
    then Right rest
    else parseError (T.concat ["Expected '", w, "'"]) txt


typeFromDomains :: [Domain () Constant] -> Domain () Constant -> Type
typeFromDomains indices inner =
  foldr TypeMatrix (typeFromDomain inner) (map typeFromDomain indices)


typeFromDomain :: Domain () Constant -> Type
typeFromDomain DomainBool = TypeBool
typeFromDomain (DomainInt t _) = TypeInt t
typeFromDomain _ = TypeAny


parseInteger :: Text -> Integer
parseInteger =
  T.foldl' (\acc d -> acc * 10 + toInteger (fromEnum d - fromEnum '0')) 0
