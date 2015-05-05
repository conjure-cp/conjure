{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Definition
    ( forgetRepr, rangesInts
    , languageEprime
    , initInfo
    , allContextsExceptReferences

    , quantifiedVar, auxiliaryVar
    , lambdaToFunction

    , e2c

    , Model(..), LanguageVersion(..)
    , ModelInfo(..), Decision(..)
    , Statement(..), SearchOrder(..), Objective(..)
    , Declaration(..), FindOrGiven(..)
    , Strategy(..)
    , QuestionAnswered(..), viewAuto, parseStrategy

    , Name(..)
    , Expression(..), ReferenceTo(..)
    , Constant(..)
    , AbstractLiteral(..)
    , AbstractPattern(..)
    , GeneratorOrCondition(..), Generator(..), generatorPat

    , ExpressionLike(..), ReferenceContainer(..)

    , extractLettings
    , tupleLitIfNeeded
    , patternToExpr

    , module Conjure.Language.NameGen

    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Pretty
import Conjure.Language.AdHoc

import Conjure.Language.Name
import Conjure.Language.NameGen ( NameGen(..), NameGenState, runNameGen )
import Conjure.Language.Constant
import Conjure.Language.AbstractLiteral
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.Expression.Op

import Conjure.Language.TypeOf
import Conjure.Language.RepresentationOf


-- aeson
import Data.Aeson ( (.=), (.:) )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector

-- uniplate
import Data.Generics.Uniplate.Zipper ( Zipper, down, right, hole )

-- containers
import Data.IntSet ( IntSet )
import qualified Data.IntSet as I


------------------------------------------------------------------------------------------------------------------------
-- Model ---------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Model = Model
    { mLanguage :: LanguageVersion
    , mStatements :: [Statement]
    , mInfo :: ModelInfo
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

type ModelZipper = Zipper Model Expression

instance Serialize Model
instance Hashable  Model
instance ToJSON    Model where toJSON = genericToJSON jsonOptions
instance FromJSON  Model where parseJSON = genericParseJSON jsonOptions

instance Default Model where
    def = Model def [] def

instance Pretty Model where
    pretty (Model lang stmts info) = vcat $ concat
        [ [pretty lang]
        , [""]
        , map pretty stmts
        , [""]
        , [pretty info | info /= def]
        ]

instance VarSymBreakingDescription Model where
    varSymBreakingDescription m = JSON.Object $ M.fromList
        [ ("type", JSON.String "Model")
        , ("symmetricChildren", JSON.Bool True)
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription $ mStatements m)
        ]


languageEprime :: Model -> Model
languageEprime m = m { mLanguage = LanguageVersion "ESSENCE'" [1,0] }

allContextsExceptReferences :: ModelZipper -> [ModelZipper]
allContextsExceptReferences z0 = concatMap subtreeOf (allSiblings z0)
    where
        -- the input has to be the left most
        allSiblings :: ModelZipper -> [ModelZipper]
        allSiblings z = z : maybe [] allSiblings (right z)

        subtreeOf :: ModelZipper -> [ModelZipper]
        subtreeOf z = z : case hole z of
            Reference{} -> []                                       -- don't go through a Reference
            _           -> maybe [] allContextsExceptReferences (down z)

------------------------------------------------------------------------------------------------------------------------
-- LanguageVersion -----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data LanguageVersion = LanguageVersion Name [Int]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize LanguageVersion
instance Hashable  LanguageVersion

instance ToJSON LanguageVersion where
    toJSON (LanguageVersion t is) =
        JSON.object [ "language" .= toJSON t
                    , "version"  .= toJSON is
                    ]

instance FromJSON LanguageVersion where
    parseJSON (JSON.Object x) =
        LanguageVersion <$> x .: "language"
                        <*> x .: "version"
    parseJSON x = bug $ "Error while parsing JSON:" <++> pretty (show x)

instance Default LanguageVersion where
    def = LanguageVersion "Essence" [1,3]

instance Pretty LanguageVersion where
    pretty (LanguageVersion language version) =
        "language" <+> pretty language
                   <+> hcat (intersperse "." (map pretty version))


------------------------------------------------------------------------------------------------------------------------
-- Statement -----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Statement
    = Declaration Declaration
    | SearchOrder [SearchOrder]
    | Where [Expression]
    | Objective Objective Expression
    | SuchThat [Expression]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Statement
instance Hashable  Statement
instance ToJSON    Statement where toJSON = genericToJSON jsonOptions
instance FromJSON  Statement where parseJSON = genericParseJSON jsonOptions

instance Pretty Statement where
    pretty (Declaration x) = pretty x
    pretty (SearchOrder nms) = "branching on" <++> prettyList prBrackets "," nms
    pretty (Where xs) = "where" <++> vcat (punctuate "," $ map pretty xs)
    pretty (Objective obj x) = pretty obj <++> pretty x
    pretty (SuchThat xs) = "such that" <++> vcat (punctuate "," $ map pretty xs)

instance VarSymBreakingDescription Statement where
    varSymBreakingDescription (Declaration x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "Declaration")
        , ("children", varSymBreakingDescription x)
        ]
    varSymBreakingDescription SearchOrder{} = JSON.Null
    varSymBreakingDescription (Where xs) = JSON.Object $ M.fromList
        [ ("type", JSON.String "Where")
        , ("symmetricChildren", JSON.Bool True)
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        ]
    varSymBreakingDescription (Objective obj x) = JSON.Object $ M.fromList
        [ ("type", JSON.String $ "Objective-" `mappend` stringToText (show obj))
        , ("children", varSymBreakingDescription x)
        ]
    varSymBreakingDescription (SuchThat xs) = JSON.Object $ M.fromList
        [ ("type", JSON.String "SuchThat")
        , ("symmetricChildren", JSON.Bool True)
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        ]


------------------------------------------------------------------------------------------------------------------------
-- SearchOrder ---------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data SearchOrder = BranchingOn Name | Cut Expression
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize SearchOrder
instance Hashable  SearchOrder
instance ToJSON    SearchOrder where toJSON = genericToJSON jsonOptions
instance FromJSON  SearchOrder where parseJSON = genericParseJSON jsonOptions

instance Pretty SearchOrder where
    pretty (BranchingOn x) = pretty x
    pretty (Cut x) = pretty x


------------------------------------------------------------------------------------------------------------------------
-- Objective -----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Objective = Minimising | Maximising
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Objective
instance Hashable  Objective
instance ToJSON    Objective where toJSON = genericToJSON jsonOptions
instance FromJSON  Objective where parseJSON = genericParseJSON jsonOptions

instance Pretty Objective where
    pretty Minimising = "minimising"
    pretty Maximising = "maximising"


------------------------------------------------------------------------------------------------------------------------
-- Declaration ---------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Declaration
    = FindOrGiven FindOrGiven Name (Domain () Expression)
    | Letting Name Expression
    | GivenDomainDefnEnum Name
    | LettingDomainDefnEnum Name [Name]
    | LettingDomainDefnUnnamed Name Expression
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Declaration
instance Hashable  Declaration
instance ToJSON    Declaration where toJSON = genericToJSON jsonOptions
instance FromJSON  Declaration where parseJSON = genericParseJSON jsonOptions

instance Pretty Declaration where
    pretty (FindOrGiven forg nm d) = hang (pretty forg <+> pretty nm <>  ":" ) 8 (pretty d)
    pretty (Letting nm (Domain x)) = hang ("letting" <+> pretty nm <+> "be domain") 8 (pretty x)
    pretty (Letting nm x) = hang ("letting" <+> pretty nm <+> "be") 8 (pretty x)
    pretty (GivenDomainDefnEnum name) =
        hang ("given"   <+> pretty name) 8 "new type enum"
    pretty (LettingDomainDefnEnum name values) =
        hang ("letting" <+> pretty name <+> "be new type enum") 8
             (prettyList prBraces "," values)
    pretty (LettingDomainDefnUnnamed name size) =
        hang ("letting" <+> pretty name <+> "be new type of size") 8 (pretty size)

instance VarSymBreakingDescription Declaration where
    varSymBreakingDescription (FindOrGiven forg name domain) = JSON.Object $ M.fromList
        [ ("type", JSON.String "FindOrGiven")
        , ("forg", toJSON forg)
        , ("name", toJSON name)
        , ("domain", toJSON domain)
        ]
    varSymBreakingDescription (Letting name x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "Letting")
        , ("name", toJSON name)
        , ("value", toJSON x)
        ]
    varSymBreakingDescription (GivenDomainDefnEnum name) = JSON.Object $ M.fromList
        [ ("type", JSON.String "GivenDomainDefnEnum")
        , ("name", toJSON name)
        ]
    varSymBreakingDescription (LettingDomainDefnEnum name xs) = JSON.Object $ M.fromList
        [ ("type", JSON.String "GivenDomainDefnEnum")
        , ("name", toJSON name)
        , ("values", JSON.Array $ V.fromList $ map toJSON xs)
        ]
    varSymBreakingDescription (LettingDomainDefnUnnamed name x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "LettingDomainDefnUnnamed")
        , ("name", toJSON name)
        , ("value", toJSON x)
        ]


data FindOrGiven = Find | Given | Quantified
        | CutFind           -- references to variables used in the definition of a cut
        | LocalFind         -- references to variables used inside WithLocals. i.e. auxiliaries.
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize FindOrGiven
instance Hashable  FindOrGiven
instance ToJSON    FindOrGiven where toJSON = genericToJSON jsonOptions
instance FromJSON  FindOrGiven where parseJSON = genericParseJSON jsonOptions

instance Pretty FindOrGiven where
    pretty Find = "find"
    pretty Given = "given"
    pretty Quantified = "quantified"
    pretty CutFind = "find"
    pretty LocalFind = "find"


------------------------------------------------------------------------------------------------------------------------
-- ModelInfo -----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data ModelInfo = ModelInfo
    { miGivens :: [Name]
    , miFinds :: [Name]
    , miEnumGivens :: [Name]
    , miEnumLettings :: [Declaration]
    , miUnnameds :: [(Name, Expression)]
    , miOriginalDomains :: [(Name, Domain () Expression)]
    , miRepresentations :: [(Name, Domain HasRepresentation Expression)]
    , miRepresentationsTree :: [(Name, [Tree (Maybe HasRepresentation)])]
    , miTrailCompact :: [(Strategy, Int, Int)]
    , miTrailVerbose :: [Decision]
    , miQuestionAnswered :: [QuestionAnswered]
    , miNameGenState :: [(Text, Int)]
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

modelInfoJSONOptions :: JSON.Options
modelInfoJSONOptions = jsonOptions { JSON.fieldLabelModifier = onHead toLower . drop 2 }
    where onHead f (x:xs) = f x : xs
          onHead _ [] = []

instance Serialize ModelInfo
instance Hashable  ModelInfo
instance ToJSON    ModelInfo where toJSON = genericToJSON modelInfoJSONOptions
instance FromJSON  ModelInfo where parseJSON = genericParseJSON modelInfoJSONOptions

instance Default ModelInfo where
    def = ModelInfo def def def def def def def def def def def def

instance Pretty ModelInfo where
    pretty = commentLines . pretty . toJSON
        where
            commentLines :: Doc -> Doc
            commentLines
                = vcat                          -- Doc
                . map ("$ " `mappend`)          -- comment each line
                . ("Conjure's" :)               -- add the heading
                . map pretty                    -- [Doc]
                . lines                         -- [String]
                . renderNormal                  -- to String

initInfo :: Model -> Model
initInfo model = model { mInfo = info }
    where
        info = (mInfo model)
            { miGivens = [ nm | Declaration (FindOrGiven Given nm _) <- mStatements model ]
            , miFinds  = [ nm | Declaration (FindOrGiven Find  nm _) <- mStatements model ]
            , miOriginalDomains =
                [ (nm, dom)
                | Declaration (FindOrGiven _ nm dom) <- mStatements model
                ]
            , miEnumGivens   = [ nm     | Declaration (GivenDomainDefnEnum nm)         <- mStatements model ]
            , miEnumLettings = [ d      | Declaration d@LettingDomainDefnEnum{}        <- mStatements model ]
            , miUnnameds     = [ (nm,s) | Declaration (LettingDomainDefnUnnamed nm s)  <- mStatements model ]
            }


data Strategy
    = PickFirst
    | PickAll
    | Interactive
    | AtRandom
    | Compact
    | FollowLog
    | Auto Strategy
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Serialize Strategy
instance Hashable  Strategy
instance ToJSON    Strategy where toJSON = genericToJSON jsonOptions
instance FromJSON  Strategy where parseJSON = genericParseJSON jsonOptions

instance Default Strategy where def = Auto Interactive

viewAuto :: Strategy -> (Strategy, Bool)
viewAuto (Auto s) = second (const True) (viewAuto s)
viewAuto s = (s, False)

parseStrategy :: String -> Maybe Strategy
parseStrategy "f" = return PickFirst
parseStrategy "x" = return PickAll
parseStrategy "i" = return Interactive
parseStrategy "r" = return AtRandom
parseStrategy ['a',s] = Auto <$> parseStrategy (return s)
parseStrategy "c" = return Compact
parseStrategy "l" = return FollowLog
parseStrategy _ = Nothing


data QuestionAnswered =
      AnsweredRepr
      { qHole_       :: Int
      , qAscendants_ :: IntSet
      , aDom_        :: Domain HasRepresentation Expression
      , aRuleName_   :: String  -- Doc has no Data or Ord instance
      }
    | AnsweredReprStored
      { qHole_       :: Int
      , qAscendants_ :: IntSet
      , aDomStored_  :: String
      , aRuleName_   :: String  -- Doc has no Data or Ord instance
      }
    | AnsweredRule
      { qHole_       :: Int
      , qAscendants_ :: IntSet
      , aRuleName_   :: String  -- Doc has no Data or Ord instance
      } deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToJSON    QuestionAnswered where toJSON    = genericToJSON jsonOptions
instance FromJSON  QuestionAnswered where parseJSON = genericParseJSON jsonOptions
instance Serialize QuestionAnswered
instance Hashable  QuestionAnswered
instance Hashable  IntSet where
    hashWithSalt s i = hashWithSalt s (I.toList i)


------------------------------------------------------------------------------------------------------------------------
-- Decision ------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Decision = Decision
    { dDescription :: [Text]
    , dNumOptions :: Int
    , dDecision :: Int
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

decisionJSONOptions :: JSON.Options
decisionJSONOptions = jsonOptions { JSON.fieldLabelModifier = map toLower . drop 1 }

instance Serialize Decision
instance Hashable  Decision
instance ToJSON    Decision where toJSON = genericToJSON decisionJSONOptions
instance FromJSON  Decision where parseJSON = genericParseJSON decisionJSONOptions


------------------------------------------------------------------------------------------------------------------------
-- Expression ----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Expression
    = Constant Constant
    | AbstractLiteral (AbstractLiteral Expression)
    | Domain (Domain () Expression)
    | Reference Name (Maybe ReferenceTo)
    | WithLocals Expression (Either AuxiliaryVars DefinednessConstraints)
    | Comprehension Expression [GeneratorOrCondition]
    | Typed Expression Type
    | Op (Op Expression)
    | ExpressionMetaVar String
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Expression
instance Hashable  Expression
instance ToJSON    Expression where toJSON = genericToJSON jsonOptions
instance FromJSON  Expression where parseJSON = genericParseJSON jsonOptions

type AuxiliaryVars = [Statement]
type DefinednessConstraints = [Expression]

viewIndexed :: Expression -> (Expression, [Doc])
viewIndexed (Op (MkOpIndexing (OpIndexing m i  ))) =
    let this = pretty i
    in  second (++ [this]) (viewIndexed m)
viewIndexed (Op (MkOpSlicing  (OpSlicing  m a b))) =
    let this = maybe prEmpty pretty a <> ".." <> maybe prEmpty pretty b
    in  second (++ [this]) (viewIndexed m)
viewIndexed m = (m, [])

instance Pretty Expression where

    prettyPrec _ (viewIndexed -> (m,is@(_:_))) = pretty m <> prettyList prBrackets "," is

    -- mostly for debugging: print what a reference is pointing at
    -- prettyPrec _ (Reference x Nothing) = pretty x <> "#`NOTHING`"
    -- prettyPrec _ (Reference x (Just (DeclHasRepr _ _ dom))) = pretty x <> "#`" <> pretty dom <> "`"
    -- prettyPrec _ (Reference x (Just r)) = pretty x <> "#`" <> pretty r <> "`"

    prettyPrec _ (Constant x) = pretty x
    prettyPrec _ (AbstractLiteral x) = pretty x
    prettyPrec _ (Domain x) = "`" <> pretty x <> "`"
    prettyPrec _ (Reference x _) = pretty x
    prettyPrec _ (WithLocals x (Left  locals)) = vcat [ "{" <+> pretty x
                                                      , "@" <+> vcat (map pretty locals)
                                                      , "}"
                                                      ]
    prettyPrec _ (WithLocals x (Right locals)) = vcat [ "{" <+> pretty x
                                                      , "@" <+> pretty (SuchThat locals)
                                                      , "}"
                                                      ]
    prettyPrec _ (Comprehension x is) = prBrackets $ pretty x <++> "|" <+> prettyList id "," is
    prettyPrec _ (Typed x ty) = prParens $ pretty x <+> ":" <+> "`" <> pretty ty <> "`"
    prettyPrec prec (Op op) = prettyPrec prec op
    prettyPrec _ (ExpressionMetaVar x) = "&" <> pretty x

instance VarSymBreakingDescription Expression where
    varSymBreakingDescription (Constant x) = toJSON x
    varSymBreakingDescription (AbstractLiteral x) = varSymBreakingDescription x
    varSymBreakingDescription (Domain domain) = varSymBreakingDescription domain
    varSymBreakingDescription (Reference name _) = JSON.Object $ M.singleton "Reference" (toJSON name)
    varSymBreakingDescription (WithLocals h (Left locs)) = JSON.Object $ M.fromList
        [ ("type", JSON.String "WithLocals")
        , ("head", varSymBreakingDescription h)
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription locs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (WithLocals h (Right locs)) = JSON.Object $ M.fromList
        [ ("type", JSON.String "WithLocals")
        , ("head", varSymBreakingDescription h)
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription locs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (Comprehension h gocs) = JSON.Object $ M.fromList
        [ ("type", JSON.String "Comprehension")
        , ("head", varSymBreakingDescription h)
        , ("gocs", JSON.Array $ V.fromList $ map varSymBreakingDescription gocs)
        ]
    varSymBreakingDescription (Typed x _) = varSymBreakingDescription x
    varSymBreakingDescription (Op op) = varSymBreakingDescription op
    varSymBreakingDescription (ExpressionMetaVar s) = JSON.Object $ M.fromList
        [ ("type", JSON.String "ExpressionMetaVar")
        , ("name", JSON.String (stringToText s))
        ]

instance TypeOf Expression where
    typeOf (Constant x) = typeOf x
    typeOf (AbstractLiteral x) = typeOf x
    typeOf (Domain x)   = typeOf x
    typeOf (Reference nm Nothing) = fail ("Type error, identifier not bound:" <+> pretty nm)
    typeOf (Reference nm (Just refTo)) =
        case refTo of
            Alias x -> typeOf x
            InComprehension gen ->
                let
                    lu pat ty = maybe
                        (bug $ vcat ["Type error, InComprehension:", pretty nm, pretty pat, pretty ty])
                        return
                        (lu' pat ty)
                    lu' (Single nm') ty | nm == nm' = Just ty
                    lu' (AbsPatTuple  pats) (TypeTuple    tys) = zipWith lu' pats tys   |> catMaybes |> listToMaybe
                    lu' (AbsPatMatrix pats) (TypeMatrix _ ty ) = [lu' p ty | p <- pats] |> catMaybes |> listToMaybe
                    lu' (AbsPatSet    pats) (TypeSet      ty ) = [lu' p ty | p <- pats] |> catMaybes |> listToMaybe
                    lu' _ _ = Nothing
                in
                    case gen of
                        GenDomainNoRepr  pat domain -> typeOf domain                 >>= lu pat
                        GenDomainHasRepr pat domain -> typeOf domain                 >>= lu (Single pat)
                        GenInExpr        pat expr   -> typeOf expr   >>= innerTypeOf >>= lu pat
            DeclNoRepr  _ _ dom -> typeOf dom
            DeclHasRepr _ _ dom -> typeOf dom
            RecordField _ ty    -> return ty
            VariantField _ ty   -> return ty
    typeOf (WithLocals x _) = typeOf x                  -- TODO: do this properly, looking into locals and other ctxt
    typeOf p@(Comprehension x gensOrConds) = do
        forM_ gensOrConds $ \ goc -> case goc of
            Generator{} -> return ()                    -- TODO: do this properly
            Condition c -> do
                ty <- typeOf c
                case ty of
                    TypeBool -> return ()
                    _        -> fail $ vcat [ "Condition is not boolean."
                                            , "Condition:" <+> pretty c
                                            , "In:" <+> pretty p
                                            ]
            ComprehensionLetting{} -> return ()
        TypeList <$> typeOf x
    typeOf (Typed _ ty) = return ty
    typeOf (Op op) = typeOf op
    typeOf x@ExpressionMetaVar{} = bug ("typeOf:" <+> pretty x)

instance RepresentationOf Expression where
    representationTreeOf (Reference _ (Just (DeclHasRepr _ _ dom))) = return (reprTree dom)
    representationTreeOf (Op (MkOpIndexing (OpIndexing m i))) = do
        iType <- typeOf i
        case iType of
            TypeBool{} -> return ()
            TypeInt{} -> return ()
            _ -> fail "representationOf, OpIndexing, not a bool or int index"
        mTree <- representationTreeOf m
        case mTree of
            Tree _ [r] -> return r
            _ -> fail "domainOf, OpIndexing, not a matrix"
    representationTreeOf _ = fail "doesn't seem to have a representation"

instance Domain () Expression :< Expression where
    inject = Domain
    project (Domain x) = return x
    project x = fail ("projecting Domain out of Expression:" <+> pretty x)

instance Op Expression :< Expression where
    inject = Op
    project (Op x) = return x
    project x = fail ("projecting Op out of Expression:" <+> pretty x)

instance Op Constant :< Constant where
    inject x = bug ("injecting Op into a Constant:" <+> pretty x)
    project x = fail ("projecting Op out of a Constant:" <+> pretty x)

instance CanBeAnAlias Expression where
    isAlias (Reference _ (Just (Alias x))) = Just x
    isAlias _ = Nothing

instance ReferenceContainer Expression where
    fromName nm = Reference nm Nothing
    nameOut (Reference nm _) = return nm
    nameOut (Constant (ConstantField nm _)) = return nm
    nameOut p = fail ("This expression isn't a 'name':" <+> pretty p)

instance ExpressionLike Expression where
    fromInt = Constant . fromInt
    intOut (Constant c) = intOut c
    intOut x = fail ("Expecting a constant, but got:" <+> pretty x)

    fromBool = Constant . fromBool
    boolOut (Constant c) = boolOut c
    boolOut x = fail ("Expecting a constant, but got:" <+> pretty x)

    fromList xs = AbstractLiteral $ AbsLitMatrix (mkDomainIntB 1 (fromInt $ genericLength xs)) xs
    listOut (AbstractLiteral (AbsLitMatrix _ xs)) = return xs
    listOut (Constant (ConstantAbstract (AbsLitMatrix _ xs))) = return (map Constant xs)
    listOut c = fail ("Expecting a matrix literal, but found:" <+> pretty c)

instance Num Expression where
    x + y = Op $ MkOpSum     $ OpSum     $ fromList [x,y]
    x - y = Op $ MkOpMinus   $ OpMinus x y
    x * y = Op $ MkOpProduct $ OpProduct $ fromList [x,y]
    abs x = Op $ MkOpTwoBars $ OpTwoBars x
    signum _ = bug "signum {Expression}"
    fromInteger = fromInt . fromInteger

instance Integral Expression where
    divMod a b = ( Op $ MkOpDiv $ OpDiv a b
                 , Op $ MkOpMod $ OpMod a b )
    quotRem = divMod
    toInteger = bug "toInteger {Expression}"

instance Real Expression where
    toRational = bug "toRational {Expression}"

instance Enum Expression where
    fromEnum = bug "fromEnum {Expression}"
    toEnum = fromInt . fromIntegral
    succ a = a + 1
    pred a = a - 1
    enumFrom x = x : enumFrom (succ x)
    enumFromThen x n = x : enumFromThen (x+n) n
    enumFromTo _x _y = bug "enumFromTo {Expression}"
    enumFromThenTo _x _n _y = bug "enumFromThenTo {Expression}"

e2c :: Expression -> Constant
e2c (Constant c) = c
e2c x = bug ("e2c, not a constant:" <+> pretty x)

quantifiedVar :: NameGen m => m (AbstractPattern, Expression)
quantifiedVar = do
    nm <- nextName "q"
    let pat = Single nm
        ref = Reference nm Nothing
    return (pat, ref)

auxiliaryVar :: NameGen m => m (Name, Expression)
auxiliaryVar = do
    nm <- nextName "aux"
    let ref = Reference nm Nothing
    return (nm, ref)


lambdaToFunction :: AbstractPattern -> Expression -> Expression -> Expression
lambdaToFunction (Single nm) body = \ p ->
    let
        replacer :: Expression -> Expression
        replacer (Reference n _) | n == nm = p
        replacer x = x
    in
        transform replacer body
lambdaToFunction (AbsPatTuple ts) body = \ p ->
    let
        unroll :: [AbstractPattern] -> [Expression] -> Expression -> Expression
        unroll [] [] b = b
        unroll (pat:pats) (val:vals) b = unroll pats vals (lambdaToFunction pat b val)
        unroll _ _ _ = bug "lambdaToFunction, AbsPatTuple, unroll"

        ps :: [Expression]
        ps = case p of
            Constant (ConstantAbstract (AbsLitTuple xs)) -> map Constant xs
            AbstractLiteral (AbsLitTuple xs) -> xs
            _ -> [ Op (MkOpIndexing (OpIndexing p i))
                 | i' <- [ 1 .. genericLength ts ]
                 , let i = fromInt i'
                 ]
    in
        unroll ts ps body
lambdaToFunction (AbsPatMatrix ts) body = \ p ->
    let
        unroll :: [AbstractPattern] -> [Expression] -> Expression -> Expression
        unroll [] [] b = b
        unroll (pat:pats) (val:vals) b = unroll pats vals (lambdaToFunction pat b val)
        unroll _ _ _ = bug "lambdaToFunction, AbsPatMatrix, unroll"

        ps :: [Expression]
        ps = case p of
            Constant (ConstantAbstract (AbsLitMatrix _ xs)) -> map Constant xs
            AbstractLiteral (AbsLitMatrix _ xs) -> xs
            _ -> bug "lambdaToFunction, AbsPatMatrix"
    in
        unroll ts ps body
lambdaToFunction (AbsPatSet ts) body = \ p ->
    let
        unroll :: [AbstractPattern] -> [Expression] -> Expression -> Expression
        unroll [] [] b = b
        unroll (pat:pats) (val:vals) b = unroll pats vals (lambdaToFunction pat b val)
        unroll _ _ _ = bug "lambdaToFunction, AbsPatSet, unroll"

        ps :: [Expression]
        ps = case p of
            Constant (ConstantAbstract (AbsLitSet xs)) -> map Constant xs
            AbstractLiteral (AbsLitSet xs) -> xs
            _ -> bug "lambdaToFunction, AbsPatSet"
    in
        unroll ts ps body
lambdaToFunction p@AbstractPatternMetaVar{} _ = bug $ "Unsupported AbstractPattern, got " <+> pretty (show p)


------------------------------------------------------------------------------------------------------------------------
-- ReferenceTo ---------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data ReferenceTo
    = Alias           Expression
    | InComprehension Generator
    | DeclNoRepr      FindOrGiven Name (Domain () Expression)
    | DeclHasRepr     FindOrGiven Name (Domain HasRepresentation Expression)
    | RecordField     Name Type         -- the type of the field with this name
    | VariantField    Name Type         -- the type of the variant with this name
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize ReferenceTo
instance Hashable  ReferenceTo
instance ToJSON    ReferenceTo where toJSON = genericToJSON jsonOptions
instance FromJSON  ReferenceTo where parseJSON = genericParseJSON jsonOptions

instance Pretty ReferenceTo where
    pretty (Alias           x  ) = "Alias"           <+> prParens (pretty x)
    pretty (InComprehension gen) = "InComprehension" <+> prParens (pretty gen)
    pretty (DeclNoRepr      forg nm dom) = "DeclNoRepr"  <+> prParens (pretty forg <+> pretty nm <> ":" <+> pretty dom)
    pretty (DeclHasRepr     forg nm dom) = "DeclHasRepr" <+> prParens (pretty forg <+> pretty nm <> ":" <+> pretty dom)
    pretty (RecordField     nm ty) = "RecordField"  <+> prParens (pretty nm <+> ":" <+> pretty ty)
    pretty (VariantField    nm ty) = "VariantField" <+> prParens (pretty nm <+> ":" <+> pretty ty)


------------------------------------------------------------------------------------------------------------------------
-- AbstractPattern -----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data AbstractPattern
    = Single Name
    | AbsPatTuple [AbstractPattern]
    | AbsPatMatrix
            -- (Domain () a)          -- TODO: Should there be a domain here?
            [AbstractPattern]
    | AbsPatSet [AbstractPattern]
    -- | AbsPatMSet [a]
    -- | AbsPatFunction [(a, a)]
    -- | AbsPatRelation [[a]]
    -- | AbsPatPartition [[a]]
    -- TODO: Consider introducing the above as abstract patterns...
    | AbstractPatternMetaVar String
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize AbstractPattern
instance Hashable  AbstractPattern
instance ToJSON    AbstractPattern where toJSON = genericToJSON jsonOptions
instance FromJSON  AbstractPattern where parseJSON = genericParseJSON jsonOptions

instance Pretty AbstractPattern where
    pretty (Single       nm) = pretty nm
    pretty (AbsPatTuple  xs) = (if length xs <= 1 then "tuple" else prEmpty) <>
                               prettyList prParens   "," xs
    pretty (AbsPatMatrix xs) = prettyList prBrackets "," xs
    pretty (AbsPatSet    xs) = prettyList prBraces "," xs
    pretty (AbstractPatternMetaVar s) = "&" <> pretty s

instance VarSymBreakingDescription AbstractPattern where
    varSymBreakingDescription (Single nm) = toJSON nm
    varSymBreakingDescription (AbsPatTuple xs) = JSON.Object $ M.fromList
        [ ("type", JSON.String "AbsPatTuple")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        ]
    varSymBreakingDescription (AbsPatMatrix xs) = JSON.Object $ M.fromList
        [ ("type", JSON.String "AbsPatMatrix")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        ]
    varSymBreakingDescription (AbsPatSet xs) = JSON.Object $ M.fromList
        [ ("type", JSON.String "AbsPatSet")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (AbstractPatternMetaVar s) = JSON.Object $ M.fromList
        [ ("type", JSON.String "AbstractPatternMetaVar")
        , ("name", JSON.String (stringToText s))
        ]


patternToExpr :: AbstractPattern -> Expression
patternToExpr (Single nm) = Reference nm Nothing
patternToExpr (AbsPatTuple  ts) = AbstractLiteral $ AbsLitTuple  $ map patternToExpr ts
patternToExpr (AbsPatMatrix ts) = AbstractLiteral $ AbsLitMatrix (DomainInt [RangeBounded 1 (fromInt (genericLength ts))])
                                                                 $ map patternToExpr ts
patternToExpr (AbsPatSet    ts) = AbstractLiteral $ AbsLitSet    $ map patternToExpr ts
patternToExpr AbstractPatternMetaVar{} = bug "patternToExpr"

------------------------------------------------------------------------------------------------------------------------
-- Generator -----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data GeneratorOrCondition
    = Generator Generator
    | Condition Expression
    | ComprehensionLetting Name Expression
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty GeneratorOrCondition where
    pretty (Generator x) = pretty x
    pretty (Condition x) = pretty x
    pretty (ComprehensionLetting n x) = "letting" <+> pretty n <+> "be" <+> pretty x

instance VarSymBreakingDescription GeneratorOrCondition where
    varSymBreakingDescription (Generator x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "Generator")
        , ("child", varSymBreakingDescription x)
        ]
    varSymBreakingDescription (Condition x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "Condition")
        , ("child", varSymBreakingDescription x)
        ]
    varSymBreakingDescription (ComprehensionLetting n x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "ComprehensionLetting")
        , ("children", JSON.Array $ V.fromList [toJSON n, varSymBreakingDescription x])
        ]

instance Serialize GeneratorOrCondition
instance Hashable  GeneratorOrCondition
instance ToJSON    GeneratorOrCondition where toJSON = genericToJSON jsonOptions
instance FromJSON  GeneratorOrCondition where parseJSON = genericParseJSON jsonOptions


data Generator
     = GenDomainNoRepr  AbstractPattern (Domain () Expression)
     | GenDomainHasRepr Name            (Domain HasRepresentation Expression)
     | GenInExpr        AbstractPattern Expression
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty Generator where
    pretty (GenDomainNoRepr  pat x) = pretty pat <+> ":"  <+> pretty x
    pretty (GenDomainHasRepr pat x) = pretty pat <+> ":"  <+> pretty x
    pretty (GenInExpr        pat x) = pretty pat <+> "<-" <+> pretty x

instance VarSymBreakingDescription Generator where
    varSymBreakingDescription (GenDomainNoRepr  pat x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "GenDomainNoRepr")
        , ("pattern", varSymBreakingDescription pat)
        , ("generator", varSymBreakingDescription x)
        ]
    varSymBreakingDescription (GenDomainHasRepr pat x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "GenDomainHasRepr")
        , ("pattern", toJSON pat)
        , ("generator", varSymBreakingDescription x)
        ]
    varSymBreakingDescription (GenInExpr        pat x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "GenInExpr")
        , ("pattern", varSymBreakingDescription pat)
        , ("generator", varSymBreakingDescription x)
        ]

instance Serialize Generator
instance Hashable  Generator
instance ToJSON    Generator where toJSON = genericToJSON jsonOptions
instance FromJSON  Generator where parseJSON = genericParseJSON jsonOptions

generatorPat :: Generator -> AbstractPattern
generatorPat (GenDomainNoRepr  pat _) = pat
generatorPat (GenDomainHasRepr pat _) = Single pat
generatorPat (GenInExpr        pat _) = pat


------------------------------------------------------------------------------------------------------------------------
-- Misc ---------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

extractLettings :: Model -> [(Name, Expression)]
extractLettings model =
    [ (n, x) | Declaration (Letting n x) <- mStatements model
             , not (isDomain x)
             ]
    where isDomain Domain{} = True
          isDomain _ = False


tupleLitIfNeeded :: [Expression] -> Expression
tupleLitIfNeeded [] = bug "tupleLitIfNeeded []"
tupleLitIfNeeded [x] = x
tupleLitIfNeeded xs = AbstractLiteral (AbsLitTuple xs)
