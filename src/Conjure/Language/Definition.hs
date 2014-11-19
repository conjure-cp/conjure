{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Language.Definition
    ( forgetRepr, rangesInts
    , freshNames
    , languageEprime
    , typeCheckModelIO, typeCheckModel
    , initInfo

    , quantifiedVar, lambdaToFunction

    , e2c

    , Model(..), LanguageVersion(..)
    , ModelInfo(..), Decision(..)
    , Statement(..), Objective(..)
    , Declaration(..), FindOrGiven(..)

    , Name(..)
    , Expression(..), ReferenceTo(..)
    , Constant(..)
    , AbstractLiteral(..)
    , AbstractPattern(..)
    , GeneratorOrFilter(..), Generator(..), generatorPat

    , ExpressionLike(..), ReferenceContainer(..)

    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Pretty
import Conjure.Language.AdHoc

import Conjure.Language.Name
import Conjure.Language.Constant
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.Ops
import Conjure.Language.TypeOf

-- aeson
import Data.Aeson ( (.=), (.:) )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

-- unordered-containers
import qualified Data.HashSet as S


------------------------------------------------------------------------------------------------------------------------
-- Model ---------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Model = Model
    { mLanguage :: LanguageVersion
    , mStatements :: [Statement]
    , mInfo :: ModelInfo
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Model
instance Hashable  Model
instance ToJSON    Model where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  Model where parseJSON = JSON.genericParseJSON jsonOptions

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

freshNames :: Model -> [Name]
freshNames model = newNames
    where
        newNames = [ name
                   | i <- allNats
                   , let name = "q" `mappend` Name (stringToText (show i))
                   , not (S.member name usedNames)
                   ]
        usedNames = S.fromList (universeBi model :: [Name])

languageEprime :: Model -> Model
languageEprime m = m { mLanguage = LanguageVersion "ESSENCE'" [1,0] }

typeCheckModelIO :: Model -> IO ()
typeCheckModelIO m =
    case typeCheckModel m of
        Nothing -> return ()
        Just msg -> userErr $ sep ["Type error, specifically:", msg]


-- | returns `Just msg` if the model is type-incorrect, msg being an explanation.
--   returns `Nothing` if the model is type-correct.
typeCheckModel :: Model -> Maybe Doc
typeCheckModel _ = Nothing
-- typeCheckModel _ = Just "Just Plain Wrong (TM)"


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
    | SearchOrder [Name]
    | Where [Expression]
    | Objective Objective Expression
    | SuchThat [Expression]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Statement
instance Hashable  Statement
instance ToJSON    Statement where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  Statement where parseJSON = JSON.genericParseJSON jsonOptions

instance Pretty Statement where
    pretty (Declaration x) = pretty x
    pretty (SearchOrder nms) = "branching on" <++> prettyList prBrackets "," nms
    pretty (Where xs) = "where" <++> vcat (punctuate "," $ map pretty xs)
    pretty (Objective obj x) = pretty obj <++> pretty x
    pretty (SuchThat xs) = "such that" <++> vcat (punctuate "," $ map pretty xs)


------------------------------------------------------------------------------------------------------------------------
-- Objective -----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Objective = Minimising | Maximising
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Objective
instance Hashable  Objective
instance ToJSON    Objective where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  Objective where parseJSON = JSON.genericParseJSON jsonOptions

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
instance ToJSON    Declaration where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  Declaration where parseJSON = JSON.genericParseJSON jsonOptions

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


data FindOrGiven = Find | Given
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize FindOrGiven
instance Hashable  FindOrGiven
instance ToJSON    FindOrGiven where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  FindOrGiven where parseJSON = JSON.genericParseJSON jsonOptions

instance Pretty FindOrGiven where
    pretty Find = "find"
    pretty Given = "given"


------------------------------------------------------------------------------------------------------------------------
-- ModelInfo -----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data ModelInfo = ModelInfo
    { miGivens :: [Name]
    , miFinds :: [Name]
    , miEnumGivens :: [Name]
    , miEnumLettings :: [Declaration]
    , miOriginalDomains :: [(Name, Domain () Expression)]
    , miRepresentations :: [(Name, Domain HasRepresentation Expression)]
    , miTrailCompact :: [(Int,[Int])]
    , miTrailVerbose :: [Decision]
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

modelInfoJSONOptions :: JSON.Options
modelInfoJSONOptions = jsonOptions { JSON.fieldLabelModifier = onHead toLower . drop 2 }
    where onHead f (x:xs) = f x : xs
          onHead _ [] = []

instance Serialize ModelInfo
instance Hashable  ModelInfo
instance ToJSON    ModelInfo where toJSON = JSON.genericToJSON modelInfoJSONOptions
instance FromJSON  ModelInfo where parseJSON = JSON.genericParseJSON modelInfoJSONOptions

instance Default ModelInfo where
    def = ModelInfo def def def def def def def def

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
            , miEnumGivens   = [ nm | Declaration (GivenDomainDefnEnum nm)  <- mStatements model ]
            , miEnumLettings = [ d  | Declaration d@LettingDomainDefnEnum{} <- mStatements model ]
            }


------------------------------------------------------------------------------------------------------------------------
-- Decision ------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Decision = Decision
    { dDescription :: [Text]
    , dOptions :: [Int]
    , dDecision :: Int
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

decisionJSONOptions :: JSON.Options
decisionJSONOptions = jsonOptions { JSON.fieldLabelModifier = map toLower . drop 1 }

instance Serialize Decision
instance Hashable  Decision
instance ToJSON    Decision where toJSON = JSON.genericToJSON decisionJSONOptions
instance FromJSON  Decision where parseJSON = JSON.genericParseJSON decisionJSONOptions


------------------------------------------------------------------------------------------------------------------------
-- Expression ----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Expression
    = Constant Constant
    | AbstractLiteral (AbstractLiteral Expression)
    | Domain (Domain () Expression)
    | Reference Name (Maybe ReferenceTo)
    | WithLocals Expression [Statement]
    | Comprehension Expression [GeneratorOrFilter]
    | Op (Ops Expression)
    | ExpressionMetaVar String
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Expression
instance Hashable  Expression
instance ToJSON    Expression where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  Expression where parseJSON = JSON.genericParseJSON jsonOptions

viewIndexed :: Expression -> (Expression, [Doc])
viewIndexed (Op (MkOpIndexing (OpIndexing m i  ))) =
    let this = pretty i
    in  second (++ [this]) (viewIndexed m)
viewIndexed (Op (MkOpSlicing  (OpSlicing  m a b))) =
    let this = maybe prEmpty pretty a <> ".." <> maybe prEmpty pretty b
    in  second (++ [this]) (viewIndexed m)
viewIndexed m = (m, [])

instance Pretty Expression where

    pretty (viewIndexed -> (m,is@(_:_))) = pretty m <> prettyList prBrackets "," is

    -- mostly for debugging: print what a reference is pointing at
    -- pretty (Reference x (Just r)) = pretty x <> "#`" <> pretty r <> "`"
    -- pretty (Reference x (Just (DeclHasRepr _ _ dom))) = pretty x <> "#`" <> pretty dom <> "`"

    pretty (Constant x) = pretty x
    pretty (AbstractLiteral x) = pretty x
    pretty (Domain x) = "`" <> pretty x <> "`"
    pretty (Reference x _) = pretty x
    pretty (WithLocals x ss) = prBraces $ pretty x <+> "@" <+> vcat (map pretty ss)
    pretty (Comprehension x is) = prBrackets $ pretty x <++> "|" <+> prettyList id "," is
    pretty (Op op) = pretty op
    pretty (ExpressionMetaVar x) = "&" <> pretty x

instance TypeOf Expression where
    typeOf (Constant x) = typeOf x
    typeOf (AbstractLiteral x) = typeOf x
    typeOf (Domain x)   = typeOf x
    typeOf (Reference nm Nothing) = bug ("Type error, identifier not bound:" <+> pretty nm)
    typeOf (Reference nm (Just refTo)) =
        case refTo of
            Alias x -> typeOf x
            InComprehension gen ->
                let
                    lu pat ty = maybe
                        (bug $ vcat ["Type error, InComprehension:", pretty pat, pretty ty])
                        return
                        (lu' pat ty)
                    lu' (Single nm') ty | nm == nm' = Just ty
                    lu' (AbsPatTuple pats) (TypeTuple tys) = zipWith lu' pats tys |> catMaybes |> listToMaybe
                    lu' _ _ = Nothing
                in
                    case gen of
                        GenDomain pat domain -> typeOf domain                 >>= lu pat
                        GenInExpr pat expr   -> typeOf expr   >>= innerTypeOf >>= lu pat
            DeclNoRepr _ _ dom -> typeOf dom
            DeclHasRepr _ _ dom -> typeOf dom
    typeOf (WithLocals x _) = typeOf x                  -- TODO: do this properly, looking into locals and other ctxt
    typeOf (Comprehension x _) = TypeList <$> typeOf x  -- TODO: do this properly, look into generators and filters
    typeOf (Op op) = typeOf op
    typeOf x@ExpressionMetaVar{} = bug ("typeOf:" <+> pretty x)

instance OperatorContainer Expression where
    injectOp = Op
    projectOp (Op op) = return op
    projectOp x = fail ("not an op: " <++> pretty (show x))

instance ReferenceContainer Expression where
    fromName nm = Reference nm Nothing

instance ExpressionLike Expression where
    fromInt = Constant . fromInt
    intOut (Constant c) = intOut c
    intOut x = fail ("Expecting a constant, but got:" <+> pretty x)
    fromBool = Constant . fromBool
    boolOut (Constant c) = boolOut c
    boolOut x = fail ("Expecting a constant, but got:" <+> pretty x)

instance Num Expression where
    (+) = opPlus
    (-) = opMinus
    (*) = opTimes
    abs = opAbs
    signum _ = bug "signum {Expression}"
    fromInteger = fromInt . fromInteger

instance Integral Expression where
    divMod a b = (opDiv a b, opMod a b)
    quotRem = divMod
    toInteger = bug "toInteger {Expression}"

instance Real Expression where
    toRational = bug "toRational {Expression}"

instance Enum Expression where
    fromEnum = bug "fromEnum {Expression}"
    toEnum = fromInt
    succ a = a + 1
    pred a = a - 1
    enumFrom x = x : enumFrom (succ x)
    enumFromThen x n = x : enumFromThen (x+n) n
    enumFromTo _x _y = bug "enumFromTo {Expression}"
    enumFromThenTo _x _n _y = bug "enumFromThenTo {Expression}"

e2c :: Expression -> Constant
e2c (Constant c) = c
e2c x = bug ("e2c, not a constant:" <+> pretty x)

quantifiedVar :: Name -> (AbstractPattern, Expression)
quantifiedVar nm =
    let pat = Single nm
        ref = Reference nm Nothing
    in  (pat, ref)

-- TODO: Add support for AbsPatTuple
-- TODO: Add support for AbsPatMatrix
-- TODO: Add support for AbsPatSet
lambdaToFunction :: AbstractPattern -> Expression -> (Expression -> Expression)
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
            Constant        (ConstantTuple xs) -> map Constant xs
            AbstractLiteral (AbsLitTuple   xs) -> xs
            _ -> bug "lambdaToFunction, AbsPatTuple"
    in
        unroll ts ps body
lambdaToFunction p _ = bug $ "Unsupported AbstractPattern, expecting `Single` but got " <+> pretty (show p)


------------------------------------------------------------------------------------------------------------------------
-- ReferenceTo ---------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data ReferenceTo
    = Alias           Expression
    | InComprehension Generator
    | DeclNoRepr      FindOrGiven Name (Domain () Expression)
    | DeclHasRepr     FindOrGiven Name (Domain HasRepresentation Expression)
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize ReferenceTo
instance Hashable  ReferenceTo
instance ToJSON    ReferenceTo where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  ReferenceTo where parseJSON = JSON.genericParseJSON jsonOptions

instance Pretty ReferenceTo where
    pretty (Alias           x  ) = "Alias"           <+> prParens (pretty x)
    pretty (InComprehension gen) = "InComprehension" <+> prParens (pretty gen)
    pretty (DeclNoRepr      forg nm dom) = "DeclNoRepr"  <+> prParens (pretty forg <+> pretty nm <> ":" <+> pretty dom)
    pretty (DeclHasRepr     forg nm dom) = "DeclHasRepr" <+> prParens (pretty forg <+> pretty nm <> ":" <+> pretty dom)


------------------------------------------------------------------------------------------------------------------------
-- AbstractLiteral -----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data AbstractLiteral x
    = AbsLitTuple [x]
    | AbsLitMatrix (Domain () x) [x]
    | AbsLitSet [x]
    | AbsLitMSet [x]
    | AbsLitFunction [(x, x)]
    | AbsLitRelation [[x]]
    | AbsLitPartition [[x]]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize x => Serialize (AbstractLiteral x)
instance Hashable  x => Hashable  (AbstractLiteral x)
instance ToJSON    x => ToJSON    (AbstractLiteral x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (AbstractLiteral x) where parseJSON = JSON.genericParseJSON jsonOptions

instance Pretty a => Pretty (AbstractLiteral a) where
    pretty (AbsLitTuple xs) = (if length xs < 2 then "tuple" else prEmpty) <+> prettyList prParens "," xs
    pretty (AbsLitMatrix index xs) = let f i = prBrackets (i <> ";" <+> pretty index) in prettyList f "," xs
    pretty (AbsLitSet       xs ) =                prettyList prBraces "," xs
    pretty (AbsLitMSet      xs ) = "mset"      <> prettyList prParens "," xs
    pretty (AbsLitFunction  xs ) = "function"  <> prettyListDoc prParens "," [ pretty a <+> "-->" <+> pretty b | (a,b) <- xs ]
    pretty (AbsLitRelation  xss) = "relation"  <> prettyListDoc prParens "," [ pretty (AbsLitTuple xs)         | xs <- xss   ]
    pretty (AbsLitPartition xss) = "partition" <> prettyListDoc prParens "," [ prettyList prBraces "," xs      | xs <- xss   ]

instance TypeOf a => TypeOf (AbstractLiteral a) where
    typeOf (AbsLitTuple        xs) = TypeTuple    <$> mapM typeOf xs
    typeOf (AbsLitMatrix ind inn ) = TypeMatrix   <$> typeOf ind <*> (homoType <$> mapM typeOf inn)
    typeOf (AbsLitSet         xs ) = TypeSet      <$> (homoType <$> mapM typeOf xs)
    typeOf (AbsLitMSet        xs ) = TypeMSet     <$> (homoType <$> mapM typeOf xs)
    typeOf (AbsLitFunction    xs ) = TypeFunction <$> (homoType <$> mapM (typeOf . fst) xs)
                                                  <*> (homoType <$> mapM (typeOf . fst) xs)
    typeOf (AbsLitRelation    xss) = do
        ty <- homoType <$> mapM (typeOf . AbsLitTuple) xss
        case ty of
            TypeTuple ts -> return (TypeRelation ts)
            _ -> bug "expecting TypeTuple in typeOf"
    typeOf (AbsLitPartition   xss) = TypePartition <$> (homoType <$> mapM typeOf (concat xss))


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
instance ToJSON    AbstractPattern where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  AbstractPattern where parseJSON = JSON.genericParseJSON jsonOptions

instance Pretty AbstractPattern where
    pretty (Single       nm) = pretty nm
    pretty (AbsPatTuple  xs) = (if length xs <= 1 then "tuple" else prEmpty) <>
                               prettyList prParens   "," xs
    pretty (AbsPatMatrix xs) = prettyList prBrackets "," xs
    pretty (AbsPatSet    xs) = prettyList prBraces "," xs
    pretty (AbstractPatternMetaVar s) = "&" <> pretty s

-- instance TypeOf AbstractPattern where
--     typeOf pat@(Single _ Nothing  ) = fail ("typeOf AbstractPattern:" <+> pretty (show pat))
--     typeOf     (Single _ (Just ty)) = return ty
--     typeOf (AbsPatTuple ts) = TypeTuple <$> mapM typeOf ts
--     typeOf pat@(AbsPatMatrix ts) = do
--         tys <- mapM typeOf ts
--         if typesUnify tys
--             then return (TypeMatrix TypeInt (mostDefined tys))
--             else fail ("Types do not unify in:" <+> pretty pat)
--     typeOf pat                      = fail ("typeOf:" <+> pretty (show pat))



------------------------------------------------------------------------------------------------------------------------
-- Generator -----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data GeneratorOrFilter = Generator Generator | Filter Expression
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty GeneratorOrFilter where
    pretty (Generator x) = pretty x
    pretty (Filter x) = pretty x

instance Serialize GeneratorOrFilter
instance Hashable  GeneratorOrFilter
instance ToJSON    GeneratorOrFilter where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  GeneratorOrFilter where parseJSON = JSON.genericParseJSON jsonOptions


data Generator
     = GenDomain AbstractPattern (Domain () Expression)
     | GenInExpr AbstractPattern Expression
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty Generator where     
    pretty (GenDomain pat x) = pretty pat <+> ":"  <+> pretty x
    pretty (GenInExpr pat x) = pretty pat <+> "<-" <+> pretty x

instance Serialize Generator
instance Hashable  Generator
instance ToJSON    Generator where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  Generator where parseJSON = JSON.genericParseJSON jsonOptions

generatorPat :: Generator -> AbstractPattern
generatorPat (GenDomain pat _) = pat
generatorPat (GenInExpr pat _) = pat
