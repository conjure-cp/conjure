{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Language.Definition
    ( forgetRepr, rangesInts
    , freshNames
    , languageEprime
    , typeCheckModelIO, typeCheckModel
    , initInfo

    , quantifiedVar
    , mkLambda, lambdaToFunction

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
instance Hashable Model
instance ToJSON Model where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Model where parseJSON = JSON.genericParseJSON jsonOptions

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
instance Hashable LanguageVersion

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
instance Hashable Statement
instance ToJSON Statement where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Statement where parseJSON = JSON.genericParseJSON jsonOptions

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
instance Hashable Objective
instance ToJSON Objective where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Objective where parseJSON = JSON.genericParseJSON jsonOptions

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
instance Hashable Declaration
instance ToJSON Declaration where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Declaration where parseJSON = JSON.genericParseJSON jsonOptions

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
instance Hashable FindOrGiven
instance ToJSON FindOrGiven where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON FindOrGiven where parseJSON = JSON.genericParseJSON jsonOptions

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
    , miTrail :: [Decision]
    , miTrailCompact :: [(Int,[Int])]
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

modelInfoJSONOptions :: JSON.Options
modelInfoJSONOptions = jsonOptions { JSON.fieldLabelModifier = onHead toLower . drop 2 }
    where onHead f (x:xs) = f x : xs
          onHead _ [] = []

instance Serialize ModelInfo
instance Hashable ModelInfo
instance ToJSON ModelInfo where toJSON = JSON.genericToJSON modelInfoJSONOptions
instance FromJSON ModelInfo where parseJSON = JSON.genericParseJSON modelInfoJSONOptions

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
instance Hashable Decision
instance ToJSON Decision where toJSON = JSON.genericToJSON decisionJSONOptions
instance FromJSON Decision where parseJSON = JSON.genericParseJSON decisionJSONOptions


------------------------------------------------------------------------------------------------------------------------
-- Expression ----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Expression
    = Constant Constant
    | AbstractLiteral (AbstractLiteral Expression)
    | Domain (Domain () Expression)
    | Reference Name (Maybe ReferenceTo)
    | WithLocals Expression [Statement]
    | Op (Ops Expression)
    | Lambda AbstractPattern Expression
    | ExpressionMetaVar String
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Expression
instance Hashable Expression
instance ToJSON Expression where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Expression where parseJSON = JSON.genericParseJSON jsonOptions

viewIndexed :: Expression -> (Expression, [Doc])
viewIndexed (Op (MkOpIndexing (OpIndexing m i  ))) =
    let this = pretty i
    in  second (++ [this]) (viewIndexed m)
viewIndexed (Op (MkOpSlicing  (OpSlicing  m a b))) =
    let this = pretty a <> ".." <> pretty b
    in  second (++ [this]) (viewIndexed m)
viewIndexed m = (m, [])


instance Pretty Expression where
    -- special case for matrix comprehensions of SR here
    pretty (Op (MkOpMapOverDomain (OpMapOverDomain (Lambda (Single nm _) body) (Domain domain)))) =
        prBrackets (pretty body <+> "|" <+> pretty nm <+> ":" <+> pretty domain)

    pretty (viewIndexed -> (m,is@(_:_))) = pretty m <> prettyList prBrackets "," is

    -- mostly for debugging: print what a reference is pointing at
    -- pretty (Reference x (Just (DeclHasRepr _ _ dom))) = pretty x <> "#`" <> pretty dom <> "`"

    pretty (Constant x) = pretty x
    pretty (AbstractLiteral x) = pretty x
    pretty (Domain x) = "`" <> pretty x <> "`"
    pretty (Reference x _) = pretty x
    pretty (WithLocals x ss) = prBraces $ pretty x <+> "@" <+> vcat (map pretty ss)
    pretty (Lambda arg x) = "lambda" <> prParens (fsep [pretty arg, "-->", pretty x])
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
            InLambda (Single _ (Just ty)) -> return ty
            InLambda{} -> bug ("Type error, InLambda:" <+> pretty nm)
            DeclNoRepr _ _ dom -> typeOf dom
            DeclHasRepr _ _ dom -> typeOf dom
    typeOf (WithLocals x _) = typeOf x                -- TODO: do this properly (looking into locals and other ctxt)
    typeOf (Op op) = typeOf op
    typeOf Lambda{}     = return TypeAny -- TODO: fix
    typeOf x@ExpressionMetaVar{} = bug ("typeOf:" <+> pretty x)

instance OperatorContainer Expression where
    injectOp = Op
    projectOp (Op op) = return op
    projectOp x = fail ("not an op: " <++> pretty (show x))

instance IntContainer Expression where
    intOut (Constant c) = intOut c
    intOut x = fail ("Expecting an integer, but got:" <+> pretty x)

instance ReferenceContainer Expression where
    fromName nm = Reference nm Nothing

instance ExpressionLike Expression where
    fromInt = Constant . fromInt
    fromBool = Constant . fromBool

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

quantifiedVar :: Name -> Type -> (AbstractPattern, Expression)
quantifiedVar nm ty =
    let pat = Single nm (Just ty)
        ref = Reference nm (Just (InLambda pat))
    in  (pat, ref)

mkLambda :: Name -> Type -> (Expression -> Expression) -> Expression
mkLambda nm ty f =
    let pat = Single nm (Just ty)
        ref = Reference nm (Just (InLambda pat))
    in  Lambda pat (f ref)

-- TODO: Add support for AbsPatTuple
-- TODO: Add support for AbsPatMatrix
-- TODO: Add support for AbsPatSet
lambdaToFunction :: AbstractPattern -> Expression -> (Expression -> Expression)
lambdaToFunction (Single nm _) body = \ p -> transform (replacer p) body
    where
        replacer new (Reference n _) | n == nm = new
        replacer _ x = x
lambdaToFunction p _ = bug $ "Unsupported AbstractPattern, expecting `Single` but got " <+> pretty (show p)


------------------------------------------------------------------------------------------------------------------------
-- ReferenceTo ---------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data ReferenceTo
    = Alias         Expression
    | InLambda      AbstractPattern
    | DeclNoRepr    FindOrGiven Name (Domain () Expression)
    | DeclHasRepr   FindOrGiven Name (Domain HasRepresentation Expression)
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize ReferenceTo
instance Hashable ReferenceTo
instance ToJSON ReferenceTo where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON ReferenceTo where parseJSON = JSON.genericParseJSON jsonOptions

instance Pretty ReferenceTo where
    pretty = pretty . show


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
    = Single Name (Maybe Type)
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
instance Hashable AbstractPattern
instance ToJSON AbstractPattern where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON AbstractPattern where parseJSON = JSON.genericParseJSON jsonOptions

instance Pretty AbstractPattern where
    pretty (Single nm Nothing  ) = pretty nm
    pretty (Single nm (Just ty)) = pretty nm <+> ":" <+> "`" <> pretty ty <> "`"
    pretty (AbsPatTuple      xs) = (if length xs <= 1 then "tuple" else prEmpty)
                                <> prettyList prParens   "," xs
    pretty (AbsPatMatrix     xs) = prettyList prBrackets "," xs
    pretty (AbsPatSet        xs) = prettyList prBraces   "," xs
    pretty (AbstractPatternMetaVar s) = "&" <> pretty s

instance TypeOf AbstractPattern where
    typeOf pat@(Single _ Nothing  ) = fail ("typeOf AbstractPattern:" <+> pretty (show pat))
    typeOf     (Single _ (Just ty)) = return ty
    typeOf (AbsPatTuple ts) = TypeTuple <$> mapM typeOf ts
    typeOf pat@(AbsPatMatrix ts) = do
        tys <- mapM typeOf ts
        if typesUnify tys
            then return (TypeMatrix TypeInt (mostDefined tys))
            else fail ("Types do not unify in:" <+> pretty pat)
    typeOf pat                      = fail ("typeOf:" <+> pretty (show pat))

