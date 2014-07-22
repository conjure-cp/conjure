{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.E.Definition
    ( module Stuff.Generic.Tag

    , Spec(..), LanguageVersion(..) , E(..), BuiltIn(..)
    , Domain(..), DomainAttributes(..), DomainAttribute(..), Range(..)
    , SetAttr(..)
    , DomainDefnEnum(..), DomainDefnUnnamed(..)
    , HasRepresentation(..)
    , Type(..)
    , Constant(..)
    , RulesDB(..), RuleRefn(..), RuleRepr(..), RuleReprCase(..), RuleReprResult(..)
    , Name(..)

    , identifierSplit, identifierConstruct
    , identifierStripRegion

    , universe, children
    , descend, descendM
    , transform, transformM
    , rewrite, rewriteM
    , replace, replaceAll, gdepth

    , qq, xMake, xMatch
    , viewTagged, viewTaggeds
    , prettyAsTree, prettyAsPaths

    , statementAsList, listAsStatement

    , forgetRepr

    , rangesInts

    ) where

import Bug
import Stuff.Generic.Tag
import Stuff.Pretty
import Stuff.MetaVariable
import Language.E.Imports

import Data.Maybe ( fromJust )
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Data.String ( IsString(..) )

-- text
import qualified Data.Text as T

-- aeson
import Data.Aeson ( ToJSON(..), (.=) )
import qualified Data.Aeson as JSON

-- uniplate
import Data.Generics.Uniplate.Data ( Uniplate, universe, children, descend, descendM, transform, transformM, rewrite, rewriteM )

-- template-haskell
import Language.Haskell.TH ( Q, Exp(..), Pat(..), Lit(..), mkName )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )

-- haskell-src-meta
import Language.Haskell.Meta.Parse.Careful

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose, oneof, vectorOf, sized )


data Spec = Spec LanguageVersion E
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize Spec

instance Hashable Spec

instance Default Spec where
    def = Spec (LanguageVersion "Essence" [1,3]) EOF


newtype Name = Name Text
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic, IsString, Serialize, Hashable, ToJSON, Monoid)

instance Arbitrary Name where
    arbitrary = do
        ch <- choose ('a', 'z')
        return $ Name $ T.pack [ch]
    shrink (Name n) = if T.length n > 1 then [Name (T.drop 1 n)] else []


data LanguageVersion = LanguageVersion Name [Int]
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize LanguageVersion

instance Hashable LanguageVersion


instance ToJSON LanguageVersion where
    toJSON (LanguageVersion t is) = JSON.object [ "language" .= toJSON (t,is) ]


data RulesDB = RulesDB { reprRules :: [RuleRepr], refnRules :: [RuleRefn] }
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize RulesDB

instance Hashable RulesDB


data RuleRefn = RuleRefn
    { ruleRefnName  :: Name
    , ruleRefnLevel :: Maybe Int
    , ruleRefnPattern :: E
    , ruleRefnTemplates :: [E]
    , ruleRefnLocals :: [E]
    }
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize RuleRefn

instance Hashable RuleRefn


data RuleRepr = RuleRepr
    { ruleReprName :: Name                  -- name of the rule
    , ruleReprReprName :: Name              -- name of the representation
    , ruleReprDomainOut :: Domain () E      -- domain out.
    , ruleReprStructural :: Maybe E         -- structural constraints
    , ruleReprLocals :: [E]                 -- locals
    , ruleReprCases :: [RuleReprCase]
    }
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize RuleRepr

instance Hashable RuleRepr


data RuleReprCase = RuleReprCase
    { ruleReprCaseDomainIn :: Domain () E   -- domain in.
    , ruleReprCaseStructural :: Maybe E     -- structural constraints
    , ruleReprCaseLocals :: [E]             -- locals
    }
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize RuleReprCase

instance Hashable RuleReprCase


data RuleReprResult = RuleReprResult
    { ruleReprResultOriginalDecl :: E               -- original declaration
    , ruleReprResultRuleName :: Name                -- rule name
    , ruleReprResultReprName :: Name                -- name of the representation
    , ruleReprResultReplacementDom :: Domain () E   -- replacement domain
    , ruleReprResultStructurals :: [E]              -- structural constraints
    }
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize RuleReprResult

instance Hashable RuleReprResult


data E
    = Prim BuiltIn
    | Tagged !Tag [E]
    | C Constant
    | D (Domain () E)
    | EOF
    | StatementAndNext E E
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize E

instance Hashable E

instance ToJSON E where
    toJSON (Prim x) = JSON.object [ "primitive" .= toJSON x ]
    toJSON (Tagged t xs) = JSON.object [ "tag" .= toJSON t
                                       , "children" .= toJSON xs
                                       ]
    toJSON (C x) = JSON.object [ "constant" .= toJSON x ]
    toJSON (D x) = JSON.object [ "domain" .= toJSON x ]
    toJSON EOF = toJSON ("EOF" :: String)
    toJSON (StatementAndNext a b) = JSON.object [ "statement" .= toJSON a
                                                , "next" .= toJSON b
                                                ]


data BuiltIn = B !Bool | I !Integer | S !Text
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize BuiltIn

instance Hashable BuiltIn

instance Pretty BuiltIn where
    pretty (B x) = pretty x
    pretty (I x) = pretty x
    pretty (S x) = pretty x

instance ToJSON BuiltIn where
    toJSON (B x) = JSON.object [ "bool"   .= toJSON x ]
    toJSON (I x) = JSON.object [ "int"    .= toJSON x ]
    toJSON (S x) = JSON.object [ "string" .= toJSON x ]

instance MetaVariable E where
    unnamedMV (Tagged "reference" [Prim (S "_")]) = True
    unnamedMV _ = False
    namedMV   (Tagged "metavar"   [Prim (S s  )]) = Just s
    namedMV   _ = Nothing

instance Arbitrary BuiltIn where
    arbitrary = do
        i <- choose (1 :: Int, 3)
        case i of
            1 -> B <$> arbitrary
            2 -> I <$> arbitrary
            3 -> do
                Name n <- arbitrary
                return (S n)
            _ -> error "Impossible: BuiltIn.arbitrary"


data DomainDefnEnum = DomainDefnEnum Name [Name]
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize DomainDefnEnum

instance Hashable DomainDefnEnum

instance ToJSON DomainDefnEnum


data DomainDefnUnnamed = DomainDefnUnnamed Name E
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize DomainDefnUnnamed

instance Hashable DomainDefnUnnamed

instance ToJSON DomainDefnUnnamed


data Domain r a
    = DomainBool
    | DomainInt [Range a]
    | DomainEnum DomainDefnEnum [Range a]
    | DomainUnnamed DomainDefnUnnamed
    | DomainTuple [Domain r a]
    | DomainMatrix (Domain () a) (Domain r a)
    | DomainSet       r (SetAttr a) (Domain r a)
    | DomainMSet      r (DomainAttributes a) (Domain r a)
    | DomainFunction  r (DomainAttributes a) (Domain r a) (Domain r a)
    | DomainRelation  r (DomainAttributes a) [Domain r a]
    | DomainPartition r (DomainAttributes a) (Domain r a)
    | DomainOp Name [Domain r a]
    | DomainHack a          -- this is an ugly hack to be able to use expressions as domains. will go away later.
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic, Functor)

instance (Serialize r, Serialize a) => Serialize (Domain r a)

instance (Hashable r, Hashable a) => Hashable (Domain r a)

instance (ToJSON r, ToJSON a) => ToJSON (Domain r a)

instance (Arbitrary r, Arbitrary a) => Arbitrary (Domain r a) where
    arbitrary = sized f
        where
            f 0 = oneof [ return DomainBool
                        , DomainInt <$> arbitrary
                        -- , DomainEnum <$> arbitrary <*> arbitrary
                        ]
            f s = do
                arity <- choose (2 :: Int, 10)
                DomainTuple <$> vectorOf arity (f (div s 10))
    shrink DomainBool = []
    shrink (DomainInt []) = [DomainBool]
    shrink (DomainInt [r]) = DomainBool : DomainInt [] : [DomainInt [r'] | r' <- shrink r]
    shrink (DomainInt rs) = [DomainInt (init rs)]
    shrink _ = []

forgetRepr :: Domain r a -> Domain () a
forgetRepr DomainBool = DomainBool
forgetRepr (DomainInt rs) = DomainInt rs
forgetRepr (DomainEnum defn rs) = DomainEnum defn rs
forgetRepr (DomainUnnamed defn) = DomainUnnamed defn
forgetRepr (DomainTuple ds) = DomainTuple (map forgetRepr ds)
forgetRepr (DomainMatrix index inner) = DomainMatrix index (forgetRepr inner)
forgetRepr (DomainSet       _ attr d) = DomainSet () attr (forgetRepr d)
forgetRepr (DomainMSet      _ attr d) = DomainMSet () attr (forgetRepr d)
forgetRepr (DomainFunction  _ attr d1 d2) = DomainFunction () attr (forgetRepr d1) (forgetRepr d2)
forgetRepr (DomainRelation  _ attr ds) = DomainRelation () attr (map forgetRepr ds)
forgetRepr (DomainPartition _ attr d) = DomainPartition () attr (forgetRepr d)
forgetRepr (DomainOp op ds) = DomainOp op (map forgetRepr ds)
forgetRepr (DomainHack a) = DomainHack a

data SetAttr a
    = SetAttrNone
    | SetAttrSize a
    | SetAttrMinSize a
    | SetAttrMaxSize a
    | SetAttrMinMaxSize a a
    | SetAttrDotDot (SetAttr a)
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic, Functor)

instance Serialize a => Serialize (SetAttr a)

instance Hashable a => Hashable (SetAttr a)

instance ToJSON a => ToJSON (SetAttr a)

instance Default (SetAttr a) where
    def = SetAttrNone


data DomainAttributes a = DomainAttributes [DomainAttribute a]
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic, Functor)

instance Serialize a => Serialize (DomainAttributes a)

instance Hashable a => Hashable (DomainAttributes a)

instance ToJSON a => ToJSON (DomainAttributes a)

instance Default (DomainAttributes a) where
    def = DomainAttributes []


data DomainAttribute a
    = DAName Name
    | DANameValue Name a
    | DADotDot
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic, Functor)

instance Serialize a => Serialize (DomainAttribute a)

instance Hashable a => Hashable (DomainAttribute a)

instance ToJSON a => ToJSON (DomainAttribute a)


data Range a
    = RangeOpen
    | RangeSingle a
    | RangeLowerBounded a
    | RangeUpperBounded a
    | RangeBounded a a
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic, Functor)

instance Serialize a => Serialize (Range a)

instance Hashable a => Hashable (Range a)

instance ToJSON a => ToJSON (Range a)

instance Arbitrary a => Arbitrary (Range a) where
    arbitrary = oneof
        [ return RangeOpen
        , RangeSingle <$> arbitrary
        , RangeLowerBounded <$> arbitrary
        , RangeUpperBounded <$> arbitrary
        , RangeBounded <$> arbitrary <*> arbitrary
        ]

rangeInts :: MonadError Doc m => Range Constant -> m [Int]
rangeInts (RangeSingle (ConstantInt x)) = return [x]
rangeInts (RangeBounded (ConstantInt x) (ConstantInt y)) = return [x .. y]
rangeInts _ = throwError "Infinite range (or not an integer range)"

rangesInts :: MonadError Doc m => [Range Constant] -> m [Int]
rangesInts = liftM (sortNub . concat) . mapM rangeInts


data HasRepresentation = NoRepresentation | HasRepresentation Name
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize HasRepresentation

instance Hashable HasRepresentation

instance ToJSON HasRepresentation

instance IsString HasRepresentation where
    fromString = HasRepresentation . Name . T.pack


data Type
    = TypeBool
    | TypeInt
    | TypeEnum DomainDefnEnum
    | TypeUnnamed DomainDefnUnnamed
    | TypeTuple [Type]
    | TypeMatrix Type Type
    | TypeSet       Type
    | TypeMSet      Type
    | TypeFunction  Type Type
    | TypeRelation  [Type]
    | TypePartition Type
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize Type

instance Hashable Type

instance ToJSON Type


data Constant
    = ConstantBool Bool
    | ConstantInt Int
    | ConstantEnum DomainDefnEnum Name
    | ConstantTuple [Constant]
    | ConstantMatrix (Domain () Constant) [Constant]
    | ConstantSet [Constant]
    | ConstantMSet [Constant]
    | ConstantFunction [(Constant, Constant)]
    | ConstantRelation [[Constant]]
    | ConstantPartition [[Constant]]
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Serialize Constant

instance Hashable Constant

instance ToJSON Constant

instance Arbitrary Constant where
    arbitrary = oneof
        [ ConstantBool <$> arbitrary
        , ConstantInt <$> arbitrary
        ]


identifierSplit :: Text -> (Text, Maybe Text, Maybe Text)
identifierSplit t =
    case T.splitOn "§" t of
        [base, rest] -> case T.splitOn "#" rest of
            [region, repr] -> (base, Just region, Just repr)
            _              -> (base, Just rest  , Nothing  )
        _            -> case T.splitOn "#" t of
            [base  , repr] -> (base, Nothing    , Just repr)
            _              -> (t   , Nothing    , Nothing  )

identifierConstruct :: Text -> Maybe Text -> Maybe Text -> Text
identifierConstruct base mregion mrepr =
    mconcat [ base
            , maybe mempty ("§" `mappend`) mregion
            , maybe mempty ("#" `mappend`) mrepr
            ]

identifierStripRegion :: Text -> Text
identifierStripRegion t =
    let (base, _, refn) = identifierSplit t
    in  identifierConstruct base Nothing refn

listAsStatement :: [E] -> E
listAsStatement []     = EOF
listAsStatement (x:xs) = StatementAndNext x (listAsStatement xs)
      
statementAsList :: E -> [E]
statementAsList EOF = []
statementAsList (StatementAndNext this next) =
    if this /= EOF
        then this : statementAsList next
        else        statementAsList next
statementAsList x = [x]


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

gdepth :: Uniplate a => a -> Int
gdepth = (1+) . maximum . map gdepth . children

replace :: (Uniplate a, Eq a) => a -> a -> a -> a
replace old new = transform $ \ i -> if i == old then new else i

replaceAll :: (Uniplate a, Eq a) => [(a, a)] -> a -> a
replaceAll [] x = x
replaceAll ((old,new):rest) x = replaceAll rest $ replace old new x

prettyAsTree :: E -> Doc
prettyAsTree (Prim p) = pretty p
prettyAsTree (Tagged tag xs) = pretty tag `hang` 4 $ vcat (map (nest 4 . prettyAsTree) xs)
prettyAsTree (C x) = "C" `hang` 4 $ pretty $ show x
prettyAsTree (D x) = "D" `hang` 4 $ pretty $ show x
prettyAsTree EOF = "EOF"
prettyAsTree (StatementAndNext a b) = vcat [prettyAsTree a, "", prettyAsTree b]

prettyAsPaths :: E -> Doc
prettyAsPaths (D d) = "D" `hang` 4 $ pretty $ show d
prettyAsPaths EOF = "EOF"
prettyAsPaths (StatementAndNext a b) = vcat [prettyAsPaths a, "", prettyAsPaths b]
prettyAsPaths e = (vcat . map pOne . toPaths) e
    where
        pOne (ts,Nothing) = hcat (map pretty $ intersperse "." $ map pretty ts) <+> ":= []"
        pOne (ts,Just p ) = hcat (map pretty $ intersperse "." $ map pretty ts) <+> ":=" <+> pretty p

        toPaths :: E -> [([Tag], Maybe BuiltIn)]
        toPaths (Prim p) = [([], Just p)]
        toPaths (Tagged s []) = [([s],Nothing)]
        toPaths (Tagged s xs) = map (first (s:)) (concatMap toPaths xs)
        toPaths C {} = bug "prettyAsPaths.toPaths C"
        toPaths D {} = bug "prettyAsPaths.toPaths D"
        toPaths EOF {} = bug "prettyAsPaths.toPaths EOF"
        toPaths StatementAndNext {} = bug "prettyAsPaths.toPaths StatementAndNext"


mkTaggedTH :: [String] -> Exp -> Exp
mkTaggedTH []     _ = error "mkTaggedTH"
mkTaggedTH [t]    g = AppE (AppE (ConE (mkName "Tagged")) (LitE (StringL t))) g
mkTaggedTH (t:ts) g = AppE (AppE (ConE (mkName "Tagged")) (LitE (StringL t))) (ListE [mkTaggedTH ts g])

mergeTaggedTH :: [Exp] -> [Exp]
mergeTaggedTH []     = error "mergeTaggedTH"
mergeTaggedTH [g]    = [g]
mergeTaggedTH gs = 
    let
        extract (AppE (AppE (ConE conName) (LitE (StringL t))) (ListE xs))
            | conName == mkName "Tagged"
            = (t, xs)
        extract (AppE (AppE (ConE conName) (LitE (StringL t))) xs)
            | conName == mkName "Tagged"
            = (t, [xs])
        extract x = error $ "extract: " ++ show x
        gs'    = map extract gs
        tag    = fst $ head gs'
        merged = mergeTaggedTH (concatMap snd gs')
    in  if all (tag==) (map fst gs')
            then [AppE (AppE (ConE (mkName "Tagged")) (LitE (StringL tag))) (ListE merged)]
            else gs

strip :: String -> String
strip = filter (`notElem` " \n\t")

stripComments :: String -> String
stripComments = unlines . map stripComment . lines
    where
        stripComment :: String -> String
        stripComment ('-':'-':' ':_) = ""
        stripComment "" = ""
        stripComment (x:xs) = x : stripComment xs

qq :: QuasiQuoter
qq = QuasiQuoter { quoteExp  = error "not implemented"
                 , quoteType = error "not implemented"
                 , quotePat  = error "not implemented"
                 , quoteDec  = error "not implemented"
                 }

xMatch :: QuasiQuoter
xMatch = qq {
    quotePat = \ inp -> do
        let
            inps :: [String]
            inps = splitOn "|" (stripComments inp)

            each :: String -> Q (Exp, Pat)
            each i = case splitOn ":=" i of
                [patternS, tag] -> do
                    let tags = splitOn "." $ strip tag
                    case parsePat patternS of
                        Left  e -> error $ "Malformed expression: " ++ e
                        Right p -> do
                            tags' <- [e| tags |]
                            return (tags', p)
                _ -> error $ "Malformed expression: " ++ i

        xs <- mapM each inps
        let lhs = AppE (VarE  $ mkName "viewTaggeds")
                       (ListE $ map fst xs)
        let rhs = ListP $ map (\ (_,i) -> ConP (mkName "Just") [i] ) xs
        return (ViewP lhs rhs)
    }

xMake :: QuasiQuoter
xMake = qq {
    quoteExp = \ inp -> do
        let
            inps :: [String]
            inps = splitOn "|" (stripComments inp)

            each :: String -> Q Exp
            each i = case splitOn ":=" i of
                [lhs,rhs] -> do
                    let stripped = strip lhs
                    let tags = map strip $ splitOn "." stripped
                    case parseExp rhs of
                        Left  e -> error $ "Malformed expression: " ++ e
                        Right x -> return $ mkTaggedTH tags x
                _ -> error $ "Malformed expression: " ++ i

        xs <- mapM each inps
        case mergeTaggedTH xs of
            [x] -> return x
            _   -> error "These do not seem to have a common root."
    }

viewTagged :: [Tag] -> E -> Maybe [E]
viewTagged [] g = Just [g]
viewTagged [t] (Tagged i []) | t == i = Just []
viewTagged (t:ts) (Tagged i xs) | t == i = do
    let justs = filter isJust $ map (viewTagged ts) xs
    if null justs
        then Nothing
        else return (concatMap fromJust justs)
viewTagged _ _ = Nothing

viewTaggeds :: [[Tag]] -> E -> [Maybe [E]]
viewTaggeds as g = map (`viewTagged` g) as

