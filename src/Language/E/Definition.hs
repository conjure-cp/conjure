{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    , transform
    , rewrite, rewriteM
    , replace, replaceAll, gdepth

    , qq, xMake, xMatch
    , viewTagged, viewTaggeds
    , prettyAsTree, prettyAsPaths

    , statementAsList, listAsStatement

    ) where

import Conjure.Prelude
import Conjure.Bug
import Stuff.Generic.Tag
import Conjure.Language.Pretty
import Stuff.MetaVariable
import Conjure.Language.Definition
    ( Domain(..), Range(..)
    , DomainDefnEnum(..), DomainDefnUnnamed(..)
    , DomainAttributes(..), DomainAttribute(..), SetAttr(..)
    , Type(..)
    , Constant(..)
    , HasRepresentation(..)
    , Name(..)
    , LanguageVersion(..)
    )

import Data.Maybe ( fromJust )
import GHC.Generics ( Generic )

-- text
import qualified Data.Text as T

-- aeson
import Data.Aeson ( (.=) )
import qualified Data.Aeson as JSON

-- uniplate
import Data.Generics.Uniplate.Data ( Uniplate, universe, children, descend, descendM, transform, rewrite, rewriteM )

-- template-haskell
import Language.Haskell.TH ( Q, Exp(..), Pat(..), Lit(..), mkName )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )

-- haskell-src-meta
import Language.Haskell.Meta.Parse.Careful

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose )


data Spec = Spec LanguageVersion E
    deriving (Eq, Ord, Show, GHC.Generics.Generic)

instance Serialize Spec

instance Hashable Spec

instance Default Spec where
    def = Spec (LanguageVersion "Essence" [1,3]) EOF


data RulesDB = RulesDB { reprRules :: [RuleRepr], refnRules :: [RuleRefn] }
    deriving (Eq, Ord, Show, GHC.Generics.Generic)

instance Serialize RulesDB

instance Hashable RulesDB


data RuleRefn = RuleRefn
    { ruleRefnName  :: Name
    , ruleRefnLevel :: Maybe Int
    , ruleRefnPattern :: E
    , ruleRefnTemplates :: [E]
    , ruleRefnLocals :: [E]
    }
    deriving (Eq, Ord, Show, GHC.Generics.Generic)

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
    deriving (Eq, Ord, Show, GHC.Generics.Generic)

instance Serialize RuleRepr

instance Hashable RuleRepr


data RuleReprCase = RuleReprCase
    { ruleReprCaseDomainIn :: Domain () E   -- domain in.
    , ruleReprCaseStructural :: Maybe E     -- structural constraints
    , ruleReprCaseLocals :: [E]             -- locals
    }
    deriving (Eq, Ord, Show, GHC.Generics.Generic)

instance Serialize RuleReprCase

instance Hashable RuleReprCase


data RuleReprResult = RuleReprResult
    { ruleReprResultOriginalDecl :: E               -- original declaration
    , ruleReprResultRuleName :: Name                -- rule name
    , ruleReprResultReprName :: Name                -- name of the representation
    , ruleReprResultReplacementDom :: Domain () E   -- replacement domain
    , ruleReprResultStructurals :: [E]              -- structural constraints
    }
    deriving (Eq, Ord, Show, GHC.Generics.Generic)

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
        pOne :: ([Tag], Maybe Doc) -> Doc
        pOne (ts,Nothing) = hcat (map pretty $ intersperse "." $ map pretty ts) <+> ":= []"
        pOne (ts,Just p ) = hcat (map pretty $ intersperse "." $ map pretty ts) <+> ":=" <+> p

        toPaths :: E -> [([Tag], Maybe Doc)]
        toPaths (Prim p) = [([], Just (pretty p))]
        toPaths (Tagged s []) = [([s],Nothing)]
        toPaths (Tagged s xs) = map (first (s:)) (concatMap toPaths xs)
        toPaths (C x) = [([], Just $ pretty $ show x)]
        toPaths (D x) = [([], Just $ pretty $ show x)]
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

