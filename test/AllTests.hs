{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import           Control.Applicative
import           Control.Monad ( forM )
import           Control.Monad.Error ( runErrorT )
import           Control.Monad.State ( evalStateT )
import           Control.Monad.Writer ( runWriterT )
import           Data.List ( isSuffixOf )
import qualified Data.Map as M
import           Test.Framework ( defaultMain )
import           Test.Framework.Providers.HUnit ( hUnitTestToTests )
-- import           Test.Framework.Providers.QuickCheck2 ( testProperty )
import           Test.HUnit ( assertEqual, assertFailure, test )
import qualified Test.HUnit as HUnit
import           System.Directory ( getDirectoryContents )

import Constants ( FreshName, mkFreshNames, newRuleVar )
import GenericOps.Core ( GNode, GPlate, mkG, GNode, BindingsMap )
import ParsecUtils ( Parser, eof, parseEither, unsafeParse )
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( renderDoc )

import Language.Essence
import Language.EssenceEvaluator ( deepSimplify )
import Language.Essence.Phases.ReadIn ( ReadIn, runReadIn )



noParse :: forall a . (Eq a, ParsePrint a) => a -> String -> HUnit.Test
noParse _ s = HUnit.TestLabel ("NoParse " ++ s) $ HUnit.TestCase $
    case parseEither (parse <* eof) s of
        Left _         -> return ()
        Right (_ :: a) -> assertFailure ("NoParse: " ++ s)

shouldParseTo :: (Eq a, Show a, ParsePrint a) => String -> a -> HUnit.Test
shouldParseTo s x = HUnit.TestLabel ("ShouldParseTo " ++ s) $ HUnit.TestCase $
    case parseEither (parse <* eof) s of
        Left msg -> assertFailure ("ShouldParseTo [cannot parse]: " ++ s ++ msg)
        Right x' -> assertEqual "ShouldParseTo [parses to a different thing]" x x'

parsePrintIso :: forall a . (Eq a, Show a, ParsePrint a) => a -> String -> HUnit.Test
parsePrintIso _ s = HUnit.TestLabel ("ParsePrintIso " ++ s) $ HUnit.TestCase $
    case parseEither (parse <* eof) s of
        Left msg        -> assertFailure ("ParsePrintIso [cannot parse]: " ++ s ++ msg)
        Right (x' :: a) -> let s' = renderDoc $ pretty x'
                           in  case parseEither parse s' of
            Left msg  -> assertFailure ("ParsePrintIso [cannot parse pretty print]: " ++ s' ++ msg)
            Right x'' -> assertEqual "ParsePrintIso [not equal]" x' x''

parsePrintIsoFile :: forall a . (Eq a, Show a, ParsePrint a, ReadIn a) => a -> FilePath -> HUnit.Test
parsePrintIsoFile _ f = HUnit.TestLabel ("ParsePrintIsoFile " ++ f) $ HUnit.TestCase $ do
    s <- readFile f
    case fst $ runReadIn f s of
        Left msg        -> assertFailure (unlines ["ParsePrintIsoFile [cannot parse]: ", f, s, show msg])
        Right (x' :: a) -> let s' = renderDoc $ pretty x'
                               f' = f ++ " [after ParsePrint]"
                           in  case fst $ runReadIn f' s' of
            Left msg  -> assertFailure (unlines ["ParsePrintIsoFile [cannot parse pretty print]: ", f', s', show msg])
            Right x'' -> assertEqual "ParsePrintIso [not equal]" x' x''

eval :: [(String,GNode)] -> String -> String -> HUnit.Test
eval bindings px py = HUnit.TestLabel ("Eval " ++ px ++ " ~~ " ++ py) $ HUnit.TestCase $ do
    case (parseEither (parse <* eof) px, parseEither (parse <* eof) py) of
        (Left msg, _) -> assertFailure (unlines ["Eval [cannot parse]", px, msg])
        (_, Left msg) -> assertFailure (unlines ["Eval [cannot parse]", py, msg])
        (Right (x :: Expr), Right (y :: Expr)) -> do
            (x',_logs) <- runWriterT $ flip evalStateT ( M.fromList bindings :: BindingsMap
                                                       , [] :: [GNode]
                                                       , [] :: [(GNode,GNode)]
                                                       ) $ runErrorT $ deepSimplify x
            (y',_logs) <- runWriterT $ flip evalStateT ( M.fromList bindings :: BindingsMap
                                                       , [] :: [GNode]
                                                       , [] :: [(GNode,GNode)]
                                                       ) $ runErrorT $ deepSimplify y
            case (x',y') of
                (Left msg, _) -> assertFailure (unlines ["Eval [simplification]", renderDoc $ pretty x, renderDoc msg])
                (_, Left msg) -> assertFailure (unlines ["Eval [simplification]", renderDoc $ pretty y, renderDoc msg])
                (Right x'', Right y'') -> assertEqual "Eval [not equal]" x'' y''

applyRuleRefn :: String -> String -> [String] -> HUnit.Test
applyRuleRefn ruleRefn expr result = HUnit.TestLabel (unlines ["ApplyRuleRefn", ruleRefn, expr]) $ HUnit.TestCase $ do
    case (parseEither (parse <* eof) ruleRefn, parseEither (parse <* eof) expr) of
        (Left msg, _) -> assertFailure (unlines ["ApplyRuleRefn [cannot parse]", ruleRefn, msg])
        (_, Left msg) -> assertFailure (unlines ["ApplyRuleRefn [cannot parse]", expr, msg])
        (Right r, Right x) -> do
            let r' = scopeIdentifiers newRuleVar r
            (mxs,_) <- runWriterT $ flip evalStateT ( M.empty         :: BindingsMap
                                                    , mkFreshNames [] :: [FreshName]
                                                    ) $ runErrorT $ applyRefnsDeep [r'] x
            putStr " == BEFORE ==> "
            print r
            putStr " == AFTER  ==> "
            print r'
            case (mxs, result) of
                (Left _  , []) -> return ()
                (Left msg, _ ) -> assertFailure (unlines ["ApplyRuleRefn [rule not applied, was expected to.]", ruleRefn, expr, show msg])
                -- (Right [], []) -> return ()
                -- (Right [], _ ) -> assertFailure (unlines ["ApplyRuleRefn [rule not applied, was expected to.]", ruleRefn, expr])
                (Right xs, []) -> assertFailure (unlines (["ApplyRuleRefn [rule applied, was expected not to.]", ruleRefn, expr, "{Results}"] ++ map (renderDoc . pretty) xs))
                (Right xs, _ ) -> do
                    ys <- forM result $ \ s -> case parseEither (parse <* eof) s of
                                                    Left msg -> assertFailure (unlines ["ApplyRuleRefn [cannot parse]", s, msg]) >> return undefined
                                                    Right y  -> return y
                    assertEqual "ApplyRuleRefn [not equal]" ys (xs :: [Expr])

applyRuleRefns :: [String] -> String -> [String] -> HUnit.Test
applyRuleRefns ruleRefns expr result = HUnit.TestLabel (unlines ("ApplyRuleRefn" : expr : ruleRefns)) $ HUnit.TestCase $ do
    rs <- forM ruleRefns $ \ ruleRefn -> do
        case parseEither (parse <* eof) ruleRefn of
            Left msg -> assertFailure (unlines ["ApplyRuleRefn [cannot parse]", ruleRefn, msg]) >> return undefined
            Right r  -> return r
    case parseEither (parse <* eof) expr of
        Left msg -> assertFailure (unlines ["ApplyRuleRefn [cannot parse]", expr, msg])
        Right x  -> do
            let rs' = map (scopeIdentifiers newRuleVar) rs
            (mxs,_) <- runWriterT $ flip evalStateT ( M.empty         :: BindingsMap
                                                    , mkFreshNames [] :: [FreshName]
                                                    ) $ runErrorT $ applyRefnsDeep rs' x
            case (mxs, result) of
                (Left _  , []) -> return ()
                (Left msg, _ ) -> assertFailure (unlines ["ApplyRuleRefn [rule not applied, was expected to.]", unlines ruleRefns, expr, show msg])
                -- (Right [], []) -> return ()
                -- (Right [], _ ) -> assertFailure (unlines ["ApplyRuleRefn [rule not applied, was expected to.]", ruleRefn, expr])
                (Right xs, []) -> assertFailure (unlines (["ApplyRuleRefn [rule applied, was expected not to.]", unlines ruleRefns, expr, "{Results}"] ++ map (renderDoc . pretty) xs))
                (Right xs, _ ) -> do
                    ys <- forM result $ \ s -> case parseEither (parse <* eof) s of
                                                    Left msg -> assertFailure (unlines ["ApplyRuleRefn [cannot parse]", s, msg]) >> return undefined
                                                    Right y  -> return y
                    assertEqual "ApplyRuleRefn [not equal]" ys (xs :: [Expr])


gnode :: forall a . (GPlate a, ParsePrint a) => a -> String -> GNode
gnode _ = mkG . unsafeParse (parse <* eof :: Parser a)

-- runParsePrintUnitTest (Eval bindings px py) = HUnit.TestLabel ("Eval " ++ px ++ " ~~ " ++ py) $ HUnit.TestCase $ do
--     case (parseEither (parse <* eof) px, parseEither (parse <* eof) py) of
--         (Left msg, _) -> assertFailure (unlines ["Eval [cannot parse]", px, msg])
--         (_, Left msg) -> assertFailure (unlines ["Eval [cannot parse]", py, msg])
--         (Right (x :: a), Right (y :: a)) -> do
--             (x',_logs) <- runWriterT $ flip evalStateT (M.fromList bindings) $ runErrorT $ deepSimplify x
--             case x' of
--                 Left msg  -> assertFailure (unlines ["Eval [simplification]", renderDoc (pretty x)])
--                 Right x'' -> assertEqual "Eval [not equal]" x'' y

getAllWithSuffix :: String -> FilePath -> IO [FilePath]
getAllWithSuffix suffix fp = map (fp++) . filter (suffix `isSuffixOf`) <$> getDirectoryContents fp

main :: IO ()
main = do
    -- let
    --     basedir :: FilePath
    --     basedir = "/Users/ozgurakgun/src/conjure-wd/"
    specs <- getAllWithSuffix ".essence" "testdata/"
    defaultMain $ hUnitTestToTests . test $ parsingExpr
                                         ++ parsingValue
                                         ++ parsingQuantifiedExpr
                                         ++ parsingIdentifier
                                         ++ parsingWhere
                                         ++ parsingLambda
                                         ++ parsingType
                                         ++ parsingDomain
                                         ++ parsingSpec specs
                                            -- ++ parsingRepr
                                            -- ++ parsingRefn

    where

        noParse_Expr                 = noParse           ( undefined :: Expr )
        parsePrintIso_Expr           = parsePrintIso     ( undefined :: Expr )
        gnode_Expr                   = gnode             ( undefined :: Expr )
        parsePrintIso_Value          = parsePrintIso     ( undefined :: Value )
        parsePrintIso_QuantifiedExpr = parsePrintIso     ( undefined :: QuantifiedExpr )
        noParse_Identifier           = noParse           ( undefined :: Identifier )
        parsePrintIso_Identifier     = parsePrintIso     ( undefined :: Identifier )
        noParse_Where                = noParse           ( undefined :: [Where] )
        parsePrintIso_Where          = parsePrintIso     ( undefined :: [Where] )
        noParse_Lambda               = noParse           ( undefined :: Lambda )
        parsePrintIso_Lambda         = parsePrintIso     ( undefined :: Lambda )
        parsePrintIso_Type           = parsePrintIso     ( undefined :: Type )
        noParse_Domain               = noParse           ( undefined :: Domain )
        parsePrintIso_Domain         = parsePrintIso     ( undefined :: Domain )
        parsePrintIsoFile_Spec       = parsePrintIsoFile ( undefined :: Spec )
        parsePrintIsoFile_Repr       = parsePrintIsoFile ( undefined :: RuleRepr )
        parsePrintIsoFile_Refn       = parsePrintIsoFile ( undefined :: RuleRefn )

        parsingExpr =
            [ noParse_Expr       "--x"
            , parsePrintIso_Expr "-x"
            , shouldParseTo      "x" $ EHole "x"
            , shouldParseTo      "1" $ V $ VInt 1
            , parsePrintIso_Expr "1+2"
            , parsePrintIso_Expr "1+2+3"
            , parsePrintIso_Expr "1+2+3+x-y"
            , parsePrintIso_Expr "a+b*c"
            , parsePrintIso_Expr "(a+b)*c"
            , parsePrintIso_Expr "a+(b*c)"
            , parsePrintIso_Expr "a[i]"
            , parsePrintIso_Expr "a[i][j]"
            , parsePrintIso_Expr "a[i,j]"
            , parsePrintIso_Expr "a[i][j][k]"
            , parsePrintIso_Expr "a[i,j,k]"
            , parsePrintIso_Expr "a[i,j][k]"
            , parsePrintIso_Expr "a[i][j,k]"
            , noParse_Expr       "[1,,]"
            , parsePrintIso_Expr "1"
            , parsePrintIso_Expr "2"
            , parsePrintIso_Expr "false"
            , parsePrintIso_Expr "true"
            , parsePrintIso_Expr "[1, 2, 3]"
            , parsePrintIso_Expr "[1, 2, 3, false, 4]"
            , parsePrintIso_Expr "[]"
            , parsePrintIso_Expr "[[]]"
            , parsePrintIso_Expr "[[1, 2, 3], [4, 5, 6]]"
            , parsePrintIso_Expr "[[1, 2, 3], [true, false]]"
            , parsePrintIso_Expr "(1, 2)"
            , parsePrintIso_Expr "(1, 2, 3, [1, 2, 3])"
            , parsePrintIso_Expr "(true, 1, (false, 2))"
            , parsePrintIso_Expr "{}"
            , parsePrintIso_Expr "{1}"
            , parsePrintIso_Expr "{a}"
            , parsePrintIso_Expr "{1, 2, true, false, (1, 2, 3)}"
            , parsePrintIso_Expr "{{1, 2, 3}, {1, 3, 5}, {2, 4, 6}}"
            , parsePrintIso_Expr "{{1, 2, 3}, {1, 3, 5}, {2, 4, 6}, {true, false}}"
            , parsePrintIso_Expr "mset(1)"
            , parsePrintIso_Expr "mset(a)"
            , parsePrintIso_Expr "mset(mset())"
            , parsePrintIso_Expr "mset(1, 2, true, false, (1, 2, 3))"
            , parsePrintIso_Expr "mset({1, 2, 3}, {1, 3, 5}, {2, 4, 6})"
            , parsePrintIso_Expr "function()"
            , parsePrintIso_Expr "function(1 --> 2)"
            , parsePrintIso_Expr "function(1 --> 2, 3 --> 4, 5 --> 6, 7 --> 8)"
            , parsePrintIso_Expr "function(1 --> {2}, 3 --> mset(4), 5 --> function (6 --> 6), 7 --> (false, true, 4))"
            , parsePrintIso_Expr "relation()"
            , parsePrintIso_Expr "relation((1, a))"
            , parsePrintIso_Expr "relation((1, a), (2, b))"
            , parsePrintIso_Expr "relation((1, {a}), (mset(2, 3, 4), b))"
            , parsePrintIso_Expr "partition()"
            , parsePrintIso_Expr "partition({}, {1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3})"
            , parsePrintIso_Expr "partition({1, 2, 3}, {4, 5, 6})"
            , parsePrintIso_Expr "1 + 2 + 3"
            , parsePrintIso_Expr "1 * 2 + 3 * 4"
            , parsePrintIso_Expr "1 * (2 + 3) * 4"
            , parsePrintIso_Expr "1 * (2 + 3) * 4 = 5"
            , noParse_Expr       "1 = 2 = 3"
            , parsePrintIso_Expr "1 = 2 <-> 2 = 3"
            , parsePrintIso_Expr "(1 = 2) <-> (2 = 3)"
            , noParse_Expr       "1 = (2 <-> 2) = 3"
            , parsePrintIso_Expr "-1 + 3 = a"
            , parsePrintIso_Expr "-a = -b + 1"
            , parsePrintIso_Expr "|a|"
            , parsePrintIso_Expr "2 ** 4 >= 2 ** 3 /\\ 2 ** 4 <= 2 ** 5 -> 2 ** 3 <= 2 ** 5"
            , parsePrintIso_Expr "true /\\ false"
            , parsePrintIso_Expr "true /\\ false -> false"
            , parsePrintIso_Expr "(1 + 2) * 3 = 10 - 2 + 1"
            , parsePrintIso_Expr "1 + 2 * 3 = 10 - 2 + 1"
            , parsePrintIso_Expr "forAll i : s , g . k"
            , parsePrintIso_Expr "forAll i : s . k"
            , parsePrintIso_Expr "(a = b) > c"
            , parsePrintIso_Expr "a = (b > c)"
            , parsePrintIso_Expr "-a > x"
            , parsePrintIso_Expr "(-a) > x"
            , parsePrintIso_Expr "!a > x"
            , parsePrintIso_Expr "(!a) > x"
            , parsePrintIso_Expr "_"
            , shouldParseTo      "_" $ EHole "_"
            , parsePrintIso_Expr "!(a > x)"
            , parsePrintIso_Expr "f(i)"
            , shouldParseTo      "f(i)" $ EOp Image ["f","i"]
            , parsePrintIso_Expr "f(i,j)"
            , shouldParseTo      "f(i,j)" $ EOp Image [ "f"
                                                      , V (VTuple ["i","j"])
                                                      ]
            , parsePrintIso_Expr "f'(i)"
            , shouldParseTo      "f'(i)" $ EOp Image ["f'","i"]
            , shouldParseTo      "preimage(f,i)" $ EOp PreImage ["f","i"]
            , parsePrintIso_Expr "a {b --> c}"
            , shouldParseTo      "a {b --> c}" $ EOp Replace ["a","b","c"]

            , shouldParseTo "m[i]" $ EOp Index ["m","i"]
            , shouldParseTo "m[i,j]" $ EOp Index [ EOp Index [ "m"
                                                             , "i"
                                                             ]
                                                 , "j"
                                                 ]
            , shouldParseTo "m[i,j,k]" $ EOp Index [ EOp Index [ EOp Index [ "m"
                                                                           , "i"
                                                                           ]
                                                               , "j"
                                                               ]
                                                   , "k"
                                                   ]
            , parsePrintIso_Expr "m[i]"
            , parsePrintIso_Expr "m[i, j]"
            , parsePrintIso_Expr "m[i, j, k]"
            , eval [] "m[1+2,3+i,i+3]" "m[3, 3+i, i+3]"
            , parsePrintIso_Expr "x!"
            , parsePrintIso_Expr "(x)!"
            ]

        parsingValue =
            [ parsePrintIso_Value "1"
            , shouldParseTo       "false" $ VBool False
            , parsePrintIso_Value "false"
            , shouldParseTo       "true"  $ VBool True
            , parsePrintIso_Value "true"
            , parsePrintIso_Value "{1,2,3}"
            , parsePrintIso_Value "{1,2+3,4-5}"
            , parsePrintIso_Value "mset (1,2,3)"
            , parsePrintIso_Value "(1,2,3)"
            , parsePrintIso_Value "tuple (1,2,3)"
            , parsePrintIso_Value "function(1-->2,2-->3,3-->4)"
            , parsePrintIso_Value "relation((1,2,3),(1,2,4),(2,3,4))"
            , parsePrintIso_Value "partition({1,2},{3,4},{x,y})"
            ]

        parsingQuantifiedExpr =
            [ parsePrintIso_QuantifiedExpr "forAll i : s . k"
            , parsePrintIso_QuantifiedExpr "exists i : s . sum k : i . k = t"
            , parsePrintIso_QuantifiedExpr "exists i : s . sum k : i . (k = t)"
            , parsePrintIso_QuantifiedExpr "exists i : s . (sum k : i . k = t)"
            , parsePrintIso_QuantifiedExpr "exists i : s . (sum k : i . k) = t"
            , parsePrintIso_QuantifiedExpr "forAll i in s, i % 2 = 0 . i in k"
            , parsePrintIso_QuantifiedExpr "forAll i : int(0..9) in s . i in k"
            , parsePrintIso_QuantifiedExpr "forAll i : set of int(0..9) subsetEq s . i in k"
            ]

        parsingIdentifier =
            [ noParse_Identifier       ""
            , noParse_Identifier       "a[i]"
            , noParse_Identifier       "a+b"
            , shouldParseTo            "i" $ Identifier "i"
            , parsePrintIso_Identifier "i"
            , parsePrintIso_Identifier "_i"
            , parsePrintIso_Identifier "_"
            ]

        parsingWhere =
            [ noParse_Where       ""
            , noParse_Where       "x+y=z"
            , noParse_Where       "wher k"
            , parsePrintIso_Where "where k"
            , parsePrintIso_Where "where k,l"
            , parsePrintIso_Where "where x+y=z,l"
            ]

        parsingLambda =
            [ parsePrintIso_Lambda "{ x:int, y:bool --> x % 2 = 0 <-> y }"
            , parsePrintIso_Lambda "{ x:int --> x % 2 }"
            , noParse_Lambda       "lambda { x:int --> x % 2 }"
            , noParse_Lambda       "{}{ x:int --> x % 2 }}"
            , noParse_Lambda       ""
            ]

        parsingSpec fs = map parsePrintIsoFile_Spec fs

        parsingRepr =
            [ parsePrintIsoFile_Repr "rules/repr/Function.AsReln.repr"
            , parsePrintIsoFile_Repr "rules/repr/Function.Matrix1D.repr"
            , parsePrintIsoFile_Repr "rules/repr/Function.Matrix2D.repr"
            , parsePrintIsoFile_Repr "rules/repr/MSet.Explicit.repr"
            , parsePrintIsoFile_Repr "rules/repr/Partition.SetOfSets_KnownPartSize.repr"
            , parsePrintIsoFile_Repr "rules/repr/Partition.SetOfSets_KnownSize.repr"
            , parsePrintIsoFile_Repr "rules/repr/Relation.SetOfTuples2.repr"
            , parsePrintIsoFile_Repr "rules/repr/Set.Explicit(IntOnly).repr"
            , parsePrintIsoFile_Repr "rules/repr/Set.Explicit.repr"
            , parsePrintIsoFile_Repr "rules/repr/Set.ExplicitVarSize(IntOnly).repr"
            , parsePrintIsoFile_Repr "rules/repr/Set.ExplicitVarSize.repr"
            , parsePrintIsoFile_Repr "rules/repr/Set.Gent.repr"
            , parsePrintIsoFile_Repr "rules/repr/Set.Occurrence.repr"
            ]

        parsingRefn =
            [parsePrintIsoFile_Refn "rules/refn/Function.Apply.AsReln.rule"
            , parsePrintIsoFile_Refn "rules/refn/Function.Apply.Matrix1D.rule"
            , parsePrintIsoFile_Refn "rules/refn/Function.Apply.Matrix2D.rule"
            , parsePrintIsoFile_Refn "rules/refn/Function.DefinedQuan.AsReln.rule"
            , parsePrintIsoFile_Refn "rules/refn/Function.Eq.rule"
            , parsePrintIsoFile_Refn "rules/refn/Function.PreImageQuan.AsReln.rule"
            , parsePrintIsoFile_Refn "rules/refn/Function.PreImageQuan.Matrix1D.rule"
            , parsePrintIsoFile_Refn "rules/refn/MSet.Eq.rule"
            , parsePrintIsoFile_Refn "rules/refn/MSet.Quantifier.Explicit.rule"
            , parsePrintIsoFile_Refn "rules/refn/Matrix.Eq.rule"
            , parsePrintIsoFile_Refn "rules/refn/Partition.QuanParts.SetOfSets_KnownSize.rule"
            , parsePrintIsoFile_Refn "rules/refn/Relation.Apply.SetOfTuples2.rule"
            , parsePrintIsoFile_Refn "rules/refn/Relation.Card.SetOfTuples2.rule"
            , parsePrintIsoFile_Refn "rules/refn/Relation.Quantifier.SetOfTuples2.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Card.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Elem.Gent.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Elem.Occurrence.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Elem.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.ElemIntersect.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Eq.Gent.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Eq.Occurrence.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Eq.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Intersect.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Max.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Min.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Neq.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Quantifier.Explicit.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Quantifier.ExplicitVarSize.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Quantifier.Gent.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Quantifier.Occurrence.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.QuantifierUnion.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.SubsetEq.rule"
            , parsePrintIsoFile_Refn "rules/refn/Set.Union.rule"
            , parsePrintIsoFile_Refn "rules/refn/Tuple2.Eq.rule"
            , parsePrintIsoFile_Refn "rules/refn/Tuple2.Neq.rule"
            , parsePrintIsoFile_Refn "rules/refn/alldifferent.rule"
            ]

        parsingType =
            [ parsePrintIso_Type "of size 4"
            , parsePrintIso_Type "of size a"
            , parsePrintIso_Type "of size a + b"
            , parsePrintIso_Type "enum {foo, bar}"
            , parsePrintIso_Type "enum {foo, bar, baz}"
            , parsePrintIso_Type "enum {foo, intt}"
            , parsePrintIso_Type "matrix indexed by [int, int] of enum {foo, bar}"
            , parsePrintIso_Type "tuple (int, fool, tuple (enum {foo, bar}, int))"
            ]

        parsingDomain =
            [ parsePrintIso_Domain "bool"
            , parsePrintIso_Domain "int"
            , parsePrintIso_Domain "int(1)"
            , parsePrintIso_Domain "int(1, 2)"
            , parsePrintIso_Domain "int(1, 2, a)"
            , parsePrintIso_Domain "int(1..)"
            , parsePrintIso_Domain "int(1..20)"
            , parsePrintIso_Domain "int(..20)"
            , noParse_Domain       "int(1...3)"
            , noParse_Domain       "int(1,,2,3)"
            , parsePrintIso_Domain "matrix indexed by [int(1..9)] of bool"
            , parsePrintIso_Domain "matrix indexed by [int(1..9), int(a..), int(..b)] of bool"
            , parsePrintIso_Domain "tuple (int, bool)"
            , parsePrintIso_Domain "(int, bool)"
            , parsePrintIso_Domain "set of int"
            , parsePrintIso_Domain "set of a"
            , parsePrintIso_Domain "set (size n) of a"
            , parsePrintIso_Domain "set (size n) of tuple (a,b)"
            , parsePrintIso_Domain "set (size n) of tuple (a, b)"
            , parsePrintIso_Domain "set (minSize n) of a"
            , parsePrintIso_Domain "set (maxSize n) of a"
            , parsePrintIso_Domain "set (representation foo) of a"
            , parsePrintIso_Domain "set (size n, minSize m, representation foo) of a"
            , parsePrintIso_Domain "set (representation foo) of set of a"
            , parsePrintIso_Domain "set (size n, _) of int"
            , parsePrintIso_Domain "set (_, maxSize n) of int"
            , parsePrintIso_Domain "set (_, maxSize n, _) of int"
            , parsePrintIso_Domain "set (_, maxSize n, _, _) of int"
            , parsePrintIso_Domain "mset of int"
            , parsePrintIso_Domain "mset of a"
            , parsePrintIso_Domain "mset (size n) of a"
            , parsePrintIso_Domain "mset (size n) of tuple (a, b)"
            , parsePrintIso_Domain "mset (minSize n) of a"
            , parsePrintIso_Domain "mset (maxSize n) of a"
            , parsePrintIso_Domain "mset (representation foo) of a"
            , parsePrintIso_Domain "mset (size n, minSize m, representation foo) of a"
            , parsePrintIso_Domain "mset (representation foo) of mset of a"
            , parsePrintIso_Domain "mset (size n, _) of a"
            , parsePrintIso_Domain "mset (_, maxSize n) of int"
            , parsePrintIso_Domain "mset (_, maxSize n, _) of int"
            , parsePrintIso_Domain "mset (_, maxSize n, _,_) of int"
            , parsePrintIso_Domain "mset (maxSize n, _) of int"
            , parsePrintIso_Domain "mset (minOccur m) of int"
            , parsePrintIso_Domain "mset (maxOccur m) of int"
            , parsePrintIso_Domain "function a --> b"
            , parsePrintIso_Domain "function (total) a --> b"
            , parsePrintIso_Domain "function (total, representation foo, _) a --> b"
            , parsePrintIso_Domain "function (total, injective) a --> b"
            , parsePrintIso_Domain "function a --> function b --> c"
            , parsePrintIso_Domain "function function a --> b --> c"
            , parsePrintIso_Domain "relation of ()"
            , parsePrintIso_Domain "relation of (a * b)"
            , parsePrintIso_Domain "relation (representation foo) of (a * b * c)"
            , parsePrintIso_Domain "relation (representation foo, _) of (a * b * c)"
            , parsePrintIso_Domain "relation (representation foo, _) of (int(0..9) * set of a * function c --> d)"
            , parsePrintIso_Domain "partition from int(0..9)"
            , parsePrintIso_Domain "partition (regular) from int(0..9)"
            , parsePrintIso_Domain "partition (regular, numParts 5) from int(0..9)"
            , parsePrintIso_Domain "partition (regular, numParts 5, _) from int(0..9)"
            , parsePrintIso_Domain "partition (regular, maxNumParts 5, representation foo, _) from int(0..9)"
            ]

        evaluating =
            [ eval [] "abs(-2+1)" "1"
            , eval [] "1+2" "3"
            , eval [("x",gnode_Expr "1")] "x+2" "3"
            , eval [] "a+(1+1)" "a+2"
            , eval [] "(a+1)+1" "a+2"
            , eval [] "a+1+1"   "a+2"
            , eval [] "1 + 1" "2"
            , eval [] "1 + 2 * 3" "7"
            , eval [] "(1 + 2) * 3" "9"
            , eval [] "(1 + 2) * 3 = 10 - 2 + 1" "true"
            , eval [] "1 + 2 + a" "3 + a"
            , eval [] "1 + a + 2" "3 + a"
            , eval [] "a + 1 + 2" "a + 3"
            , eval [] "a + 1 + 2" "3 + a"
            -- , eval [] "1 + c + 2 + b + 3 + a" "6 + a + b + c"
            , eval [] "11-11-11" "-11"
            , eval [] "11-(11-11)" "11"
            , eval [] "2 * c * d * 0" "0"
            , eval [] "(2 - c + d) * (2 * 2 - 4)" "0"
            , eval [] "(a - a) * 3" "0"
            -- , eval [] "a * 3 + 2 * b" "2*b + 3*a"
            , eval [] "a * b - b * a" "0"
            , eval [] "1+1+1+1+1+1+1+1+1+1" "10"
            , eval [] "1-1-1-1-1-1-1-1-1-1" "-8"
            , eval [] "1*1*1*1*1*1*1*1*1*1" "1"
            , eval [] "1/1/1/1/1/1/1/1/1/1" "1"
            , eval [] "14 % 8" "6"
            , eval [] "14 % 8 % 4" "2"
            -- , eval [] "1+a+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1" "21 + a"
            -- , eval [] "1+a+1+1+1+1+1+1+1+1+1+1" "11 + a"
            -- , eval [] "1+a+1+1+1+b+1+1+1+1+1+1" "10 + a + b"
            -- , eval [] "1+b+1+1+1+a+1+1+1+1+1+1" "10 + a + b"
            , eval [] "a - a + 1" "1"
            , eval [] "a - a + b" "b"
            , eval [] "a+b=b+a" "true"
            , eval [] "12 + 3 > 11" "true"
            , eval [] "12 + 3 < 11" "false"
            , eval [] "1 in {1,2,3}" "true"
            , eval [] "{1,3,5} union {2,4,6}" "{1,2,3,4,5,6}"
            , eval [] "{1,3,5} intersect {2,4,6}" "{}"
            , eval [] "1+a+2+b*0" "3+a"
            , eval [] "true /\\ false" "false"
            , eval [] "(true /\\ false) -> false" "true"
            , eval [] "((2**4 >= 2**3) /\\ (2**4 <= 2**5)) -> (2**3 <= 2**5)" "true"
            , eval [] "(1 + 2) * 3 = 10 - 2 + 1" "true"
            , eval [] "1 + 2 * 3 = 10 - 2 + 1" "false"
            -- -- , eval [] "a+a+2*a+b*a" "(4 + b) * a"
            -- -- , eval [] "(a+b)*(a+b)+(a+b)**2" "(a+b)**2 * 2"
            , eval [] "!(true -> x)" "!x"
            , eval [] "!true -> x" "true"
            , eval [] "false -> x" "true"
            , eval [] "(!true) -> x" "true"
            ]

        applyingRefn =
            [ applyRuleRefn "x+y ~~> 2*x" "1+3" ["2*1"]
            
            , applyRuleRefn "x+y ~~> 2*x" "1-3" []
            
            , applyRuleRefn "x+y ~~> 2*x where x = y" "5+5" ["2*5"]
            
            , applyRuleRefn "x+y ~~> 2*x where x = y" "5+6" []
            
            , applyRuleRefn (unlines [ "x+y ~~> res"
                                     , "where x = y"
                                     , "letting res be 2 * x"
                                     ])
                                     "5+5" ["2*5"]
            
            , applyRuleRefn (unlines [ "x+y ~~> res"
                                     , "letting theGuard be x = y"
                                     , "where theGuard"
                                     , "where x :: int, y :: int"
                                     , "letting foo be 2"
                                     , "letting bar be x"
                                     , "letting baz be foo * bar"
                                     , "letting res be baz"
                                     ])
                                     "7+7" ["2*7"]
            
            , applyRuleRefn (unlines [ "x+y ~~> res"
                                     , "letting theGuard be x = y"
                                     , "where theGuard"
                                     , "where !(x :: set of _), !(y :: function int --> int)"
                                     , "letting foo be 2"
                                     , "letting bar be x"
                                     , "letting baz be foo * bar"
                                     , "letting res be baz"
                                     ])
                                    "7+7" ["2*7"]
            
            , applyRuleRefn (unlines [ "x+y ~~> res"
                                     , "letting theGuard be x = y"
                                     , "where theGuard"
                                     , "letting foo be 2"
                                     , "letting bar be x"
                                     , "letting baz be foo * bar"
                                     , "letting res be baz"
                                     ])
                                     "a+a" ["2*a"]
            
            , applyRuleRefn (unlines [ "x+y ~~> res"
                                     , "letting theGuard be x = y"
                                     , "where theGuard"
                                     , "letting foo be 2"
                                     , "letting bar be x"
                                     , "letting baz be foo * bar"
                                     , "letting res be baz"
                                     ])
                                     "a+b" []
            
            , applyRuleRefn "x+y ~~> { x * y, y * x }" "a + b" [ "a * b"
                                                               , "b * a" ]
            
            , applyRuleRefn "x+y ~~> { x * y, y * x }" "a - b" []
            
            , applyRuleRefn "x + y ~~> 2 * x where x = y" "2 - (7 + 7)" ["2 - (2 * 7)"]
            
            , applyRuleRefns [ "x + y ~~> 2 * x where x = y" ] "2 - (7 + 7)" ["2 - (2 * 7)"]
            
            , applyRuleRefns [ "x + y ~~> 2 * x where x = y"
                             , "x - y ~~> 0     where x = y"
                             ]
                             "(2 * a) - (a + a)"
                             [ "0" ]
            
            , applyRuleRefns [ "x + y ~~> 2 * x where x = y"
                             , "x - y ~~> 0     where x = y"
                             ]
                             "(2 * x) - (x + x)"
                             [ "0" ]
            
            , applyRuleRefns [ "x + y ~~> 2 * x where x = y"
                             , "x - y ~~> 0     where x = y"
                             ]
                             "(2 * x) - (y + z)"
                             []
            ]






-- -- 
-- -- *** TypeOf
-- --     1 + 2
-- --     ~~ int
-- --     ~~ expression
-- -- 
-- -- *** TypeOf
-- --     2 ** 4 >= 2 ** 3 /\ 2 ** 4 <= 2 ** 5 => 2 ** 3 <= 2 ** 5
-- --     ~~ bool
-- --     ~~ expression
-- -- 
-- -- *** TypeOf { given a : int }
-- --     a ~~ int ~~ given
-- -- 
-- -- *** TypeOf { given a : int
-- --              find b : int
-- --            }
-- --     a = b
-- --     ~~ bool
-- --     ~~ expression
-- -- 
-- -- *** TypeOf { given a,b : int
-- --              find s : set of int }
-- --     set {a,b} subsetEq s
-- --     ~~ bool
-- --     ~~ expression
-- -- 
-- -- *** TypeOf 1                ~~ int        ~~ value
-- -- *** TypeOf int(1..9)        ~~ int        ~~ domain
-- -- *** TypeOf set{1,2,3}       ~~ set of int ~~ value
-- -- *** TypeOf set of int(1..9) ~~ set of int ~~ domain
-- -- 
-- -- *** Eval { letting a be 5
-- --            given b : bool }
-- --     b => a + 8 > 10
-- --     ~~ true
-- -- 



-- -- *** Eval |12+3| ~~ 15
-- -- 
-- -- *** Eval |-12*3| ~~ 36
-- -- 
-- -- *** Eval a /\ true /\ b ~~ a /\ b
-- -- *** Eval (a /\ true) /\ b ~~ a /\ b
-- -- *** Eval a /\ (true /\ b) ~~ a /\ b
-- -- 
-- -- *** Eval a / 0 ~~ a / 0
-- -- *** Eval 3 / 0 ~~ 3 / 0
-- -- *** Eval {letting a be 4} a / 2 ~~ 2
-- -- 
-- -- *** Eval a % 0 ~~ a % 0
-- -- *** Eval 3 % 0 ~~ 3 % 0
-- -- *** Eval {letting a be 4} a % 2 ~~ 0
-- -- *** Eval {letting a be 5} a % 2 ~~ 1
-- -- 
-- -- *** Eval 5 ** 0 ~~ 1
-- -- *** Eval {letting a be 5} a ** 0 ~~ 1
-- -- *** Eval a ** 0 ~~ 1
-- -- *** Eval 5 ** 2 ~~ 25
-- -- *** Eval {letting a be 5} a ** 2 ~~ 25
-- -- *** Eval 4 ** 3 ** 2 ~~ 262144
-- -- *** Eval x ** 1 ~~ x
-- -- *** Eval (x ** y * x) ~~ x ** (1 + y)
-- -- 
-- -- *** Eval 2 != 3 ~~ true
-- -- *** Eval 2 != 2 ~~ false
-- -- 
-- -- *** Eval 1 in set {1,2,3} ~~ true
-- -- *** Eval 4 in set {1,2,3} ~~ 4 in set {1,2,3}
-- -- 
-- -- *** Eval 1 in mset {1,2,3} ~~ true
-- -- *** Eval 4 in mset {1,2,3} ~~ 4 in mset {1,2,3}
-- -- 
-- -- *** Eval 5 % 5 ~~ 0
-- -- *** Eval a % a ~~ 0
-- -- 
-- -- *** Eval (x * y) + x ~~ (1 + y) * x
-- -- *** Eval (x * y) + y ~~ (1 + x) * y
-- -- *** Eval (x * z) + (y * z) ~~ (x + y) * z
-- -- 
-- -- *** Eval 0 + x ~~ x
-- -- *** Eval 0 - x ~~ -x
-- -- *** Eval 0 * x ~~ 0
-- -- *** Eval 1 * x ~~ x
-- -- *** Eval 1 * (a+b) ~~ a+b
-- -- *** Eval x / 1 ~~ x
-- -- 
-- -- *** Eval false /\ x ~~ false
-- -- *** Eval false \/ x ~~ x
-- -- *** Eval true \/ x ~~ true
-- -- *** Eval x => (!x) ~~ (!x)
-- -- *** Eval (x = y) => (x != y) ~~ x != y
-- -- *** Eval x => y ~~ x => y
-- -- *** Eval x => x ~~ true
-- -- 
-- -- *** Eval x <=> y ~~ x <=> y
-- -- *** Eval x <=> x ~~ true
-- -- *** Eval true <=> x ~~ x
-- -- *** Eval false <=> x ~~ !x
-- -- 
-- -- *** Eval (x+y)-x ~~ y
-- -- *** Eval (x+y)-y ~~ x
-- -- *** Eval (x+y)-z ~~ (x+y)-z
-- -- 
-- -- *** Eval (y-x)+x ~~ y
-- -- *** Eval (x-y)+y ~~ x
-- -- *** Eval (x-y)+x ~~ -y
-- -- *** Eval (x-y)+z ~~ (x-y)+z
-- -- 
-- -- *** Eval -(-x) ~~ x
-- -- 
-- -- *** Eval (z+y)+x ~~ x+y+z
-- -- *** Eval (z=y)=x ~~ (y=z)=x
-- -- 