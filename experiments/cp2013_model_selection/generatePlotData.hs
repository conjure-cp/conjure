{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding ( FilePath, reverse )
import qualified Data.Text.Lazy as LT
-- default (LT.Text)

import Shelly

import Control.Monad
import System.Environment
import Data.List ( genericLength )
import qualified Data.HashMap.Strict as M
import qualified Debug.Trace as Debug


main :: IO ()
main = shelly $ verbosely $ do
    allFiles <- ls "."
    let modelNamesInner
            = LT.intercalate ","
            [ stripped
            | filepath <- allFiles
            , let name = toTextIgnore filepath
            , ".eprime" `LT.isSuffixOf` name
            , let stripped = LT.reverse $ LT.drop 7 $ LT.reverse $ LT.drop 2 name
            ]
    let varModelNames = "var modelNames = [" `LT.append` modelNamesInner `LT.append` "]"
    liftIO $ putStrLn $ LT.unpack varModelNames

    let
        readValues :: LT.Text -> LT.Text -> ShIO [LT.Text]
        readValues whichParam whichAttr | whichAttr `elem` solutionAttrs = do
            allFiles <- ls "."
            outs <- forM allFiles $ \ filepath ->
                if ".eprime-solution" `LT.isSuffixOf` toTextIgnore filepath
                    then do
                        let [eprime,param]
                                = LT.split ('-'==)
                                $ LT.reverse
                                $ LT.drop (genericLength (".eprime-solution" :: String))
                                $ LT.reverse
                                $ LT.drop 2
                                $ toTextIgnore filepath
                        if whichParam == param
                            then do
                                contents <- readfile filepath
                                forM (Prelude.drop 1 $ LT.lines contents) $ \ line ->
                                    if "$ " `LT.isPrefixOf` line
                                        then case LT.split (==':') $ LT.drop 2 line of
                                                [attribute,value]
                                                    | attribute == whichAttr
                                                    -> return [value]
                                                _ -> return []
                                        else return []
                            else return []
                    else return []
            return $ concat $ concat outs

        readValues whichParam whichAttr = do
            allFiles <- ls "."
            outs <- forM allFiles $ \ filepath ->
                if ".eprime-minion-stats" `LT.isSuffixOf` toTextIgnore filepath
                    then do
                        let [eprime,param]
                                = LT.split ('-'==)
                                $ LT.reverse
                                $ LT.drop (genericLength (".eprime-minion-stats" :: String))
                                $ LT.reverse
                                $ LT.drop 2
                                $ toTextIgnore filepath
                        if whichParam == param
                            then do
                                contents <- readfile filepath
                                forM (LT.lines contents) $ \ line ->
                                    if not $ "#" `LT.isPrefixOf` line
                                        then case LT.split (==':') line of
                                                [attribute,value] -> do
                                                    if whichAttr == attribute
                                                        then return [value]
                                                        else return []
                                                _     -> return []
                                        else return []
                            else return []
                    else return []
            return $ concat $ concat outs

    let
        displaySeries (param, attr, values) = LT.concat
            [ "{"
            , "name: '", param, " - ", attr, "',"
            , "data: ["
            , LT.intercalate ", " values
            -- , LT.intercalate ", " (normalise' 1 values)
            , "]"
            , "}"
            ]

    args <- liftIO $ getArgs
    let param = LT.pack $ head args

    -- let attrs = [ "stats_varcount", "stats_conscount" ]
    let attrs' = id
               -- $ filter (not . ("_0" `LT.isSuffixOf`))
               -- $ filter (not . ("alldiff" `LT.isInfixOf`))
               -- $ filter ("count" `LT.isInfixOf`)
                 allAttributes
    let attrs
            =  []
            ++ ["Savile Row TotalTime"]
            -- ++ ["minion TotalTime"]
            -- ++ ["minion Nodes"]
            -- ++ solutionAttrs
            -- ++ attrs'
            -- ++ allAttributes
            ++ ["stats_varcount", "stats_conscount"]
    series <- forM attrs $ \ attr -> do
        values <- readValues param attr
        return $ displaySeries (param, attr, values)
    let varSeries = LT.concat [ "var series = "
                              , "["
                              , LT.intercalate ", " series
                              , "]"
                              ]
    liftIO $ putStrLn $ LT.unpack varSeries


normalise' :: Double -> [LT.Text] -> [LT.Text]
normalise' upperBound xs
    = map (LT.pack . show )
    $ normalise upperBound
    $ map (read . LT.unpack)
      xs

normalise :: Double -> [Double] -> [Double]
normalise _ [] = []
normalise upperBound xs = ys
-- normalise upperBound xs = Debug.trace (unlines [ "normalise" , show xs , show ys ]) ys
    where
        maxVal = maximum xs
        minVal = minimum xs
        range  = maxVal - minVal
        ys = if range == 0
                then []
                else [ y
                     | x <- xs
                     , let y = (x - minVal) * upperBound / range
                     ]


solutionAttrs = [ "minion Nodes"
                , "minion TotalTime"
                , "Savile Row TotalTime"
                ]

allAttributes =
    [ "stats_varcount"
    , "stats_var_bool"
    , "stats_var_discrete"
    , "stats_var_bound"
    , "stats_var_sparsebound"
    , "stats_VarMemory"
    , "stats_DomainProductLog"
    , "stats_dom_0"
    , "stats_dom_25"
    , "stats_dom_50"
    , "stats_dom_75"
    , "stats_dom_100"
    , "stats_dom_mean"
    , "stats_dom_not2_2_ratio"
    , "stats_discrete_bool_ratio"
    , "stats_branchingvars"
    , "stats_auxvars"
    , "stats_auxvar_branching_ratio"
    , "stats_conscount"
    , "stats_arity_0"
    , "stats_arity_25"
    , "stats_arity_50"
    , "stats_arity_75"
    , "stats_arity_100"
    , "stats_TotalArity"
    , "stats_arity_mean"
    , "stats_arity_mean_normalised"
    , "stats_cts_per_var_mean"
    , "stats_cts_per_var_mean_normalised"
    , "stats_alldiffdomovervars_0"
    , "stats_alldiffdomovervars_25"
    , "stats_alldiffdomovervars_50"
    , "stats_alldiffdomovervars_75"
    , "stats_alldiffdomovervars_100"
    , "stats_alldiffdomovervars_mean"
    , "stats_alldiff_count"
    , "stats_alldiff_proportion"
    , "stats_sums_count"
    , "stats_sums_proportion"
    , "stats_or_atleastk_count"
    , "stats_or_atleastk_proportion"
    , "stats_ternary_count"
    , "stats_ternary_proportion"
    , "stats_binary_count"
    , "stats_binary_proportion"
    , "stats_reify_count"
    , "stats_reify_proportion"
    , "stats_table_count"
    , "stats_table_proportion"
    , "stats_lex_count"
    , "stats_lex_proportion"
    , "stats_unary_count"
    , "stats_unary_proportion"
    , "stats_nullary_count"
    , "stats_nullary_proportion"
    , "stats_element_count"
    , "stats_element_proportion"
    , "stats_minmax_count"
    , "stats_minmax_proportion"
    , "stats_occurrence_count"
    , "stats_occurrence_proportion"
    , "stats_multi_shared_vars"
    , "stats_edge_density"
    , "stats_Local_Variance"
    , "stats_tightness_0"
    , "stats_tightness_25"
    , "stats_tightness_50"
    , "stats_tightness_75"
    , "stats_tightness_100"
    , "stats_tightness_mean"
    , "stats_literal_tightness_0"
    , "stats_literal_tightness_25"
    , "stats_literal_tightness_50"
    , "stats_literal_tightness_75"
    , "stats_literal_tightness_100"
    , "stats_literal_tightness_mean"
    , "stats_literal_coeff_of_variation"
    ]

