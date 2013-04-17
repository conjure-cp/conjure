{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}


import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Database.SQLite.Simple
import Web.Scotty
import Text.Shakespeare.Text
import Data.List
import Data.List.Split

import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Data.Monoid (mconcat)
import System.Environment


main :: IO ()
main = scotty 3000 $ do
    get "/plot.js" $ do
        header "content-type" "application/javascript"
        file "plots/plot.js"
    get "/viewplot/:essence/:params/:attributes" $ do
        conjure_repo <- liftIO $ getEnv "CONJURE_REPO"
        conn         <- liftIO $ open $ conjure_repo ++ "/experiments/cp2013_model_selection/results.db"
        essence     :: String <- param "essence"
        params'     :: String <- param "params"
        attributes' :: String <- param "attributes"
        let attributes = splitOn "+" attributes'
        let params     = splitOn "+" params'
        models :: [String] <- fmap concat $
            liftIO $ query conn "SELECT DISTINCT MODEL FROM attributes WHERE SPEC=(?)" [essence]
        attributeValues :: [[(String, String, String, Double)]] <- do
            whichParams <-
                if params' == "all"
                    then do
                        allParams :: [String] <- fmap concat $ liftIO $ query conn "SELECT DISTINCT PARAM FROM attributes WHERE SPEC=(?)" [essence]
                        return allParams
                    else return params

            fmap concat $ 
                forM whichParams $ \ p ->
                    forM attributes $ \ a ->
                        liftIO $ query conn 
                            "SELECT MODEL, PARAM, ATTRIBUTE, VALUE FROM attributes WHERE SPEC=(?) AND PARAM=(?) AND ATTRIBUTE=(?) ORDER BY MODEL"
                            (essence, p, a)

        let modelsHtml = [lt| var modelNames = #{show models} |]
        let seriesHtmlOne name vals = concat ["{name: \"", name, "\", data: ", show vals, "}"]
        let seriesHtmlOneCall xs@((model, param, attr, _):_) = seriesHtmlOne (intercalate " - " [model, param, attr])
                                                                             (map thd xs) where thd (_,_,_,i) = i
            seriesHtmlOneCall [] = ""
        let seriesHtml = [lt| var series = [#{intercalate "," (map seriesHtmlOneCall attributeValues)}] |]

        html [lt|
<!DOCTYPE html>
<html>
<head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8">
    <title> Plots </title>

    <script type='text/javascript' src='https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js'></script>
    <script src="http://code.highcharts.com/highcharts.js"></script>
    <script src="http://code.highcharts.com/modules/exporting.js"></script>

    <script> #{modelsHtml} ; #{seriesHtml} </script>
    <script type='text/javascript' src="/plot.js"></script>

</head>
<body>
    <div id="container" style="min-width: 400px; height: 800px; margin: 0 20px"></div>
</body>
</html>
|]
        liftIO $ close conn
    get "/plotdata/:essence/:attributes" $ do
        conjure_repo <- liftIO $ getEnv "CONJURE_REPO"
        essence <- param "essence"
        attributes' <- param "attributes"
        let attributes = T.splitOn "+" attributes'
        conn    <- liftIO $ open $ conjure_repo ++ "/experiments/cp2013_model_selection/results.db"
        results <- liftIO $ query conn "SELECT MODEL, PARAM, ATTRIBUTE, VALUE FROM attributes WHERE SPEC=(?) AND ATTRIBUTE=(?)"
                    ( T.unpack essence
                    , T.unpack (head attributes)
                    )
        html $ T.unlines $
            [ "<pre>", "essence\n", essence, "attributes\n"]
            ++ attributes
            ++ map (T.pack . show) (results :: [(String, String, String, Double)])
            ++ ["</pre>"]
        liftIO $ close conn


