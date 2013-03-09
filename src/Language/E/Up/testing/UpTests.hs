module Main ( main ) where

import Language.E

import Language.E.Up.EprimeToEssence
import Language.E.Up.IO (getTestSpecs, getSpec)

import System.FilePath

import Test.Hspec hiding (Spec)
import qualified Test.Hspec as Hspec
import Test.Hspec.HUnit ()


essences :: IO [FilePath]
-- Every test
essences = allFilesWithSuffix ".essence" "files/uptests/"

-- Basic tests
--essences = essencesDirs "files/uptests/" ["___parts","___simple","___types"]

-- Tuples and matrixes
--essences = essencesDirs "files/uptests/" ["___simple", "_tuples_of_matrix", "_matrix_of_tuples","_zznested_singletons","_zothers"]

--essences = essencesDirs "files/uptests/" ["____"]

essencesDirs :: FilePath -> [FilePath] -> IO [FilePath]
essencesDirs base arr = 
    concatMapM ( allFilesWithSuffix ".essence") (map (base </>) arr) 

specs :: IO [( (FilePath, FilePath, FilePath, Maybe FilePath, Maybe FilePath), FilePath )]
specs = do
    es <- essences
    return $ map getFiles es

    where 
        getFiles f = 
            let base = dropExtension f 
                name = takeFileName base 
                dir = joinPath [base, "0001"] in
            (
             (
                 addExtension dir "eprime",
                 addExtension dir "eprime.solution",
                 f,
                 Just $ addExtension (joinPath [base,name]) "param",
                 Just $ addExtension (joinPath [base,name]) "essence-param"
             ),
             addExtension dir "solution" 
            )

runSpec :: ((FilePath, FilePath, FilePath, Maybe FilePath, Maybe FilePath)
            , FilePath) -> IO ()
runSpec  (sps@(_, _,orgF,_,_),ansF) = do
    (spec,sol,org,orgP) <- getTestSpecs sps

    ansS <- getSpec ansF
    let ans = es ansS

    let resultEssence = mainPure(spec,sol,org,orgP)

    show (pretty resultEssence) `shouldBe` show (pretty ans)
    where
    es :: Language.E.Spec -> [E]
    es (Spec _ xs) = statementAsList xs

tests :: IO Hspec.Spec
tests = do
    xs <- specs
    return $
        describe "Converting eprime back to essence" $
            forM_ xs $ \every@( (specF,_,_,_,_),_) ->
                it specF $ runSpec every

main :: IO ()
main = do 
    ts <- tests
    hspec ts
