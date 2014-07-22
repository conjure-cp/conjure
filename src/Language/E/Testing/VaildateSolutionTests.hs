{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
-- |
-- Lots of test cases for vaildate solution

module Language.E.Testing.VaildateSolutionTests where

import Language.E
import Language.E.ValidateSolution(validateSolutionPureNew,validateSolution)
import Language.E.Pipeline.ReadIn(readSpecFromFile)

import qualified Control.Exception as Exc



type Dom = E

singleVarErrors :: [(Dom, E)]
singleVarErrors = [
      ([dMake| set (size 2) of int(1..2) |]   , [eMake| {3} |])
    , ([dMake| set (minSize 2) of int(1..2) |], [eMake| {1} |])
    ]

singleVarCorrect :: [(Dom, E)]
singleVarCorrect = [
       ([dMake| set (minSize 2) of int(1..2) |], [eMake| {1,2} |])
     , ([dMake| set  of int(1..2) |], [eMake| {1,2,4} |])
    ]

runTests :: IO ()
runTests = do
    mapM_ ( runVaildate "MISSING ERROR" "" ) singleVarErrors
    mapM_ ( runVaildate "" "UNEXPECTED ERROR" ) singleVarCorrect

    return ()

    where
    runVaildate :: Doc -> Doc -> (Dom, E) -> IO ()
    runVaildate success failure (dom, e) = do
        let [domS, solS]  = map mkSpec [ [mkFind ("var0", dom)], [mkLetting ("var0", e)] ]
        Exc.handle (handler dom e failure) $ do
            validateSolution domS Nothing solS
            putStrLn . show $
                vcat  [ success
                      ,"\tDomain:" <+>  pretty dom
                      ,"\tValue: " <+>  pretty e
                      , ""
                      , "~~~~~~"
                      , ""
                      ]

        return ()

    handler :: Dom -> E -> Doc -> Exc.ErrorCall -> IO ()
    handler dom e doc (Exc.ErrorCall i) = putStrLn . show $
        vcat  [ doc
              , "\tDomain:" <+>  pretty dom
              ,"\tValue: " <+>  pretty e
              , ""
              , pretty i
              , "~~~~~~"
              , ""
              ]



mkSpec :: [E] -> Spec
mkSpec es =
    Spec (LanguageVersion "Essence" [1,3]) . listAsStatement $ es

mkFind :: (Text,Dom) -> E
mkFind (name,dom) =[xMake| topLevel.declaration.find.name   := [mkName name]
                         | topLevel.declaration.find.domain := [dom]
                         |]

mkLetting :: (Text,E) -> E
mkLetting (name,expr) =[xMake| topLevel.letting.name := [mkName name]
                             | topLevel.letting.expr := [expr]
                             |]

mkName :: Text -> E
mkName name = [xMake| reference :=  [Prim (S name)]  |]


_aa :: FilePath -> FilePath -> IO ()
_aa e s = do
    ee <- readSpecFromFile e
    ss <- readSpecFromFile s
    let (b, logs) = validateSolutionPureNew ee Nothing ss
    putStrLn . show . pretty $ b
    putStrLn . show . pretty $ logs


