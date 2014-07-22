{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
-- |
-- Lots of test cases for vaildate solution

module Language.E.Testing.VaildateSolutionTests(runTests) where

import Language.E
import Language.E.ValidateSolution(validateSolutionPureNew,validateSolution)
import Language.E.Pipeline.ReadIn(readSpecFromFile)

import qualified Control.Exception as Exc
import qualified Data.Text as T


type Dom = E

-- Test cases which should be found to be invaild
singleVarErrors :: [([Dom], [E])]
singleVarErrors = map ( \(d,e) -> ([d], [e]) )  [
      ([dMake| set (size 2) of int(1..2) |]   , [eMake| {3} |])
    , ([dMake| set (minSize 2) of int(1..2) |], [eMake| {1} |])
    ]

-- Test cases which should be found to be vaild
singleVarCorrect :: [([Dom], [E])]
singleVarCorrect = map ( \(d,e) -> ([d], [e]) ) [
       ([dMake| set (minSize 2) of int(1..2) |], [eMake| {1,2} |])
     , ([dMake| set  of int(1..2) |], [eMake| {1,2,4} |])
    ]


-- Test cases which should be found to be invaild
varErrors :: [([Dom], [E])]
varErrors = []

-- Test cases which should be found to be vaild
varCorrect :: [([Dom], [E])]
varCorrect = []


runTests :: IO ()
runTests = do
    mapM_ ( runVaildate (Just "MISSING ERROR") Nothing ) singleVarErrors
    mapM_ ( runVaildate Nothing (Just "UNEXPECTED ERROR") ) singleVarCorrect

    mapM_ ( runVaildate (Just "MISSING ERROR") Nothing ) varErrors
    mapM_ ( runVaildate Nothing (Just "UNEXPECTED ERROR") ) varCorrect

    return ()

    where
    runVaildate :: Maybe Doc -> Maybe Doc -> ([Dom], [E]) -> IO ()
    runVaildate success failure (dom, e) = do
        let [domS, solS]  = map mkSpec [
                zipWith (\a b ->  mkFind    (T.pack $ "var" ++ show (b :: Integer)) a )
                    dom [0..],
                zipWith (\a b ->  mkLetting (T.pack $ "var" ++ show (b :: Integer)) a )
                    e   [0..]
                ]
        Exc.handle (handler dom e failure) $ do
            validateSolution domS Nothing solS
            case success of
                Nothing -> return ()
                Just doc ->
                    putStrLn . show $
                        vcat  [ doc
                              ,"\tDomain:" <+>  pretty dom
                              ,"\tValue: " <+>  pretty e
                              , ""
                              , "~~~~~~"
                              , ""
                              ]

        return ()

    handler :: [Dom] -> [E] -> Maybe Doc -> Exc.ErrorCall -> IO ()
    handler dom e mdoc (Exc.ErrorCall i) =
        case mdoc of
            Nothing -> return ()
            Just doc -> do
                putStrLn . show $
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

mkFind :: Text -> Dom -> E
mkFind  name dom = [xMake| topLevel.declaration.find.name   := [mkName name]
                         | topLevel.declaration.find.domain := [dom]
                         |]

mkLetting :: Text -> E  -> E
mkLetting name expr = [xMake| topLevel.letting.name := [mkName name]
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


