{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
module Language.E.Testing.ValidateSolutionTests(runTests) where

import Language.E
import Language.E.ValidateSolution(validateSolutionPureNew,validateSolution)
import Language.E.Pipeline.ReadIn(readSpecFromFile)

import qualified Control.Exception as Exc
import qualified Data.Text as T

-- Lots of test cases for vaildate solution

type Dom = E

-- Test cases which should be found to be invaild
singleVarErrors :: [([Dom], [[E]] )]
singleVarErrors = map ( \(d,e) -> ([d], (map (\f -> [f]) e) ) )  [
   ([dMake| set (size 2) of int(1..2) |], [
          [eMake| {3} |]
         ,[eMake| {4} |]
     ])
 , ([dMake| set (minSize 2) of int(1..2) |], [
         [eMake| {1} |]
     ])
 , ([dMake| mset (maxSize 3, minSize 2, size 3) of int(1..4) |], [
        [eMake| mset(1,2) |]
    ])
 , ([dMake| set (maxSize 2) of int(1..4) |], [
        [eMake| {1,2,3} |]
    ])
 , ([dMake| set (maxSize 3, minSize 2, size 3) of int(1..4) |], [
        [eMake| {1,4} |]
    ])
 , ([dMake| set of int(1..2) |], [
          [eMake| {4} |]
         ,[eMake| {1,1} |]
    ])
 , ([dMake| matrix indexed by [int(2..4)] of int(3,4..5) |], [
        [eMake| [3,4,5] |]
    ])
 , ([dMake| partition (complete, maxPartSize 5, minNumParts 3) from int(5..5) |], [

    ])
 , ([dMake| partition from int(4..5) |], [
          [eMake| partition( {4}, {4,5} ) |]
        , [eMake| partition( {4,4} ) |]
        , [eMake| partition( {6} ) |]
    ])
 , ([dMake| relation of (int(5..5)) |], [

    ])
 , ([dMake| relation (total) of (int(5..5) * int(2..3)) |], [

    ])
 , ([dMake| partition (partSize 1, minNumParts 2, regular, maxPartSize 5, numParts 5)
        from int(1..1) |], [

    ])
 , ([dMake| relation of (set of int(4..4) * matrix indexed by [int(3..3)]
        of int(4..4)) |], [

    ])
 , ([dMake| function set of int(2..2) --> set of int(5..5) |], [

    ])
 , ([dMake| function int(2..4) --> int(5..5) |], [

    ])
 , ([dMake| set (size 4) of relation
        of (relation (functional, total) of (int(4..4) * int(3..3))) |], [

    ])
 , ([dMake| function (maxSize 0, minSize 1, size 5, injective, surjective)
        int(2..4) --> int(3..3) |], [

    ])
 , ([dMake| function int(1..2) --> set of int(1..2) |], [
        [eMake| function(1 --> {1,4}) |]
    ])
 , ([dMake| function (size 2) int(1..2) --> set of int(1..2) |], [
        [eMake| function(1 --> {1,4}) |]
    ])
 , ([dMake| function  int(1..2) --> set of int(1..2) |], [
        [eMake| function(1 --> {1}, 1 --> {1}) |]
    ])
 , ([dMake| function (total)  int(1..2) --> set of int(1..2) |], [
        [eMake| function(1 --> {1} ) |]
    ])
 , ([dMake| function (injective) int(1..2) --> set of int(1..2) |], [
        [eMake| function(1 --> {1}, 2 --> {1}) |]
    ])
 , ([dMake| function (surjective, injective,  total )  int(1..2) -->  int(1..2) |], [
        [eMake| function(1 --> 1 ) |]
    ])
 ]


-- Test cases which should be found to be vaild
singleVarCorrect :: [([Dom], [[E]])]
singleVarCorrect = map ( \(d,e) -> ([d], (map (\f -> [f]) e) ) )  [
   ([dMake| set (minSize 2) of int(1..2) |], [
        [eMake| {1,2} |]
    ])
 , ([dMake| mset (maxSize 3, minSize 2, size 3) of int(1..4) |], [
        [eMake| mset(1,2,3) |]
    ])
 , ([dMake| set (maxSize 2) of int(1..4) |], [
        [eMake| {1,2} |]
    ])
 , ([dMake| set (maxSize 3, minSize 2, size 3) of int(1..4) |], [
        [eMake| {1,2,4} |]
    ])
 , ([dMake| set  of int(1..2) |], [
        [eMake| {1,2} |]
    ])
 , ([dMake| mset  of int(1..2) |], [
        [eMake| mset(1,2,2) |]
    ])
 , ([dMake| matrix indexed by [int(1..3)] of int(3,4..5) |], [
        [eMake| [3,4,5] |]
    ])
 , ([dMake| partition (complete, maxPartSize 5, minNumParts 3) from int(5..5) |], [

    ])
 , ([dMake| partition from int(4..5) |], [
         [eMake| partition( {4}, {5} ) |]
        ,[eMake| partition( {4} ) |]
    ])
 , ([dMake| relation of (int(5..5)) |], [

    ])
 , ([dMake| relation (total) of (int(5..5) * int(2..3)) |], [

    ])
 , ([dMake| partition (partSize 1, minNumParts 2, regular, maxPartSize 5, numParts 5)
        from int(1..1) |], [

    ])
 , ([dMake| relation of (set of int(4..4) * matrix indexed by [int(3..3)]
        of int(4..4)) |], [

    ])
 , ([dMake| function set of int(2..2) --> set of int(5..5) |], [

    ])
 , ([dMake| function int(2..4) --> int(5..5) |], [
        [eMake| function( 2 --> 5, 3 -->5) |]
    ])
 , ([dMake| set (size 4) of relation
        of (relation (functional, total) of (int(4..4) * int(3..3))) |], [

    ])
 , ([dMake| function (maxSize 0, minSize 1, size 5, injective, surjective)
        int(2..4) --> int(3..3) |], [

    ])
 , ([dMake| function int(1..2) --> set of int(1..2) |], [
        [eMake| function(1 --> {1}) |]
    ])
 , ([dMake| function (total)  int(1..2) --> set of int(1..2) |], [
        [eMake| function(1 --> {1},  2 --> {1} ) |]
    ])
 , ([dMake| function (injective) int(1..2) --> set of int(1..2) |], [
        [eMake| function(1 --> {1}, 2 --> {2}) |]
    ])
 , ([dMake| function (surjective, injective,  total )  int(1..2) -->  int(1..2) |], [
        [eMake| function(1 --> 1, 2 --> 2 ) |]
    ])
 ]


-- Test cases which should be found to be invaild
varErrors :: [([Dom], [[E]])]
varErrors = []

-- Test cases which should be found to be vaild
varCorrect :: [([Dom], [[E]])]
varCorrect = []


runTests :: IO ()
runTests = do
    mapM_ ( runVaildate (Just "MISSING ERROR") Nothing ) singleVarErrors
    mapM_ ( runVaildate Nothing (Just "UNEXPECTED ERROR") ) singleVarCorrect

    mapM_ ( runVaildate (Just "MISSING ERROR") Nothing ) varErrors
    mapM_ ( runVaildate Nothing (Just "UNEXPECTED ERROR") ) varCorrect

    return ()

runVaildate :: Maybe Doc -> Maybe Doc -> ([Dom], [[E]]) -> IO ()
runVaildate success failure (dom, es) =
   mapM_ (runVaildate' success failure dom) es

runVaildate' :: Maybe Doc -> Maybe Doc -> [Dom] -> [E] -> IO ()
runVaildate' success failure dom e = do
    let [domS, solS]  = map mkSpec [
            zipWith (\a b ->  mkFind    (T.pack $ "var" ++ show (b :: Integer)) a )
                dom [0..],
            zipWith (\a b ->  mkLetting (T.pack $ "var" ++ show (b :: Integer)) a )
                e   [0..]
            ]
    Exc.handle (handler dom e failure) $ do
        putStrLn ""
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

_bb :: Dom -> E -> IO ()
_bb d e = do

    let [domS, solS]  = map mkSpec [
            zipWith (\a b ->  mkFind    (T.pack $ "var" ++ show (b :: Integer)) a )
                [d] [0..],
            zipWith (\a b ->  mkLetting (T.pack $ "var" ++ show (b :: Integer)) a )
                [e]   [0..]
            ]

    let (b, logs) = validateSolutionPureNew domS Nothing solS
    putStrLn . show . pretty $ b
    putStrLn . show . pretty $ logs

