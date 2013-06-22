{-# LANGUAGE OverloadedStrings #-}
module Conjure.Mode where

import Control.Monad ( (>=>), guard, msum, mzero )
import Data.Char ( toLower )
import Data.Maybe ( listToMaybe, mapMaybe )

import Data.HashSet as S ( HashSet, fromList, member )
import Data.HashMap.Strict as M ( HashMap, fromList, lookup )

import Safe ( readMay )

import qualified Text.PrettyPrint as Pr
import Stuff.Pretty
import RepositoryVersion ( repositoryVersion )


type GenericArgs
    = ( M.HashMap String String  -- key-value pairs
      , S.HashSet String         -- flags
      , [String]                 -- rest
      )

parseGenericArgs :: [String] -> GenericArgs
parseGenericArgs inp =
    let (pairs, flags, rest) = go inp
    in  (M.fromList pairs, S.fromList flags, rest)
    where
        go (flag:args)
            | isFlag flag
            = let (pairs, flags, rest) = go args
              in  (pairs, flag:flags, rest)
        go (key:value:args)
            | isKey key
            = let (pairs, flags, rest) = go args
              in  ((key,value):pairs, flags, rest)
        go (arg:args)
            = let (pairs, flags, rest) = go args
              in  (pairs, flags, arg:rest)
        go [] = ([],[],[])


data ConjureModeSingle
    = ModeRandom
    | ModeFirst
    | ModeCompact
    deriving (Show)

data ConjureModeMultiple
    = DFAll
    | DFCompactParam
    | DFNoChannelling
    deriving (Show)

data ConjureMode
    = ModeUnknown
    | ModeDiff FilePath FilePath
    | ModeRefineParam
        FilePath    -- Essence
        FilePath    -- Essence Param
        FilePath    -- Essence'
        FilePath    -- Essence' Param
    | ModeTranslateSolution
        FilePath            -- Input:  Essence
        (Maybe FilePath)    -- Input:  Essence Param
        FilePath            -- Input:  Essence'
        (Maybe FilePath)    -- Input:  Essence' Param
        FilePath            -- Input:  Essence' Solution
        FilePath            -- Output: Essence  Solution
    | ModeTypeCheck
        (Maybe FilePath)    -- input
    | ModePrettify
        (Maybe FilePath)    -- input
        (Maybe FilePath)    -- output
    | ModeValidateSolution
        FilePath            -- Essence
        (Maybe FilePath)    -- Essence Param
        FilePath            -- Essence Solution
    | ModeMultipleOutput
        ConjureModeMultiple
        FilePath            -- Essence
        (Maybe FilePath)    -- output dir
        (Maybe Int)         -- limit the number of output models.
    | ModeSingleOutput
        ConjureModeSingle
        FilePath    -- Essence
        FilePath    -- Essence'
    | ModeGenerateRandomParam
        FilePath    -- Essence
        FilePath    -- Essence Param file
    | ModeGenerateRandomParam2
        FilePath    -- In:  Essence
        FilePath    -- Out: Essence Param file
        FilePath    -- Directory to store intermediate files
        (Maybe String) -- prefix so that GenerateParam2 can be run in parallel
    deriving (Show)

data ConjureModeWithFlags
    = ConjureModeWithFlags
        ConjureMode                     -- the mode
        (M.HashMap String String)       -- all key-value pairs
        (S.HashSet String)              -- all flags
        [String]                        -- all the rest
    deriving (Show)

conjureHelp :: Doc
conjureHelp =  Pr.vcat  $ helpStart :
    [ modeDiff
    , modeRefineParam
    , modeTranslateSolution
    , modeTypeCheck
    , modePrettify
    , modeValidateSolution
    , modeDFAll
    , modeDFCompactParam
    , modeDFNoChannel
    , modeRandom
    , modeFirst
    , modeSmallest
    ]

    where
    helpStart =  Pr.vcat . map pretty $ [
        "conjure --mode=Mode Args "
        ,"    Version: " ++ repositoryVersion
        ,""
        ]

    modeDiff = mode "diff" [
         key "--in-essence"
       , key "--in-essence-param"
       , key "--in-eprime"
       , key "--out-eprime-param"
       ]

    modeRefineParam = mode "refineParam" [
          key "--in-essence"
        , key "--in-essence-param"
        , key "--in-eprime"
        , key "--out-eprime-param"
        ]

    modeTranslateSolution = mode "translateSolution" [
          key "--in-essence"
        , optional $ key "--in-essence-param"
        , key "--in-eprime"
        , optional $ key "--in-eprime-param"
        , key "--in-eprime-solution"
        , anyKey $ words' "--out-solution --out-essence-solution"
        ]

    modeTypeCheck = mode "typeCheck" [
          optional $ anyKey $ words' "--in-essence --in"
        ]

    modePrettify = mode "pretty" [
         optional $ anyKey $ words' "--in-essence --in"
        ,optional $ anyKey $ words' "--out-essence --out"
        ]

    modeValidateSolution = mode "validateSolution" [
          key "--in-essence"
        , optional $ key "--in-param"
        , key "--in-solution"
        ]

    modeDFAll = mode "df" [
         anyKey $ words' "--in-essence --in"
        ]

    modeDFCompactParam = mode "df-compact-param" [
          anyKey $ words' "--in-essence --in"
        ]

    modeDFNoChannel = mode "df-no-channelling" [
         anyKey $ words' "--in-essence --in"
        ]

    modeRandom = mode "random" [
        ]

    modeFirst = mode "first" [
        ]

    modeSmallest = mode "compact" [
        ]

    key flag = flag
    optional = Pr.brackets
    words'   = pretty . head . words
    anyKey   = id

    mode :: String -> [Doc] -> Doc
    mode title docs = header title Pr.$+$ Pr.nest 4 (Pr.vcat docs) <> "\n" 

    header :: String -> Doc
    header title = pretty $ "--mode " ++  title 

parseArgs :: GenericArgs -> Maybe ConjureModeWithFlags
parseArgs (pairs, flags, rest) = msum
    [ modeDiff
    , modeRefineParam
    , modeTranslateSolution
    , modeTypeCheck
    , modePrettify
    , modeValidateSolution
    , modeGenerateRandomParam
    , modeGenerateRandomParam2
    , modeDFAll
    , modeDFCompactParam
    , modeDFNoChannel
    , modeRandom
    , modeFirst
    , modeSmallest
    ]
    where
        modeDiff = do
            mode $ words "diff"
            case rest of
                [in1, in2] -> returnMode $ ModeDiff in1 in2
                _          -> mzero

        modeRefineParam = do
            mode $ words "refineParam"
            inEssence <- key "--in-essence"
            inParam   <- key "--in-essence-param"
            inEprime  <- key "--in-eprime"
            outParam  <- key "--out-eprime-param"
            returnMode $ ModeRefineParam inEssence inParam inEprime outParam

        modeTranslateSolution = do
            mode $ words "transSol translateSol translateSolution"
            inEssence        <- key "--in-essence"
            inParam          <- optional $ key "--in-essence-param"
            inEprime         <- key "--in-eprime"
            inEprimeParam    <- optional $ key "--in-eprime-param"
            inEprimeSolution <- key "--in-eprime-solution"
            outSolution      <- anyKey $ words "--out-solution --out-essence-solution"
            returnMode $ ModeTranslateSolution
                        inEssence inParam
                        inEprime inEprimeParam inEprimeSolution
                        outSolution

        modeTypeCheck = do
            mode $ words "typeCheck"
            inp  <- optional $ anyKey $ words "--in-essence --in"
            returnMode $ ModeTypeCheck inp

        modePrettify = do
            mode $ words "pretty prettify"
            inp  <- optional $ anyKey $ words "--in-essence --in"
            out  <- optional $ anyKey $ words "--out-essence --out"
            returnMode $ ModePrettify inp out

        modeValidateSolution = do
            mode $ words "validateSolution validateSol validateSoln"
            essence  <- key "--in-essence"
            param    <- optional $ key "--in-param"
            solution <- key "--in-solution"
            returnMode $ ModeValidateSolution essence param solution

        modeGenerateRandomParam = do
            mode $ words "generateRandomParam"
            inEssence <- anyKey $ words "--in-essence --in"
            outParam  <- anyKey $ words "--out-param --out-essence --out"
            returnMode $ ModeGenerateRandomParam inEssence outParam

        modeGenerateRandomParam2 = do
            mode $ words "generateRandomParam2"
            inEssence       <- anyKey $ words "--in-essence --in"
            outParam        <- anyKey $ words "--out-param --out"
            intermediateDir <- anyKey $ words "--intermediate"
            basename        <- optional $ key "--prefix"
            returnMode $ ModeGenerateRandomParam2 inEssence outParam intermediateDir basename

        modeDFAll = do
            mode $ words "df depthfirst depth-first"
            inEssence <- anyKey $ words "--in-essence --in"
            outDir    <- optional $ anyKey $ words "--output-directory --out-dir --out"
            limit     <- optional $ readKey "--limit-models"
            returnMode $ ModeMultipleOutput DFAll inEssence outDir limit

        modeDFCompactParam = do
            mode $ words "df-compact-param"
            inEssence <- anyKey $ words "--in-essence --in"
            outDir    <- optional $ anyKey $ words "--output-directory --out-dir --out"
            limit     <- optional $ readKey "--limit-models"
            returnMode $ ModeMultipleOutput DFCompactParam inEssence outDir limit

        modeDFNoChannel = do
            mode $ words "df-no-channelling"
            inEssence <- anyKey $ words "--in-essence --in"
            outDir    <- optional $ anyKey $ words "--output-directory --out-dir --out"
            limit     <- optional $ readKey "--limit-models"
            returnMode $ ModeMultipleOutput DFNoChannelling inEssence outDir limit

        modeRandom = do
            mode $ words "random rand rnd"
            modeSingleOutput $ ModeSingleOutput ModeRandom

        modeFirst = do
            mode $ words "first"
            modeSingleOutput $ ModeSingleOutput ModeFirst

        modeSmallest = do
            mode $ words "compact"
            modeSingleOutput $ ModeSingleOutput ModeCompact

        -- helper functions for the above
        mode xs = do
            m <- key "--mode"
            guard (m =~= xs)
        anyKey = listToMaybe . mapMaybe key
        key = (`M.lookup` pairs)
        readKey = key >=> readMay
        optional = return
        _flag = (`S.member` flags)
        x =~= ys = map toLower x `elem` map (map toLower) ys

        returnMode m = return $ ConjureModeWithFlags m pairs flags rest

        modeSingleOutput mk = do
            inEssence <- anyKey $ words "--in --in-essence"
            outEprime <- anyKey $ words "--out --out-eprime"
            returnMode $ mk inEssence outEprime


isKey :: String -> Bool
isKey ('-':'-':_) = True
isKey _ = False

isFlag :: String -> Bool
isFlag = (`elem` allFlags)
    where
        allFlags = [ "--better"
                   ]

