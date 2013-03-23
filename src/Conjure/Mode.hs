module Conjure.Mode where

import Control.Monad ( guard, msum, mzero )
import Data.Char ( toLower )
import Data.Maybe ( listToMaybe, mapMaybe )

import Data.HashSet as S ( HashSet, fromList, member )
import Data.HashMap.Strict as M ( HashMap, fromList, lookup )


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
    | ModeDFAll
        FilePath    -- Essence
    | ModeSingleOutput
        ConjureModeSingle
        FilePath    -- Essence
        FilePath    -- Essence'
    | ModeGenerateParam
        FilePath    -- Essence
        FilePath    -- Essence Param file
    deriving (Show)

data ConjureModeWithFlags
    = ConjureModeWithFlags
        ConjureMode                     -- the mode
        (M.HashMap String String)       -- all key-value pairs
        (S.HashSet String)              -- all flags
        [String]                        -- all the rest

parseArgs :: GenericArgs -> Maybe ConjureModeWithFlags
parseArgs (pairs, flags, rest) = msum
    [ modeDiff
    , modeRefineParam
    , modeTranslateSolution
    , modeTypeCheck
    , modePrettify
    , modeValidateSolution
    , modeGenerateParam
    , modeDFAll
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
            inp  <- optional $ key "--in"
            returnMode $ ModeTypeCheck inp

        modePrettify = do
            mode $ words "pretty prettify"
            inp  <- optional $ key "--in"
            out  <- optional $ key "--out"
            returnMode $ ModePrettify inp out

        modeValidateSolution = do
            mode $ words "validateSolution validateSol validateSoln"
            essence  <- key "--in-essence"
            param    <- optional $ key "--in-param"
            solution <- key "--in-solution"
            returnMode $ ModeValidateSolution essence param solution

        modeGenerateParam = do
            mode $ words "generateParam genParam"
            inEssence <- anyKey $ words "--in --in-essence"
            outParam  <- anyKey $ words "--out --out-param"
            returnMode $ ModeGenerateParam inEssence outParam

        modeDFAll = do
            mode $ words "df depthfirst depth-first"
            inEssence <- anyKey $ words "--in --in-essence"
            returnMode $ ModeDFAll inEssence

        modeRandom = do
            mode $ words "rand random"
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
        allFlags = [] -- we don't have any flags, yet.

