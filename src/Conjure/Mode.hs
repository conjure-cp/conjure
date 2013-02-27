module Conjure.Mode where

import Control.Monad ( guard, msum, mzero )
import Data.Char ( toLower )
import Data.Maybe ( listToMaybe, mapMaybe )


type GenericArgs
    = ( [(String,String)]  -- key-value pairs
      , [String]           -- flags
      , [String]           -- rest
      )

parseGenericArgs :: [String] -> GenericArgs 
parseGenericArgs = go
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
    | ModeSmallest
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
    deriving (Show)

parseArgs :: GenericArgs -> Maybe ConjureMode
parseArgs (pairs, flags, rest) = msum
    [ modeDiff
    , modeRefineParam
    , modeTranslateSolution
    , modeTypeCheck
    , modePrettify
    , modeValidateSolution
    , modeDFAll
    , modeRandom
    , modeFirst
    , modeSmallest
    ]
    where
        modeDiff = do
            mode <- key "--mode"
            guard (mode =~= words "diff")
            case rest of
                [in1, in2] -> return $ ModeDiff in1 in2
                _          -> mzero

        modeRefineParam = do
            mode      <- key "--mode"
            guard (mode =~= words "refineParam")
            inEssence <- key "--in-essence"
            inParam   <- key "--in-essence-param"
            inEprime  <- key "--in-eprime"
            outParam  <- key "--out-eprime-param"
            return $ ModeRefineParam inEssence inParam inEprime outParam

        modeTranslateSolution = do
            mode             <- key "--mode"
            guard (mode =~= words "transSol translateSol translateSolution")
            inEssence        <- key "--in-essence"
            inParam          <- optional $ key "--in-essence-param"
            inEprime         <- key "--in-eprime"
            inEprimeParam    <- optional $ key "--in-eprime-param"
            inEprimeSolution <- key "--in-eprime-solution"
            outSolution      <- anyKey $ words "--out-solution --out-essence-solution"
            return $ ModeTranslateSolution
                        inEssence inParam
                        inEprime inEprimeParam inEprimeSolution
                        outSolution

        modeTypeCheck = do
            mode <- key "--mode"
            guard (mode =~= words "typeCheck")
            inp  <- optional $ key "--in"
            return $ ModeTypeCheck inp

        modePrettify = do
            mode <- key "--mode"
            guard (mode =~= words "pretty prettify")
            inp  <- optional $ key "--in"
            out  <- optional $ key "--out"
            return $ ModePrettify inp out

        modeValidateSolution = do
            mode <- key "--mode"
            guard (mode =~= words "validateSolution validateSol validateSoln")
            essence  <- key "--in-essence"
            param    <- optional $ key "--in-param"
            solution <- key "--in-solution"
            return $ ModeValidateSolution essence param solution

        modeDFAll = do
            mode <- key "--mode"
            guard (mode =~= words "df depthfirst depth-first")
            inEssence <- anyKey $ words "--in --in-essence"
            return $ ModeDFAll inEssence

        modeRandom = do
            mode <- key "--mode"
            guard (mode =~= words "rand random")
            modeSingleOutput $ ModeSingleOutput ModeRandom

        modeFirst = do
            mode <- key "--mode"
            guard (mode =~= words "first")
            modeSingleOutput $ ModeSingleOutput ModeFirst

        modeSmallest = do
            mode <- key "--mode"
            guard (mode =~= words "small smallest best")
            modeSingleOutput $ ModeSingleOutput ModeSmallest

        -- helper functions for the above
        anyKey = listToMaybe . mapMaybe key
        key = (`lookup` pairs)
        optional = return
        _flag = (`elem` flags)
        x =~= ys = map toLower x `elem` map (map toLower) ys

        modeSingleOutput mk = do
            inEssence <- anyKey $ words "--in --in-essence"
            outEprime <- anyKey $ words "--out --out-eprime"
            return $ mk inEssence outEprime


isKey :: String -> Bool
isKey ('-':'-':_) = True
isKey _ = False

isFlag :: String -> Bool
isFlag = (`elem` allFlags)
    where
        allFlags = [] -- we don't have any flags, yet.

