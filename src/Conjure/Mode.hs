module Conjure.Mode where

import Control.Arrow ( first, second )
import Control.Monad ( guard, msum )
import Data.Char ( toLower )
import Data.List ( partition )
import Data.Maybe ( listToMaybe, mapMaybe )


type GenericArgs
    = ( [(String,String)]  -- key-value pairs
      , [String]           -- flags
      , [String]           -- rest
      )

parseGenericArgs :: [String] -> GenericArgs 
parseGenericArgs xs =
    let
        kvOut (key:value:rest)
            | key `elem` allKeys
            = ((key,value):) `first` kvOut rest
        kvOut (x:rest) = (x:) `second` kvOut rest
        kvOut [] = ([],[])

        (flags, xs1) = partition (`elem` allFlags) xs
        (pairs, xs2) = kvOut xs1
    in
        ( pairs , flags , xs2 )

data ConjureModeSingle
    = ModeRandom
    | ModeFirst
    | ModeSmallest
    | ModeBest
    deriving (Show)

data ConjureMode
    = ModeUnknown
    | ModeRefineParam
        FilePath    -- Essence
        FilePath    -- Essence Param
        FilePath    -- Essence'
        FilePath    -- Essence' Param
    | ModeTranslateSolution
        FilePath    -- Input:  Essence
        FilePath    -- Input:  Essence Param
        FilePath    -- Input:  Essence'
        FilePath    -- Input:  Essence' Param
        FilePath    -- Input:  Essence' Solution
        FilePath    -- Output: Essence  Solution
    | ModePrettify
        FilePath    -- input
        FilePath    -- output
    | ModeDFAll
        FilePath    -- Essence
    | ModeSingleOutput
        ConjureModeSingle
        FilePath    -- Essence
        FilePath    -- Essence'
    deriving (Show)

parseArgs :: GenericArgs -> Maybe ConjureMode
parseArgs (pairs, flags, _rest) = msum
    [ modeRefineParam
    , modeTranslateSolution
    , modePrettify
    , modeDFAll
    , modeRandom
    , modeFirst
    , modeSmallest
    , modeBest
    ]
    where
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
            inEssence        <- key "--in-essence"              -- Input:  Essence specification
            inParam          <- key "--in-essence-param"        -- Input:  Essence parameters
            inEprime         <- key "--in-eprime"               -- Input:  Essence' model
            inEprimeParam    <- key "--in-eprime-param"         -- Input:  Essence' parameters
            inEprimeSolution <- key "--in-eprime-solution"      -- Input:  Essence' solution
            outSolution      <- key "--out-solution"            -- Output: Essence solution
            return $ ModeTranslateSolution inEssence inParam inEprime inEprimeParam inEprimeSolution outSolution

        modePrettify = do
            mode <- key "--mode"
            guard (mode =~= words "pretty prettify")
            modeSingleOutput ModePrettify

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
            guard (mode =~= words "small smallest")
            modeSingleOutput $ ModeSingleOutput ModeSmallest

        modeBest = do
            mode <- key "--mode"
            guard (mode =~= words "best")
            modeSingleOutput $ ModeSingleOutput ModeBest

        -- helper functions for the above
        anyKey = listToMaybe . mapMaybe key
        key = (`lookup` pairs)
        _flag = (`elem` flags)
        x =~= ys = map toLower x `elem` map (map toLower) ys

        modeSingleOutput mk = do
            inEssence <- anyKey $ words "--in --in-essence"
            outEprime <- anyKey $ words "--out --out-eprime"
            return $ mk inEssence outEprime


allKeys :: [String]
allKeys =
    [ "--mode"
    , "--in"
    , "--in-essence"
    , "--in-essence-param"
    , "--in-essence-solution"
    , "--in-eprime"
    , "--in-eprime-param"
    , "--in-eprime-solution"
    , "--in-param"
    , "--in-solution"
    , "--out"
    , "--out-essence"
    , "--out-essence-param"
    , "--out-essence-solution"
    , "--out-eprime"
    , "--out-eprime-param"
    , "--out-eprime-solution"
    , "--out-param"
    , "--out-solution"
    ]

allFlags :: [String]
allFlags =
    [ "--trace-logs"
    ]

