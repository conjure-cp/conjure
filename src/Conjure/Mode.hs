module Conjure.Mode where

import Control.Arrow ( first, second )
import Control.Monad ( guard, msum )
import Data.Char ( toLower )
import Data.List ( partition )


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


data ConjureMode
    = ModeUnknown
    | ModeRefineParam
        FilePath    -- Essence
        FilePath    -- Essence Param
        FilePath    -- Essence'
        FilePath    -- Essence' Param
    | ModePrettify
        FilePath    -- input
        FilePath    -- output
    | ModeDFAll
        FilePath    -- Essence
    | ModeBest
        FilePath    -- Essence
    | ModeRandom
        FilePath    -- Essence
    | ModeFirst
        FilePath    -- Essence
    | ModeSmallest
        FilePath    -- Essence
    deriving ( Show )

parseArgs :: GenericArgs -> Maybe ConjureMode
parseArgs (pairs, _flags, _rest) = msum
    [ modeRefineParam
    , modePrettify
    , modeDFAll
    , modeRandom
    , modeFirst
    , modeSmallest
    ]
    where
        key = (`lookup` pairs)
        -- flag = (`elem` flags)
        x =~= ys = map toLower x `elem` map (map toLower) ys

        modeRefineParam = do
            mode      <- key "--mode"
            guard (mode =~= words "refineparam")
            inEssence <- key "--in-essence"
            inParam   <- key "--in-essence-param"
            outEprime <- key "--in-eprime"
            outParam  <- key "--out-eprime-param"
            return $ ModeRefineParam inEssence inParam outEprime outParam

        modePrettify = do
            mode <- key "--mode"
            guard (mode =~= words "pretty prettify")
            inp <- key "--in"
            out <- key "--out"
            return $ ModePrettify inp out

        modeDFAll = do
            mode <- key "--mode"
            guard (mode =~= words "df depthfirst depth-first")
            inEssence <- key "--in-essence"
            return $ ModeDFAll inEssence

        modeRandom = do
            mode <- key "--mode"
            guard (mode =~= words "rand random")
            inEssence <- key "--in-essence"
            return $ ModeRandom inEssence

        modeFirst = do
            mode <- key "--mode"
            guard (mode =~= words "first")
            inEssence <- key "--in-essence"
            return $ ModeSmallest inEssence

        modeSmallest = do
            mode <- key "--mode"
            guard (mode =~= words "small smallest")
            inEssence <- key "--in-essence"
            return $ ModeSmallest inEssence

allKeys :: [String]
allKeys =
    [ "--mode"
    , "--in"
    , "--in-eprime-solution"
    , "--in-eprime"
    , "--in-essence"
    , "--in-essence-param"
    , "--out"
    , "--out-eprime-param"
    , "--out-essence-solution"
    ]

allFlags :: [String]
allFlags =
    [ "--trace-logs"
    ]

