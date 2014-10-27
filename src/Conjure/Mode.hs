module Conjure.Mode where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.RepositoryVersion ( repositoryVersion )
import Conjure.Language.Pretty

import Data.HashSet as S ( HashSet, fromList )
import Data.HashMap.Strict as M ( HashMap, fromList, lookup )
import qualified Text.PrettyPrint as Pr


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
    | DFSample
    deriving (Show)

data ConjureMode
    = ModeUnknown
    | ModeDiff FilePath FilePath
    | ModeRefineParam
        FilePath    -- Essence Param
        FilePath    -- Essence'
        FilePath    -- Essence' Param
    | ModeTranslateSolution
        (Maybe FilePath)    -- Input:  Essence Param
        FilePath            -- Input:  Essence'
        FilePath            -- Input:  Essence' Solution
        FilePath            -- Output: Essence  Solution
    | ModeTypeCheck
        (Maybe FilePath)    -- input
    | ModePrettify
        (Maybe FilePath)    -- input
        (Maybe FilePath)    -- output
    | ModeJSON
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
    deriving (Show)

data ConjureModeWithFlags
    = ConjureModeWithFlags
        ConjureMode                     -- the mode
        (M.HashMap String String)       -- all key-value pairs
        (S.HashSet String)              -- all flags
        [String]                        -- all the rest
        TimeLimit
    deriving (Show)

data TimeLimit = NoTimeLimit | TimeLimit Int -- seconds
    deriving (Show)

conjureHelp :: Doc
conjureHelp =  Pr.vcat  $ helpStart :
    [ modeDiff
    , modeRefineParam
    , modeTranslateSolution
    , modeTypeCheck
    , modePrettify
    , modeJSON
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

    modeJSON = mode "json" [
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
    optional = prBrackets
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
    , modeJSON
    , modeValidateSolution
    , modeDFAll
    , modeDFCompactParam
    , modeDFNoChannel
    , modeRandom
    , modeFirst
    , modeSmallest
    , modeSample
    ]
    where
        modeDiff = do
            mode $ words "diff"
            case rest of
                [in1, in2] -> returnMode $ ModeDiff in1 in2
                _          -> mzero

        modeRefineParam = do
            mode $ words "refineParam"
            inParam   <- key "--in-essence-param"
            inEprime  <- key "--in-eprime"
            outParam  <- key "--out-eprime-param"
            returnMode $ ModeRefineParam inParam inEprime outParam

        modeTranslateSolution = do
            mode $ words "transSol translateSol translateSolution"
            inParam          <- optional $ key "--in-essence-param"
            inEprime         <- key "--in-eprime"
            inEprimeSolution <- key "--in-eprime-solution"
            outSolution      <- anyKey $ words "--out-solution --out-essence-solution"
            returnMode $ ModeTranslateSolution
                        inParam
                        inEprime
                        inEprimeSolution
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

        modeJSON = do
            mode $ words "json"
            inp  <- optional $ anyKey $ words "--in-essence --in"
            out  <- optional $ anyKey $ words "--out-essence --out"
            returnMode $ ModeJSON inp out

        modeValidateSolution = do
            mode $ words "validateSolution validateSol validateSoln"
            essence  <- key "--in-essence"
            param    <- optional $ key "--in-param"
            solution <- key "--in-solution"
            returnMode $ ModeValidateSolution essence param solution

        modeDFAll = do
            mode $ words "df depthfirst depth-first"
            inEssence <- anyKey $ words "--in-essence --in"
            outDir    <- optional $ anyKey $ words "--output-directory --out-dir --out"
            limit     <- optional $ readKey "--limit-models"
            returnMode $ ModeMultipleOutput DFAll inEssence outDir (limit >>= nothingIfZero)

        modeDFCompactParam = do
            mode $ words "df-compact-param"
            inEssence <- anyKey $ words "--in-essence --in"
            outDir    <- optional $ anyKey $ words "--output-directory --out-dir --out"
            limit     <- optional $ readKey "--limit-models"
            returnMode $ ModeMultipleOutput DFCompactParam inEssence outDir (limit >>= nothingIfZero)

        modeDFNoChannel = do
            mode $ words "df-no-channelling"
            inEssence <- anyKey $ words "--in-essence --in"
            outDir    <- optional $ anyKey $ words "--output-directory --out-dir --out"
            limit     <- optional $ readKey "--limit-models"
            returnMode $ ModeMultipleOutput DFNoChannelling inEssence outDir (limit >>= nothingIfZero)

        modeRandom = do
            mode $ words "random rand rnd"
            modeSingleOutput $ ModeSingleOutput ModeRandom



        modeSample = do
            mode $ words "sample"
            inEssence <- anyKey $ words "--in-essence --in"
            outDir    <- optional $ anyKey $ words "--output-directory --out-dir --out"
            limit     <- readKey "--limit-models"
            returnMode $  ModeMultipleOutput DFSample inEssence outDir (Just limit)


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
        -- flagSet = (`S.member` flags)
        x =~= ys = map toLower x `elem` map (map toLower) ys

        returnMode m = do
            mtimelimit <- optional $ key "--timelimit"
            let timelimit = case mtimelimit of
                                Nothing -> NoTimeLimit
                                Just str -> case readMay str of
                                    Nothing -> userErr ("Cannot parse timelimit value as integer: " <+> pretty str)
                                    Just i  -> TimeLimit i
            return $ ConjureModeWithFlags m pairs flags rest timelimit

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
                   , "--pretty"
                   , "--no-dontCare"
                   ]

nothingIfZero :: Int -> Maybe Int
nothingIfZero 0 = Nothing
nothingIfZero x = Just x

