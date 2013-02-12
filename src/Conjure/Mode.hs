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
    = RefineParam
        FilePath    -- Essence
        FilePath    -- Essence Param
        FilePath    -- Essence'
        FilePath    -- Essence' Param
    | DFAll
        FilePath    -- Essence
    | Random
        FilePath    -- Essence
    deriving ( Show )

parseArgs :: GenericArgs -> Maybe ConjureMode
parseArgs (pairs, _flags, _rest) = msum
    [ refineParam
    , dfAll
    , random
    ]
    where
        key = (`lookup` pairs)
        -- flag = (`elem` flags)
        x =~= ys = map toLower x `elem` map (map toLower) ys

        refineParam = do
            mode      <- key "--mode"
            guard (mode =~= words "refineparam")
            inEssence <- key "--in-essence"
            inParam   <- key "--in-param"
            outEprime <- key "--in-eprime"
            outParam  <- key "--out-param"
            return $ RefineParam inEssence inParam outEprime outParam

        dfAll = do
            mode <- key "--mode"
            guard (mode =~= words "df depthfirst")
            inEssence <- key "--in-essence"
            return $ DFAll inEssence

        random = do
            mode <- key "--mode"
            guard (mode =~= words "rand random")
            inEssence <- key "--in-essence"
            return $ Random inEssence

allKeys :: [String]
allKeys =
    [ "--mode"
    , "--in-essence"
    , "--in-param"
    , "--in-eprime"
    , "--out-param"
    ]

allFlags :: [String]
allFlags =
    [ "--trace-logs"
    ]

