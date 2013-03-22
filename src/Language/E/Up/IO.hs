{-# LANGUAGE OverloadedStrings  #-}
module Language.E.Up.IO (
     getSpecs
    ,getSpecMaybe
    ,getSpec
    ,getSpec'
    ,getFiles
    ,getTestSpecs
) where

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Up.ReduceSpec
import Language.E.Up.Debug


import qualified Data.Text as T

import System.FilePath
import System.Directory (doesFileExist)

type Essence   = Spec
type Eprime    = Spec
type ESolution = Spec
type EssenceWithParamOnly = Spec

getSpec ::  FilePath -> IO Spec
getSpec = getSpec' True

getSpecMaybe :: Maybe FilePath -> Maybe (IO Spec)
getSpecMaybe (Just f) = Just (getSpec f)
getSpecMaybe Nothing = Nothing 

getSpec' ::  Bool -> FilePath -> IO Spec
getSpec' removeContraints filepath = do
    (fp,txt) <-  pairWithContents filepath

    -- I don't need any of the constraints, speed up running the test lot
    let txt' = if removeContraints then
                   (func "maximising" . func "minimising" . func  "such that")  txt
                else txt
    handleInIOSingle =<< runCompEIOSingle
                "Parsing problem specification"
                (readSpec (fp,txt') )

    where 
        func t =  fst . T.breakOn t


getSpecs :: (FilePath, FilePath, FilePath, Maybe FilePath,Maybe FilePath) 
         -> IO (Eprime, ESolution, Essence,EssenceWithParamOnly)
getSpecs (specF, solF, orgF,paramF,orgParamF) = do
    let param    = getSpecMaybe paramF
    let orgParam = getSpecMaybe orgParamF

    spec  <- getSpec specF >>= introduceParams param >>= reduceSpec >>= simSpecMaybe param >>= removeNegatives
    sol   <- getSpec solF  >>= removeNegatives >>= removeIndexRanges
    orgP  <- getSpec orgF  >>= introduceParams orgParam
    org   <- reduceSpec  orgP
    return (spec,sol,org,orgP)


getTestSpecs :: (FilePath, FilePath, FilePath, Maybe FilePath, Maybe FilePath) 
             -> IO (Eprime, ESolution, Essence,EssenceWithParamOnly)
getTestSpecs (specF, solF, orgF,paramF,orgParamF) = do
    param    <- getTestSpecMaybe paramF
    orgParam <- getTestSpecMaybe orgParamF

    spec  <- getSpec specF >>= introduceParams param >>= reduceSpec >>= simSpecMaybe param >>= removeNegatives
    sol   <- getSpec solF  >>= removeNegatives >>= removeIndexRanges
    orgP  <- getSpec orgF  >>= introduceParams orgParam
    org   <- reduceSpec  orgP
    return (spec,sol,org,orgP)

getTestSpecMaybe :: Maybe FilePath -> IO (Maybe (IO Spec))
getTestSpecMaybe (Just f) = do 
    b <- doesFileExist f
    if b then 
         return $  (Just . getSpec) f 
    else 
        return Nothing

getTestSpecMaybe Nothing = return Nothing


getFiles :: String -> String ->  Int -> (FilePath, FilePath, FilePath,Maybe FilePath,Maybe FilePath)
getFiles base name n =
   let spec = addExtension (joinPath [base,name, zeroPad n]) "eprime" in
   (spec
   ,addExtension spec "solution"
   ,addExtension (joinPath [base,name]) "essence"
   ,Just $ addExtension (joinPath [base,name,takeBaseName name]) "param"
   ,Just $ addExtension (joinPath [base,name,takeBaseName name]) "essence-param"
   )


-- Only need to simplify the expressions if there are parameters in the expressions.
simSpecMaybe :: Monad m => Maybe a -> Spec -> m Spec
simSpecMaybe Nothing s = return s
simSpecMaybe (Just _) s = simSpec s


zeroPad :: Int -> String
zeroPad n = replicate (4 - length sn) '0'  ++ sn
 where sn = show n

_bug :: String -> [E] -> t
_bug  s = upBug  ("Up.IO: " ++ s)
_bugi :: (Show a) => String -> (a, [E]) -> t
_bugi s = upBugi ("Up.IO: " ++ s )
_bugg :: String -> t
_bugg s = _bug s []

