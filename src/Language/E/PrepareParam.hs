{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
module Language.E.PrepareParam(prepareParamSpecification) where

import Bug
import Language.E hiding (mkLog)
import Language.E.GenerateRandomParam.Common(mkLog)
import Language.E.Up.IO(getSpec')


import System.Directory(getCurrentDirectory)
import System.FilePath((</>))

--import Text.Groom(groom)

type Essence          = Spec
type EssenceParamSpec = Spec

prepareParamSpecification :: (MonadConjure m) => Essence -> m EssenceParamSpec
prepareParamSpecification (Spec v es) = do
    --mkLog "start " ( vcat . map prettyAsPaths $ (statementAsList es))
    mkLog "start " (pretty es)

    let es' = filter removeStuff  (statementAsList es)

    let (result,_) = head .  runCompE "Transform" $ bottomUpE' changer  (listAsStatement es')
    let res = either (bug . pretty) id result

    mkLog "result" (pretty . sort . statementAsList $ res)
    return $ Spec v res

changer :: MonadConjure m => E -> m E
changer [xMatch| dom := topLevel.declaration.given  |] =
 return [xMake|  topLevel.declaration.find := dom   |]

changer [xMatch| dom := topLevel.where    |] =
 return [xMake|  topLevel.suchThat := dom |]

changer  e= return e

removeStuff :: E -> Bool
removeStuff [xMatch| _ := topLevel.declaration.find |] = False
removeStuff [xMatch| _ := topLevel.suchThat |]         = False
removeStuff [xMatch| _ := topLevel.objective |]        = False
removeStuff _ = True


-- Debuging
-- _x =<< _r "everything"
_r :: FilePath -> IO [(Either Doc EssenceParamSpec, LogTree)]
_r name= _e (_getTest name)

_e :: IO Essence -> IO [(Either Doc EssenceParamSpec, LogTree)]
_e sp = do
    spec <- sp
    return $ runCompE "gen" (prepareParamSpecification spec)

-- Only print the logs
_x :: [(Either Doc a, LogTree)] -> IO ()
_x ((_, lg):_) =   print (pretty lg)
_x _ = return ()

_getTest :: FilePath -> IO Spec
_getTest f = do
    dir <- getCurrentDirectory  -- Assume running from conjure directory
    getSpec' False $ dir </> "test/generateParams" </> f  ++ ".essence"

