{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds  #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.E.GenerateParams where

import Bug
import Language.E hiding (mkLog)
import Language.E.DomainOf(domainOf)
import Language.E.Imports
import Language.E.NormaliseSolution(normaliseSolutionEs)
import Language.E.Pipeline.Driver ( driverConjureSingle,toFile )
import Language.E.Pipeline.ReadIn(readSpecPreambleFromFile)
import Language.E.Up.Debug(upBug)
import Language.E.Up.EprimeToEssence(convertUnamed)
import Language.E.Up.GatherInfomation(getEnumMapping,getEnumsAndUnamed)
import Language.E.Up.ReduceSpec(reduceSpec,removeNegatives)
import Language.E.ValidateSolution(validateSolutionPure)

import Language.E.GenerateRandomParam.Common(mkLog,printPrettym,printPretty)
import Language.E.GenerateParams.Data
import Language.E.GenerateParams.Toolchain(runSavilerow,runModelsWithParam,gatherData)
import Language.E.GenerateParams.Typedefs

import Language.E.Lenses(viewReference,mkReference)

import Control.Arrow((&&&),arr,(***),(|||),(+++))
import Control.Monad.State(runState,modify)
import Data.List(permutations,transpose,mapAccumL,foldl1')

import System.Directory(getCurrentDirectory,getDirectoryContents,makeRelativeToCurrentDirectory)
import System.FilePath((</>),(<.>),takeExtension,takeBaseName,takeFileName)
import Text.Groom(groom)

import qualified Data.Map as Map

import Database.SQLite.Simple
import System.Environment(getEnv)

-- Moves this pretty stuff
import Stuff.Pretty(prettyListDoc)
import Text.PrettyPrint(parens)
{-
instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
    pretty (a,b,c) = prettyListDoc parens "," [pretty a, pretty b, pretty c]

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a,b,c,d) where
    pretty (a,b,c,d) = prettyListDoc parens "," [pretty a, pretty b, pretty c, pretty d]
-}
type EprimeDir = FilePath


generateParams :: EssenceFP -> EprimeDir -> OutputDir -> IO ()
generateParams essenceFP eprimeDir outputDir = do
    essence <- readSpecPreambleFromFile essenceFP

    eprimes <- liftM  ( map ( eprimeDir </>  ) . filter  ( (==) ".eprime" . takeExtension ) )
                      (getDirectoryContents eprimeDir)

    -- Make paths relative, to avoid hard coding
    eprimes' <-  mapM makeRelativeToCurrentDirectory eprimes

    putStrLn  "Create a param"
    vars <-  getRights $  runCompE "getVars" (getVars essence)
    printPrettym "vars" vars
    let varsWithState = map (\(e,dom) -> (e,dom,VarInt 1 9) ) vars
    let startingState = startingParmGenState varsWithState

    (paramPath,results) <- createParamAndRun eprimes' startingState
    let solvedLen = length . filter (\(_,ModelResults{minionTimeout =t}) -> t) $ results
        allLen    = length results 

    printPretty "solved,all" (solvedLen,allLen)

    return ()

    where
    eprimeDirName = takeBaseName eprimeDir
    essenceBaseName = takeBaseName essenceFP


    createParamAndRun :: [EprimeFP] -> ParamGenState -> IO (EssenceParamFP, [(EprimeFP,ModelResults)])
    createParamAndRun eprimes startingState = do
        let ((param,name),state)  = runState generateParam startingState
        paramPath <- makeRelativeToCurrentDirectory $ outputDir </> name <.> ".param"
        toFile paramPath (renderNormal param)

        putStrLn "Running SR on each eprime with the param"
        runModelsWithParam eprimeDirName  paramPath eprimes

        putStrLn "Storing results in results.db"
        gatherData eprimeDirName
        results <- getData essenceBaseName (takeBaseName  paramPath)

        printPretty "paramPath" paramPath
        printPrettym "results" results
        return (paramPath,results)



generateParam :: MonadParamGen  (EssenceParam,String)
generateParam = do
    varsDoms <- gets vars
    let (newState,vars) = unzip $ map createValue varsDoms
    {-modify (\p@ParamGenState{vars=v} -> p{vars=newState})-}
    res <- wrapping vars
    return (res, createName vars)

    where
    createName :: [(Text,E)] -> String
    createName = intercalate "-" . map createName'

    createName' :: (Text,E) -> String 
    createName' (_,e) = show . pretty $ e

createValue :: (Text,Dom, VarState) -> ((Text,Dom,VarState), (Text,E) )
createValue (name, e, s@(VarInt lower upper)) =
    ( (name,e,s), (name, val) )

    where val = [xMake| value.literal := [Prim (I (lower + upper  `div` 2))] |]

-- Returns eprime, MinionTimeout, MinionSatisfiable, IsOptimum
getData :: EssenceFP -> EssenceParamFP -> IO [(EprimeFP, ModelResults)]
getData  essence param = do
    base_dir <- getEnv "REPOSITORY_BASE"
    conn     <- open $ base_dir ++ "/results.db"
    rawData ::[(String,Bool,Bool,Bool)]  <- query conn
        "Select eprime , Cast(MinionTimeout as Integer) as MinionTimeout,  Cast(MinionSatisfiable as Integer) as MinionSatisfiable, Cast(IsOptimum as Integer) as IsOptimum  From Timings2 where essence = ? and param = ? Order by eprime"
        [essence,param]
    return $ map convert rawData

    where convert (e,b1,b2,b3) = (e, ModelResults b1 b2 b3)

getVars :: (MonadConjure m) => Essence ->  m [(Text,E)]
getVars essence = do
    givens <- plumming essence
    doms <- mapM domainOf givens
    let refs = map getRef givens

    let vars = zip refs doms
    mkLog "vars" (vcat $ map pretty vars)
    return vars




plumming :: MonadConjure m => Spec -> m [E]
plumming essence' = do
    essence <- removeNegatives essence'
    let stripped@(Spec _ f) = stripDecVars essence
    (Spec _ e) <- reduceSpec stripped

    {-mkLog "GivensSpec"   (vcat .  map (pretty . prettyAsTree) $  (statementAsList f))-}
    mkLog "GivensSpec" (pretty f)

    let enumMapping1     = getEnumMapping essence
        enums1           = getEnumsAndUnamed essence
        (enumMapping, _) = convertUnamed enumMapping1 enums1
        filterer [xMatch| _ := topLevel.where  |] = False
        filterer _ = True
        es  = filter filterer (statementAsList e)

    mkLog "Reduced   " $ pretty es <+> "\n"
    {-mkLog "enums" (pretty . groom $ enumMapping)-}

    return es


wrapping :: (Monad m) => [(Text,E)] -> m EssenceParam
wrapping vars = do
    let lettings = map (uncurry  makeLetting) vars
    {-mkLog "Lettings" (vcat $ map pretty lettings)-}
    --mkLog "Lettings" (vcat $ map (\a -> prettyAsTree a <+> "\n" ) lettings )

    let essenceParam = Spec ("Essence", [1,3]) (listAsStatement lettings )
    return essenceParam


makeLetting :: Text -> E -> E
makeLetting name val =
    [xMake| topLevel.letting.name := [mkReference name]
          | topLevel.letting.expr := [val]|]

getRef :: E -> Text
getRef [xMatch|  _           := topLevel.declaration.given.name.reference
              | [Prim (S n)] := topLevel.declaration.given.name.reference |] = n
getRef e = error . show . prettyAsTree $  e


stripDecVars :: Essence -> Essence
stripDecVars (Spec v x) = Spec v y
    where
        xs = statementAsList x
        ys = filter stays xs
        y  = listAsStatement ys

        stays [xMatch| _ := topLevel.declaration.given |] = True
        stays [xMatch| _ := topLevel.letting           |] = True
        stays [xMatch| _ := topLevel.where             |] = True
        stays _ = False


-- e.g _r "prob109-test"

_r :: String -> IO ()
_r name = do
    let mode = "-df-compact-param-better"
    let dir = "/Users/bilalh/CS/paramgen/models/_other" </> name
    generateParams (dir </> name <.> "essence") (dir </> name ++ mode) (dir </> "params")


_x :: [(Either Doc a, LogTree)] -> IO ()
_x ((_, lg):_) =   print (pretty lg)
_x _ = return ()

_bug :: String -> [E] -> t
_bug  s = upBug  ("GenerateParams: " ++ s)
_bugg :: String -> t
_bugg s = _bug s []

