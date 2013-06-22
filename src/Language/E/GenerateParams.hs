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

import Language.E.GenerateRandomParam.Common(mkLog,printPrettym,printPretty,tracePretty)
import Language.E.GenerateParams.Data
import Language.E.GenerateParams.Toolchain(runSavilerow,runModelsWithParam,gatherData)
import Language.E.GenerateParams.Typedefs

import Language.E.Lenses(viewReference,mkReference)

import Control.Arrow((&&&),arr,(***),(|||),(+++))
import Control.Monad.State(runState,modify)
import Data.List(permutations,transpose,mapAccumL,foldl1')

import System.Directory(getCurrentDirectory,getDirectoryContents,makeRelativeToCurrentDirectory,doesFileExist)
import System.FilePath((</>),(<.>),takeExtension,takeBaseName,takeFileName,replaceExtension)
import Language.E.GenerateParams.Groom2(groom)

import qualified Data.Map as Map

import Database.SQLite.Simple
import System.Environment(getEnv)

import qualified Data.Map as M

-- Moves this pretty stuff
{-
import Stuff.Pretty(prettyListDoc)
import Text.PrettyPrint(parens)
]

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
    printPretty "essence" essence
    vars <-  getRights $  runCompE "getVars" (getVars essence)
    printPrettym "vars" vars

    let varsWithState = map (\(e,dom) -> (e,dom,VarInt 1 70) ) vars
    let startingState = startingParmGenState varsWithState (length eprimes')
    printPretty "StartingState" startingState

    endState <- process eprimes' startingState
    {-let endState = startingState-}
    toFile (replaceExtension essenceFP "goodParams") (unlines . pgoodParams $ endState)
    return ()

    where
    eprimeDirName   = takeBaseName eprimeDir
    essenceBaseName = takeBaseName essenceFP

    process :: [EprimeFP] -> ParamGenState -> IO ParamGenState
    process  eprimes state =  do
        (paramPath,results) <- createParamAndRun eprimes state
        let newState = updateState (length eprimes) paramPath state results
        printPretty "updated" newState

        if finished newState then
            return newState
        else
            process eprimes newState

        where
        finished s    = all finishedVar (pvars s)
        finishedVar (_,_,VarInt lower upper) = lower > upper

    createParamAndRun :: [EprimeFP] -> ParamGenState -> IO (EssenceParamFP, [(EprimeFP,ModelResults)])
    createParamAndRun eprimes startingState = do
        let ((param,name),state)  = runState generateParam startingState
        paramPath <- makeRelativeToCurrentDirectory $ outputDir </> name <.> ".param"
        b <- doesFileExist paramPath
        if b then do
            printPretty "Skipping" ("Skipping " <+> pretty paramPath)
            let  (a,mapping) = fromMaybe (error $ "This can not happen param shoudld be removed" ++ groom (presults startingState) )
                               $ M.lookup paramPath $ presults startingState
            return (paramPath, M.toList mapping)
        else do

        toFile paramPath (renderNormal param)

        putStrLn "Running SR on each eprime with the param"
        runModelsWithParam eprimeDirName  paramPath eprimes

        putStrLn "Storing results in results.db"
        gatherData eprimeDirName
        results <- getData essenceBaseName (takeBaseName  paramPath)

        printPretty "paramPath" paramPath
        {-printPrettym "results" results-}
        return (paramPath,results)


updateState :: Int -> EssenceParamFP -> ParamGenState -> [(EprimeFP,ModelResults)] -> ParamGenState
updateState numEprimes paramFP state@ParamGenState{presults=pr}  results =
    let resultsMap = (solvedLen, M.fromList results)
        pr'        = M.insert  paramFP resultsMap pr

    in  state{presults         = pr'
             ,pgoodParams      = paramFP : pgoodParams  state
             ,pgoodParamsCount = 1 + pgoodParamsCount state
             ,pvars            = updateVars (pvars state) (pprevSolved state) solvedLen numEprimes 
             ,pprevSolved      = solvedLen
             }

    where
    solvedLen = length . filter (\(_,ModelResults{minionTimeout =t}) -> not t) $ results
    allLen    = length results

    updateVars :: [(Text,Dom,VarState)] -> Int -> Int -> Int -> [(Text,Dom,VarState)]
    updateVars vars prevSolved curSolved  total=
        map (updateVar prevSolved curSolved total) vars

updateVar :: Int -> Int -> Int -> (Text, Dom, VarState) ->  (Text, Dom, VarState)

updateVar prev cur total (name,dom,VarInt lower upper) | cur <= 1 =
    {-(name, dom, VarInt lower (mid - 1)  )-}
    tracePretty ("updateVar high" <+> "low=" <+> pretty lower
                <+> "high="  <+> pretty upper <+> "mid=" <+> pretty mid
                <+> "normal=" <+> pretty (mid - 1) <+> "bi="  <+> pretty (upper - move - 1)
                <+> "c/t" <+> pretty (cur,total)
                <+> "res=" ) 
    (name, dom, VarInt lower (upper - move - 1)  )

    where
    mid  = (lower + upper) `quot` 2
    move = floor $ toRational (upper - mid ) * frac prev cur total


-- Multiple models could solve the param, so try to find a harder param
updateVar prev cur total (name,dom,VarInt lower upper) =
    {-(name, dom, VarInt (mid + 1)  upper )-}
    tracePretty ("updateVar low" <+> "low=" <+> pretty lower
                <+> "high="  <+> pretty upper <+> "mid=" <+> pretty mid
                <+> "normal=" <+> pretty (mid + 1) <+> "bi="  <+> pretty (lower + move + 1)
                <+> "c/t" <+> pretty (cur,total)
                <+> "res=" ) 
    (name, dom, VarInt (lower + move + 1)  upper )

    where
    mid  = (lower + upper) `quot` 2
    move = floor $ toRational (mid - lower) * frac prev cur total 

frac :: Int -> Int -> Int -> Rational
frac 0 0 _  =  0.5
frac _ cur total =  toRational cur / toRational total

generateParam :: MonadParamGen  (EssenceParam,String)
generateParam = do
    varsDoms <- gets pvars
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

    where val = [xMake| value.literal := [Prim (I ( (lower + upper)  `quot` 2))] |]


-- Returns eprime, MinionTimeout, MinionSatisfiable, IsOptimum
getData :: EssenceFP -> EssenceParamFP -> IO [(EprimeFP, ModelResults)]
getData  essence param = do
    base_dir <- getEnv "REPOSITORY_BASE"
    conn     <- open $ base_dir ++ "/results.db"
    rawData ::[(String,Bool,Bool,Bool)]  <- query conn
        "Select eprime , Cast(MinionTimeout as Integer) as MinionTimeout,  Cast(MinionSatisfiable as Integer) as MinionSatisfiable, Cast(IsOptimum as Integer) as IsOptimum  From Timings2 where essence = ? and param = ? Order by eprime"
        [essence,param]
    close conn
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

    let 
        {-enumMapping1     = getEnumMapping essence-}
        {-enums1           = getEnumsAndUnamed essence-}
        {-(enumMapping, _) = convertUnamed enumMapping1 enums1-}
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
        {-stays [xMatch| _ := topLevel.letting           |] = True-}
        stays [xMatch| _ := topLevel.where             |] = True
        stays _ = False


-- e.g _r "prob109-test"

_r :: String -> IO ()
_r name = do
    {-let mode = "-df-compact-param-better"-}
    let mode = "-df"
    let dir = "/Users/bilalh/CS/paramgen/models/_other" </> name
    generateParams (dir </> name <.> "essence") (dir </> name ++ mode) (dir </> "params")


_x :: [(Either Doc a, LogTree)] -> IO ()
_x ((_, lg):_) =   print (pretty lg)
_x _ = return ()

_bug :: String -> [E] -> t
_bug  s = upBug  ("GenerateParams: " ++ s)
_bugg :: String -> t
_bugg s = _bug s []

