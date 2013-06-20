{-# LANGUAGE OverloadedStrings #-}
module Language.E.GenerateParams.Data where

import Bug
import Language.E
import Language.E.Pipeline.Driver

import Language.E.GenerateParams.Typedefs

import Control.Monad.State

import Data.Map(Map)
import qualified Data.Map as M
import Language.E.GenerateParams.Groom2(groom)
import Text.Show.Pretty(ppShow, ppDoc)

import Stuff.Pretty(prettyListDoc)
import Text.PrettyPrint(parens,braces)
import Data.Typeable(Typeable)

type Dom = E

data VarState =
    VarInt Integer Integer  --- for binary Search lower upper bound
    deriving (Show)

data ModelResults = ModelResults{
     minionTimeout     :: Bool
    ,minionSatisfiable :: Bool
    ,isOptimum         :: Bool
    }
    deriving(Show)


--TODO  use Sequence?

-- results' int number of models solved 
data ParamGenState = ParamGenState{
     presults         :: Map EssenceParamFP (Int, Map EprimeFP ModelResults )
    ,pvars            :: [(Text,Dom,VarState)]
    ,pgoodParams      :: [EssenceParamFP]
    ,pgoodParamsCount :: Int
    ,pprevSolved      :: Int
    ,pmodels          :: Int
} deriving (Show)


instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
    pretty (a,b,c) = prettyListDoc parens "," [pretty a, pretty b, pretty c]

instance Pretty ParamGenState where
    pretty p = "ParamGenState" <+>  braces ( prettyListDoc parens "," [
         _pp "pmodels"       pmodels           p
        ,_pp "pprevSolved"   pprevSolved       p
        ,_pc "pvars"         pvars             p
        ,_pp "pgoodParams#"  pgoodParamsCount  p
        ,_pc "pgoodParams"   pgoodParams       p
        ,_pg "presults"      presults          p
        ] )

        where
        _pc s f a =  s <> "=" <+> (_pm . f)  a
        _pp s f a = s <> "=" <+> (pretty . f) a
        _pg s f a = s <> "=" <+> ( pretty . groom. f) a

        _pm [] = "[]"
        _pm a  = vcat . map pretty $ a

instance Pretty VarState      where pretty = pretty . show
instance Pretty ModelResults  where pretty = pretty . show
{-instance Pretty ParamGenState where pretty = pretty . groom -}


type MonadParamGen a = State ParamGenState a

driverParamGen
    :: Bool     -- generate the *.logs file or not
    -> Bool     -- generate *.errors file or not
    -> FilePath -- the output filepath
    -> ParamGenState -- Starting state
    -> [(Either Doc (MonadParamGen EssenceParam), LogTree)]
    -> IO ParamGenState
driverParamGen logsOut _ pathOut startingState [(Right x, logs)] = do
    toFile  pathOut              (renderNormal (evalState x startingState) )
    _ <- when logsOut $ toFile (pathOut ++ ".logs" ) (renderWide logs)
    return $ execState x startingState

driverParamGen _ False _ _ [(Left x, _ )] = bug $ pretty x

driverParamGen logsOut True pathOut _ [(Left  x, logs)] = do
    toFile (pathOut ++ ".error") (renderNormal x)
    when logsOut $ toFile (pathOut ++ ".logs" ) (renderWide logs)
    bug $ pretty x

driverParamGen _ _ _ _ _ = error "driverParamGen could not happen"


getRights :: [(Either Doc [(Text, E)], LogTree)] -> IO [(Text,E)]
getRights [(Right x, logs )] = do
    printLogs  logs
    return x

getRights [(Left x, _ )]     = bug $ pretty x
getRights a                  = bug $ pretty $ show  a


startingParmGenState :: [(Text,Dom,VarState)] -> Int -> ParamGenState
startingParmGenState vs numEprime = ParamGenState{

    presults         = M.empty,
    pvars            = vs,
    pgoodParams      = [],
    pgoodParamsCount = 0,
    pprevSolved      = (-1),
    pmodels          = numEprime 
}
