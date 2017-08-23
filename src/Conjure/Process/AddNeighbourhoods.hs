{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.AddNeighbourhoods where

import Conjure.Prelude
import Conjure.Language
import Conjure.UI.TypeCheck ( typeCheckModel )
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Language.Expression.DomainSizeOf ( getMaxNumberOfElementsInContainer )
import qualified Conjure.Rules.Definition as Config ( Config(..) )


addNeighbourhoods
    :: ( NameGen m
       , MonadFail m
       , MonadUserError m
       , MonadLog m
       )
    => Config.Config -> Model -> m Model
addNeighbourhoods config inpModel | not (Config.generateNeighbourhoods config) = return inpModel
addNeighbourhoods _ inpModel = do
    neighbourhoods <-
        concatForM (mStatements inpModel) $ \case
            Declaration (FindOrGiven Find nm dom) -> generateNeighbourhoods nm (Reference nm Nothing) dom
            _ -> return []
    let outModel = inpModel { mStatements = mStatements inpModel
                                         ++ neighbourhoods
                            }
    -- TODO: this is here only temporarily, remove!
    traceM $ unlines [ "Added the following SNS Neighbourhoods"
                     , show $ pretty (inpModel { mStatements = neighbourhoods
                                               , mInfo = def
                                               })
                     ]
    (resolveNames >=> typeCheckModel) outModel


generateNeighbourhoods :: NameGen m => Name -> Expression -> Domain () Expression -> m [Statement]
generateNeighbourhoods theVarName theVar domain = do
    neighbourhoods <- allNeighbourhoods theVar domain
    return $ nub $ concatMap (skeleton theVarName theVar domain) neighbourhoods


skeleton
    :: Name -> Expression -> Domain () Expression
    -> NeighbourhoodGenResult
    -> [Statement]
skeleton varName var domain gen =
    let

        (generatorName, neighbourhoodSize,  consGen) = gen

        neighbourhoodGroupName = mconcat [varName, "_neighbourhoodGroup"]

        neighbourhoodName     = mconcat [varName, "_", generatorName]
        neighbourhoodSizeName = mconcat [neighbourhoodName, "_", "size"]
        neighbourhoodSizeVar = Reference neighbourhoodSizeName Nothing

        statements =  consGen neighbourhoodSizeVar neighbourhoodSize

    in
        [ SNS_Group neighbourhoodGroupName [var]
        , SNS_Neighbourhood neighbourhoodName
                            neighbourhoodGroupName
                            neighbourhoodSizeName
                            [essenceDomain| int(1..&neighbourhoodSize) |]
                            statements
        ]



type NeighbourhoodGenResult = (Name, Expression, Expression -> Expression -> [Statement])


allNeighbourhoods :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
allNeighbourhoods theVar domain = concatMapM (\ gen -> gen theVar domain )
    [ 
     setRemove
    ]




setRemove :: Monad m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setRemove theVar domain@(DomainSet{}) = do
    let generatorName = "setRemove"
    let neighbourhoodMaxSize = getMaxNumberOfElementsInContainer domain
    return
        [( generatorName, neighbourhoodMaxSize
         , \ neighbourhoodSize _maxNeighbourhoodSize ->
                 [essenceStmts|
                    such that
                        &theVar subsetEq incumbent(&theVar),
                        |incumbent(&theVar)| - |&theVar| = &neighbourhoodSize
                 |]
        )]
setRemove _ _ = return []
