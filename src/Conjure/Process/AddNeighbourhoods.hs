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
    let theIncumbentVar = [essence|incumbent(&theVar)|]
    neighbourhoods <- allNeighbourhoods theIncumbentVar theVar domain
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


allNeighbourhoods :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
allNeighbourhoods theIncumbentVar theVar domain = concatMapM (\ gen -> gen theIncumbentVar theVar domain )
    [setLiftSingle 
     , setRemove
    ]





setLiftSingle :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setLiftSingle theIncumbentVar theVar (DomainSet _ _ inner) = do
    let generatorName = "setLiftSingle"
    (incumbent_iPat, incumbent_i) <- auxiliaryVar
    (iPat, i) <- auxiliaryVar
    let
        liftCons (SuchThat cs) = SuchThat [ let conjCs  =  make opAnd $ fromList cs in
            [essence| frameUpdate(&theIncumbentVar, &theVar, [&incumbent_iPat], [&iPat], &conjCs ) |]]
        liftCons st            = st

    ns <- allNeighbourhoods incumbent_i i inner
    return
        [ ( mconcat [generatorName, "_", innerGeneratorName]
        , innerNeighbourhoodSize
          , \ neighbourhoodSize maxNeighbourhoodSize ->
              let statements = rule neighbourhoodSize maxNeighbourhoodSize
              in  map liftCons statements
          )
        | (innerGeneratorName, innerNeighbourhoodSize, rule) <- ns
        ]
setLiftSingle _ _ _ = return []

setRemove :: Monad m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setRemove theIncumbentVar theVar domain@(DomainSet{}) = do
    let generatorName = "setRemove"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer domain
    return
        [( generatorName, calculatedMaxNhSize
         , \ neighbourhoodSize _maxNeighbourhoodSize ->
                 [essenceStmts|
                    such that
                        &theVar subsetEq &theIncumbentVar,
                        |&theIncumbentVar| - |&theVar| = &neighbourhoodSize
                 |]
        )]
setRemove _ _ _ = return []






setAdd :: Monad m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setAdd theIncumbentVar theVar theDomain@(DomainSet{}) = do
    let generatorName = "setAdd"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    return
        [( generatorName
        , calculatedMaxNhSize 
         , \ neighbourhoodSize _maxNeighbourhoodSize ->
                [essenceStmts|
                    such that
                        &theIncumbentVar subsetEq &theVar,
                        |&theVar| - |&theIncumbentVar| = &neighbourhoodSize
                |]
        )]
setAdd _ _ _ = return []


setSwap :: Monad m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setSwap theIncumbentVar theVar theDomain@(DomainSet{}) = do
    let generatorName = "setSwap"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    return
        [( generatorName
        , calculatedMaxNhSize 
         , \ neighbourhoodSize _maxNeighbourhoodSize ->
                [essenceStmts|
                    such that
                        |&theVar - &theIncumbentVar| = &neighbourhoodSize,
                        |&theIncumbentVar| = |&theVar|
                |]
        )]
setSwap _ _ _ = return []


setSwapAdd :: Monad m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setSwapAdd theIncumbentVar theVar theDomain@(DomainSet{}) = do
    let generatorName = "setSwapAdd"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    return
        [( generatorName
        , calculatedMaxNhSize
         , \ neighbourhoodSize _maxNeighbourhoodSize ->
                [essenceStmts|
                    such that
                        |&theVar - &theIncumbentVar| = &neighbourhoodSize
                |]
        )]
setSwapAdd _ _ _ = return []

