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

        statements =  consGen neighbourhoodSizeVar

    in
        [ SNS_Group neighbourhoodGroupName [var]
        , SNS_Neighbourhood neighbourhoodName
                            neighbourhoodGroupName
                            neighbourhoodSizeName
                            [essenceDomain| int(1..&neighbourhoodSize) |]
                            statements
        ]



type NeighbourhoodGenResult = (Name, Expression, Expression -> [Statement])
type MultiContainerNeighbourhoodGenResult = (Name, Expression, Int, Int, Expression -> [Expression] -> [Expression] -> [Statement])


allNeighbourhoods :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
allNeighbourhoods theIncumbentVar theVar domain = concatMapM (\ gen -> gen theIncumbentVar theVar domain )
    [setLiftSingle
    , setLiftMultiple 
     , setRemove
     , setAdd
     , setSwap
     , setSwapAdd
     , sequenceReverseSub
     
    ]

multiContainerNeighbourhoods :: NameGen m => Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
multiContainerNeighbourhoods domain = concatMapM (\ gen -> gen domain )
    [setMove
    , setCrossOver
    , setSplit
    , setMerge]

makeFrameUpdate :: NameGen m => Int -> Int -> Expression -> Expression -> m ([Expression], [Expression], Expression -> Expression)
makeFrameUpdate numberIncumbents numberPrimaries theIncumbentVar theVar = do
    incumbents <- replicateM numberIncumbents auxiliaryVar
    primaries <- replicateM numberPrimaries auxiliaryVar
    return (map snd incumbents, map snd primaries, buildFrameUpdate (map fst incumbents) (map fst primaries)) where
        --todo written by saad
        --below local function should build a frame update with any number of arguments lifted.
         --not sure how to do this in haskel so special cased 1 and 2 lifts
        buildFrameUpdate :: [Name] -> [Name] -> (Expression -> Expression)
        buildFrameUpdate [iPat1,iPat2] [jPat1,jPat2] = \c -> [essence| frameUpdate(&theIncumbentVar, &theVar, [&iPat1,&iPat2], [&jPat1,&jPat2], &c) |]
        buildFrameUpdate [iPat1] [jPat1] = \c -> [essence| frameUpdate(&theIncumbentVar, &theVar, [&iPat1], [&jPat1], &c) |]
        buildFrameUpdate [iPat1,iPat2] [jPat1] = \c -> [essence| frameUpdate(&theIncumbentVar, &theVar, [&iPat1,&iPat2], [&jPat1], &c) |]
        buildFrameUpdate [iPat1] [jPat1,jPat2] = \c -> [essence| frameUpdate(&theIncumbentVar, &theVar, [&iPat1], [&jPat1,&jPat2], &c) |]

setLiftSingle :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setLiftSingle theIncumbentVar theVar (DomainSet _ _ inner) = do
    let generatorName = "setLiftSingle"
    ([incumbent_i], [i], frameUpdate) <- makeFrameUpdate 1 1 theIncumbentVar theVar
    let
        liftCons (SuchThat cs) = SuchThat [frameUpdate $ make opAnd $ fromList cs]
        liftCons st            = st

    ns <- allNeighbourhoods incumbent_i i inner
    return
        [ ( mconcat [generatorName, "_", innerGeneratorName]
        , innerNeighbourhoodSize
          , \ neighbourhoodSize ->
              let statements = rule neighbourhoodSize 
              in  map liftCons statements
          )
        | (innerGeneratorName, innerNeighbourhoodSize, rule) <- ns
        ]
setLiftSingle _ _ _ = return []


setLiftMultiple :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setLiftMultiple theIncumbentVar theVar (DomainSet _ _ inner) = do
    let generatorName = "setLiftMultiple"
    let
        liftCons frame (SuchThat cs) = SuchThat [ let conjCs  =  make opAnd $ fromList cs in
            frame conjCs]
        liftCons frame st = st

    ns :: [MultiContainerNeighbourhoodGenResult] <- multiContainerNeighbourhoods inner
    mapM (\ (innerGeneratorName, innerNeighbourhoodSize, numberIncumbents, numberPrimaries, rule)  -> do
        (incumbents, primaries, frame) <- makeFrameUpdate numberIncumbents numberPrimaries theIncumbentVar theVar
        return  ( mconcat [generatorName, "_", innerGeneratorName]
            , innerNeighbourhoodSize
            , \ neighbourhoodSize -> let statements = rule neighbourhoodSize incumbents primaries in
                map (liftCons frame) statements)) ns

setLiftMultiple _ _ _ = return []


setRemove :: Monad m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setRemove theIncumbentVar theVar theDomain@(DomainSet{}) = do
    let generatorName = "setRemove"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    return
        [( generatorName, calculatedMaxNhSize
         , \ neighbourhoodSize ->
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
         , \ neighbourhoodSize  ->
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
         , \ neighbourhoodSize  ->
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
         , \ neighbourhoodSize  ->
                [essenceStmts|
                    such that
                        |&theVar - &theIncumbentVar| = &neighbourhoodSize
                |]
        )]
setSwapAdd _ _ _ = return []





setMove :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
setMove theDomain@(DomainSet{}) = do
    let generatorName = "setMove"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    let numberIncumbents = 2
    let numberPrimaries = 2
    (kPat, k) <- quantifiedVar

    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize [theIncumbentVar1, theIncumbentVar2] [theVar1,theVar2] ->
                 [essenceStmts|
                    such that
                    &theVar1 subsetEq &theIncumbentVar1,
                    |&theIncumbentVar1| - |&theVar1| = &neighbourhoodSize,
                    &theIncumbentVar2 subsetEq &theVar2,
                    |&theVar2| - |&theIncumbentVar2| = &neighbourhoodSize,
                    and([&k in &theVar2 | &kPat <- &theIncumbentVar1, !(&k in &theVar1)])
                     |]
        )]
setMove _ = return []



setCrossOver :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
setCrossOver theDomain@(DomainSet{}) = do
    let generatorName = "setCrossOver"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    let numberIncumbents = 2
    let numberPrimaries = 2
    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize [theIncumbentVar1, theIncumbentVar2] [theVar1,theVar2] ->
             [essenceStmts|
             such that
            &theIncumbentVar1 union &theIncumbentVar2 = &theVar1 union &theVar2,
            |&theVar1 - &theIncumbentVar1| = &neighbourhoodSize,
            |&theVar2 - &theIncumbentVar2| = &neighbourhoodSize
                     |]
        )]
setCrossOver _ = return []


setSplit :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
setSplit theDomain@(DomainSet{}) = do
    let generatorName = "setSplit"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    let numberIncumbents = 1
    let numberPrimaries = 2
    (kPat, k) <- quantifiedVar

    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize [theIncumbentVar1] [theVar1,theVar2] ->
                 [essenceStmts|
                such that
                &theVar1 subsetEq &theIncumbentVar1,
                |&theIncumbentVar1| - |&theVar1| = &neighbourhoodSize,
                |&theVar2| = &neighbourhoodSize,
                and([&k in &theVar2 | &kPat <- &theIncumbentVar1, !(&k in &theVar1)])
                     |]
        )]
setSplit _ = return []


setMerge :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
setMerge theDomain@(DomainSet{}) = do
    let generatorName = "setMerge"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    let numberIncumbents = 2
    let numberPrimaries = 1
    (kPat, k) <- quantifiedVar

    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize [theIncumbentVar1, theIncumbentVar2] [theVar1] ->
                 [essenceStmts|
                such that
                |&theIncumbentVar1| <= &neighbourhoodSize,
                &theIncumbentVar1 union &theIncumbentVar2  = &theVar1
                 |]
        )]
setMerge _ = return []



sequenceReverseSub :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceReverseSub theIncumbentVar theVar theDomain@(DomainSequence _ (SequenceAttr sizeAttr _) _) = do

    let generatorName = "sequenceReverseSub"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain

    (iPat, i) <- auxiliaryVar
    (jPat, j) <- auxiliaryVar
    (kPat, k) <- quantifiedVar

    return
        [( generatorName
        , calculatedMaxNhSize 
         , \ neighbourhoodSize ->
                [essenceStmts|
                    find &iPat, &jPat :  int(1..&calculatedMaxNhSize)
                    such that
                        and([ &j - &i = &neighbourhoodSize
                        , &i <= |&theVar|
                        , &j <= |&theVar|
                        , and([ &theVar(&k) = &theIncumbentVar(&k)
                              | &kPat : int(1..&calculatedMaxNhSize)
                              , &k < &i \/ &k > &j
                              , &k <= |&theVar|
                              ])
                        , and([ &theVar(&i + &k) = &theIncumbentVar(&j - &k)
                              | &kPat : int(0..&calculatedMaxNhSize)
                              , &k <= &neighbourhoodSize
                              ])
                        ])
                |]
        )]
sequenceReverseSub _ _ _ = return []

