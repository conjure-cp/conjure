{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.AddNeighbourhoods where
import Conjure.Bug
import Conjure.Prelude
import Conjure.Language
import Conjure.UI.TypeCheck ( typeCheckModel )
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Language.Expression.DomainSizeOf ( getMaxNumberOfElementsInContainer )
import qualified Conjure.Rules.Definition as Config ( Config(..) )
import Conjure.Language.DomainSizeOf ( domainSizeOf )

addNeighbourhoods
    :: ( NameGen m
       , MonadFail m
       , MonadUserError m
       , MonadLog m
       )
    => Config.Config -> Model -> m Model
addNeighbourhoods config inpModel | not (Config.generateNeighbourhoods config) = return inpModel
addNeighbourhoods config inpModel = do
    neighbourhoods <-
        concatForM (mStatements inpModel) $ \case
            Declaration (FindOrGiven Find nm dom) -> generateNeighbourhoods nm (Reference nm Nothing) dom
            _ -> return []
    -- TODO: this is here only temporarily, remove!
    traceM $ unlines [ "Generated the following SNS Neighbourhoods (" ++ show (length neighbourhoods) ++ " of them in total)"
                     , case Config.filterNeighbourhoods config of
                         [] -> "Adding all of them to the model, use --filter-neighbourhoods to select a subset."
                         xs -> "Adding only these due to --filter-neighbourhoods " ++ show xs
                     , show $ pretty (inpModel { mStatements = neighbourhoods
                                               , mInfo = def
                                               })
                     ]
    let outModel = inpModel { mStatements = mStatements inpModel
                                         -- filter and add those neighbourhoods that we are filtering
                                         ++ [ n | (i, n) <- zip [1..] neighbourhoods
                                                , i `elem` Config.filterNeighbourhoods config
                                                ]
                                         -- add all neighbourhoods if we are not filtering
                                         ++ if null (Config.filterNeighbourhoods config)
                                             then neighbourhoods
                                             else []
                            }
    (resolveNames >=> typeCheckModel) outModel


generateNeighbourhoods :: NameGen m => Name -> Expression -> Domain () Expression -> m [Statement]
generateNeighbourhoods theVarName theVar domain = do
    let theIncumbentVar = [essence| incumbent(&theVar) |]
    neighbourhoods <- allNeighbourhoods theIncumbentVar theVar domain
    return $ sortNub $ concatMap (skeleton theVarName theVar domain) neighbourhoods


skeleton
    :: Name -> Expression -> Domain () Expression
    -> NeighbourhoodGenResult
    -> [Statement]
skeleton varName var _ gen =
    let
        (generatorName, neighbourhoodSize, consGen) = gen

        neighbourhoodGroupName = mconcat [varName, "_neighbourhoodGroup"]

        neighbourhoodName     = mconcat [varName, "_", generatorName]
        neighbourhoodSizeName = "size"
        neighbourhoodSizeVar  = Reference neighbourhoodSizeName Nothing

        statements = consGen neighbourhoodSizeVar

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
    [ mSetOrSetLiftSingle
    , functionLiftSingle
    , functionLiftMultiple
    , mSetOrSetLiftMultiple
    , mSetOrSetRemove
    , mSetOrSetAdd
    , mSetOrSetSwap
    , mSetOrSetSwapAdd
    , mSetOrSetSwapRemove
    , sequenceReverseSub
    , sequenceAnySwap
    , sequenceRelaxSub
    , sequenceRotateRight
    , sequenceRotateLeft
    , sequenceAddRight
    , sequenceAddLeft
    , sequenceRemoveRight
    , sequenceRemoveLeft
    , functionLessInjective
    , functionMoreInjective
    , functionAnySwap
    , functionMoreDefined
    , functionLessDefined
    ]

multiContainerNeighbourhoods :: NameGen m => Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
multiContainerNeighbourhoods domain = concatMapM (\ gen -> gen domain )
    [ mSetOrSetMove
    , mSetOrSetCollect
    , mSetOrSetCrossOver
    , mSetOrSetSplit
    , mSetOrSetMerge
    , sequenceRemoveLeftAddLeftOrRight
    , sequenceRemoveRightAddLeftOrRight
    , sequenceMergeLeftOrRight
    , sequenceSplitLeftOrRight
    , sequenceCrossOverSub
    , sequenceCrossOverAny
    , functionCrossOver
    ]


--some helper functions

--todo saad change hard coded fix error to better explanation. 
--Explanation is already there, it is just commented  out as don't know how to pass [char] to bug.
{-Function should be:
multiContainerNeighbourhoodError name expNumberIncumbents expNumberPrimaries (incumbents,primaries) = bug $ concat $ map show ["Error in neighbourhood generator ", name 
    , ".\nNumber incumbents expected: ", expNumberIncumbents, " but got ", (length incumbents)
    , "\nNumber primaries expected: ", expNumberPrimaries, " but got ", (length primaries)]
-}
multiContainerNeighbourhoodError :: b -> Int -> Int -> ([x],[x]) -> a
multiContainerNeighbourhoodError _ _ _ _ = bug "Wrong number of incumbents/primaries passed to MultiContainerNeighbourhoodGenResult"


makeFrameUpdate :: NameGen m => Int -> Int -> Expression -> Expression -> m ([Expression], [Expression], Expression -> Expression)
makeFrameUpdate numberIncumbents numberPrimaries theIncumbentVar theVar = do
    incumbents <- replicateM numberIncumbents auxiliaryVar
    primaries <- replicateM numberPrimaries auxiliaryVar
    return (map snd incumbents, map snd primaries, buildFrameUpdate (map fst incumbents) (map fst primaries)) where
        --todo written by saad
        --below local function should build a frame update with any number of arguments lifted.
         --not sure how to do this in haskel so special cased 1 and 2 lifts
        buildFrameUpdate :: [Name] -> [Name] -> (Expression -> Expression)
        buildFrameUpdate is js =
            let iList = fromList [ Reference n Nothing | n <- is ]
                jList = fromList [ Reference n Nothing | n <- js ]
            in  \ c -> [essence| frameUpdate(&theIncumbentVar, &theVar, &iList, &jList, &c) |]


makeFunctionFrameUpdate :: NameGen m => Int -> Int -> Domain () Expression -> Expression -> Expression -> m ([Expression], [Expression], Expression -> [Statement])
makeFunctionFrameUpdate numberIncumbents numberPrimaries functionFromDomain theIncumbentVar theVar = do
    incumbents <- replicateM numberIncumbents auxiliaryVar
    primaries <- replicateM numberPrimaries auxiliaryVar
    (kPat,k) <- quantifiedVar
    let myWrap var val = [essence| &var(&val) |]
    return     (map (\ (_,v) -> myWrap theIncumbentVar v) incumbents, map (\ (_,v) -> myWrap theVar v) primaries, buildFunctionFrameUpdate incumbents primaries kPat k) where
        --todo written by saad
        --below local function should build a function frame update with any number of arguments lifted.
         --not sure how to do this in haskel so special cased 1 and 2 lifts
        buildFunctionFrameUpdate :: [(Name,Expression)] -> [(Name,Expression)] -> AbstractPattern -> Expression -> (Expression -> [Statement])
        buildFunctionFrameUpdate [(iPat1, i1),(iPat2,i2)] [(jPat1,j1),(jPat2,j2)] kPat k = \c -> [essenceStmts| 
            find &iPat1, &iPat2, &jPat1, &jPat2 : &functionFromDomain
            such that
            &i1 != &i2, &j1 != &j2,
            &j1 = &i1 \/ &j1 = &i2,
            &j2 = &i1 \/ &j2 = &i2,
            {&i1,&i2} subsetEq defined(&theIncumbentVar),
            defined(&theIncumbentVar) = defined(&theVar),
            and([&theIncumbentVar(&k) = &theVar(&k) 
            | &kPat <- defined(&theIncumbentVar),
            !(&k in {&i1,&i2})]),
            &c|]
        buildFunctionFrameUpdate [(iPat1,i1)] [(jPat1,j1)] kPat k = \c -> [essenceStmts|
            find &iPat1,&jPat1 : &functionFromDomain
            such that
            &j1 = &i1,
            &i1 in defined(&theIncumbentVar),
            defined(&theIncumbentVar) = defined(&theVar),
            and([&theIncumbentVar(&k) = &theVar(&k) 
            | &kPat <- defined(&theIncumbentVar),
            &k != &i1]),
            &c|]
        buildFunctionFrameUpdate [(iPat1,i1),(iPat2,i2)] [(jPat1,j1)] kPat k = \c -> [essenceStmts| 
            find &iPat1, &iPat2, &jPat1 : &functionFromDomain
            such that
            &i1 != &i2,
            &j1 = &i1 \/ &j1 = &i2,
            {&i1,&i2} subsetEq defined(&theIncumbentVar),
            |defined(&theIncumbentVar)| = |defined(&theVar)| + 1,
            and([&theIncumbentVar(&k) = &theVar(&k) 
            | &kPat <- defined(&theIncumbentVar),
            !(&k in {&i1,&i2})]),
            &c|]
        buildFunctionFrameUpdate [(iPat1,i1)] [(jPat1,j1),(jPat2,j2)] kPat k = \c -> [essenceStmts|
            find &iPat1,&jPat1,&jPat2 : &functionFromDomain
            such that
            &j1 != &j2,
            &i1 = &j1 \/ &i1 = &j2,
            &i1 in defined(&theIncumbentVar),
            {&j1,&j2} subsetEq defined(&theVar),
            |defined(&theIncumbentVar)| = |defined(&theVar)| - 1,
            and([&theIncumbentVar(&k) = &theVar(&k) 
            | &kPat <- defined(&theIncumbentVar),
            &k != &i1]),
            &c|]
        buildFunctionFrameUpdate _ _ _ _ = bug "todo, extend the buildFunctionFrameUpdate pattern in AddNeighbourhood.hs to support frameUpdate with more than two focus variables."


mSetOrSetName :: Domain () Expression -> Maybe Name
mSetOrSetName (DomainSet{}) = Just "set"
mSetOrSetName (DomainMSet{}) = Just "mSet"
mSetOrSetName _ = Nothing

isFixedSizeMSetOrSet :: Domain () Expression -> Bool
isFixedSizeMSetOrSet (DomainSet _ (SetAttr (SizeAttr_Size _)) _)  = True
isFixedSizeMSetOrSet (DomainMSet _ (MSetAttr (SizeAttr_Size _) _) _) = True
isFixedSizeMSetOrSet _ = False

--not sure how to use innerDomainOf, so special casing
innerDomainOfMSetOrSet :: Domain () Expression -> Domain () Expression
innerDomainOfMSetOrSet (DomainSet _ _ inner) = inner
innerDomainOfMSetOrSet (DomainMSet _ _ inner) = inner
innerDomainOfMSetOrSet _ = bug "I special case this function, called with type not set or MSet"

mSetOrSetLiftSingle :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
mSetOrSetLiftSingle theIncumbentVar theVar theDomain
    | Just typeName <- mSetOrSetName theDomain = do
        let generatorName = mconcat [typeName,"LiftSingle"]
        let inner = innerDomainOfMSetOrSet theDomain
        ([incumbent_i], [i], frameUpdate) <- makeFrameUpdate 1 1 theIncumbentVar theVar
        let
            liftCons (SuchThat cs) = SuchThat [frameUpdate $ make opAnd $ fromList cs]
            liftCons st            = st

        ns <- allNeighbourhoods incumbent_i i inner
        return [ ( mconcat [generatorName, "_", innerGeneratorName]
            , innerNeighbourhoodSize
            , \ neighbourhoodSize ->
              let statements = rule neighbourhoodSize 
              in  map liftCons statements
             )
             | (innerGeneratorName, innerNeighbourhoodSize, rule) <- ns
             ]
mSetOrSetLiftSingle _ _ _ = return []


functionLiftSingle :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
functionLiftSingle theIncumbentVar theVar (DomainFunction _ _ frDomain toDomain) = do
        let generatorName = "functionLiftSingle"
        ([incumbent_i], [i], frameUpdate) <- makeFunctionFrameUpdate 1 1 frDomain theIncumbentVar theVar
        let
            liftCons (SuchThat cs) = frameUpdate $ make opAnd $ fromList cs
            liftCons st            = [st]

        ns <- allNeighbourhoods incumbent_i i toDomain
        return [ ( mconcat [generatorName, "_", innerGeneratorName]
            , innerNeighbourhoodSize
            , \ neighbourhoodSize ->
              let statements = rule neighbourhoodSize 
              in concat $ map liftCons statements
             )
             | (innerGeneratorName, innerNeighbourhoodSize, rule) <- ns
             ]
functionLiftSingle _ _ _ = return []


functionLiftMultiple :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
functionLiftMultiple theIncumbentVar theVar (DomainFunction _ _ frDomain toDomain) = do
        let generatorName = "functionLiftMultiple"
        let
            liftCons frame (SuchThat cs) =  let conjCs  =  make opAnd $ fromList cs in
                frame conjCs
            liftCons _ st = [st]
    
        ns :: [MultiContainerNeighbourhoodGenResult] <- multiContainerNeighbourhoods toDomain
        mapM (\ (innerGeneratorName, innerNeighbourhoodSize, numberIncumbents, numberPrimaries, rule)  -> do
            (incumbents, primaries, frame) <- makeFunctionFrameUpdate numberIncumbents numberPrimaries frDomain theIncumbentVar theVar 
            return  ( mconcat [generatorName, "_", innerGeneratorName]
                , innerNeighbourhoodSize
                , \ neighbourhoodSize -> let statements = rule neighbourhoodSize incumbents primaries in
                    concat $ map (liftCons frame) statements)) ns

functionLiftMultiple _ _ _ = return []



mSetOrSetLiftMultiple :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
mSetOrSetLiftMultiple theIncumbentVar theVar theDomain
    | Just typeName <- mSetOrSetName theDomain = do
        let generatorName = mconcat [typeName, "LiftMultiple"]
        let inner = innerDomainOfMSetOrSet theDomain
        let
            liftCons frame (SuchThat cs) = SuchThat [ let conjCs  =  make opAnd $ fromList cs in
                frame conjCs]
            liftCons _ st = st
    
        ns :: [MultiContainerNeighbourhoodGenResult] <- multiContainerNeighbourhoods inner
        mapM (\ (innerGeneratorName, innerNeighbourhoodSize, numberIncumbents, numberPrimaries, rule)  -> do
            (incumbents, primaries, frame) <- makeFrameUpdate numberIncumbents numberPrimaries theIncumbentVar theVar
            return  ( mconcat [generatorName, "_", innerGeneratorName]
                , innerNeighbourhoodSize
                , \ neighbourhoodSize -> let statements = rule neighbourhoodSize incumbents primaries in
                    map (liftCons frame) statements)) ns

mSetOrSetLiftMultiple _ _ _ = return []


mSetOrSetRemove :: Monad m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
mSetOrSetRemove theIncumbentVar theVar theDomain
    | (Just typeName, False) <- (mSetOrSetName theDomain, isFixedSizeMSetOrSet theDomain) = do
        let generatorName = mconcat [typeName, "Remove"]
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
mSetOrSetRemove _ _ _ = return []






mSetOrSetAdd :: Monad m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
mSetOrSetAdd theIncumbentVar theVar theDomain
    | (Just typeName, False) <- (mSetOrSetName theDomain, isFixedSizeMSetOrSet theDomain) = do
        let generatorName = mconcat [typeName, "Add"]
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
mSetOrSetAdd _ _ _ = return []


mSetOrSetSwap :: Monad m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
mSetOrSetSwap theIncumbentVar theVar theDomain
     | Just typeName <- mSetOrSetName theDomain = do
        let generatorName = mconcat [typeName, "Swap"]
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
mSetOrSetSwap _ _ _ = return []


mSetOrSetSwapAdd :: Monad m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
mSetOrSetSwapAdd theIncumbentVar theVar theDomain
     | Just typeName <- mSetOrSetName theDomain = do
        let generatorName = mconcat [typeName, "SwapAdd"]
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
mSetOrSetSwapAdd _ _ _ = return []




mSetOrSetSwapRemove :: Monad m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
mSetOrSetSwapRemove theIncumbentVar theVar theDomain
     | Just typeName <- mSetOrSetName theDomain = do
        let generatorName = mconcat [typeName, "SwapRemove"]
        let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
        return
            [( generatorName
            , calculatedMaxNhSize
             , \ neighbourhoodSize  ->
                    [essenceStmts|
                        such that
                            |&theIncumbentVar - &theVar| = &neighbourhoodSize
                    |]
            )]
mSetOrSetSwapRemove _ _ _ = return []




mSetOrSetMove :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
mSetOrSetMove theDomain
    | Just typeName <- mSetOrSetName theDomain = do
        let generatorName = mconcat [typeName, "Move"]
        let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
        let numberIncumbents = 2
        let numberPrimaries = 2
        (kPat, k) <- quantifiedVar
    
        return
            [( generatorName, calculatedMaxNhSize
            , numberIncumbents, numberPrimaries 
             , \ neighbourhoodSize incumbents primaries -> case (incumbents, primaries) of
                ([theIncumbentVar1, theIncumbentVar2], [theVar1,theVar2]) ->
                     [essenceStmts|
                        such that
                        &theVar1 subsetEq &theIncumbentVar1,
                        |&theIncumbentVar1| - |&theVar1| = &neighbourhoodSize,
                        &theIncumbentVar2 subsetEq &theVar2,
                        |&theVar2| - |&theIncumbentVar2| = &neighbourhoodSize,
                        and([&k in &theVar2 | &kPat <- &theIncumbentVar1, !(&k in &theVar1)])
                         |]
                other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
            )]
mSetOrSetMove _ = return []



mSetOrSetCollect :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
mSetOrSetCollect theDomain
    | Just typeName <- mSetOrSetName theDomain = do
        let generatorName = mconcat [typeName, "Collect"]
        let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
        let numberIncumbents = 3
        let numberPrimaries = 3
        (kPat, k) <- quantifiedVar
    
        return
            [( generatorName, calculatedMaxNhSize
            , numberIncumbents, numberPrimaries 
             , \ neighbourhoodSize incumbents primaries -> case (incumbents, primaries) of
                ([theIncumbentVar1, theIncumbentVar2, theIncumbentVar3], [theVar1,theVar2, theVar3]) ->
                     [essenceStmts|
                        such that
                        &theVar1 subsetEq &theIncumbentVar1,
                        |&theIncumbentVar1| - |&theVar1| = &neighbourhoodSize,
                        &theVar2 subsetEq &theIncumbentVar2,
                        |&theIncumbentVar2| - |&theVar2| = &neighbourhoodSize,
                        (&theIncumbentVar1 - &theVar1) = (&theIncumbentVar2 - &theVar2),
                        &theIncumbentVar3 subsetEq &theVar3,
                        |&theVar3| - |&theIncumbentVar3| = &neighbourhoodSize,
                        and([&k in &theVar3 | &kPat <- &theIncumbentVar1, !(&k in &theVar1)])
                         |]
                other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
            )]
mSetOrSetCollect _ = return []


mSetOrSetCrossOver :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
mSetOrSetCrossOver theDomain
    | Just typeName <- mSetOrSetName theDomain = do
        let generatorName = mconcat [typeName, "CrossOver"]
        let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
        let numberIncumbents = 2
        let numberPrimaries = 2
        return
            [( generatorName, calculatedMaxNhSize
            , numberIncumbents, numberPrimaries 
             , \ neighbourhoodSize incumbents primaries -> case (incumbents,primaries) of
                ([theIncumbentVar1, theIncumbentVar2],[theVar1,theVar2]) ->
                 [essenceStmts|
                 such that
                &theIncumbentVar1 union &theIncumbentVar2 = &theVar1 union &theVar2,
                |&theVar1 - &theIncumbentVar1| = &neighbourhoodSize,
                |&theVar2 - &theIncumbentVar2| = &neighbourhoodSize
                         |]
                other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
                        )]
mSetOrSetCrossOver _ = return []


mSetOrSetSplit :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
mSetOrSetSplit theDomain
    | Just typeName <- mSetOrSetName theDomain = do
        let generatorName = mconcat [typeName, "Split"]
        let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
        let numberIncumbents = 1
        let numberPrimaries = 2
        (kPat, k) <- quantifiedVar
    
        return
            [( generatorName, calculatedMaxNhSize
            , numberIncumbents, numberPrimaries 
             , \ neighbourhoodSize incumbents primaries -> case (incumbents, primaries) of 
                ([theIncumbentVar1], [theVar1,theVar2]) ->
                     [essenceStmts|
                    such that
                    &theVar1 subsetEq &theIncumbentVar1,
                    |&theIncumbentVar1| - |&theVar1| = &neighbourhoodSize,
                    |&theVar2| = &neighbourhoodSize,
                    and([&k in &theVar2 | &kPat <- &theIncumbentVar1, !(&k in &theVar1)])
                         |]
                other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
            )]
mSetOrSetSplit _ = return []


mSetOrSetMerge :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
mSetOrSetMerge theDomain
    | Just typeName <- mSetOrSetName theDomain = do
        let generatorName = mconcat [typeName, "Merge"]
        let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
        let numberIncumbents = 2
        let numberPrimaries = 1
    
        return
            [( generatorName, calculatedMaxNhSize
            , numberIncumbents, numberPrimaries 
             , \ neighbourhoodSize incumbents primaries -> case (incumbents, primaries) of
                ([theIncumbentVar1, theIncumbentVar2], [theVar1]) ->
                     [essenceStmts|
                    such that
                    |&theIncumbentVar1| <= &neighbourhoodSize,
                    &theIncumbentVar1 union &theIncumbentVar2  = &theVar1
                     |]
                other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
            )]
mSetOrSetMerge _ = return []


sequenceReverseSub :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceReverseSub theIncumbentVar theVar theDomain@(DomainSequence{}) = do

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
                        , &i <= |&theIncumbentVar|
                        , &j <= |&theIncumbentVar|
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


sequenceAnySwap :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceAnySwap theIncumbentVar theVar theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceAnySwap"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    (iPat, i) <- quantifiedVar
    return
        [( generatorName
        , calculatedMaxNhSize
         , \ neighbourhoodSize ->
                [essenceStmts|
                    such that
                        &neighbourhoodSize * 2
                            = sum([ toInt(&theVar(&i) != &theIncumbentVar(&i))
                                  | &iPat : int(1..&calculatedMaxNhSize)
                                  , &i <= |&theVar|
                                  ])
                |]
        )]
sequenceAnySwap _ _ _ = return []


sequenceRelaxSub :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceRelaxSub theIncumbentVar theVar theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceRelaxSub"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    (iPat, i) <- auxiliaryVar
    (jPat, j) <- auxiliaryVar
    (kPat, k) <- quantifiedVar
    (lPat, l) <- auxiliaryVar

    return
        [( generatorName
        , calculatedMaxNhSize
         , \ neighbourhoodSize  ->
                [essenceStmts|
                    find &iPat, &jPat, &lPat :  int(1..&calculatedMaxNhSize)
                    such that
                        &j - &i = &neighbourhoodSize
                            , &i <= |&theIncumbentVar|
                            , &j <= |&theIncumbentVar|
                            , and([ &theVar(&k) = &theIncumbentVar(&k)
                                  | &kPat : int(1..&calculatedMaxNhSize)
                                  , &k <= |&theVar|
                                  , &k < &i \/ &k > &j
                                  ])
                            , &l >= &i
                            , &l <= &j
                            , &theVar(&l) != &theIncumbentVar(&l)
                |]
        )]
sequenceRelaxSub _ _ _ = return []





sequenceRotateRight :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceRotateRight theIncumbentVar theVar theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceRotateRight"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    (kPat, k) <- quantifiedVar
    return
        [( generatorName
        , calculatedMaxNhSize
         , \ neighbourhoodSize  ->
                [essenceStmts|
                    such that
                            and([ &theVar(&k) = &theIncumbentVar((((&k-1) + &neighbourhoodSize) % (|&theIncumbentVar|-1) ) + 1)
                                  | &kPat : int(1..&calculatedMaxNhSize)
                                  , &k <= |&theIncumbentVar|
                                  ])
                |]
        )]
sequenceRotateRight _ _ _ = return []


sequenceRotateLeft :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceRotateLeft theIncumbentVar theVar theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceRotateLeft"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    (kPat, k) <- quantifiedVar
    return
        [( generatorName
        , calculatedMaxNhSize
         , \ neighbourhoodSize  ->
                [essenceStmts|
                    such that
                            and([ &theVar(&k) = &theIncumbentVar((((&k-1) - &neighbourhoodSize) % (|&theIncumbentVar| -1)) + 1)
                                  | &kPat : int(1..&calculatedMaxNhSize)
                                  , &k <= |&theIncumbentVar|
                                  ])
                |]
        )]
sequenceRotateLeft _ _ _ = return []




sequenceAddRight :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceAddRight _ _  (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceAddRight theIncumbentVar theVar theDomain@(DomainSequence{}) =  do
    let generatorName = "sequenceAddRight"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    (kPat, k) <- quantifiedVar
    return
        [( generatorName
        , calculatedMaxNhSize
          , \ neighbourhoodSize ->
                [essenceStmts|
                    such that
                    and([ &theVar(&k) = &theIncumbentVar(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theIncumbentVar|
                    ])
                    , |&theVar| = |&theIncumbentVar| + &neighbourhoodSize
                |]
        )]
sequenceAddRight _ _ _ = return []


sequenceAddLeft :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceAddLeft _ _ (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceAddLeft theIncumbentVar theVar theDomain@(DomainSequence{}) = do

    let generatorName = "sequenceAddLeft"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    (kPat, k) <- quantifiedVar
    return
        [( generatorName
        , calculatedMaxNhSize
          , \ neighbourhoodSize ->
                [essenceStmts|
                    such that
                    |&theVar| = |&theIncumbentVar| + &neighbourhoodSize
                    , and([ &theVar(&k + &neighbourhoodSize) = &theIncumbentVar(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theIncumbentVar| ])
                |]
        )]
sequenceAddLeft _ _ _ = return []



sequenceRemoveRight :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceRemoveRight _ _ (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceRemoveRight theIncumbentVar theVar theDomain@(DomainSequence{}) =  do
    let generatorName = "sequenceRemoveRight"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    (kPat, k) <- quantifiedVar

    return
        [( generatorName
        , calculatedMaxNhSize 
         , \ neighbourhoodSize ->
                [essenceStmts|
                    such that
                    and([ &theVar(&k) = &theIncumbentVar(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theVar|
                    ])
                    , |&theVar| = |&theIncumbentVar| - &neighbourhoodSize
                |]
        )]
sequenceRemoveRight _ _ _ = return []


sequenceRemoveLeft :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceRemoveLeft _ _ (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceRemoveLeft theIncumbentVar theVar theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceRemoveLeft"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    (kPat, k) <- quantifiedVar
    return
        [( generatorName
        , calculatedMaxNhSize 
         , \ neighbourhoodSize ->
                [essenceStmts|
                    such that
                    and([ &theVar(&k) = &theIncumbentVar(&k + &neighbourhoodSize)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theVar|
                    ])
                    , |&theVar| = |&theIncumbentVar| - &neighbourhoodSize
                |]
        )]
sequenceRemoveLeft _ _ _ = return []





sequenceRemoveRightAddLeftOrRight  :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
sequenceRemoveRightAddLeftOrRight (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceRemoveRightAddLeftOrRight  theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceRemoveRightAddLeftOrRight"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    let numberIncumbents = 2
    let numberPrimaries = 2
    (kPat, k) <- quantifiedVar

    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize incumbents primaries -> case (incumbents, primaries) of
            ([theIncumbentVar1, theIncumbentVar2], [theVar1,theVar2]) ->
                 [essenceStmts|
                     such that
                     |&theVar1| = |&theIncumbentVar1| - &neighbourhoodSize
                     , |&theVar2| = |&theIncumbentVar2| + &neighbourhoodSize
                    , and([ &theVar1(&k) = &theIncumbentVar1(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theVar1|
                    ])
                    , or([
                    and([ &theVar2(&k) = &theIncumbentVar2(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theIncumbentVar2|
                    ]) /\
                    and([&theVar2(&k) = &theIncumbentVar1(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k > |&theVar1|
                    , &k <= |&theVar2|
                    ])
                    , and([ &theVar2(&k + &neighbourhoodSize) = &theIncumbentVar2(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theIncumbentVar2|
                    ]) /\
                     and([&theVar2(&k) = &theIncumbentVar1(|&theVar1|  + &k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= &neighbourhoodSize])

                    ])
                     |]
            other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
        )]
sequenceRemoveRightAddLeftOrRight _ = return []



sequenceRemoveLeftAddLeftOrRight  :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
sequenceRemoveLeftAddLeftOrRight (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceRemoveLeftAddLeftOrRight  theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceRemoveLeftAddLeftOrRight"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    let numberIncumbents = 2
    let numberPrimaries = 2
    (kPat, k) <- quantifiedVar

    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize incumbents primaries -> case (incumbents, primaries) of
            ([theIncumbentVar1, theIncumbentVar2], [theVar1,theVar2]) ->
                 [essenceStmts|
                    such that
                    |&theVar1| = |&theIncumbentVar1| - &neighbourhoodSize
                    , |&theVar2| = |&theIncumbentVar2| + &neighbourhoodSize
                    , and([ &theVar1(&k) = &theIncumbentVar1(&k + &neighbourhoodSize)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theVar1|
                    ])
                    , or([
                    and([ &theVar2(&k) = &theIncumbentVar2(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theIncumbentVar2|
                    ]) /\
                    and([&theVar2(|&theIncumbentVar2| + &k) = &theIncumbentVar1(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= &neighbourhoodSize])
                    , and([ &theVar2(&k + &neighbourhoodSize) = &theIncumbentVar2(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theIncumbentVar2|
                    ]) /\
                    and([&theVar2(&k) = &theIncumbentVar1(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= &neighbourhoodSize])
                    ])
                     |]
            other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
        )]
sequenceRemoveLeftAddLeftOrRight  _ = return []





sequenceMergeLeftOrRight  :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
sequenceMergeLeftOrRight (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceMergeLeftOrRight  theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceMergeLeftOrRight"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    let numberIncumbents = 2
    let numberPrimaries = 1
    (kPat, k) <- quantifiedVar

    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize incumbents primaries -> case (incumbents, primaries) of
            ([theIncumbentVar1, theIncumbentVar2], [theVar1]) ->
                 [essenceStmts|
                    such that
                    |&theVar1| = |&theIncumbentVar1| + |&theIncumbentVar2|
                    , |&theIncumbentVar1| = &neighbourhoodSize
                    , or([
                    and([&theVar1(&k) = &theIncumbentVar1(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= &neighbourhoodSize]) /\
                    and([&theVar1(&k + &neighbourhoodSize) = &theIncumbentVar2(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theIncumbentVar2|
                    ])
                    
                    , and([&theVar1(&k) = &theIncumbentVar2(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theIncumbentVar2|
                    ]) /\
                    and([&theVar1(&k + |&theIncumbentVar2|) = &theIncumbentVar1(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= &neighbourhoodSize])
                    ])

                     |]
            other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
        )]
sequenceMergeLeftOrRight  _ = return []






sequenceSplitLeftOrRight  :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
sequenceSplitLeftOrRight (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceSplitLeftOrRight  theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceSplitLeftOrRight"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    let numberIncumbents = 1
    let numberPrimaries = 2
    (kPat, k) <- quantifiedVar

    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize incumbents primaries -> case (incumbents, primaries) of
            ([theIncumbentVar1], [theVar1, theVar2]) ->
                 [essenceStmts|
                    such that
                    |&theVar1| = |&theIncumbentVar1| - &neighbourhoodSize
                    , |&theVar2| = &neighbourhoodSize
                    , or([
                    and([&theVar1(&k) = &theIncumbentVar1(&k + &neighbourhoodSize)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theVar1|
                    ]) /\
                    and([&theVar2(&k) = &theIncumbentVar1(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= &neighbourhoodSize])
                    
                    , and([&theVar1(&k) = &theIncumbentVar1(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theVar1|
                    ]) /\
                    and([&theVar2(&k) = &theIncumbentVar1(&k + |&theVar1|)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= &neighbourhoodSize
                    ])
                    ])

                     |]
            other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
        )]
sequenceSplitLeftOrRight  _ = return []








sequenceCrossOverSub :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
sequenceCrossOverSub theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceCrossOverSub"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    let numberIncumbents = 2
    let numberPrimaries = 2
    (iPat,i) <- auxiliaryVar
    (jPat,j) <- auxiliaryVar
    (kPat, k) <- quantifiedVar
    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize incumbents primaries -> case (incumbents,primaries) of
            ([theIncumbentVar1, theIncumbentVar2],[theVar1,theVar2]) ->
                    [essenceStmts|
                    find &iPat : int(1..&calculatedMaxNhSize)
                    find &jPat : int(1..&calculatedMaxNhSize)
                    such that
                    &j - &i = &neighbourhoodSize
                    , and([&theVar1(&k) = &theIncumbentVar1(&k) /\
                    &theVar2(&k) = &theIncumbentVar2(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k < &i
                    , &k > &j
                    , &k < |&theIncumbentVar1|
                    , &k <= |&theIncumbentVar2|
                    ])
                    , and([&theVar1(&k) = &theIncumbentVar2(&k) /\
                    &theVar2(&k) = &theIncumbentVar1(&k)
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k >= &i
                    , &k <= &j
                    ])
                    |]
            other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
                    )]
sequenceCrossOverSub _ = return []



sequenceCrossOverAny :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
sequenceCrossOverAny theDomain@(DomainSequence{}) = do
    let generatorName = "sequenceCrossOverAny"
    let calculatedMaxNhSize = getMaxNumberOfElementsInContainer theDomain
    let numberIncumbents = 2
    let numberPrimaries = 2
    (kPat, k) <- quantifiedVar
    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize incumbents primaries -> case (incumbents,primaries) of
            ([theIncumbentVar1, theIncumbentVar2],[theVar1,theVar2]) ->
                    [essenceStmts|
                    such that
                    &neighbourhoodSize = sum([toInt(&theVar1(&k) = &theIncumbentVar2(&k) /\
                    &theVar2(&k) = &theIncumbentVar1(&k) /\
                    &theIncumbentVar1(&k) != &theIncumbentVar2(&k))
                    | &kPat : int(1..&calculatedMaxNhSize)
                    , &k <= |&theIncumbentVar1|
                    , &k <= |&theIncumbentVar2|
                    ])
                    |]
            other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
                    )]
sequenceCrossOverAny _ = return []




functionLessInjective :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
functionLessInjective theIncumbentVar theVar (DomainFunction _ (FunctionAttr _ PartialityAttr_Total JectivityAttr_None) functionFromDomain@(DomainInt _) _) = do
    let generatorName = "functionLessInjective"
    let calculatedMaxNhSize = case domainSizeOf functionFromDomain of
                Left err -> bug err
                Right size -> size
    let isOdd = [essence|&calculatedMaxNhSize % 2 != 0 |]
    (iPat, i) <- quantifiedVar
    (isPat,is) <- auxiliaryVar
    (jsPat,js) <- auxiliaryVar
    return
        [( generatorName
        , calculatedMaxNhSize
         , \ neighbourhoodSize  ->
                [essenceStmts|
                    find &isPat : matrix indexed by [int(1..&calculatedMaxNhSize/2)] of &functionFromDomain
                    find &jsPat : matrix indexed by [int(1..&calculatedMaxNhSize/2 + toInt(&isOdd))] of &functionFromDomain

                    such that
                    allDiff(flatten([&is,&js]))
                    , forAll &iPat : int(1..&calculatedMaxNhSize/2) .
                    (&i <= &neighbourhoodSize ->
                    (&theIncumbentVar(&is[&i]) !=
                    &theIncumbentVar(&js[&i]) /\
                    &theVar(&is[&i]) =
                    &theVar(&js[&i]))) /\
                    (&i > &neighbourhoodSize ->
                    (&theIncumbentVar(&is[&i]) =
                    &theVar(&is[&i]) /\
                    &theIncumbentVar(&js[&i]) =
                    &theVar(&js[&i])))
                    , &isOdd ->
                    &theVar(&js[&calculatedMaxNhSize /2 +1]) =
                        &theIncumbentVar(&js[&calculatedMaxNhSize/2+1])
                |]
        )]
functionLessInjective _ _ _ = return []



functionMoreInjective :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
functionMoreInjective theIncumbentVar theVar (DomainFunction _ (FunctionAttr _ PartialityAttr_Total JectivityAttr_None) functionFromDomain@(DomainInt _) _) = do
    let generatorName = "functionMoreInjective"
    let calculatedMaxNhSize = case domainSizeOf functionFromDomain of
                Left err -> bug err
                Right size -> size
    let isOdd = [essence|&calculatedMaxNhSize % 2 != 0 |]
    (iPat, i) <- quantifiedVar
    (isPat,is) <- auxiliaryVar
    (jsPat,js) <- auxiliaryVar
    return
        [( generatorName
        , calculatedMaxNhSize
         , \ neighbourhoodSize  ->
                [essenceStmts|
                    find &isPat : matrix indexed by [int(1..&calculatedMaxNhSize/2)] of &functionFromDomain
                    find &jsPat : matrix indexed by [int(1..&calculatedMaxNhSize/2 + toInt(&isOdd))] of &functionFromDomain

                    such that
                    allDiff(flatten([&is,&js]))
                    , forAll &iPat : int(1..&calculatedMaxNhSize/2) .
                    (&i <= &neighbourhoodSize ->
                    (&theIncumbentVar(&is[&i]) =
                    &theIncumbentVar(&js[&i]) /\
                    &theVar(&is[&i]) !=
                    &theVar(&js[&i]))) /\
                    (&i > &neighbourhoodSize ->
                    (&theIncumbentVar(&is[&i]) =
                    &theVar(&is[&i]) /\
                    &theIncumbentVar(&js[&i]) =
                    &theVar(&js[&i])))
                    , &isOdd ->
                    &theVar(&js[&calculatedMaxNhSize /2 +1]) =
                        &theIncumbentVar(&js[&calculatedMaxNhSize/2+1])
                |]
        )]
functionMoreInjective _ _ _ = return []



functionMoreDefined :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
functionMoreDefined theIncumbentVar theVar (DomainFunction _ (FunctionAttr _ PartialityAttr_Partial JectivityAttr_None) functionFromDomain@(DomainInt _) _) = do
    let generatorName = "functionMoreDefined"
    let calculatedMaxNhSize = case domainSizeOf functionFromDomain of
                Left err -> bug err
                Right size -> size
    (iPat, i) <- quantifiedVar
    return
        [( generatorName
        , calculatedMaxNhSize
         , \ neighbourhoodSize  ->
                [essenceStmts|
                such that
                defined(&theIncumbentVar) subsetEq defined(&theVar),
                |defined(&theVar)| - |defined(&theIncumbentVar)| = &neighbourhoodSize
                , and([&theVar(&i) = &theIncumbentVar(&i)
                | &iPat <- defined(&theIncumbentVar)])
                |]
        )]
functionMoreDefined _ _ _ = return []



functionLessDefined :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
functionLessDefined theIncumbentVar theVar (DomainFunction _ (FunctionAttr _ PartialityAttr_Partial JectivityAttr_None) functionFromDomain@(DomainInt _) _) = do
    let generatorName = "functionLessDefined"
    let calculatedMaxNhSize = case domainSizeOf functionFromDomain of
                Left err -> bug err
                Right size -> size
    (iPat, i) <- quantifiedVar
    return
        [( generatorName
        , calculatedMaxNhSize
         , \ neighbourhoodSize  ->
                [essenceStmts|
                such that
                defined(&theVar) subsetEq defined(&theIncumbentVar),
                |defined(&theIncumbentVar)| - |defined(&theVar)| = &neighbourhoodSize
                , and([&theVar(&i) = &theIncumbentVar(&i)
                | &iPat <- defined(&theVar)])
                |]
        )]
functionLessDefined _ _ _ = return []


functionAnySwap :: NameGen m => Expression -> Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
functionAnySwap theIncumbentVar theVar (DomainFunction _ _ functionFromDomain _) = do
    let generatorName = "functionAnySwap"
    let calculatedMaxNhSize = case domainSizeOf functionFromDomain of
                Left err -> bug err
                Right size -> size
    (iPat, i) <- quantifiedVar
    return
        [( generatorName
        , calculatedMaxNhSize
         , \ neighbourhoodSize ->
                [essenceStmts|
                    such that
                        &neighbourhoodSize * 2
                            = sum([ toInt(&theVar(&i) != &theIncumbentVar(&i))
                                  | &iPat  <- defined(&theIncumbentVar)])
                |]
        )]
functionAnySwap _ _ _ = return []



functionCrossOver :: NameGen m =>  Domain () Expression -> m [MultiContainerNeighbourhoodGenResult]
functionCrossOver (DomainFunction _ _ functionFromDomain _) = do
    let generatorName = "functionCrossOver"
    let calculatedMaxNhSize = case domainSizeOf functionFromDomain of
                Left err -> bug err
                Right size -> size
    let numberIncumbents = 2
    let numberPrimaries = 2
    (kPat, k) <- quantifiedVar
    return
        [( generatorName, calculatedMaxNhSize
        , numberIncumbents, numberPrimaries 
         , \ neighbourhoodSize incumbents primaries -> case (incumbents,primaries) of
            ([theIncumbentVar1, theIncumbentVar2],[theVar1,theVar2]) ->
                    [essenceStmts|
                    such that
                    &neighbourhoodSize  = sum &kPat in defined(&theIncumbentVar1)  intersect defined(&theIncumbentVar2) .
                        toInt( &theVar1(&k) = &theIncumbentVar2(&k) /\
                               &theVar2(&k) = &theIncumbentVar1(&k) /\
                               &theIncumbentVar1(&k) != &theIncumbentVar2(&k) )
                    |]
            other -> multiContainerNeighbourhoodError generatorName numberIncumbents numberPrimaries other 
                    )]
functionCrossOver _ = return []
