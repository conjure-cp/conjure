{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.AddNeighbourhoods where

import Conjure.Prelude
import Conjure.Language
import Conjure.UI.TypeCheck ( typeCheckModel )
import Conjure.Language.NameResolution ( resolveNames )
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
    let maxNeighbourhoodSizeDecl = Declaration (FindOrGiven Given maxNeighbourhoodSizeVarName (DomainInt []))
    let inpModelInfo = mInfo inpModel
    let outModel = inpModel { mStatements = mStatements inpModel
                                         ++ [maxNeighbourhoodSizeDecl]
                                         ++ neighbourhoods
                            , mInfo = inpModelInfo { miGivens = miGivens inpModelInfo
                                                        ++ [maxNeighbourhoodSizeVarName]
                                                   , miOriginalDomains = miOriginalDomains inpModelInfo
                                                        ++ [(maxNeighbourhoodSizeVarName, DomainInt [])]
                                                   }
                            }
    (resolveNames >=> typeCheckModel) outModel


maxNeighbourhoodSizeVarName :: Name
maxNeighbourhoodSizeVarName = "SNS_MAX_NEIGHBOURHOOD_SIZE"

maxNeighbourhoodSizeVar :: Expression
maxNeighbourhoodSizeVar = Reference maxNeighbourhoodSizeVarName Nothing


generateNeighbourhoods :: NameGen m => Name -> Expression -> Domain () Expression -> m [Statement]
generateNeighbourhoods theVarName theVar domain = do
    neighbourhoods <- allNeighbourhoods theVar domain
    return $ concatMap (skeleton theVarName theVar) neighbourhoods


skeleton
    :: Name -> Expression
    -> NeighbourhoodGenResult
    -> [Statement]
skeleton varName var gen =
    let
        (generatorName, consGen) = gen

        neighbourhoodName     = mconcat [varName, "_", generatorName]

        activatorName         = mconcat [neighbourhoodName, "_", "activator"]
        neighbourhoodSizeName = mconcat [neighbourhoodName, "_", "size"]

        activatorVar          = Reference activatorName Nothing
        neighbourhoodSize     = Reference neighbourhoodSizeName Nothing

        (statements, consPositive, consNegative) = consGen neighbourhoodSize
    in
        [essenceStmts|
            find &activatorName : bool
            find &neighbourhoodSizeName : int(1..&maxNeighbourhoodSizeVar)
            neighbourhood &neighbourhoodName : (&neighbourhoodSizeName, &activatorName, [&var])
        |]
        ++ fromMaybe [] statements
        ++ concat [ [essenceStmts| such that  &activatorVar -> &c |] | Just c <- [consPositive] ]
        ++ concat [ [essenceStmts| such that !&activatorVar -> &c |] | Just c <- [consNegative] ]
        ++ concat [ [essenceStmts| such that !&activatorVar -> dontCare(&neighbourhoodSize) |] ]


type NeighbourhoodGenResult = (Name, Expression -> (Maybe [Statement], Maybe Expression, Maybe Expression))


allNeighbourhoods :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
allNeighbourhoods theVar domain = concatMapM (\ gen -> gen theVar domain )
    [ setLiftExists
    , setRemove
    , setAdd
    , setSwap
    , setSwapAdd

    , sequenceReverseSubSeq
    , sequenceAnySwap
    , sequenceRelaxSub
    , sequenceAddRight
    , sequenceAddLeft
    , sequenceRemoveRight
    , sequenceRemoveLeft
    ]


setLiftExists :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setLiftExists theVar (DomainSet _ _ inner) = do
    let generatorName = "setLiftExists"
    (iPat, i) <- quantifiedVar
    let liftCons c = [essence| exists &iPat in &theVar . &c |]
    ns <- allNeighbourhoods i inner
    return
        [ ( mconcat [generatorName, "_", innerGeneratorName]
          , \ neighbourhoodSize ->
              let (statements, consPositive, consNegative) = rule neighbourhoodSize
              in  ( statements
                  , liftCons <$> consPositive
                  , liftCons <$> consNegative
                  )
          )
        | (innerGeneratorName, rule) <- ns
        ]
setLiftExists _ _ = return []


setRemove :: Monad m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setRemove theVar DomainSet{} = do
    let generatorName = "setRemove"
    return
        [( generatorName
         , \ neighbourhoodSize ->
            ( Nothing
            , Just [essence|
                &theVar subsetEq incumbent(&theVar) /\
                |incumbent(&theVar)| - |&theVar| = &neighbourhoodSize
              |]
            , Nothing
            )
        )]
setRemove _ _ = return []


setAdd :: Monad m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setAdd theVar DomainSet{} = do
    let generatorName = "setAdd"
    return
        [( generatorName
         , \ neighbourhoodSize ->
            ( Nothing
            , Just [essence|
                incumbent(&theVar) subsetEq &theVar /\
                |&theVar| - |incumbent(&theVar)| = &neighbourhoodSize
              |]
            , Nothing
            )
        )]
setAdd _ _ = return []


setSwap :: Monad m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setSwap theVar DomainSet{} = do
    let generatorName = "setSwap"
    return
        [( generatorName
         , \ neighbourhoodSize ->
            ( Nothing
            , Just [essence|
                (|&theVar - incumbent(&theVar)| = &neighbourhoodSize) /\
                (|incumbent(&theVar)| = |&theVar|)
              |]
            , Nothing
            )
        )]
setSwap _ _ = return []


setSwapAdd :: Monad m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setSwapAdd theVar DomainSet{} = do
    let generatorName = "setSwapAdd"
    return
        [( generatorName
         , \ neighbourhoodSize ->
            ( Nothing
            , Just [essence|
                |&theVar - incumbent(&theVar)| = &neighbourhoodSize
              |]
            , Nothing
            )
        )]
setSwapAdd _ _ = return []


sequenceReverseSubSeq :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceReverseSubSeq theVar (DomainSequence _ (SequenceAttr sizeAttr _) _)
    | Just maxSize <- getMaxFrom_SizeAttr sizeAttr = do

    let generatorName = "sequenceReverseSubSeq"

    (iPat, i) <- auxiliaryVar
    (jPat, j) <- auxiliaryVar
    (kPat, k) <- quantifiedVar

    return
        [( generatorName
         , \ neighbourhoodSize ->
            ( Just [essenceStmts|
                find &iPat, &jPat :  int(1..&maxSize)
              |]
            , Just [essence|
                and([ &j - &i = &neighbourhoodSize
                    , &i <= |&theVar|
                    , &j <= |&theVar|
                    , and([ &theVar(&k) = incumbent(&theVar)(&k)
                          | &kPat : int(1..&maxNeighbourhoodSizeVar)
                          , &k <= &neighbourhoodSize
                          , &k < &i
                          , &k > &j
                          ])
                    , and([ &theVar(&k) = incumbent(&theVar)(&j - (&k - &i))
                          | &kPat : int(1..&maxNeighbourhoodSizeVar)
                          , &k <= &neighbourhoodSize
                          , &k >= &i
                          , &k <= &j
                          ])
                    ])
              |]
            , Just [essence| dontCare(tuple (&i, &j)) |]
            )
        )]
sequenceReverseSubSeq _ _ = return []


sequenceAnySwap :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceAnySwap theVar (DomainSequence _ (SequenceAttr sizeAttr _) _)
    | Just maxSize <- getMaxFrom_SizeAttr sizeAttr = do

    let generatorName = "sequenceAnySwap"

    (iPat, i) <- quantifiedVar

    return
        [( generatorName
         , \ neighbourhoodSize ->
             ( Nothing
             , Just [essence|
                 &neighbourhoodSize * 2
                     = sum([ toInt(&theVar(&i) != incumbent(&theVar)(&i))
                           | &iPat : int(1..&maxSize)
                           , &i <= |&theVar|
                           ])
               |]
             , Nothing
             )
        )]
sequenceAnySwap _ _ = return []


sequenceRelaxSub :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceRelaxSub theVar (DomainSequence _ (SequenceAttr sizeAttr _) _)
    | Just maxSize <- getMaxFrom_SizeAttr sizeAttr = do

    let generatorName = "sequenceRelaxSub"

    (iPat, i) <- auxiliaryVar
    (jPat, j) <- auxiliaryVar
    (kPat, k) <- quantifiedVar
    (lPat, l) <- auxiliaryVar

    return
        [( generatorName
        , \ neighbourhoodSize ->
            ( Just [essenceStmts|
                find &iPat, &jPat, &lPat :  int(1..&maxSize)
              |]
            , Just [essence|
                and([ &j - &i = &neighbourhoodSize
                    , &i <= |&theVar|
                    , &j <= |&theVar|
                    , and([ &theVar(&k) = incumbent(&theVar)(&k)
                          | &kPat : int(1..&maxNeighbourhoodSizeVar)
                          , &k <= &neighbourhoodSize
                          , &k < &i
                          , &k > &j
                          ])
                    , &l >= &i
                    , &l <= &j
                    , &theVar(&l) != incumbent(&theVar)(&l)
                    ])
              |]
            , Just [essence|
                dontCare(tuple (&i, &j, &l))
              |]
            )
        )]
sequenceRelaxSub _ _ = return []


sequenceAddRight :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceAddRight _ (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceAddRight theVar (DomainSequence _ (SequenceAttr sizeAttr _) _)
    | Just maxSize <- getMaxFrom_SizeAttr sizeAttr = do

    let generatorName = "sequenceAddRight"

    (iPat, i) <- auxiliaryVar
    (kPat, k) <- quantifiedVar

    return
        [( generatorName
        , \ neighbourhoodSize ->
            ( Just [essenceStmts|
                find &iPat :  int(1..&maxNeighbourhoodSizeVar)
              |]
            , Just [essence|
                and([ and([ &theVar(&k) = incumbent(&theVar)(&k)
                          | &kPat : int(1..&maxSize)
                          , &k <= |&theVar|
                          ])
                    , |incumbent(&theVar)| = |&theVar| + &i
                    , &i <= &neighbourhoodSize
                    ])
              |]
            , Just [essence|
                dontCare(&i)
              |]
            )
        )]
sequenceAddRight _ _ = return []


sequenceAddLeft :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceAddLeft _ (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceAddLeft theVar (DomainSequence _ (SequenceAttr sizeAttr _) _)
    | Just maxSize <- getMaxFrom_SizeAttr sizeAttr = do

    let generatorName = "sequenceAddLeft"

    (iPat, i) <- auxiliaryVar
    (kPat, k) <- quantifiedVar

    return
        [( generatorName
        , \ neighbourhoodSize ->
            ( Just [essenceStmts|
                find &iPat :  int(1..&maxNeighbourhoodSizeVar)
              |]
            , Just [essence|
                and([ and([ &theVar(&k) = incumbent(&theVar)(&k + &i)
                          | &kPat : int(1..&maxSize)
                          , &k <= |&theVar|
                          ])
                    , |incumbent(&theVar)| = |&theVar| + &i
                    , &i <= &neighbourhoodSize
                    ])
              |]
            , Just [essence|
                dontCare(&i)
              |]
            )
        )]
sequenceAddLeft _ _ = return []


sequenceRemoveRight :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceRemoveRight _ (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceRemoveRight theVar (DomainSequence _ (SequenceAttr sizeAttr _) _)
    | Just maxSize <- getMaxFrom_SizeAttr sizeAttr = do

    let generatorName = "sequenceRemoveRight"

    (iPat, i) <- auxiliaryVar
    (kPat, k) <- quantifiedVar

    return
        [( generatorName
        , \ neighbourhoodSize ->
            ( Just [essenceStmts|
                find &iPat :  int(1..&maxNeighbourhoodSizeVar)
              |]
            , Just [essence|
                and([ and([ &theVar(&k) = incumbent(&theVar)(&k)
                          | &kPat : int(1..&maxSize)
                          , &k <= |&theVar| - &i
                          ])
                    , |incumbent(&theVar)| = |&theVar| - &i
                    , &i <= &neighbourhoodSize
                    ])
              |]
            , Just [essence|
                dontCare(&i)
              |]
            )
        )]
sequenceRemoveRight _ _ = return []


sequenceRemoveLeft :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceRemoveLeft _ (DomainSequence _ (SequenceAttr (SizeAttr_Size _) _) _) = return []
sequenceRemoveLeft theVar (DomainSequence _ (SequenceAttr sizeAttr _) _)
    | Just maxSize <- getMaxFrom_SizeAttr sizeAttr = do

    let generatorName = "sequenceRemoveLeft"

    (iPat, i) <- auxiliaryVar
    (kPat, k) <- quantifiedVar

    return
        [( generatorName
        , \ neighbourhoodSize ->
            ( Just [essenceStmts|
                find &iPat :  int(1..&maxNeighbourhoodSizeVar)
              |]
            , Just [essence|
                and([ and([ &theVar(&k) = incumbent(&theVar)(&k - &i)
                          | &kPat : int(1..&maxSize)
                          , &k <= |&theVar| - &i
                          ])
                    , |incumbent(&theVar)| = |&theVar| - &i
                    , &i <= &neighbourhoodSize
                    ])
              |]
            , Just [essence|
                dontCare(&i)
              |]
            )
        )]
sequenceRemoveLeft _ _ = return []

