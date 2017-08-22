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

        (generatorName, consGen) = gen

        neighbourhoodGroupName = mconcat [varName, "_neighbourhoodGroup"]

        neighbourhoodName     = mconcat [varName, "_", generatorName]

        neighbourhoodSizeName = mconcat [neighbourhoodName, "_", "size"]

        neighbourhoodSize = getMaxNumberOfElementsInContainer domain

        neighbourhoodSizeVar = Reference neighbourhoodSizeName Nothing

        statements = consGen neighbourhoodSizeVar neighbourhoodSize

    in
        [ SNS_Group neighbourhoodGroupName [var]
        , SNS_Neighbourhood neighbourhoodName
                            neighbourhoodGroupName
                            neighbourhoodSizeName
                            [essenceDomain| int(1..&neighbourhoodSize) |]
                            statements
        ]



type NeighbourhoodGenResult = (Name, Expression -> Expression -> [Statement])


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
    let
        liftCons (SuchThat cs) = SuchThat [ [essence| exists &iPat in &theVar . &c |] | c <- cs ]
        liftCons st            = st

    ns <- allNeighbourhoods i inner
    return
        [ ( mconcat [generatorName, "_", innerGeneratorName]
          , \ neighbourhoodSize maxNeighbourhoodSize ->
              let statements = rule neighbourhoodSize maxNeighbourhoodSize
              in  map liftCons statements
          )
        | (innerGeneratorName, rule) <- ns
        ]
setLiftExists _ _ = return []


setRemove :: Monad m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setRemove theVar DomainSet{} = do
    let generatorName = "setRemove"
    return
        [( generatorName
         , \ neighbourhoodSize _maxNeighbourhoodSize ->
                 [essenceStmts|
                    such that
                        &theVar subsetEq incumbent(&theVar),
                        |incumbent(&theVar)| - |&theVar| = &neighbourhoodSize
                 |]
        )]
setRemove _ _ = return []


setAdd :: Monad m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setAdd theVar DomainSet{} = do
    let generatorName = "setAdd"
    return
        [( generatorName
         , \ neighbourhoodSize _maxNeighbourhoodSize ->
                [essenceStmts|
                    such that
                        incumbent(&theVar) subsetEq &theVar,
                        |&theVar| - |incumbent(&theVar)| = &neighbourhoodSize
                |]
        )]
setAdd _ _ = return []


setSwap :: Monad m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setSwap theVar DomainSet{} = do
    let generatorName = "setSwap"
    return
        [( generatorName
         , \ neighbourhoodSize _maxNeighbourhoodSize ->
                [essenceStmts|
                    such that
                        |&theVar - incumbent(&theVar)| = &neighbourhoodSize,
                        |incumbent(&theVar)| = |&theVar|
                |]
        )]
setSwap _ _ = return []


setSwapAdd :: Monad m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setSwapAdd theVar DomainSet{} = do
    let generatorName = "setSwapAdd"
    return
        [( generatorName
         , \ neighbourhoodSize _maxNeighbourhoodSize ->
                [essenceStmts|
                    such that
                        |&theVar - incumbent(&theVar)| = &neighbourhoodSize
                |]
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
         , \ neighbourhoodSize maxNeighbourhoodSize ->
                [essenceStmts|
                    find &iPat, &jPat :  int(1..&maxSize)
                    such that
                        and([ &j - &i = &neighbourhoodSize
                        , &i <= |&theVar|
                        , &j <= |&theVar|
                        , and([ &theVar(&k) = incumbent(&theVar)(&k)
                              | &kPat : int(1..&maxNeighbourhoodSize)
                              , &k <= &neighbourhoodSize
                              , &k < &i
                              , &k > &j
                              ])
                        , and([ &theVar(&k) = incumbent(&theVar)(&j - (&k - &i))
                              | &kPat : int(1..&maxNeighbourhoodSize)
                              , &k <= &neighbourhoodSize
                              , &k >= &i
                              , &k <= &j
                              ])
                        ])
                |]
        )]
sequenceReverseSubSeq _ _ = return []


sequenceAnySwap :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceAnySwap theVar (DomainSequence _ (SequenceAttr sizeAttr _) _)
    | Just maxSize <- getMaxFrom_SizeAttr sizeAttr = do

    let generatorName = "sequenceAnySwap"

    (iPat, i) <- quantifiedVar

    return
        [( generatorName
         , \ neighbourhoodSize _maxNeighbourhoodSize ->
                [essenceStmts|
                    such that
                        &neighbourhoodSize * 2
                            = sum([ toInt(&theVar(&i) != incumbent(&theVar)(&i))
                                  | &iPat : int(1..&maxSize)
                                  , &i <= |&theVar|
                                  ])
                |]
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
         , \ neighbourhoodSize maxNeighbourhoodSize ->
                [essenceStmts|
                    find &iPat, &jPat, &lPat :  int(1..&maxSize)
                    such that
                        and([ &j - &i = &neighbourhoodSize
                            , &i <= |&theVar|
                            , &j <= |&theVar|
                            , and([ &theVar(&k) = incumbent(&theVar)(&k)
                                  | &kPat : int(1..&maxNeighbourhoodSize)
                                  , &k <= &neighbourhoodSize
                                  , &k < &i
                                  , &k > &j
                                  ])
                            , &l >= &i
                            , &l <= &j
                            , &theVar(&l) != incumbent(&theVar)(&l)
                            ])
                |]
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
          , \ neighbourhoodSize maxNeighbourhoodSize ->
                [essenceStmts|
                    find &iPat :  int(1..&maxNeighbourhoodSize)
                    such that
                        and([ and([ &theVar(&k) = incumbent(&theVar)(&k)
                                  | &kPat : int(1..&maxSize)
                                  , &k <= |&theVar|
                                  ])
                            , |incumbent(&theVar)| = |&theVar| + &i
                            , &i <= &neighbourhoodSize
                            ])
                |]
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
          , \ neighbourhoodSize maxNeighbourhoodSize ->
                [essenceStmts|
                    find &iPat :  int(1..&maxNeighbourhoodSize)
                    such that
                        and([ and([ &theVar(&k) = incumbent(&theVar)(&k + &i)
                                  | &kPat : int(1..&maxSize)
                                  , &k <= |&theVar|
                                  ])
                            , |incumbent(&theVar)| = |&theVar| + &i
                            , &i <= &neighbourhoodSize
                            ])
                |]
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
         , \ neighbourhoodSize maxNeighbourhoodSize ->
                [essenceStmts|
                    find &iPat :  int(1..&maxNeighbourhoodSize)
                    such that
                        and([ and([ &theVar(&k) = incumbent(&theVar)(&k)
                                  | &kPat : int(1..&maxSize)
                                  , &k <= |&theVar| - &i
                                  ])
                            , |incumbent(&theVar)| = |&theVar| - &i
                            , &i <= &neighbourhoodSize
                            ])
                |]
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
         , \ neighbourhoodSize maxNeighbourhoodSize ->
                [essenceStmts|
                    find &iPat :  int(1..&maxNeighbourhoodSize)
                    such that
                        and([ and([ &theVar(&k) = incumbent(&theVar)(&k - &i)
                                  | &kPat : int(1..&maxSize)
                                  , &k <= |&theVar| - &i
                                  ])
                            , |incumbent(&theVar)| = |&theVar| - &i
                            , &i <= &neighbourhoodSize
                            ])
                |]
        )]
sequenceRemoveLeft _ _ = return []

