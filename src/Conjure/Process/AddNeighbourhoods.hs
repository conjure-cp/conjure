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
            Declaration (FindOrGiven Find nm dom) -> generateNeighbourhoods nm dom
            _ -> return []
    let maxNeighbourhoodSizeDecl = Declaration (FindOrGiven Given maxNeighbourhoodSizeVarName (DomainInt []))
    let outModel = inpModel { mStatements = mStatements inpModel
                                         ++ [maxNeighbourhoodSizeDecl]
                                         ++ neighbourhoods }
    (resolveNames >=> typeCheckModel) outModel


maxNeighbourhoodSizeVarName :: Name
maxNeighbourhoodSizeVarName = "SNS_MAX_NEIGHBOURHOOD_SIZE"

maxNeighbourhoodSizeVar :: Expression
maxNeighbourhoodSizeVar = Reference maxNeighbourhoodSizeVarName Nothing


generateNeighbourhoods :: NameGen m => Name -> Domain () Expression -> m [Statement]
generateNeighbourhoods name domain = concatMapM (\ gen -> gen name domain )
    [ setRemove
    , setAdd
    , setSwap
    , setSwapAdd

    , sequenceReverseSubSeq
    , sequenceAnySwap
    , sequenceRelaxSub
    ]


setRemove :: Monad m => Name -> Domain () Expression -> m [Statement]
setRemove name DomainSet{} = do
    let
        generatorName     = "setRemove"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

        theVar            = Reference name Nothing
        activatorVar      = Reference activatorName Nothing
        sizeVar           = Reference sizeName Nothing

    return
        [essenceStmts|
            find &activatorName : bool
            find &sizeName : int(1..&maxNeighbourhoodSizeVar)
            such that
                &activatorVar ->
                    &theVar subsetEq incumbent(&theVar) /\
                    |incumbent(&theVar)| - |&theVar| = &sizeVar
            such that
                !&activatorVar -> dontCare(&sizeVar)
            neighbourhood &neighbourhoodName : (&sizeName, &activatorName, [&theVar])
        |]
setRemove _ _ = return []


setAdd :: Monad m => Name -> Domain () Expression -> m [Statement]
setAdd name DomainSet{} = do
    let
        generatorName     = "setAdd"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

        theVar            = Reference name Nothing
        activatorVar      = Reference activatorName Nothing
        sizeVar           = Reference sizeName Nothing

    return
        [essenceStmts|
            find &activatorName : bool
            find &sizeName : int(1..&maxNeighbourhoodSizeVar)
            such that
                &activatorVar ->
                    incumbent(&theVar) subsetEq &theVar /\
                    |&theVar| - |incumbent(&theVar)| = &sizeVar
            such that
                !&activatorVar -> dontCare(&sizeVar)
            neighbourhood &neighbourhoodName : (&sizeName, &activatorName, [&theVar])
        |]
setAdd _ _ = return []


setSwap :: Monad m => Name -> Domain () Expression -> m [Statement]
setSwap name DomainSet{} = do
    let
        generatorName     = "setSwap"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

        theVar            = Reference name Nothing
        activatorVar      = Reference activatorName Nothing
        sizeVar           = Reference sizeName Nothing

    return
        [essenceStmts|
            find &activatorName : bool
            find &sizeName : int(1..&maxNeighbourhoodSizeVar)
            such that
                &activatorVar ->
                    (|&theVar - incumbent(&theVar)| = &sizeVar) /\
                    (|incumbent(&theVar)| = |&theVar|)
            such that
                !&activatorVar -> dontCare(&sizeVar)
            neighbourhood &neighbourhoodName : (&sizeName, &activatorName, [&theVar])
        |]
setSwap _ _ = return []


setSwapAdd :: Monad m => Name -> Domain () Expression -> m [Statement]
setSwapAdd name DomainSet{} = do
    let
        generatorName     = "setSwapAdd"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

        theVar            = Reference name Nothing
        activatorVar      = Reference activatorName Nothing
        sizeVar           = Reference sizeName Nothing

    return
        [essenceStmts|
            find &activatorName : bool
            find &sizeName : int(1..&maxNeighbourhoodSizeVar)
            such that
                &activatorVar ->
                    |&theVar - incumbent(&theVar)| = &sizeVar
            such that
                !&activatorVar -> dontCare(&sizeVar)
            neighbourhood &neighbourhoodName : (&sizeName, &activatorName, [&theVar])
        |]
setSwapAdd _ _ = return []


sequenceReverseSubSeq :: NameGen m => Name -> Domain () Expression -> m [Statement]
sequenceReverseSubSeq name (DomainSequence _ (SequenceAttr (SizeAttr_Size size) _) _) = do
    let
        generatorName     = "sequenceReverseSubSeq"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

        theVar            = Reference name Nothing
        activatorVar      = Reference activatorName Nothing
        sizeVar           = Reference sizeName Nothing

    (iPat, i) <- auxiliaryVar
    (jPat, j) <- auxiliaryVar
    (kPat, k) <- quantifiedVar

    return
        [essenceStmts|
            find &activatorName : bool
            find &sizeName : int(1..&maxNeighbourhoodSizeVar)
            find &iPat, &jPat :  int(1..&size)
            such that 
                &activatorVar ->
                    and([ &j - &i = &sizeVar
                        , and([ &theVar(&k) = incumbent(&theVar)(&k)
                              | &kPat : int(1..&sizeVar)
                              , &k < &i
                              , &k > &j
                              ])
                        , and([ &theVar(&k) = incumbent(&theVar)(&j - (&k - &i))
                              | &kPat : int(1..&sizeVar)
                              , &k >= &i
                              , &k <= &j
                              ])
                        ]),
                !&activatorVar -> dontCare(tuple (&i, &j, &sizeVar))
                neighbourhood &neighbourhoodName : (&sizeName, &activatorName, [&theVar])
        |]
sequenceReverseSubSeq _ _ = return []


sequenceAnySwap :: NameGen m => Name -> Domain () Expression -> m [Statement]
sequenceAnySwap name (DomainSequence _ (SequenceAttr (SizeAttr_Size size) _) _) = do
    let
        generatorName     = "sequenceAnySwap"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

        theVar            = Reference name Nothing
        activatorVar      = Reference activatorName Nothing
        sizeVar           = Reference sizeName Nothing

    (iPat, i) <- quantifiedVar

    return
        [essenceStmts|
            find &activatorName : bool
            find &sizeName : int(1..&maxNeighbourhoodSizeVar)
            such that 
                &activatorVar ->
                    (sum &iPat : int(1..&size) . toInt(&theVar(&i) != incumbent(&theVar)(&i))) = &sizeVar * 2,
                !&activatorVar -> dontCare(&sizeVar)
                neighbourhood &neighbourhoodName : (&sizeName, &activatorName, [&theVar])
        |]
sequenceAnySwap _ _ = return []


sequenceRelaxSub :: NameGen m => Name -> Domain () Expression -> m [Statement]
sequenceRelaxSub name (DomainSequence _ (SequenceAttr (SizeAttr_Size size) _) _) = do
    let
        generatorName     = "sequenceRelaxSub"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

        theVar            = Reference name Nothing
        activatorVar      = Reference activatorName Nothing
        sizeVar           = Reference sizeName Nothing

    (iPat, i) <- auxiliaryVar
    (jPat, j) <- auxiliaryVar
    (kPat, k) <- quantifiedVar
    (lPat, l) <- auxiliaryVar

    return
        [essenceStmts|
            find &activatorName : bool
            find &sizeName : int(1..&maxNeighbourhoodSizeVar)
            find &iPat, &jPat, &lPat :  int(1..&size)
            such that 
                &activatorVar ->
                    and([ &j - &i = &sizeVar
                        , and([ &theVar(&k) = incumbent(&theVar)(&k)
                              | &kPat : int(1..&sizeVar)
                              , &k < &i
                              , &k > &j
                              ])
                        , &l >= &i
                        , &l <= &j
                        , &theVar(&l) != incumbent(&theVar)(&l)
                        ]),
                !&activatorVar -> dontCare(tuple (&i, &j, &l, &sizeVar))
                neighbourhood &neighbourhoodName : (&sizeName, &activatorName, [&theVar])
        |]
sequenceRelaxSub _ _ = return []

