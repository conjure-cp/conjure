{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.AddNeighbourhoods where

import Conjure.Prelude
import Conjure.Language


addNeighbourhoods :: Monad m => Model -> m Model
addNeighbourhoods inpModel = do
    neighbourhoods <-
        concatForM (mStatements inpModel) $ \case
            Declaration (FindOrGiven Find nm dom) -> generateNeighbourhoods nm dom
            _ -> return []
    let maxNeighbourhoodSizeDecl = Declaration (FindOrGiven Given maxNeighbourhoodSizeVarName (DomainInt []))
    let outModel = inpModel { mStatements = mStatements inpModel
                                         ++ [maxNeighbourhoodSizeDecl]
                                         ++ neighbourhoods }
    return outModel


maxNeighbourhoodSizeVarName :: Name
maxNeighbourhoodSizeVarName = "SNS_MAX_NEIGHBOURHOOD_SIZE"

maxNeighbourhoodSizeVar :: Expression
maxNeighbourhoodSizeVar = Reference maxNeighbourhoodSizeVarName Nothing


generateNeighbourhoods :: Monad m => Name -> Domain () Expression -> m [Statement]
generateNeighbourhoods name domain = concatMapM (\ gen -> gen name domain )
    [ setRemove
    , setAdd
    , setSwap
    , setSwapAdd
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

