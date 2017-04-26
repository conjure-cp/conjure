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
    let outModel = inpModel { mStatements = mStatements inpModel
                                         ++ [maxNeighbourhoodSizeDecl]
                                         ++ neighbourhoods }
    (resolveNames >=> typeCheckModel) outModel


maxNeighbourhoodSizeVarName :: Name
maxNeighbourhoodSizeVarName = "SNS_MAX_NEIGHBOURHOOD_SIZE"

maxNeighbourhoodSizeVar :: Expression
maxNeighbourhoodSizeVar = Reference maxNeighbourhoodSizeVarName Nothing


generateNeighbourhoods :: NameGen m => Name -> Expression -> Domain () Expression -> m [Statement]
generateNeighbourhoods name theVar domain = concatMapM (\ gen -> gen name theVar domain )
    [ setRemove
    , setAdd
    , setSwap
    , setSwapAdd

    , sequenceReverseSubSeq
    , sequenceAnySwap
    , sequenceRelaxSub
    ]


skeleton :: Name                                            -- neighbourhood generator name
         -> Name -> Expression                              -- name of the variable, and a Reference to it
         -> (Expression -> (Expression, Expression))        -- constraint generator
         -> [Statement]                                     -- neighbourhood definition
skeleton generatorName varName var consGen =
    let
        neighbourhoodName = mconcat [varName, "_", generatorName]

        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

        activatorVar      = Reference activatorName Nothing
        sizeVar           = Reference sizeName Nothing

        (consPositive, consNegative) = consGen sizeVar
    in
        [essenceStmts|
            find &activatorName : bool
            find &sizeName : int(1..&maxNeighbourhoodSizeVar)
            neighbourhood &neighbourhoodName : (&sizeName, &activatorName, [&var])
            such that
                &activatorVar -> &consPositive,
                !&activatorVar -> &consNegative
        |]


setRemove :: Monad m => Name -> Expression -> Domain () Expression -> m [Statement]
setRemove theVarName theVar DomainSet{} = do
    let generatorName = "setRemove"
    return $ skeleton generatorName theVarName theVar $ \ sizeVar ->
        ( [essence|
            &theVar subsetEq incumbent(&theVar) /\
            |incumbent(&theVar)| - |&theVar| = &sizeVar
          |]
        , [essence|
            dontCare(&sizeVar)
          |]
        )
setRemove _ _ _ = return []


setAdd :: Monad m => Name -> Expression -> Domain () Expression -> m [Statement]
setAdd name theVar DomainSet{} = do
    let
        generatorName     = "setAdd"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

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
setAdd _ _ _ = return []


setSwap :: Monad m => Name -> Expression -> Domain () Expression -> m [Statement]
setSwap name theVar DomainSet{} = do
    let
        generatorName     = "setSwap"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

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
setSwap _ _ _ = return []


setSwapAdd :: Monad m => Name -> Expression -> Domain () Expression -> m [Statement]
setSwapAdd name theVar DomainSet{} = do
    let
        generatorName     = "setSwapAdd"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

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
setSwapAdd _ _ _ = return []


sequenceReverseSubSeq :: NameGen m => Name -> Expression -> Domain () Expression -> m [Statement]
sequenceReverseSubSeq name theVar (DomainSequence _ (SequenceAttr (SizeAttr_Size size) _) _) = do
    let
        generatorName     = "sequenceReverseSubSeq"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

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
sequenceReverseSubSeq _ _ _ = return []


sequenceAnySwap :: NameGen m => Name -> Expression -> Domain () Expression -> m [Statement]
sequenceAnySwap name theVar (DomainSequence _ (SequenceAttr (SizeAttr_Size size) _) _) = do
    let
        generatorName     = "sequenceAnySwap"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

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
sequenceAnySwap _ _ _ = return []


sequenceRelaxSub :: NameGen m => Name -> Expression -> Domain () Expression -> m [Statement]
sequenceRelaxSub name theVar (DomainSequence _ (SequenceAttr (SizeAttr_Size size) _) _) = do
    let
        generatorName     = "sequenceRelaxSub"
        neighbourhoodName = mconcat [name, "_", generatorName]
        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

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
sequenceRelaxSub _ _ _ = return []

