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

        neighbourhoodName = mconcat [varName, "_", generatorName]

        activatorName     = mconcat [neighbourhoodName, "_", "activator"]
        sizeName          = mconcat [neighbourhoodName, "_", "size"]

        activatorVar      = Reference activatorName Nothing
        sizeVar           = Reference sizeName Nothing

        (statements, consPositive, consNegative) = consGen sizeVar
    in
        [essenceStmts|
            find &activatorName : bool
            find &sizeName : int(1..&maxNeighbourhoodSizeVar)
            neighbourhood &neighbourhoodName : (&sizeName, &activatorName, [&var])
        |]
        ++ fromMaybe [] statements
        ++ concat [ [essenceStmts| such that  &activatorVar -> &c |] | Just c <- [consPositive] ]
        ++ concat [ [essenceStmts| such that !&activatorVar -> &c |] | Just c <- [consNegative] ]
        ++ concat [ [essenceStmts| such that !&activatorVar -> dontCare(&sizeVar) |] ]


type NeighbourhoodGenResult = (Name, Expression -> (Maybe [Statement], Maybe Expression, Maybe Expression))


allNeighbourhoods :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
allNeighbourhoods theVar domain = concatMapM (\ gen -> gen theVar domain )
    [ setRemove
    , setAdd
    , setSwap
    , setSwapAdd

    , sequenceReverseSubSeq
    , sequenceAnySwap
    , sequenceRelaxSub
    ]


-- setLiftExists :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
-- setLiftExists theVar (DomainSet _ _ inner) = do
--     let generatorName = "setLiftExists"
--     (iPat, i) <- quantifiedVar
--     ns <- allNeighbourhoods i inner
--     return
--         [ ( mconcat [generatorName, "_", innerGeneratorName]
--           , \ sizeVar ->
--               let (consPositive, consNegative) = rule sizeVar
--               in  ( [essence| exists &iPat in &theVar . &consPositive |]
--                   , [essence| exists &iPat in &theVar . &consNegative |]
--                   )
--           )
--         | (innerGeneratorName, rule) <- ns
--         ]
-- setLiftExists _ _ = return []


setRemove :: Monad m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
setRemove theVar DomainSet{} = do
    let generatorName = "setRemove"
    return
        [( generatorName
         , \ sizeVar ->
            ( Nothing
            , Just [essence|
                &theVar subsetEq incumbent(&theVar) /\
                |incumbent(&theVar)| - |&theVar| = &sizeVar
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
         , \ sizeVar ->
            ( Nothing
            , Just [essence|
                incumbent(&theVar) subsetEq &theVar /\
                |&theVar| - |incumbent(&theVar)| = &sizeVar
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
         , \ sizeVar ->
            ( Nothing
            , Just [essence|
                (|&theVar - incumbent(&theVar)| = &sizeVar) /\
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
         , \ sizeVar ->
            ( Nothing
            , Just [essence|
                |&theVar - incumbent(&theVar)| = &sizeVar
              |]
            , Nothing
            )
        )]
setSwapAdd _ _ = return []


sequenceReverseSubSeq :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceReverseSubSeq theVar (DomainSequence _ (SequenceAttr (SizeAttr_Size size) _) _) = do
    let generatorName = "sequenceReverseSubSeq"

    (iPat, i) <- auxiliaryVar
    (jPat, j) <- auxiliaryVar
    (kPat, k) <- quantifiedVar

    return
        [( generatorName
         , \ sizeVar ->
            ( Just [essenceStmts|
                find &iPat, &jPat :  int(1..&size)
              |]
            , Just [essence|
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
                    ])
              |]
            , Just [essence| dontCare(tuple (&i, &j)) |]
            )
        )]
sequenceReverseSubSeq _ _ = return []


sequenceAnySwap :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceAnySwap theVar (DomainSequence _ (SequenceAttr (SizeAttr_Size size) _) _) = do
    let generatorName = "sequenceAnySwap"

    (iPat, i) <- quantifiedVar

    return
        [( generatorName
         , \ sizeVar ->
             ( Nothing
             , Just [essence|
                 (sum &iPat : int(1..&size) . toInt(&theVar(&i) != incumbent(&theVar)(&i))) = &sizeVar * 2
               |]
             , Nothing
             )
        )]
sequenceAnySwap _ _ = return []


sequenceRelaxSub :: NameGen m => Expression -> Domain () Expression -> m [NeighbourhoodGenResult]
sequenceRelaxSub theVar (DomainSequence _ (SequenceAttr (SizeAttr_Size size) _) _) = do
    let generatorName = "sequenceRelaxSub"

    (iPat, i) <- auxiliaryVar
    (jPat, j) <- auxiliaryVar
    (kPat, k) <- quantifiedVar
    (lPat, l) <- auxiliaryVar

    return
        [( generatorName
        , \ sizeVar -> 
            ( Just [essenceStmts|
                find &iPat, &jPat, &lPat :  int(1..&size)
              |]
            , Just [essence|
                and([ &j - &i = &sizeVar
                    , and([ &theVar(&k) = incumbent(&theVar)(&k)
                          | &kPat : int(1..&sizeVar)
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

