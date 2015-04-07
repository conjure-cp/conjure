{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Conjure.UI.Model
    ( outputModels
    , Strategy(..), Config(..), parseStrategy
    , nbUses
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Expression.Internal.Generated ()
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.CategoryOf
import Conjure.Language.TypeOf
import Conjure.Language.DomainOf
import Conjure.Language.Lenses
import Conjure.Language.TH ( essence )
import Conjure.Language.Expression.Op
import Conjure.Language.ModelStats ( modelInfo )
import Conjure.Language.Instantiate ( instantiateExpression, trySimplify )
import Conjure.Process.Sanity ( sanityChecks )
import Conjure.Process.Enums ( removeEnumsFromModel )
import Conjure.Process.Unnameds ( removeUnnamedsFromModel )
import Conjure.Process.FiniteGivens ( finiteGivens )
import Conjure.Process.LettingsForComplexInDoms ( lettingsForComplexInDoms, inlineLettingDomainsForDecls )
import Conjure.Process.AttributeAsConstraints ( attributeAsConstraints, mkAttributeToConstraint )
import Conjure.Process.DealWithCuts ( dealWithCuts )
import Conjure.Language.NameResolution ( resolveNames, resolveNamesX )
import Conjure.UI.TypeCheck ( typeCheckModel )
import Conjure.UI.LogFollow ( logFollow, storeChoice )

import Conjure.Representations ( downX1, downD, reprOptions, getStructurals )

import Conjure.Rules.Definition

import qualified Conjure.Rules.Vertical.Tuple as Vertical.Tuple
import qualified Conjure.Rules.Vertical.Record as Vertical.Record
import qualified Conjure.Rules.Vertical.Variant as Vertical.Variant
import qualified Conjure.Rules.Vertical.Matrix as Vertical.Matrix

import qualified Conjure.Rules.Horizontal.Set as Horizontal.Set
import qualified Conjure.Rules.Vertical.Set.Explicit as Vertical.Set.Explicit
import qualified Conjure.Rules.Vertical.Set.ExplicitVarSizeWithFlags as Vertical.Set.ExplicitVarSizeWithFlags
import qualified Conjure.Rules.Vertical.Set.ExplicitVarSizeWithMarker as Vertical.Set.ExplicitVarSizeWithMarker
import qualified Conjure.Rules.Vertical.Set.Occurrence as Vertical.Set.Occurrence

import qualified Conjure.Rules.Horizontal.MSet as Horizontal.MSet
import qualified Conjure.Rules.Vertical.MSet.ExplicitVarSizeWithFlags as Vertical.MSet.ExplicitVarSizeWithFlags

import qualified Conjure.Rules.Horizontal.Function as Horizontal.Function
import qualified Conjure.Rules.Vertical.Function.Function1D as Vertical.Function.Function1D
import qualified Conjure.Rules.Vertical.Function.Function1DPartial as Vertical.Function.Function1DPartial
import qualified Conjure.Rules.Vertical.Function.FunctionND as Vertical.Function.FunctionND
import qualified Conjure.Rules.Vertical.Function.FunctionNDPartial as Vertical.Function.FunctionNDPartial
import qualified Conjure.Rules.Vertical.Function.FunctionAsRelation as Vertical.Function.FunctionAsRelation

import qualified Conjure.Rules.Horizontal.Sequence as Horizontal.Sequence
import qualified Conjure.Rules.Vertical.Sequence.ExplicitBounded as Vertical.Sequence.ExplicitBounded

import qualified Conjure.Rules.Horizontal.Relation as Horizontal.Relation
import qualified Conjure.Rules.Vertical.Relation.RelationAsMatrix as Vertical.Relation.RelationAsMatrix
import qualified Conjure.Rules.Vertical.Relation.RelationAsSet as Vertical.Relation.RelationAsSet

import qualified Conjure.Rules.Horizontal.Partition as Horizontal.Partition
import qualified Conjure.Rules.Vertical.Partition.PartitionAsSet as Vertical.Partition.PartitionAsSet
import qualified Conjure.Rules.Vertical.Partition.Occurrence as Vertical.Partition.Occurrence

import qualified Conjure.Rules.BubbleUp as BubbleUp
import qualified Conjure.Rules.DontCare as DontCare


-- uniplate
import Data.Generics.Uniplate.Zipper ( Zipper, zipperBi, fromZipper, hole, replaceHole )
import qualified Data.Generics.Uniplate.Zipper as Zipper ( up )

-- pipes
import Pipes ( Producer, await, yield, (>->), cat )
import qualified Pipes.Prelude as Pipes ( foldM )


outputModels
    :: (MonadIO m, MonadFail m, MonadLog m, NameGen m)
    => Config
    -> Model
    -> m ()
outputModels config model = do
    let dir = outputDirectory config
    liftIO $ createDirectoryIfMissing True dir

    let
        limitModelsIfNeeded = maybe Pipes.cat limitModelsNeeded (limitModels config)
        limitModelsNeeded 0 = return ()
        limitModelsNeeded n = do
            x <- Pipes.await
            Pipes.yield x
            case x of
                Left {} -> limitModelsNeeded n              -- yielded a log, still n models to produce
                Right{} -> limitModelsNeeded (n-1)          -- yielded a model, produce n-1 more models

        each i logOrModel =
            case logOrModel of
                Left (l,msg) -> do
                    log l msg
                    return i
                Right eprime -> do
                    let gen =
                            if smartFilenames config
                                then [ choice
                                     | [_question, (_strategy, choice, numOptions)] <-
                                             eprime |> mInfo |> miTrailCompact |> chunksOf 2
                                     , numOptions > 1
                                     ] |> map (('_':) . show)
                                       |> concat
                                else paddedNum i
                    let filename = dir </> "model" ++ gen ++ ".eprime"
                    liftIO $ writeFile filename (renderNormal eprime)
                    return (i+1)

    Pipes.foldM each
                (return (numberingStart config))
                (const $ return ())
                (toCompletion config model
                    >-> limitModelsIfNeeded)


toCompletion
    :: (MonadIO m, MonadFail m, NameGen m)
    => Config
    -> Model
    -> Producer LogOrModel m ()
toCompletion config m = do
    m2 <- prologue m
    logInfo $ modelInfo m2
    loopy m2
    where
        driver = strategyToDriver config
        loopy model = do
            logDebug $ "[loopy]" <+> pretty model
            qs <- remaining config model
            if null qs
                then do
                    model' <- epilogue model
                    yield (Right model')
                else do
                    nextModels <- driver qs
                    mapM_ loopy nextModels


remaining
    :: (MonadFail m, MonadLog m, NameGen m)
    => Config
    -> Model
    -> m [Question]
remaining config model | Just modelZipper <- zipperBi model = do
    let
        loopLevels :: Monad m => [m [a]] -> m [a]
        loopLevels [] = return []
        loopLevels (a:as) = do bs <- a
                               if null bs
                                   then loopLevels as
                                   else return bs

        processLevel :: (MonadFail m, MonadLog m, NameGen m)
                     => [Rule]
                     -> m [(Zipper Model Expression, [(Doc, RuleResult m)])]
        processLevel rulesAtLevel =
            fmap catMaybes $ forM (allContextsExceptReferences modelZipper) $ \ x -> do
                ys <- applicableRules config rulesAtLevel x
                return $ if null ys
                            then Nothing
                            else Just (x, ys)

    questions <- loopLevels $ map processLevel (allRules config)
    forM questions $ \ (focus, answers0) -> do
        answers1 <- forM answers0 $ \ (ruleName, RuleResult{..}) -> do
            importNameGenState $ model |> mInfo |> miNameGenState
            ruleResultExpr <- ruleResult
            let fullModelBeforeHook = fromZipper (replaceHole ruleResultExpr focus)
            let mtyBefore = typeOf (hole focus)
            let mtyAfter  = typeOf ruleResultExpr
            case (mtyBefore, mtyAfter) of
                (Right tyBefore, Right tyAfter) ->
                    if typesUnify [tyBefore, tyAfter]
                        then return ()
                        else bug $ vcat
                                [ "Rule application changes type."
                                , "Before:" <+> pretty tyBefore
                                , "After :" <+> pretty tyAfter
                                ]
                (Left msg, _) -> bug $ vcat
                                [ "Type error before rule application:" <+> pretty ruleName
                                , "Before:" <+> pretty (hole focus)
                                , "After :" <+> pretty ruleResultExpr
                                , "Error :" <+> pretty msg
                                ]
                (_, Left msg) -> bug $ vcat
                                [ "Type error after rule application:" <+> pretty ruleName
                                , "Before:" <+> pretty (hole focus)
                                , "After :" <+> pretty ruleResultExpr
                                , "Error :" <+> pretty msg
                                ]
            let updNameGenState m0 = do
                    namegenst <- exportNameGenState
                    let mi0 = mInfo m0
                    let mi1 = mi0 { miNameGenState = namegenst }
                    let m1  = m0  { mInfo = mi1 }
                    return m1
            fullModelAfterHook <- ruleResultHook fullModelBeforeHook >>= updNameGenState
            return
                ( Answer
                    { aText = ruleName <> ":" <+> ruleResultDescr
                    , aRuleName = ruleName
                    , aAnswer = ruleResultExpr
                    , aFullModel = fullModelAfterHook
                    }
                , ruleResultType
                )
        let qTypes = map snd answers1
        qType' <- if all (head qTypes ==) (tail qTypes)
                    then return (head qTypes)
                    else bug "Rules of different rule kinds applicable, this is a bug."
        return Question
            { qType = qType'
            , qHole = hole focus
            , qAscendants = tail (ascendants focus)
            , qAnswers = map fst answers1
            }
remaining _ _ = return []


strategyToDriver :: Config -> Driver
strategyToDriver config questions = do
    let optionsQ =
            [ (doc, q)
            | (n, q) <- zip allNats questions
            , let doc =
                    vcat $ ("Question" <+> pretty n <> ":" <+> pretty (qHole q))
                         : [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
                           | i <- allNats
                           | c <- qAscendants q
                           ]
            ]
    pickedQs <- executeStrategy optionsQ (strategyQ config)
    fmap concat $ forM pickedQs $ \ (pickedQNumber, pickedQDescr, pickedQ) -> do
        let optionsA =
                [ (doc, a)
                | (n, a) <- zip allNats (qAnswers pickedQ)
                , let doc =
                        nest 4 $ "Answer" <+> pretty n <> ":" <+> vcat [ pretty (aText a)
                                                                       , pretty (aAnswer a) ]
                ]
        let strategyA' = case qType pickedQ of
                ChooseRepr            -> representations
                ChooseRepr_Find       -> representationsFinds
                ChooseRepr_Given      -> representationsGivens
                ChooseRepr_Auxiliary  -> representationsAuxiliaries
                ChooseRepr_Quantified -> representationsQuantifieds
                ChooseRepr_Cut        -> representationsCuts
                ExpressionRefinement  -> strategyA
        pickedAs <- executeAnswerStrategy config pickedQ optionsA (strategyA' config)
        return
            [ theModel
                |> addToTrail
                        config
                        (strategyQ  config) pickedQNumber (length optionsQ) [pickedQDescr]
                        (strategyA' config) pickedANumber (length optionsA) [pickedADescr]
            | (pickedANumber, pickedADescr, pickedA) <- pickedAs
            , let theModel = aFullModel pickedA
            ]


executeStrategy :: (MonadIO m, MonadLog m) => [(Doc, a)] -> Strategy -> m [(Int, Doc, a)]
executeStrategy [] _ = bug "executeStrategy: nothing to choose from"
executeStrategy [(doc, option)] (viewAuto -> (_, True)) = do
    logInfo ("Picking the only option:" <+> doc)
    return [(1, doc, option)]
executeStrategy options@((doc, option):_) (viewAuto -> (strategy, _)) =
    case strategy of
        Auto _      -> bug "executeStrategy: Auto"
        PickFirst   -> do
            logInfo ("Picking the first option:" <+> doc)
            return [(1, doc, option)]
        PickAll     -> return [ (i,d,o) | (i,(d,o)) <- zip [1..] options ]
        Interactive -> liftIO $ do
            print (vcat (map fst options))
            let
                pickIndex = do
                    putStr "Pick option: "
                    line <- getLine
                    case (line, readMay line) of
                        ("", _) -> return 1
                        (_, Just lineInt) | lineInt >= 1 && lineInt <= length options -> return lineInt
                        (_, Nothing) -> do
                            putStrLn "Enter an integer value."
                            pickIndex
                        (_, Just _) -> do
                            print $ pretty $ "Enter a value between 1 and" <+> pretty (length options)
                            pickIndex
            pickedIndex <- pickIndex
            let (pickedDescr, picked) = at options (pickedIndex - 1)
            return [(pickedIndex, pickedDescr, picked)]
        AtRandom -> do
            let nbOptions = length options
            pickedIndex <- liftIO $ randomRIO (1, nbOptions)
            let (pickedDescr, picked) = at options (pickedIndex - 1)
            logInfo ("Randomly picking option #" <> pretty pickedIndex <+> "out of" <+> pretty nbOptions)
            return [(pickedIndex, pickedDescr, picked)]
        Compact -> bug "executeStrategy: Compact"
        FollowLog -> bug "executeStrategy: FollowLog"


executeAnswerStrategy :: (MonadIO m, MonadLog m)
                      => Config -> Question -> [(Doc, Answer)] -> Strategy -> m [(Int, Doc, Answer)]
executeAnswerStrategy _  _ [] _ = bug "executeStrategy: nothing to choose from"
executeAnswerStrategy config q [(doc, option)] (viewAuto -> (_, True)) = do
    logInfo ("Picking the only option:" <+> doc)
    c <- storeChoice config q option
    return [(1, doc, c)]
executeAnswerStrategy config question options st@(viewAuto -> (strategy, _)) =
    case strategy of
        Compact -> do
            let (n,(doc,c)) = minimumBy (compactCompareAnswer `on` (snd . snd)) (zip [1..] options)
            c' <- storeChoice config question c
            return [(n, doc, c')]
        FollowLog -> logFollow config question options
        AtRandom -> do
          [(n, doc, ans0)] <- executeStrategy options st
          ans1             <- storeChoice config question ans0
          return [(n, doc, ans1)]
        _  -> do
          cs <- executeStrategy options st
          forM cs $ \ (n, d, c) -> do
              c' <- storeChoice config question c
              return (n, d, c')


compactCompareAnswer :: Answer -> Answer -> Ordering
compactCompareAnswer = comparing (expressionDepth . aAnswer)
    where
        expressionDepth :: Data a => a -> Int
        expressionDepth x = 1 + maximum (0 : map expressionDepth (children x))


addToTrail
    :: Config
    -> Strategy -> Int -> Int -> [Doc]
    -> Strategy -> Int -> Int -> [Doc]
    -> Model -> Model
addToTrail Config{..}
           questionStrategy questionNumber questionNumbers questionDescr
           answerStrategy   answerNumber   answerNumbers   answerDescr
           model = model { mInfo = newInfo }
    where
        oldInfo = mInfo model
        newInfo = oldInfo { miTrailCompact = miTrailCompact oldInfo ++
                                    [ (questionStrategy, questionNumber, questionNumbers)
                                    , (answerStrategy  , answerNumber,   answerNumbers  )
                                    ]
                          , miTrailVerbose = if verboseTrail
                                                  then miTrailVerbose oldInfo ++ [theQ, theA]
                                                  else []
                          }
        theQ = Decision
            { dDescription = map (stringToText . renderWide)
                $ ("Question #" <> pretty questionNumber <+> "out of" <+> pretty (show questionNumbers))
                : ("  (Using strategy:" <+> pretty (show questionStrategy) <> ")")
                : questionDescr
            , dDecision = questionNumber
            , dNumOptions = questionNumbers
            }
        theA = Decision
            { dDescription = map (stringToText . renderWide)
                $ ("Answer #" <> pretty answerNumber <+> "out of" <+> pretty (show answerNumbers))
                : ("  (Using strategy:" <+> pretty (show answerStrategy) <> ")")
                : answerDescr
            , dDecision = answerNumber
            , dNumOptions = answerNumbers
            }


-- | Add a true-constraint, for every decision variable and
--                          for every parameter that is not used in the model.
--   A true-constraint has no effect, other than forcing Conjure to produce a representation.
--   It can be used to make sure that a declaration doesn't get lost (if it isn't used anywhere in the model)
--   It can also be used to produce "extra" representations (if it is used in the model)
addTrueConstraints :: Model -> Model
addTrueConstraints m =
    let
        mkTrueConstraint forg nm dom = Op $ MkOpTrue $ OpTrue (Reference nm (Just (DeclNoRepr forg nm dom)))
        trueConstraints = [ mkTrueConstraint forg nm d
                          | (Declaration (FindOrGiven forg nm d), after) <- withAfter (mStatements m)
                          , forg == Find || (forg == Given && nbUses nm after == 0)
                          ]
    in
        m { mStatements = mStatements m ++ [SuchThat trueConstraints] }

nbUses :: Data x => Name -> x -> Int
nbUses nm here = length [ () | Reference nm2 _ <- universeBi here, nm == nm2 ]


oneSuchThat :: Model -> Model
oneSuchThat m = m { mStatements = onStatements (mStatements m) }

    where

        onStatements :: [Statement] -> [Statement]
        onStatements xs =
            let
                (suchThats0, objectives, others) = xs |> map collect |> mconcat
                suchThats = suchThats0
                      |> map breakConjunctions                         -- break top level /\'s
                      |> mconcat
                      |> filter (/= Constant (ConstantBool True))      -- remove top level true's
                      |> nub                                           -- uniq
            in
                others ++ objectives ++ [SuchThat (combine suchThats)]

        collect :: Statement -> ( [Expression]                  -- SuchThats
                                , [Statement]                   -- Objectives
                                , [Statement]                   -- other statements
                                )
        collect (SuchThat s) = (s, [], [])
        collect s@(Objective{}) = ([], [s], [])
        collect s = ([], [], [s])

        combine :: [Expression] -> [Expression]
        combine xs = if null xs
                        then [Constant (ConstantBool True)]
                        else xs

        breakConjunctions :: Expression -> [Expression]
        breakConjunctions p@(Op (MkOpAnd (OpAnd x))) =
            case listOut x of
                Nothing -> [p] -- doesn't contain a list
                Just xs -> concatMap breakConjunctions xs
        breakConjunctions x = [x]


toIntIsNoOp :: Model -> Model
toIntIsNoOp model =
    let
        f [essence| toInt(&x) |] = x
        f x = x
    in
        model { mStatements = mStatements model |> transformBi f }


emptyMatrixLiterals :: Model -> Model
emptyMatrixLiterals model =
    let
        f (TypeList ty) = TypeMatrix TypeInt ty
        f x = x
    in
        model { mStatements = mStatements model |> transformBi f }


inlineDecVarLettings :: Model -> Model
inlineDecVarLettings model =
    let
        inline p@(Reference nm _) = do
            x <- gets (lookup nm)
            return (fromMaybe p x)
        inline p = return p

        statements = catMaybes
                        $ flip evalState []
                        $ forM (mStatements model)
                        $ \ st ->
            case st of
                Declaration (Letting nm x)
                    | categoryOf x == CatDecision
                    -> modify ((nm,x) :) >> return Nothing
                -- The following doesn't work when the identifier is used in a domain
                -- Declaration (Letting nm x@Reference{})
                --     -> modify ((nm,x) :) >> return Nothing
                _ -> Just <$> transformBiM inline st
    in
        model { mStatements = statements }


updateDeclarations :: Model -> Model
updateDeclarations model =
    let
        representations = model |> mInfo |> miRepresentations

        statements = concatMap onEachStatement (withAfter (mStatements model))

        onEachStatement (inStatement, afters) =
            case inStatement of
                Declaration (FindOrGiven forg nm _) ->
                    let
                        -- the refined domains for the high level declaration
                        domains = [ d | (n, d) <- representations, n == nm ]
                    in
                        -- duplicate declarations can happen, due to say ExplicitVarSizeWithMarker in the outer level
                        -- and 2 disticnt representations in the inner level. removing them with nub.
                        nub $ concatMap (onEachDomain forg nm) domains
                Declaration (GivenDomainDefnEnum name) ->
                    [ Declaration (FindOrGiven Given (name `mappend` "_EnumSize") (DomainInt [])) ]
                Declaration (Letting nm _)             -> [ inStatement | nbUses nm afters > 0 ]
                Declaration LettingDomainDefnEnum{}    -> []
                Declaration LettingDomainDefnUnnamed{} -> []
                SearchOrder orders -> return $ SearchOrder $ concat
                    [ case order of
                        BranchingOn nm ->
                            let domains = [ d | (n, d) <- representations, n == nm ]
                            in  nub $ concatMap (onEachDomainSearch nm) domains
                        Cut{} -> bug "updateDeclarations, Cut shouldn't be here"
                    | order <- orders
                    ]
                _ -> [inStatement]

        onEachDomain forg nm domain =
            case downD (nm, domain) of
                Left err -> bug err
                Right outs -> [ Declaration (FindOrGiven forg n d')
                              | (n, d) <- outs
                              , let d' = fmap trySimplify $ forgetRepr d
                              ]

        onEachDomainSearch nm domain =
            case downD (nm, domain) of
                Left err -> bug err
                Right outs -> [ BranchingOn n
                              | (n, _) <- outs
                              ]

    in
        model { mStatements = statements }


-- | checking whether any `Reference`s with `DeclHasRepr`s are left in the model
checkIfAllRefined :: MonadFail m => Model -> m Model
checkIfAllRefined m | Just modelZipper <- zipperBi m = do
    let returnMsg x = return
            $ ""
            : ("Not refined:" <+> pretty (hole x))
            : [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
              | i <- allNats
              | c <- tail (ascendants x)
              ]

    fails <- fmap concat $ forM (allContextsExceptReferences modelZipper) $ \ x ->
                case hole x of
                    Reference _ (Just (DeclHasRepr _ _ dom))
                        | not (isPrimitiveDomain dom) ->
                        return $ ""
                               : ("Not refined:" <+> pretty (hole x))
                               : ("Domain     :" <+> pretty dom)
                               : [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
                                 | i <- allNats
                                 | c <- tail (ascendants x)
                                 ]
                    Constant (ConstantAbstract AbsLitMatrix{}) -> return []
                    AbstractLiteral AbsLitMatrix{} -> return []
                    Constant ConstantAbstract{} -> returnMsg x
                    AbstractLiteral{} -> returnMsg x
                    WithLocals{} -> returnMsg x
                    _ -> return []
    unless (null fails) (fail (vcat fails))
    return m
checkIfAllRefined m = return m


topLevelBubbles :: MonadFail m => Model -> m Model
topLevelBubbles m = do
    let
        onStmt (SuchThat xs) = onExprs SuchThat xs
        onStmt (Where    xs) = onExprs Where    xs
        onStmt (Objective obj (WithLocals h (Left  locals))) = (          locals  ++ [Objective obj h]) |> onStmts
        onStmt (Objective obj (WithLocals h (Right locals))) = ([SuchThat locals] ++ [Objective obj h]) |> onStmts
        onStmt s = [s]

        onExpr wrap (WithLocals h (Left  locals)) = (      locals  ++ [wrap [h]]) |> onStmts
        onExpr wrap (WithLocals h (Right locals)) = ([wrap locals] ++ [wrap [h]]) |> onStmts
        onExpr wrap x = [wrap [x]]

        onStmts = concatMap onStmt
        onExprs wrap = concatMap (onExpr wrap)

    return m { mStatements = onStmts (mStatements m) }


sliceThemMatrices :: Monad m => Model -> m Model
sliceThemMatrices model = do
    let
        -- nothing stays with a matrix type
        -- we are doing this top down
        -- when we reach a matrix-typed expression, we know it needs to be sliced
        -- we descend otherwise
        -- we also descend into components of the matrix-typed expression during slicing
        onExpr :: Monad m => Expression -> m Expression
        onExpr p = do
            let isIndexedMatrix = do
                    (m, is) <- match opMatrixIndexing p
                    tyM     <- typeOf m
                    return (m, is, tyM)
            case isIndexedMatrix of
                Nothing -> descendM onExpr p
                Just (m, is, tyM) -> do
                    let nestingLevel (TypeMatrix _ a) = 1 + nestingLevel a
                        nestingLevel (TypeList     a) = 1 + nestingLevel a
                        nestingLevel _ = 0 :: Int
                    let howMany = nestingLevel tyM - length is
                    let unroll a 0 = a
                        unroll a i = make opSlicing (unroll a (i-1)) Nothing Nothing
                    m'  <- descendM onExpr m
                    is' <- mapM onExpr is
                    let p' = make opMatrixIndexing m' is'
                    return $ unroll p' howMany

    statements <- descendBiM onExpr (mStatements model)
    return model { mStatements = statements }


prologue :: (MonadFail m, MonadLog m, NameGen m) => Model -> m Model
prologue model = return model
                                      >>= logDebugId "[input]"
    >>= sanityChecks                  >>= logDebugId "[sanityChecks]"
    >>= attributeAsConstraints        >>= logDebugId "[attributeAsConstraints]"
    >>= inlineLettingDomainsForDecls  >>= logDebugId "[inlineLettingDomainsForDecls]"
    >>= lettingsForComplexInDoms      >>= logDebugId "[lettingsForComplexInDoms]"
    >>= return . initInfo             >>= logDebugId "[initInfo]"
    >>= removeUnnamedsFromModel       >>= logDebugId "[removeUnnamedsFromModel]"
    >>= removeEnumsFromModel          >>= logDebugId "[removeEnumsFromModel]"
    >>= finiteGivens                  >>= logDebugId "[finiteGivens]"
    >>= resolveNames                  >>= logDebugId "[resolveNames]"
    >>= typeCheckModel                >>= logDebugId "[typeCheckModel]"
    >>= categoryChecking              >>= logDebugId "[categoryChecking]"
    >>= dealWithCuts                  >>= logDebugId "[dealWithCuts]"
    >>= return . addTrueConstraints   >>= logDebugId "[addTrueConstraints]"


epilogue :: (MonadFail m, MonadLog m) => Model -> m Model
epilogue model = return model
                                      >>= logDebugId "[epilogue]"
    >>= return . updateDeclarations   >>= logDebugId "[updateDeclarations]"
    >>= return . inlineDecVarLettings >>= logDebugId "[inlineDecVarLettings]"
    >>= topLevelBubbles               >>= logDebugId "[topLevelBubbles]"
    >>= checkIfAllRefined             >>= logDebugId "[checkIfAllRefined]"
    >>= sliceThemMatrices             >>= logDebugId "[sliceThemMatrices]"
    >>= return . toIntIsNoOp          >>= logDebugId "[toIntIsNoOp]"
    >>= return . emptyMatrixLiterals  >>= logDebugId "[emptyMatrixLiterals]"
    >>= return . oneSuchThat          >>= logDebugId "[oneSuchThat]"
    >>= return . languageEprime       >>= logDebugId "[languageEprime]"


applicableRules
    :: forall m n . ( MonadLog n, NameGen n
                    , MonadLog m, NameGen m, MonadFail m
                    )
    => Config
    -> [Rule]
    -> Zipper Model Expression
    -> n [(Doc, RuleResult m)]
applicableRules Config{..} rulesAtLevel x = do
    let logAttempt = if logRuleAttempts  then logInfo else const (return ())
    let logFail    = if logRuleFails     then logInfo else const (return ())
    let logSuccess = if logRuleSuccesses then logInfo else const (return ())

    mys <- sequence [ do logAttempt ("attempting rule" <+> rName r <+> "on" <+> pretty (hole x))
                         applied <- runExceptT $ rApply r x (hole x)
                         return (rName r, applied)
                    | r <- rulesAtLevel ]
    forM_ mys $ \ (rule, my) ->
        case my of
            Left  failed -> unless ("N/A" `isPrefixOf` show failed) $ logFail $ vcat
                [ " rule failed:" <+> rule
                , "          on:" <+> pretty (hole x)
                , "     message:" <+> failed
                ]
            Right ys     -> logSuccess $ vcat
                [ "rule applied:" <+> rule
                , "          on:" <+> pretty (hole x)
                , "     message:" <+> vcat (map ruleResultDescr ys)
                ]
    return [ (name, res {ruleResult = ruleResult'})
           | (name, Right ress) <- mys
           , res <- ress
           , let ruleResult' = do
                    rResult <- ruleResult res
                    resolveNamesX rResult
           ]


allRules :: Config -> [[Rule]]
allRules config =
    [ [ rule_FullEvaluate
      ]
    , [ rule_PartialEvaluate
      ]
    , paramRules
    , [ rule_ChooseRepr config
      , rule_ChooseReprForComprehension
      , rule_ChooseReprForLocals
      ]
    , verticalRules
    , horizontalRules
    ] ++ otherRules
      ++ delayedRules


-- | For information that can be readily pulled out from parameters.
--   Some things are easier when everything involved is a param.
--   These rules aren't necessary for correctness, but they can help remove some verbose expressions from the output.
--   Make Savile Row happier so it makes us happier. :)
paramRules :: [Rule]
paramRules =
    [ Horizontal.Set.rule_Param_MinOfSet
    , Horizontal.Set.rule_Param_MaxOfSet
    , Horizontal.Set.rule_Param_Card
    , Horizontal.Relation.rule_Param_Card
    ]

verticalRules :: [Rule]
verticalRules =
    [ Vertical.Tuple.rule_Tuple_Eq
    , Vertical.Tuple.rule_Tuple_Neq
    , Vertical.Tuple.rule_Tuple_Leq
    , Vertical.Tuple.rule_Tuple_Lt
    , Vertical.Tuple.rule_Tuple_Index

    , Vertical.Record.rule_Record_Eq
    , Vertical.Record.rule_Record_Neq
    , Vertical.Record.rule_Record_Leq
    , Vertical.Record.rule_Record_Lt
    , Vertical.Record.rule_Record_Index

    , Vertical.Variant.rule_Variant_Eq
    , Vertical.Variant.rule_Variant_Neq
    , Vertical.Variant.rule_Variant_Leq
    , Vertical.Variant.rule_Variant_Lt
    , Vertical.Variant.rule_Variant_Index
    , Vertical.Variant.rule_Variant_Active

    , Vertical.Matrix.rule_Comprehension_Literal
    , Vertical.Matrix.rule_Comprehension_LiteralIndexed
    , Vertical.Matrix.rule_Comprehension_Nested
    , Vertical.Matrix.rule_Comprehension_Hist
    , Vertical.Matrix.rule_Comprehension_ToSet
    -- , Vertical.Matrix.rule_Comprehension_ToSet2
    , Vertical.Matrix.rule_Matrix_Eq
    , Vertical.Matrix.rule_Matrix_Neq
    , Vertical.Matrix.rule_Matrix_Leq_Primitive
    , Vertical.Matrix.rule_Matrix_Leq_Decompose
    , Vertical.Matrix.rule_Matrix_Lt_Primitive
    , Vertical.Matrix.rule_Matrix_Lt_Decompose
    , Vertical.Matrix.rule_IndexingIdentical

    , Vertical.Set.Explicit.rule_Card
    , Vertical.Set.Explicit.rule_Comprehension
    , Vertical.Set.Explicit.rule_PowerSet_Comprehension
    , Vertical.Set.ExplicitVarSizeWithFlags.rule_Comprehension
    , Vertical.Set.ExplicitVarSizeWithFlags.rule_PowerSet_Comprehension
    , Vertical.Set.ExplicitVarSizeWithMarker.rule_Card
    , Vertical.Set.ExplicitVarSizeWithMarker.rule_Comprehension
    , Vertical.Set.ExplicitVarSizeWithMarker.rule_PowerSet_Comprehension
    , Vertical.Set.Occurrence.rule_Comprehension
    , Vertical.Set.Occurrence.rule_PowerSet_Comprehension
    , Vertical.Set.Occurrence.rule_In

    , Vertical.MSet.ExplicitVarSizeWithFlags.rule_Comprehension
    , Vertical.MSet.ExplicitVarSizeWithFlags.rule_Freq

    , Vertical.Function.Function1D.rule_Comprehension
    , Vertical.Function.Function1D.rule_Comprehension_Defined
    , Vertical.Function.Function1D.rule_Image

    , Vertical.Function.Function1DPartial.rule_Comprehension
    , Vertical.Function.Function1DPartial.rule_Image_NotABool
    , Vertical.Function.Function1DPartial.rule_Image_Bool
    , Vertical.Function.Function1DPartial.rule_InDefined

    , Vertical.Function.FunctionND.rule_Comprehension
    , Vertical.Function.FunctionND.rule_Comprehension_Defined
    , Vertical.Function.FunctionND.rule_Image

    , Vertical.Function.FunctionNDPartial.rule_Comprehension
    , Vertical.Function.FunctionNDPartial.rule_Image_NotABool
    , Vertical.Function.FunctionNDPartial.rule_Image_Bool
    , Vertical.Function.FunctionNDPartial.rule_InDefined

    , Vertical.Function.FunctionAsRelation.rule_Comprehension
    , Vertical.Function.FunctionAsRelation.rule_Image_Eq

    , Vertical.Sequence.ExplicitBounded.rule_Comprehension
    , Vertical.Sequence.ExplicitBounded.rule_Card
    , Vertical.Sequence.ExplicitBounded.rule_Image_Bool
    , Vertical.Sequence.ExplicitBounded.rule_Image_NotABool

    , Vertical.Relation.RelationAsMatrix.rule_Comprehension
    , Vertical.Relation.RelationAsMatrix.rule_Image

    , Vertical.Relation.RelationAsSet.rule_Comprehension

    , Vertical.Partition.PartitionAsSet.rule_Comprehension

    , Vertical.Partition.Occurrence.rule_Comprehension

    ]

horizontalRules :: [Rule]
horizontalRules =
    [ Horizontal.Set.rule_Comprehension_Literal
    , Horizontal.Set.rule_Eq
    , Horizontal.Set.rule_Neq
    , Horizontal.Set.rule_DotLeq
    , Horizontal.Set.rule_DotLt
    , Horizontal.Set.rule_Subset
    , Horizontal.Set.rule_SubsetEq
    , Horizontal.Set.rule_Supset
    , Horizontal.Set.rule_SupsetEq
    , Horizontal.Set.rule_In
    , Horizontal.Set.rule_Card
    , Horizontal.Set.rule_Intersect
    , Horizontal.Set.rule_Union
    , Horizontal.Set.rule_Difference
    , Horizontal.Set.rule_PowerSet_Difference
    , Horizontal.Set.rule_MaxMin

    , Horizontal.MSet.rule_Comprehension_Literal
    , Horizontal.MSet.rule_Comprehension_ToSet_Literal
    , Horizontal.MSet.rule_Eq
    , Horizontal.MSet.rule_Neq
    , Horizontal.MSet.rule_DotLeq
    , Horizontal.MSet.rule_DotLt
    , Horizontal.MSet.rule_Subset
    , Horizontal.MSet.rule_SubsetEq
    , Horizontal.MSet.rule_Supset
    , Horizontal.MSet.rule_SupsetEq
    , Horizontal.MSet.rule_Freq
    , Horizontal.MSet.rule_In
    , Horizontal.MSet.rule_Card
    , Horizontal.MSet.rule_MaxMin

    , Horizontal.Function.rule_Comprehension_Literal
    , Horizontal.Function.rule_Image_Bool
    , Horizontal.Function.rule_Image_Int
    , Horizontal.Function.rule_Comprehension_Image
    , Horizontal.Function.rule_Image_Literal
    , Horizontal.Function.rule_Eq
    , Horizontal.Function.rule_Neq
    , Horizontal.Function.rule_DotLeq
    , Horizontal.Function.rule_DotLt
    , Horizontal.Function.rule_Subset
    , Horizontal.Function.rule_SubsetEq
    , Horizontal.Function.rule_Supset
    , Horizontal.Function.rule_SupsetEq
    , Horizontal.Function.rule_Inverse
    , Horizontal.Function.rule_Card
    , Horizontal.Function.rule_Comprehension_PreImage
    , Horizontal.Function.rule_Comprehension_Defined
    , Horizontal.Function.rule_Comprehension_Defined_Literal
    , Horizontal.Function.rule_Comprehension_Range
    , Horizontal.Function.rule_Comprehension_Range_Literal
    , Horizontal.Function.rule_In
    , Horizontal.Function.rule_Restrict_Image
    , Horizontal.Function.rule_Restrict_Comprehension
    , Horizontal.Function.rule_Comprehension_Defined_Size
    , Horizontal.Function.rule_Comprehension_Range_Size
    , Horizontal.Function.rule_Defined_Intersect
    , Horizontal.Function.rule_DefinedOrRange_Union
    , Horizontal.Function.rule_DefinedOrRange_Difference

    , Horizontal.Sequence.rule_Comprehension_Literal
    , Horizontal.Sequence.rule_Image_Bool
    , Horizontal.Sequence.rule_Image_Int
    , Horizontal.Sequence.rule_Comprehension_Image
    , Horizontal.Sequence.rule_Image_Literal_Bool
    , Horizontal.Sequence.rule_Image_Literal_Int
    , Horizontal.Sequence.rule_Eq
    , Horizontal.Sequence.rule_Neq
    , Horizontal.Sequence.rule_DotLeq
    , Horizontal.Sequence.rule_DotLt
    , Horizontal.Sequence.rule_Subset
    , Horizontal.Sequence.rule_SubsetEq
    , Horizontal.Sequence.rule_Supset
    , Horizontal.Sequence.rule_SupsetEq
    , Horizontal.Sequence.rule_Card
    , Horizontal.Sequence.rule_Comprehension_PreImage
    , Horizontal.Sequence.rule_Comprehension_Defined
    , Horizontal.Sequence.rule_Comprehension_Range
    , Horizontal.Sequence.rule_In
    , Horizontal.Sequence.rule_Restrict_Image
    , Horizontal.Sequence.rule_Restrict_Comprehension
    , Horizontal.Sequence.rule_Substring
    , Horizontal.Sequence.rule_Subsequence

    , Horizontal.Relation.rule_Comprehension_Literal
    , Horizontal.Relation.rule_Comprehension_Projection
    , Horizontal.Relation.rule_PowerSet_Comprehension
    , Horizontal.Relation.rule_Image
    , Horizontal.Relation.rule_In
    , Horizontal.Relation.rule_Eq
    , Horizontal.Relation.rule_Neq
    , Horizontal.Relation.rule_DotLeq
    , Horizontal.Relation.rule_DotLt
    , Horizontal.Relation.rule_Subset
    , Horizontal.Relation.rule_SubsetEq
    , Horizontal.Relation.rule_Supset
    , Horizontal.Relation.rule_SupsetEq
    , Horizontal.Relation.rule_Card

    , Horizontal.Partition.rule_Comprehension_Literal
    , Horizontal.Partition.rule_Eq
    , Horizontal.Partition.rule_Neq
    , Horizontal.Partition.rule_DotLeq
    , Horizontal.Partition.rule_DotLt
    , Horizontal.Partition.rule_Together
    , Horizontal.Partition.rule_Apart
    , Horizontal.Partition.rule_Party
    , Horizontal.Partition.rule_Participants
    , Horizontal.Partition.rule_Card
    , Horizontal.Partition.rule_In

    ]

otherRules :: [[Rule]]
otherRules =
    [
        [ rule_TrueIsNoOp
        , rule_FlattenOf1D
        , rule_Decompose_AllDiff

        , rule_GeneratorsFirst

        , rule_DomainCardinality

        , BubbleUp.rule_MergeNested
        -- , BubbleUp.rule_Comprehension
        -- , BubbleUp.rule_LocalInComprehension
        , BubbleUp.rule_ToAnd
        , BubbleUp.rule_NotBoolYet
        -- , BubbleUp.rule_VarDecl
        , BubbleUp.rule_LiftVars

        , DontCare.rule_Bool
        , DontCare.rule_Int
        , DontCare.rule_Tuple
        , DontCare.rule_Record
        , DontCare.rule_Variant
        , DontCare.rule_Matrix
        , DontCare.rule_Abstract

        , rule_ComplexAbsPat

        , rule_AttributeToConstraint

        , rule_QuantifierShift

        , rule_DotLt_IntLike
        , rule_DotLeq_IntLike

        ]

    ,   [ rule_InlineConditions
        ]

    ]

-- | These rules depend on other rules firing first.
delayedRules :: [[Rule]]
delayedRules =
    [
        [ Vertical.Matrix.rule_Comprehension_Singleton
        , Vertical.Matrix.rule_Comprehension_SingletonDomain
        , Vertical.Matrix.rule_MatrixIndexing
        ]
    ]


rule_ChooseRepr :: Config -> Rule
rule_ChooseRepr config = Rule "choose-repr" (const theRule) where

    theRule (Reference nm (Just (DeclNoRepr forg _ inpDom))) | forg `elem` [Find, Given, CutFind] = do
        let domOpts = reprOptions inpDom
        when (null domOpts) $
            bug $ "No representation matches this beast:" <++> pretty inpDom
        let options =
                [ RuleResult { ruleResultDescr = msg
                             , ruleResultType = case forg of
                                    Find    -> ChooseRepr_Find
                                    Given   -> ChooseRepr_Given
                                    CutFind -> ChooseRepr_Cut
                                    _       -> bug "rule_ChooseRepr ruleResultType"
                             , ruleResult = return out
                             , ruleResultHook = hook
                             }
                | dom <- domOpts
                , let msg = "Choosing representation for" <+> pretty nm <> ":" <++> pretty dom
                , let out = Reference nm (Just (DeclHasRepr forg nm dom))
                , let hook = mkHook (channelling config) forg nm dom
                ]
        return options
    theRule _ = na "rule_ChooseRepr"

    mkHook
        :: ( MonadLog m
           , MonadFail m
           , NameGen m
           )
        => Bool
        -> FindOrGiven
        -> Name
        -> Domain HasRepresentation Expression
        -> Model
        -> m Model
    mkHook useChannelling   -- whether to use channelling or not
           forg             -- find or given
           name             -- name of the original declaration
           domain           -- domain with representation selected
           model = do
        let

            representations     = model |> mInfo |> miRepresentations
            representationsTree = model |> mInfo |> miRepresentationsTree
                                        |> concatMap (\ (n, ds) -> map (n,) ds )

            usedBefore = (name, reprTree domain) `elem` representationsTree

            mkStructurals :: (MonadLog m, MonadFail m, NameGen m) => m [Expression]
            mkStructurals = do
                logDebugVerbose "Generating structural constraints."
                let ref = Reference name (Just (DeclHasRepr forg name domain))
                structurals <- getStructurals downX1 domain >>= \ gen -> gen ref
                logDebugVerbose $ "Before name resolution:" <+> vcat (map pretty structurals)
                resolved    <- mapM resolveNamesX structurals     -- re-resolving names
                logDebugVerbose $ "After  name resolution:" <+> vcat (map pretty resolved)
                return resolved

            addStructurals :: (MonadLog m, MonadFail m, NameGen m) => Model -> m Model
            addStructurals
                | forg == Given = return
                | usedBefore = return
                | otherwise = \ m -> do
                    structurals <- mkStructurals
                    return $ if null structurals
                        then m
                        else m { mStatements = mStatements m ++ [SuchThat structurals] }

            channels =
                [ make opEq this that
                | (n, d) <- representations
                , n == name
                , let this = Reference name (Just (DeclHasRepr forg name domain))
                , let that = Reference name (Just (DeclHasRepr forg name d))
                ]

            addChannels
                | forg == Given = return
                | usedBefore = return
                | null channels = return
                | otherwise = \ m -> return
                    m { mStatements = mStatements m ++ [SuchThat channels] }

            recordThis
                | usedBefore = return
                | otherwise = \ m ->
                let
                    oldInfo = mInfo m
                    newInfo = oldInfo
                        { miRepresentations     =  representations     ++ [(name, domain)]
                        , miRepresentationsTree = (representationsTree ++ [(name, reprTree domain)])
                                                |> sortBy (comparing fst)
                                                |> groupBy ((==) `on` fst)
                                                |> map (\ grp -> (fst (head grp), map snd grp) )
                        }
                in  return m { mInfo = newInfo }

            fixReprForOthers
                | useChannelling = return           -- no-op, if channelling=yes
                | otherwise = \ m ->
                let
                    f (Reference nm _) | nm == name = Reference nm (Just (DeclHasRepr forg name domain))
                    f x = x
                in
                    return m { mStatements = transformBi f (mStatements m) }

        logDebugVerbose $ vcat
            [ "Name        :" <+> pretty name
            , "Previously  :" <+> vcat [ pretty (show d) | (n,d) <- representations, n == name ]
            , "This guy    :" <+> pretty (show domain)
            , "usedBefore? :" <+> pretty usedBefore
            ]

        return model
            >>= addStructurals       -- unless usedBefore: add structurals
            >>= addChannels          -- for each in previously recorded representation
            >>= recordThis           -- unless usedBefore: record (name, domain) as being used in the model
            >>= fixReprForOthers     -- fix the representation of this guy in the whole model, if channelling=no


rule_ChooseReprForComprehension :: Rule
rule_ChooseReprForComprehension = Rule "choose-repr-for-comprehension" (const theRule) where

    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (nm, domain), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenDomainNoRepr (Single nm) domain) -> return (nm, domain)
            _ -> na "rule_ChooseReprForComprehension"

        ty <- typeOf domain

        let domOpts = reprOptions domain
        when (null domOpts) $
            bug $ "No representation matches this beast:" <++> pretty domain

        let genOptions =
                [ do
                    outs <- downD (nm, dom)
                    structurals <- mkStructurals nm dom
                    return (dom, outs, structurals)
                | dom <- domOpts
                ]

        return
            [ RuleResult
                { ruleResultDescr = "Choosing representation for quantified variable" <+> pretty nm
                                        <+> "(with type:" <+> pretty ty <> ")"
                , ruleResultType = ChooseRepr_Quantified
                , ruleResult = bugFailT "rule_ChooseReprForComprehension" $ do
                    option <- genOption
                    let (thisDom, outDomains, structurals) = option
                    let updateRepr (Reference nm' _)
                            | nm == nm'
                            = Reference nm (Just (DeclHasRepr Quantified nm thisDom))
                        updateRepr p = p
                    let out' = Comprehension (transform updateRepr body)
                                $  gocBefore
                                ++ [ Generator (GenDomainHasRepr name dom)
                                   | (name, dom) <- outDomains ]
                                ++ map Condition structurals
                                ++ transformBi updateRepr gocAfter
                    out <- resolveNamesX out'
                    return out
                , ruleResultHook = return
                }
            | genOption <- genOptions
            ]
    theRule _ = na "rule_ChooseReprForComprehension"

    mkStructurals name domain = do
        let ref = Reference name (Just (DeclHasRepr Quantified name domain))
        gen  <- getStructurals downX1 domain
        gen ref


rule_ChooseReprForLocals :: Rule
rule_ChooseReprForLocals = Rule "choose-repr-for-locals" (const theRule) where

    theRule (WithLocals body (Left locals)) = do
        (stmtBefore, (nm, domain), stmtAfter) <- matchFirst locals $ \ local -> case local of
            Declaration (FindOrGiven LocalFind nm domain) -> return (nm, domain)
            _ -> na "rule_ChooseReprForLocals"

        let
            isReferencedWithoutRepr (Reference nm' (Just DeclNoRepr{})) | nm == nm' = True
            isReferencedWithoutRepr _ = False

        unless (any isReferencedWithoutRepr (universeBi (body, stmtBefore, stmtAfter))) $
            fail $ "This local variable seems to be handled before:" <+> pretty nm

        let domOpts = reprOptions domain
        when (null domOpts) $
            bug $ "No representation matches this beast:" <++> pretty domain

        let genOptions =
                [ do
                    outs <- downD (nm, dom)
                    structurals <- mkStructurals nm dom
                    return (dom, outs, structurals)
                | dom <- domOpts
                ]

        return
            [ RuleResult
                { ruleResultDescr = "Choosing representation for local variable" <+> pretty nm
                , ruleResultType = ChooseRepr_Auxiliary
                , ruleResult = bugFailT "rule_ChooseReprForLocals" $ do
                    option <- genOption
                    let (thisDom, outDomains, structurals) = option
                    let updateRepr (Reference nm' _)
                            | nm == nm'
                            = Reference nm (Just (DeclHasRepr LocalFind nm thisDom))
                        updateRepr p = p
                    let out' = WithLocals (transform updateRepr body) $ Left
                                (  stmtBefore
                                ++ [ Declaration (FindOrGiven
                                                    LocalFind
                                                    name
                                                    (forgetRepr dom))
                                   | (name, dom) <- outDomains ]
                                ++ [ SuchThat structurals | not (null structurals) ]
                                ++ transformBi updateRepr stmtAfter
                                )
                    out <- resolveNamesX out'
                    return out
                , ruleResultHook = return
                }
            | genOption <- genOptions
            ]
    theRule _ = na "rule_ChooseReprForLocals"

    mkStructurals name domain = do
        let ref = Reference name (Just (DeclHasRepr LocalFind name domain))
        gen  <- getStructurals downX1 domain
        gen ref


rule_GeneratorsFirst :: Rule
rule_GeneratorsFirst = "generators-first" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        let gens       = [ x | x@Generator{} <- gensOrConds ]
        let conditions = [ x | x@Condition{} <- gensOrConds ]
        let gensOrConds' = gens ++ conditions
        when (gensOrConds == gensOrConds') $ na "rule_GeneratorsFirst"
        return
            ( "Generators come first."
            , return $ Comprehension body gensOrConds'
            )
    theRule _ = na "rule_GeneratorsFirst"


rule_TrueIsNoOp :: Rule
rule_TrueIsNoOp = "true-is-noop" `namedRule` theRule where
    theRule (Op (MkOpTrue (OpTrue ref))) =
        case ref of
            Reference _ (Just DeclHasRepr{}) ->
                return ( "Remove the argument from true."
                       , return $ Constant $ ConstantBool True
                       )
            _ -> fail "The argument of true doesn't have a representation."
    theRule _ = na "rule_TrueIsNoOp"


rule_FlattenOf1D :: Rule
rule_FlattenOf1D = "flatten-of-1D" `namedRule` theRule where
    theRule p = do
        x                   <- match opFlatten p
        TypeMatrix _ xInner <- typeOf x
        case xInner of
            TypeBool{} -> return ()
            TypeInt{}  -> return ()
            _ -> na "rule_FlattenOf1D"
        return ( "1D matrices do not need a flatten."
               , return x
               )


rule_Decompose_AllDiff :: Rule
rule_Decompose_AllDiff = "decompose-allDiff" `namedRule` theRule where
    theRule [essence| allDiff(&m) |] = do
        ty <- typeOf m
        case ty of
            TypeMatrix _ TypeBool -> fail "allDiff can stay"
            TypeMatrix _ TypeInt  -> fail "allDiff can stay"
            TypeMatrix _ _ -> return ()
            _ -> fail "allDiff on something other than a matrix."
        DomainMatrix index _ <- domainOf m
        return
            ( "Decomposing allDiff. Type:" <+> pretty ty
            , do
                (iPat, i) <- quantifiedVar
                (jPat, j) <- quantifiedVar
                return
                    [essence|
                        and([ &m[&i] != &m[&j]
                            | &iPat : &index
                            , &jPat : &index
                            , &i < &j
                            ])
                    |]
            )
    theRule _ = na "rule_Decompose_AllDiff"


rule_DomainCardinality :: Rule
rule_DomainCardinality = "domain-cardinality" `namedRule` theRule where
    theRule p = do
        maybeDomain <- match opTwoBars p
        d <- case maybeDomain of
            Domain d -> return d
            Reference _ (Just (Alias (Domain d))) -> return d
            _ -> na "rule_DomainCardinality"
        return
            ( "Cardinality of a domain"
            , do
                (iPat, _) <- quantifiedVar
                return [essence| sum([ 1 | &iPat : &d ]) |]
            )


rule_ComplexAbsPat :: Rule
rule_ComplexAbsPat = "complex-pattern" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, domainOrExpr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenDomainNoRepr pat@AbsPatTuple{} domain) -> return (pat, Left domain)
            Generator (GenInExpr       pat@AbsPatTuple{} expr)   -> return (pat, Right expr)
            _ -> na "rule_ComplexAbsPat"
        return
            ( "complex pattern on tuple patterns"
            , do
                (iPat, i) <- quantifiedVar
                let replacements = [ (p, make opMatrixIndexing i (map (fromInt . fromIntegral) is))
                                   | (p, is) <- genMappings pat
                                   ]
                let f x@(Reference nm _) = fromMaybe x (lookup nm replacements)
                    f x = x
                return $ Comprehension (transform f body)
                            $  gocBefore
                            ++ [ either (Generator . GenDomainNoRepr iPat)
                                        (Generator . GenInExpr       iPat)
                                        domainOrExpr ]
                            ++ transformBi f gocAfter
            )
    theRule _ = na "rule_ComplexAbsPat"

    -- i         --> i -> []
    -- (i,j)     --> i -> [1]
    --               j -> [2]
    -- (i,(j,k)) --> i -> [1]
    --               j -> [2,1]
    --               k -> [2,2]
    genMappings :: AbstractPattern -> [(Name, [Int])]
    genMappings (Single nm) = [(nm, [])]
    genMappings (AbsPatTuple pats)
        = concat
            [ [ (patCore, i:is) | (patCore, is) <- genMappings pat ]
            | (i, pat) <- zip [1..] pats
            ]
    genMappings (AbsPatMatrix pats)
        = concat
            [ [ (patCore, i:is) | (patCore, is) <- genMappings pat ]
            | (i, pat) <- zip [1..] pats
            ]
    genMappings pat = bug ("rule_ComplexLambda.genMappings:" <+> pretty (show pat))


-- this rule doesn't `namedRule` because it need access to ascendants through the zipper
rule_InlineConditions :: Rule
rule_InlineConditions = Rule "inline-conditions" theRule where
    theRule z (Comprehension body gensOrConds) = do
        let (toInline, toKeep) = mconcat
                [ case goc of
                    Condition x | categoryOf x == CatDecision -> ([x],[])
                    _ -> ([],[goc])
                | goc <- gensOrConds
                ]
        theGuard <- case toInline of
            []  -> fail "No condition to inline."
            xs  -> return $ make opAnd $ fromList xs
        (nameQ, opSkip) <- queryQ z
        return
            [ RuleResult
                { ruleResultDescr = "Inlining conditions, inside" <+> nameQ
                , ruleResultType  = ExpressionRefinement
                , ruleResult      = return $ Comprehension (opSkip theGuard body) toKeep
                , ruleResultHook  = return
                } ]
    theRule _ _ = na "rule_InlineConditions"

    -- keep going up, until finding a quantifier
    -- when found, return the skipping operator for the quantifier
    -- if none exists, do not apply the rule.
    -- (or maybe we should call bug right ahead, it can't be anything else.)
    queryQ z = do
        let h = hole z
        case ( match opAnd h
             , match opOr h
             , match opSum h
             , match opMax h
             , match opMin h ) of
            (Just _, _, _, _, _) -> return ("and", opAndSkip )
            (_, Just _, _, _, _) -> return ("or" , opOrSkip  )
            (_, _, Just _, _, _) -> return ("sum", opSumSkip )
            (_, _, _, Just _, _) -> return ("max", opMaxSkip )
            (_, _, _, _, Just _) -> return ("min", opMinSkip )
            _ -> case Zipper.up z of
                Nothing -> fail "queryQ"
                Just u  -> queryQ u

    opAndSkip b x = [essence| &b -> &x |]
    opOrSkip  b x = [essence| &b /\ &x |]
    opSumSkip b x = [essence| toInt(&b) * &x |]
    opMaxSkip b x = [essence| toInt(&b) * &x |]                          -- MININT is 0
    opMinSkip b x = [essence| toInt(&b) * &x + toInt(!&b) * 9999 |]      -- MAXINT is 9999


rule_DotLt_IntLike :: Rule
rule_DotLt_IntLike = "intLike-DotLt" `namedRule` theRule where
    theRule p = do
        (a,b) <- match opDotLt p
        tya   <- typeOf a
        tyb   <- typeOf b
        case mostDefined [tya, tyb] of
            TypeBool{} -> return ()
            TypeInt{}  -> return ()
            _ -> na "rule_DotLt_IntLike"
        return
            ( "Horizontal rule for int-like .<" <+> pretty (make opLt a b)
            , return $ make opLt a b
            )


rule_DotLeq_IntLike :: Rule
rule_DotLeq_IntLike = "intLike-DotLeq" `namedRule` theRule where
    theRule p = do
        (a,b) <- match opDotLeq p
        tya   <- typeOf a
        tyb   <- typeOf b
        case mostDefined [tya, tyb] of
            TypeBool{} -> return ()
            TypeInt{}  -> return ()
            _ -> na "rule_DotLeq_IntLike"
        return
            ( "Horizontal rule for int-like .<" <+> pretty (make opLeq a b)
            , return $ make opLeq a b
            )


rule_AttributeToConstraint :: Rule
rule_AttributeToConstraint = "attribute-to-constraint" `namedRule` theRule where
    theRule (Op (MkOpAttributeAsConstraint (OpAttributeAsConstraint thing attr mval))) = do
        dom <- domainOf thing
        let conv = mkAttributeToConstraint dom attr mval thing
        return
            ( "Converting an attribute to a constraint"
            , bugFailT "rule_AttributeToConstraint" conv
            )
    theRule _ = na "rule_AttributeToConstraint"


rule_FullEvaluate :: Rule
rule_FullEvaluate = "full-evaluate" `namedRule` theRule where
    theRule Constant{} = na "rule_FullEvaluate"
    theRule Domain{} = na "rule_FullEvaluate"
    theRule p = do
        constant <- instantiateExpression [] p
        if null [() | ConstantUndefined{} <- universe constant]
            then return ()
            else na "rule_PartialEvaluate, undefined"
        return
            ( "Full evaluator"
            , return $ Constant constant
            )


rule_PartialEvaluate :: Rule
rule_PartialEvaluate = "partial-evaluate" `namedRule` theRule where
    theRule (Op x) = do
        x' <- simplifyOp x
        when (Op x == x') $ bug $ vcat
            [ "rule_PartialEvaluate, simplifier returns the input unchanged."
            , "input:" <+> vcat [ pretty (Op x)
                                , pretty (show (Op x))
                                ]
            ]
        return
            ( "Partial evaluator"
            , return x'
            )
    theRule _ = na "rule_PartialEvaluate"


-- | shifting quantifiers inwards, if they operate on a row of a 2d matrix,
--   make them operate on the rows directly then index
rule_QuantifierShift :: Rule
rule_QuantifierShift = "quantifier-shift" `namedRule` theRule where
    theRule p = do
        (mkQuan, inner  )               <- match opQuantifier p
        (matrix, indexer)               <- match opMatrixIndexing inner
        (TypeMatrix _ ty, index, elems) <- match matrixLiteral matrix
        case ty of
            TypeMatrix{} -> return ()
            TypeList{} -> return ()
            _ -> na "rule_QuantifierShift"
        return
            ( "Shifting quantifier inwards"
            , return $ make opMatrixIndexing
                        (make matrixLiteral
                            TypeAny
                            index
                            (map mkQuan elems))
                        indexer
            )
