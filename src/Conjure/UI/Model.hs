{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}

module Conjure.UI.Model
    ( outputModel, outputModels
    , pickFirst
    , interactive, interactiveFixedQs, interactiveFixedQsAutoA
    , allFixedQs
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Ops hiding ( opOr, opAnd, opIn, opEq, opLt, opMapOverDomain, opMapInExpr
                                   , opSubsetEq, opDontCare, opFilter, opImply, opTimes, opToInt
                                   , opLeq )
import Conjure.Language.Lenses
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.DomainOf
import Conjure.Representations

import Data.Generics.Uniplate.Zipper as Zipper ( Zipper, zipperBi, fromZipper, hole, replaceHole, up )

import System.IO.Unsafe ( unsafeInterleaveIO )



data Question = Question
    { qHole       :: Expression
    , qAscendants :: [Expression]
    , qAnswers    :: [Answer]
    }

data Answer = Answer
    { aText      :: Doc
    , aAnswer    :: Expression
    , aFullModel :: Model
    }

type Driver      = (forall m . (MonadIO m, MonadFail m) => [Question] -> m Model)

type MultiDriver = (forall m . (MonadIO m, MonadFail m) => [Question] -> m [Model])

type RuleResult = ( Doc                     -- describe this transformation
                  , [Name] -> Expression    -- the result
                  , Model -> Model          -- post-application hook
                  )

data Rule = Rule
    { rName  :: Doc
    , rApply :: forall m . (Functor m, MonadIO m) => Expression -> m [RuleResult]
    }

namedRule
    :: Doc
    -> (forall m . (Functor m, MonadIO m) => Expression -> m (Maybe (Doc, [Name] -> Expression)))
    -> Rule
namedRule nm f = Rule
    { rName = nm
    , rApply = \ x -> let addId (d,y) = (d,y,id)
                      in  liftM (map addId . maybeToList) (f x)
    } 


remaining :: (Functor m, Applicative m, MonadIO m) => Model -> m [Question]
remaining model = do
    let freshNames' = freshNames model
    let modelZipper = fromJustNote "Creating the initial zipper." (zipperBi model)
    questions <- fmap catMaybes $ forM (allContexts modelZipper) $ \ x -> do
        ys <- applicableRules (hole x)
        return $ if null ys
            then Nothing
            else Just (x, ys)
    return
        [ Question
            { qHole = hole focus
            , qAscendants = tail (ascendants focus)
            , qAnswers =
                [ Answer
                    { aText = ruleName <> ":" <+> ruleText
                    , aAnswer = ruleResultExpr
                    , aFullModel = hook (fromZipper (replaceHole ruleResultExpr focus))
                                    |> addToTrail nQuestion [1 .. length questions]
                                                  (("Focus:" <+> pretty (hole focus))
                                                   : [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
                                                     | i <- allNats
                                                     | c <- tail (ascendants focus)
                                                     ])
                                                  nAnswer   [1 .. length answers]
                                                  [ruleName <> ":" <+> ruleText]
                    }
                | (nAnswer, (ruleName, (ruleText, ruleResult, hook))) <- zip allNats answers
                , let ruleResultExpr = ruleResult freshNames'
                ]
            }
        | (nQuestion, (focus, answers)) <- zip allNats questions
        ]


addToTrail
    :: Int -> [Int] -> [Doc]
    -> Int -> [Int] -> [Doc]
    -> Model -> Model
addToTrail nQuestion nQuestions tQuestion
           nAnswer nAnswers tAnswer
           model = model { mInfo = newInfo }
    where
        oldInfo = mInfo model
        newInfo = oldInfo { miTrail = miTrail oldInfo ++ [theQ, theA]
                          , miTrailCompact = miTrailCompact oldInfo ++ [(nQuestion, nQuestions), (nAnswer, nAnswers)]
                          }
        theQ = Decision
            { dDescription = map (stringToText . renderWide)
                $ ("Question #" <> pretty nQuestion <+> "out of" <+> pretty (length nQuestions))
                : tQuestion
            , dOptions = nQuestions
            , dDecision = nQuestion
            }
        theA = Decision
            { dDescription = map (stringToText . renderWide)
                $ ("Answer #" <> pretty nAnswer <+> "out of" <+> pretty (length nAnswers))
                : tAnswer
            , dOptions = nAnswers
            , dDecision = nAnswer
            }


toCompletion :: (MonadIO m, MonadFail m) => Driver -> Model -> m Model
toCompletion driver = loopy . prologue
    where
        loopy model = do
            qs <- remaining model
            if null qs
                then failCheaply (epilogue model)
                else do
                    nextModel <- driver qs
                    loopy nextModel


toCompletionMulti :: MultiDriver -> Model -> IO [Model]
toCompletionMulti driver = loopy . prologue
    where
        loopy model = do
            qs <- remaining model
            if null qs
                then return <$> failCheaply (epilogue model)
                else do
                    nextModels <- driver qs
                    fmap concat $ sequence [ unsafeInterleaveIO $ loopy m
                                           | m <- nextModels
                                           ]


outputModel :: (MonadIO m, MonadFail m) => Driver -> FilePath -> Int -> Model -> m ()
outputModel driver dir i essence = do
    liftIO $ createDirectoryIfMissing True dir
    eprime <- toCompletion driver essence
    let filename = dir </> "model" ++ paddedNum i ++ ".eprime"
    liftIO $ writeFile filename (renderWide eprime)


outputModels :: MultiDriver -> FilePath -> Int -> Model -> IO ()
outputModels driver dir i essence = do
    createDirectoryIfMissing True dir
    eprimes <- toCompletionMulti driver essence
    sequence_
        [ writeFile filename (renderWide eprime)
        | (j, eprime) <- zip [i..] eprimes
        , let filename = dir </> "model" ++ paddedNum j ++ ".eprime"
        ]


allFixedQs :: MultiDriver
allFixedQs [] = fail "pickFirst: No questions!"
allFixedQs (question:_) = return (map aFullModel (qAnswers question))


pickFirst :: Driver
pickFirst [] = fail "pickFirst: No questions!"
pickFirst (question:_) =
    case qAnswers question of
        [] -> fail "pickFirst: No answers!"
        (answer:_) -> return (aFullModel answer)


interactive :: Driver
interactive questions = liftIO $ do
    putStrLn ""
    putStrLn " ----------------------------------------"

    forM_ (zip allNats questions) $ \ (nQ,q) -> do
        print ("Question" <+> pretty nQ <> ":" <+> pretty (qHole q))
        unless (null (qAscendants q)) $
            print $ nest 4 $ vcat
                [ "Context #" <> pretty i <> ":" <+> pretty c
                | i <- allNats
                | c <- qAscendants q
                ]
        
    putStr "Pick question: "
    pickedQIndex <- readNote "Expecting an integer." <$> getLine
    let pickedQ = questions `at` (pickedQIndex - 1)

    forM_ (zip allNats (qAnswers pickedQ)) $ \ (nA,a) ->
        print $ nest 4 $ "Answer" <+> pretty nA <> ":" <+> vcat [ pretty (aText a)
                                                                , pretty (aAnswer a) ]
        -- print (nest 8 $ pretty (aFullModel a))
    putStr "Pick answer: "
    pickedAIndex <- readNote "Expecting an integer." <$> getLine
    let pickedA = qAnswers pickedQ `at` (pickedAIndex - 1)

    let pickedModel = aFullModel pickedA
    putStrLn "Current model:"
    print $ nest 8 $ pretty pickedModel
    return pickedModel


interactiveFixedQs :: Driver
interactiveFixedQs [] = fail "interactiveFixedQs: No questions!"
interactiveFixedQs (pickedQ:_) = liftIO $ do
    putStrLn ""
    putStrLn " ----------------------------------------"

    print ("Question:" <+> pretty (qHole pickedQ))
    unless (null (qAscendants pickedQ)) $
        print $ nest 4 $ vcat
            [ "Context #" <> pretty i <> ":" <+> pretty c
            | i <- allNats
            | c <- qAscendants pickedQ
            ]

    forM_ (zip allNats (qAnswers pickedQ)) $ \ (nA,a) ->
        print $ nest 4 $ "Answer" <+> pretty nA <> ":" <+> vcat [ pretty (aText a)
                                                                , pretty (aAnswer a) ]
        -- print (nest 8 $ pretty (aFullModel a))
    putStr "Pick answer: "
    pickedAIndex <- readNote "Expecting an integer." <$> getLine
    let pickedA = qAnswers pickedQ `at` (pickedAIndex - 1)

    let pickedModel = aFullModel pickedA
    putStrLn "Current model:"
    print $ nest 8 $ pretty pickedModel
    return pickedModel


interactiveFixedQsAutoA :: Driver
interactiveFixedQsAutoA [] = fail "interactiveFixedQs: No questions!"
interactiveFixedQsAutoA (pickedQ:_) = liftIO $ do
    putStrLn ""
    putStrLn " ----------------------------------------"

    print ("Question:" <+> pretty (qHole pickedQ))
    unless (null (qAscendants pickedQ)) $
        print $ nest 4 $ vcat
            [ "Context #" <> pretty i <> ":" <+> pretty c
            | i <- allNats
            | c <- qAscendants pickedQ
            ]

    forM_ (zip allNats (qAnswers pickedQ)) $ \ (nA,a) ->
        print $ nest 4 $ "Answer" <+> pretty nA <> ":" <+> vcat [ pretty (aText a)
                                                                , pretty (aAnswer a) ]
    -- print (nest 8 $ pretty (aFullModel a))

    pickedA <- case qAnswers pickedQ of
        [a] -> do
            putStrLn "Automatically picking the only answer."
            return a
        _   -> do
            putStr "Pick answer: "
            pickedAIndex <- readNote "Expecting an integer." <$> getLine
            let pickedA = qAnswers pickedQ `at` (pickedAIndex - 1)
            return pickedA

    let pickedModel = aFullModel pickedA
    -- putStrLn "Current model:"
    -- print $ nest 8 $ pretty pickedModel
    return pickedModel


ascendants :: Zipper a b -> [b]
ascendants z = hole z : maybe [] ascendants (Zipper.up z)



-- | For every parameter and decision variable add a true-constraint.
--   A true-constraint has no effect, other than forcing Conjure to produce a representation.
--   It can be used to make sure that the decision variable doesn't get lost when it isn't mentioned anywhere.
--   It can also be used to produce "extra" representations.
--   Currently this function will add a true for every declaration, no matter if it is mentioned or not.
addTrueConstraints :: Model -> Model
addTrueConstraints m =
    let
        mkTrueConstraint k nm dom = Op $ MkOpTrue $ OpTrue (Reference nm (Just (DeclNoRepr k nm dom)))
        trueConstraints = [ mkTrueConstraint forg nm d
                          | Declaration (FindOrGiven forg nm d) <- mStatements m
                          ]
    in
        m { mStatements = mStatements m ++ [SuchThat trueConstraints] }


oneSuchThat :: Model -> Model
oneSuchThat m = m { mStatements = others ++ [SuchThat suchThat] }
    where
        suchThat = if null suchThats
                    then [Constant (ConstantBool True)]
                    else suchThats

        (others, suchThats) = mStatements m
              |> map collect                                            -- separate such thats from the rest
              |> mconcat
              |> second (map breakConjunctions)                         -- break top level /\'s
              |> second mconcat
              |> second (filter (/= Constant (ConstantBool True)))      -- remove top level true's
              |> second nub                                             -- uniq

        collect (SuchThat s) = ([], s)
        collect s = ([s], [])

        breakConjunctions   (Op (MkOpAnd (OpAnd [ ]))) = bug "empty /\\"
        breakConjunctions x@(Op (MkOpAnd (OpAnd [_]))) = [x]
        breakConjunctions (Op (MkOpAnd (OpAnd xs))) = concatMap breakConjunctions xs
        breakConjunctions x = [x]


updateDeclarations :: Model -> Model
updateDeclarations model =
    let
        representations = model |> mInfo |> miRepresentations

        statements = concatMap onEachStatement (mStatements model)

        onEachStatement inStatement =
            case inStatement of
                Declaration (FindOrGiven forg nm _) ->
                    case [ d | (n, d) <- representations, n == nm ] of
                        [] -> bug $ "No representation chosen for: " <+> pretty nm
                        domains -> concatMap (onEachDomain forg nm) domains
                _ -> [inStatement]

        onEachDomain forg nm domain =
            case downD (nm, domain) of
                Left err -> bug err
                Right outs -> [Declaration (FindOrGiven forg n (forgetRepr d)) | (n, d) <- outs]

    in
        model { mStatements = statements }


-- | checking whether any `Reference`s with `DeclHasRepr`s are left in the model
checkIfAllRefined :: MonadFail m => Model -> m ()
checkIfAllRefined m = do
    let modelZipper = fromJustNote "checkIfAllRefined: Creating zipper." (zipperBi m)
    fails <- fmap concat $ forM (allContexts modelZipper) $ \ x ->
                case hole x of
                    Reference _ (Just (DeclHasRepr _ _ dom)) | not (isPrimitiveDomain dom) ->
                        return $ ("Not refined:" <+> pretty (hole x))
                               : [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
                                 | i <- allNats
                                 | c <- tail (ascendants x)
                                 ]
                    _ -> return []
    unless (null fails) (fail (vcat fails))


prologue :: Model -> Model
prologue essence = essence
    |> addTrueConstraints
    |> initInfo


epilogue :: MonadFail m => Model -> m Model
epilogue eprime = do
    checkIfAllRefined eprime
    eprime
        |> updateDeclarations
        |> oneSuchThat
        |> languageEprime
        |> return


representationOf :: MonadFail m => Expression -> m Name
representationOf (Reference _ Nothing) = fail "doesn't seem to have a representation"
representationOf (Reference _ (Just refTo)) =
    case refTo of
        DeclHasRepr _ _ d ->
            case reprAtTopLevel d of
                Nothing -> fail "doesn't seem to have a representation"
                Just NoRepresentation -> fail "doesn't seem to have a representation"
                Just (HasRepresentation r) -> return r
        _ -> fail "not a DeclHasRepr"
representationOf _ = fail "not a reference"


applicableRules :: (Applicative m, MonadIO m) => Expression -> m [(Doc, RuleResult)]
applicableRules x = concat <$> sequence [ do res <- rApply r x
                                             return (map (rName r,) res)
                                        | r <- allRules ]


allRules :: [Rule]
allRules =
    [ rule_ChooseRepr

    , rule_TrueIsNoOp
    , rule_ToIntIsNoOp

    , rule_DontCareBool
    , rule_DontCareInt

    , rule_InlineFilterInsideMap

    , rule_TupleIndex

    , rule_SetEq
    , rule_SetSubsetEq

    , rule_Set_MapInExpr_Explicit
    , rule_Set_MapInExpr_Occurrence
    , rule_Set_MapInExpr_ExplicitVarSizeWithMarker
    , rule_Set_MapInExpr_ExplicitVarSizeWithFlags

    , rule_SetIn_Explicit
    , rule_SetIn_Occurrence
    , rule_SetIn_ExplicitVarSizeWithMarker
    , rule_SetIn_ExplicitVarSizeWithFlags
    ]


rule_ChooseRepr :: Rule
rule_ChooseRepr = Rule "choose-repr" theRule where

    theRule (Reference nm (Just (DeclNoRepr ty _ inpDom))) = do
        let domOpts = reprOptions inpDom
        when (null domOpts) $
            bug $ "No representation matches this beast:" <++> pretty inpDom
        return [ (msg, const out, hook)
               | dom <- domOpts
               , let msg = "Selecting representation for" <+> pretty nm <> ":" <+> pretty dom
               , let out = Reference nm (Just (DeclHasRepr ty nm dom))
               , let hook = mkHook ty nm dom
               ]
    theRule _ = return []

    mkHook ty name domain model =
        let

            freshNames' = freshNames model

            representations = model |> mInfo |> miRepresentations

            usedBefore = (name, domain) `elem` representations

            structurals =
                case getStructurals (name, domain) of
                    Left err -> bug err
                    Right Nothing -> []
                    Right (Just xs) -> xs freshNames'

            addStructurals
                | usedBefore = id
                | null structurals = id
                | otherwise = \ m ->
                    m { mStatements = mStatements m ++ [SuchThat structurals] } 

            otherRepresentations =
                [ d
                | (n, d) <- representations
                , n == name
                , d /= domain
                ]

            channels =
                [ make opEq this that
                | d <- otherRepresentations
                , let this = Reference name (Just (DeclHasRepr ty name domain))
                , let that = Reference name (Just (DeclHasRepr ty name d))
                ]

            addChannels
                | null channels = id
                | otherwise = \ m ->
                    m { mStatements = mStatements m ++ [SuchThat channels] }

            recordThis
                | usedBefore = id
                | otherwise = \ m ->
                let
                    oldInfo = mInfo m
                    newInfo = oldInfo { miRepresentations = miRepresentations oldInfo ++ [(name, domain)] }
                in  m { mInfo = newInfo }

        in
            model
                |> addStructurals               -- unless usedBefore: add structurals
                |> addChannels                  -- for each in otherRepresentations
                |> recordThis                   -- unless usedBefore: record (name, domain) as being used in the model



rule_TrueIsNoOp :: Rule
rule_TrueIsNoOp = "true-is-noop" `namedRule` (return . theRule)
    where
        theRule (Op (MkOpTrue (OpTrue ref))) =
            case ref of
                Reference _ (Just DeclHasRepr{}) ->
                    Just ( "Remove the argument from true."
                         , const $ Constant $ ConstantBool True
                         )
                _ -> Nothing
        theRule _ = Nothing


rule_ToIntIsNoOp :: Rule
rule_ToIntIsNoOp = "toInt-is-noop" `namedRule` (return . theRule)
    where
        theRule (Op (MkOpToInt (OpToInt b))) = Just ( "Remove the toInt wrapper, it is implicit in SR."
                                                    , const b
                                                    )
        theRule _ = Nothing


rule_DontCareBool :: Rule
rule_DontCareBool = "dontCare-bool" `namedRule` theRule where
    theRule p = runMaybeT $ do
        x          <- match opDontCare p
        DomainBool <- domainOf x
        return ( "dontCare value for bools is false."
               , const $ make opEq x (fromBool False)
               )


rule_DontCareInt :: Rule
rule_DontCareInt = "dontCare-int" `namedRule` theRule where
    theRule p = runMaybeT $ do
        x                          <- match opDontCare p
        xDomain@(DomainInt ranges) <- domainOf x
        let raiseBug = bug ("dontCare on domain:" <+> pretty xDomain)
        let val = case ranges of
                [] -> raiseBug
                (r:_) -> case r of
                    RangeOpen -> raiseBug
                    RangeSingle v -> v
                    RangeLowerBounded v -> v
                    RangeUpperBounded v -> v
                    RangeBounded v _ -> v
        return ( "dontCare value for this integer is" <+> pretty val
               , const $ make opEq x val
               )


rule_InlineFilterInsideMap :: Rule
rule_InlineFilterInsideMap = "inline-filter-inside-map" `namedRule` theRule
    where
        theRule p = runMaybeT $ msum
            [ ruleGen opAnd (\ x y -> make opImply x y                ) p
            , ruleGen opOr  (\ x y -> make opAnd  [x,y]               ) p
            , ruleGen opSum (\ b x -> make opTimes (make opToInt b) x ) p
            ]

        ruleGen opQ opSkip p = do
            [x]                    <- match opQ p
            (Lambda f1 f2, rest  ) <- match opMapOverDomain x
            (Lambda g1 g2, domain) <- match opFilter rest
            ty                     <- typeOf domain

            let f = lambdaToFunction f1 f2
            let g = lambdaToFunction g1 g2

            return ( "Inlining the filter."
                   , \ fresh -> make opAnd
                           [ make opMapOverDomain
                                   (mkLambda (headInf fresh) ty $ \ i -> opSkip (g i) (f i))
                                   domain ]
                   )


rule_TupleIndex :: Rule
rule_TupleIndex = "tuple-index" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (t,i)       <- match opIndexing p
        TypeTuple{} <- typeOf t
        iInt        <- match constantInt i
        ts          <- downX1 t
        return ( "Tuple indexing on:" <+> pretty p
               , const $ atNote "Tuple indexing" ts (iInt-1)
               )


rule_SetIn_Explicit :: Rule
rule_SetIn_Explicit = "set-in{Explicit}" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (x,s)                <- match opIn p
        TypeSet sInnerTy     <- typeOf s
        "Explicit"           <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        -- exists i : index . m[i] = x
        -- or([ m[i] = x | i : index ])
        -- or(map_domain(i --> m[i]))
        let body iName = mkLambda iName sInnerTy $ \ i ->
                        make opEq (make opIndexing m i) x
        return ( "Vertical rule for set-in, Explicit representation."
               , \ fresh -> make opOr [make opMapOverDomain (body (headInf fresh)) (Domain index)]
               )


rule_Set_MapInExpr_Explicit :: Rule
rule_Set_MapInExpr_Explicit = "set-quantification{Explicit}" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (Lambda lPat@Single{} lBody, s) <- match opMapInExpr p
        TypeSet sInnerTy     <- typeOf s
        "Explicit"           <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let f = lambdaToFunction lPat lBody
        let body iName = mkLambda iName sInnerTy $ \ i ->
                            f (make opIndexing m i)
        -- map_in_expr(f(i), x)
        -- map_domain(f(m[i]), domain)
        return ( "Vertical rule for set-quantification, Explicit representation"
               , \ fresh -> make opMapOverDomain
                               (body (headInf fresh))
                               (Domain index)
               )
        

rule_SetEq :: Rule
rule_SetEq = "set-eq" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (x,y)                <- match opEq p
        TypeSet{}            <- typeOf x
        TypeSet{}            <- typeOf y
        return ( "Horizontal rule for set equality"
               , const $ make opAnd [ make opSubsetEq x y
                                    , make opSubsetEq y x
                                    ]
               )


rule_SetSubsetEq :: Rule
rule_SetSubsetEq = "set-subsetEq" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (x,y)                <- match opSubsetEq p
        TypeSet tyXInner     <- typeOf x
        TypeSet{}            <- typeOf y
        let body iName = mkLambda iName tyXInner (\ i -> make opIn i y)
        return ( "Horizontal rule for set subsetEq"
               , \ fresh -> make opAnd [make opMapInExpr (body (headInf fresh)) x]
               )


rule_SetIn_Occurrence :: Rule
rule_SetIn_Occurrence = "set-in{Occurrence}" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (x,s)                <- match opIn p
        TypeSet{}            <- typeOf s
        "Occurrence"         <- representationOf s
        [m]                  <- downX1 s
        return ( "Vertical rule for set-in, Occurrence representation"
               , const $ make opIndexing m x
               )


rule_Set_MapInExpr_Occurrence :: Rule
rule_Set_MapInExpr_Occurrence = "set-quantification{Occurrence}" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (lambda, s)          <- match opMapInExpr p
        "Occurrence"         <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let filterBody iName = mkLambda iName TypeInt $ \ i -> make opIndexing m i
        -- map_in_expr(f(i), x)
        -- map_domain(f(i), filter(m[i], domain))
        return ( "Vertical rule for set-quantification, Explicit representation"
               , \ fresh -> make opMapOverDomain
                               lambda
                               (make opFilter
                                    (filterBody (headInf fresh))
                                    (Domain index))
               )


rule_SetIn_ExplicitVarSizeWithMarker :: Rule
rule_SetIn_ExplicitVarSizeWithMarker = "set-in{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (x,s)                       <- match opIn p
        TypeSet sInnerTy            <- typeOf s
        "ExplicitVarSizeWithMarker" <- representationOf s
        [marker,values]             <- downX1 s
        DomainMatrix index _        <- domainOf values
        -- exists i : index , i <= marker. m[i] = x
        -- exists i : index . i <= marker /\ m[i] = x
        -- or([ i < marker /\ m[i] = x | i : index ])
        -- or(map_domain(i --> i < marker /\ m[i] = x))
        let body iName = mkLambda iName sInnerTy $ \ i ->
                    make opAnd [ make opEq (make opIndexing values i) x
                               , make opLeq i marker
                               ]
        return ( "Vertical rule for set-in, ExplicitVarSizeWithMarker representation"
               , \ fresh -> make opOr [make opMapOverDomain (body (headInf fresh)) (Domain index)]
               )

rule_Set_MapInExpr_ExplicitVarSizeWithMarker :: Rule
rule_Set_MapInExpr_ExplicitVarSizeWithMarker = "set-quantification{ExplicitVarSizeWithMarker}"
                                               `namedRule` theRule where
    theRule p = runMaybeT $ do
        (Lambda lPat lBody, s)      <- match opMapInExpr p
        TypeSet sInnerTy            <- typeOf s
        "ExplicitVarSizeWithMarker" <- representationOf s
        [marker, values]            <- downX1 s
        DomainMatrix index _        <- domainOf values
        let f = lambdaToFunction lPat lBody
        let mapBody    iName = mkLambda iName sInnerTy $ \ i -> f (make opIndexing values i)
        let filterBody iName = mkLambda iName sInnerTy $ \ i -> make opLeq i marker
        -- map_in_expr(f(i), x)
        -- map_domain(f(values[i]), filter(i <= marker, domain))
        return ( "Vertical rule for set-quantification, ExplicitVarSizeWithMarker representation"
               , \ fresh -> make opMapOverDomain
                               (mapBody (headInf fresh))
                               (make opFilter
                                    (filterBody (headInf fresh))
                                    (Domain index))
               )


rule_SetIn_ExplicitVarSizeWithFlags :: Rule
rule_SetIn_ExplicitVarSizeWithFlags = "set-in{ExplicitVarSizeWithFlags}" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (x,s)                       <- match opIn p
        TypeSet sInnerTy            <- typeOf s
        "ExplicitVarSizeWithFlags"  <- representationOf s
        [flags,values]              <- downX1 s
        DomainMatrix index _        <- domainOf values
        -- exists i : index , i < marker. m[i] = x
        -- exists i : index . i < marker /\ m[i] = x
        -- or([ i < marker /\ m[i] = x | i : index ])
        -- or(map_domain(i --> flags[i] /\ m[i] = x))
        let body iName = mkLambda iName sInnerTy $ \ i ->
                    make opAnd [ make opEq (make opIndexing values i) x
                               , make opIndexing flags i
                               ]
        return ( "Vertical rule for set-in, Occurrence representation"
               , \ fresh -> make opOr [make opMapOverDomain (body (headInf fresh)) (Domain index)]
               )


rule_Set_MapInExpr_ExplicitVarSizeWithFlags :: Rule
rule_Set_MapInExpr_ExplicitVarSizeWithFlags = "set-quantification{ExplicitVarSizeWithFlags}"
                                               `namedRule` theRule where
    theRule p = runMaybeT $ do
        (Lambda lPat lBody, s)      <- match opMapInExpr p
        TypeSet sInnerTy            <- typeOf s
        "ExplicitVarSizeWithFlags"  <- representationOf s
        [flags, values]             <- downX1 s
        DomainMatrix index _        <- domainOf values
        let f = lambdaToFunction lPat lBody
        let mapBody    iName = mkLambda iName sInnerTy $ \ i -> f (make opIndexing values i)
        let filterBody iName = mkLambda iName sInnerTy $ \ i ->    make opIndexing flags  i
        -- map_in_expr(f(i), x)
        -- map_domain(f(values[i]), filter(flags[i], domain))
        return ( "Vertical rule for set-quantification, ExplicitVarSizeWithFlags representation"
               , \ fresh -> make opMapOverDomain
                               (mapBody (headInf fresh))
                               (make opFilter
                                    (filterBody (headInf fresh))
                                    (Domain index))
               )

