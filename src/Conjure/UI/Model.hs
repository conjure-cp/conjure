{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Conjure.UI.Model
    ( outputModels
    , pickFirst
    , interactive, interactiveFixedQs, interactiveFixedQsAutoA
    , allFixedQs
    , Strategy(..), Config(..), parseStrategy
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.DomainOf
import Conjure.Language.Lenses
import Conjure.Language.TH ( essence )
import Conjure.Language.Ops hiding ( opOr, opAnd, opIn, opEq, opLt, opMapOverDomain, opMapInExpr
                                   , opSubsetEq, opDontCare, opFilter, opImply, opTimes, opToInt
                                   , opLeq, opFlatten, opToSet, opIntersect, opUnion, opFunctionImage )

import Conjure.Language.ModelStats ( modelInfo )
import Conjure.Process.Enums ( deenumifyModel )
import Conjure.Language.NameResolution ( resolveNames )

import Conjure.Representations ( downX1, downToX1, downD, reprOptions, getStructurals )

-- uniplate
import Data.Generics.Uniplate.Zipper ( zipperBi, fromZipper, hole, replaceHole )


import qualified Pipes as Pipes ( Producer, yield )
import qualified Pipes.Prelude as Pipes ( foldM )


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

type Driver = (forall m . (MonadIO m, MonadFail m, MonadLog m) => [Question] -> m [Model])

data Strategy
    = PickFirst
    | PickAll
    | Interactive
    -- | AtRandom
    | Auto Strategy
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Default Strategy where def = Auto Interactive

viewAuto :: Strategy -> (Strategy, Bool)
viewAuto (Auto s) = second (const True) (viewAuto s)
viewAuto s = (s, False)

parseStrategy :: String -> Maybe Strategy
parseStrategy "f" = return PickFirst
parseStrategy "x" = return PickAll
parseStrategy "i" = return Interactive
parseStrategy ['a',s] = Auto <$> parseStrategy (return s)
parseStrategy _ = Nothing

data Config = Config
    { logLevel                  :: LogLevel
    , verboseTrail              :: Bool
    , logRuleFails              :: Bool
    , logRuleSuccesses          :: Bool
    , logRuleAttempts           :: Bool
    , strategyQ                 :: Strategy
    , strategyA                 :: Strategy
    , outputDirectory           :: FilePath
    , pickFirstReprForParams    :: Bool
    }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Default Config where
    def = Config LogInfo False False False False Interactive Interactive "conjure-output" True


type RuleResult = ( Doc                     -- describe this transformation
                  , [Name] -> Expression    -- the result
                  , Model -> Model          -- post-application hook
                  )

data Rule = Rule
    { rName  :: Doc
    , rApply :: forall m . MonadFail m => Expression -> m [RuleResult]
                           -- fail in a rule just means that the rule isn't applicable
    }

namedRule
    :: Doc
    -> (forall m . MonadFail m => Expression -> m (Doc, [Name] -> Expression))
    -> Rule
namedRule nm f = Rule
    { rName = nm
    , rApply = \ x -> let addId (d,y) = (d,y,id)
                      in  liftM (return . addId) (f x)
    }


outputModels
    :: (MonadIO m, MonadFail m, MonadLog m)
    => Config
    -> Model
    -> m ()
outputModels config model = do
    let dir = outputDirectory config
    liftIO $ createDirectoryIfMissing True dir
    Pipes.foldM (\ j eprime -> liftIO $ do
                        let filename = dir </> "model" ++ paddedNum j ++ ".eprime"
                        writeFile filename (renderWide eprime)
                        return (j+1)
                )
                (return (1 :: Int))
                (const $ return ())
                (toCompletion config model)


toCompletion
    :: (MonadIO m, MonadFail m, MonadLog m)
    => Config
    -> Model
    -> Pipes.Producer Model m ()
toCompletion config@Config{..} m = do
    m2 <- prologue m
    logInfo $ modelInfo m2
    loopy m2
    where
        driver = strategyToDriver strategyQ strategyA
        loopy model = do
            qs <- remaining config model
            if null qs
                then do
                    model' <- epilogue model
                    Pipes.yield model'
                else do
                    nextModels <- driver qs
                    mapM_ loopy nextModels


remaining
    :: (Functor m, Applicative m, Monad m, MonadLog m)
    => Config
    -> Model
    -> m [Question]
remaining config model = do
    let freshNames' = freshNames model
    let modelZipper = fromJustNote "Creating the initial zipper." (zipperBi model)
    questions <- fmap catMaybes $ forM (allContexts modelZipper) $ \ x -> do
        ys <- applicableRules config (hole x)
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
                                    |> addToTrail config nQuestion [1 .. length questions]
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


strategyToDriver :: Strategy -> Strategy -> Driver
strategyToDriver strategyQ strategyA questions = do
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
    pickedQs <- executeStrategy optionsQ strategyQ
    fmap concat $ forM pickedQs $ \ pickedQ -> do
        let optionsA =
                [ (doc, a) 
                | (n, a) <- zip allNats (qAnswers pickedQ)
                , let doc =
                        nest 4 $ "Answer" <+> pretty n <> ":" <+> vcat [ pretty (aText a)
                                                                       , pretty (aAnswer a) ]
                ]
        pickedAs <- executeStrategy optionsA strategyA
        return (map aFullModel pickedAs)


executeStrategy :: (MonadIO m, MonadLog m) => [(Doc, a)] -> Strategy -> m [a]
executeStrategy [] _ = bug "executeStrategy: nothing to choose from"
executeStrategy [(doc, option)] (viewAuto -> (_, True)) = do
    logInfo ("Picking the only option:" <+> doc)
    return [option]
executeStrategy options@((doc, option):_) (viewAuto -> (strategy, _)) =
    case strategy of
        Auto _      -> bug "executeStrategy: Auto"
        PickFirst   -> do
            logInfo ("Picking the first option:" <+> doc)
            return [option]
        PickAll     -> return (map snd options)
        Interactive -> do
            pickedIndex <- liftIO $ do
                print (vcat (map fst options))
                putStr "Pick option: "
                readNote "Expecting an integer." <$> getLine
            let picked = snd (at options (pickedIndex - 1))
            return [picked]


addToTrail
    :: Config
    -> Int -> [Int] -> [Doc]
    -> Int -> [Int] -> [Doc]
    -> Model -> Model
addToTrail Config{..}
           nQuestion nQuestions tQuestion
           nAnswer nAnswers tAnswer
           model = model { mInfo = newInfo }
    where
        oldInfo = mInfo model
        newInfo = oldInfo { miTrailCompact = miTrailCompact oldInfo ++ [(nQuestion, nQuestions), (nAnswer, nAnswers)]
                          , miTrailVerbose = if verboseTrail
                                                  then miTrailVerbose oldInfo ++ [theQ, theA]
                                                  else []
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


allFixedQs :: Driver
allFixedQs = strategyToDriver PickFirst PickAll

pickFirst :: Driver
pickFirst = strategyToDriver PickFirst PickFirst

interactive :: Driver
interactive = strategyToDriver Interactive Interactive

interactiveFixedQs :: Driver
interactiveFixedQs = strategyToDriver PickFirst Interactive

interactiveFixedQsAutoA :: Driver
interactiveFixedQsAutoA = strategyToDriver PickFirst (Auto Interactive)



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
oneSuchThat m =
    let outStatements = transformBi onLocals (mStatements m)
    in  m { mStatements = onStatements outStatements }

    where

        onStatements :: [Statement] -> [Statement]
        onStatements xs =
            let
                (others, suchThats) = xs
                      |> map collect                                            -- separate such thats from the rest
                      |> mconcat
                      |> second (map breakConjunctions)                         -- break top level /\'s
                      |> second mconcat
                      |> second (filter (/= Constant (ConstantBool True)))      -- remove top level true's
                      |> second nub                                             -- uniq
            in
                others ++ [SuchThat (combine suchThats)]

        onLocals :: Expression -> Expression
        onLocals (WithLocals x locals) = WithLocals x (onStatements locals)
        onLocals x = x

        collect :: Statement -> ([Statement], [Expression])
        collect (SuchThat s) = ([], s)
        collect s = ([s], [])

        combine :: [Expression] -> [Expression]
        combine xs = if null xs
                        then [Constant (ConstantBool True)]
                        else xs

        breakConjunctions :: Expression -> [Expression]
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
                Declaration (GivenDomainDefnEnum name) ->
                    [ Declaration (FindOrGiven Given (name `mappend` "_EnumSize") (DomainInt [])) ]
                Declaration LettingDomainDefnEnum{}    -> []
                Declaration LettingDomainDefnUnnamed{} -> []
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


prologue :: (MonadFail m, MonadLog m) => Model -> m Model
prologue model = return model
    >>= return . initInfo
    >>= deenumifyModel
    >>= resolveNames
    >>= return . addTrueConstraints


epilogue :: MonadFail m => Model -> m Model
epilogue eprime = do
    checkIfAllRefined eprime
    eprime
        |> updateDeclarations
        |> oneSuchThat
        |> languageEprime
        |> return


representationOf :: MonadFail m => Expression -> m Name
representationOf x = do
    dom <- domainOf x
    case reprAtTopLevel dom of
        Nothing -> fail "doesn't seem to have a representation"
        Just NoRepresentation -> fail "doesn't seem to have a representation"
        Just (HasRepresentation r) -> return r


hasRepresentation :: MonadFail m => Expression -> m ()
hasRepresentation x = do
    dom <- domainOf x
    case reprAtTopLevel dom of
        Nothing -> fail "doesn't seem to have a representation"
        Just NoRepresentation -> fail "doesn't seem to have a representation"
        Just HasRepresentation{} -> return ()


applicableRules
    :: (Applicative m, Monad m, MonadLog m)
    => Config
    -> Expression
    -> m [(Doc, RuleResult)]
applicableRules config@Config{..} x | not (logRuleAttempts || logRuleFails || logRuleSuccesses) =
    concat <$> sequence [ do res <- runMaybeT (rApply r x)
                             return (map (rName r,) (concat (maybeToList res)))
                        | r <- allRules config ]
applicableRules config@Config{..} x = do
    let logAttempt = if logRuleAttempts  then logInfo else const (return ())
    let logFail    = if logRuleFails     then logInfo else const (return ())
    let logSuccess = if logRuleSuccesses then logInfo else const (return ())

    mys <- sequence [ do logAttempt ("attempting rule" <+> rName r <+> "on" <+> pretty x)
                         mres <- runExceptT (rApply r x)
                         return (rName r, mres :: Either Doc [RuleResult])
                    | r <- allRules config ]
    forM_ mys $ \ (rule, my) ->
        case my of
            Left  failed -> unless (failed == "No match.") $ logFail $ vcat
                [ " rule failed:" <+> rule
                , "          on:" <+> pretty x
                , "     message:" <+> failed
                ]
            Right ys     -> logSuccess $ vcat
                [ "rule applied:" <+> rule
                , "          on:" <+> pretty x
                , "  to produce:" <+> vcat (map fst3 ys)
                ]
    return [ (name, res)
           | (name, Right ress) <- mys
           , res <- ress
           ]


allRules :: Config -> [Rule]
allRules config =
    [ rule_ChooseRepr config

    , rule_TrueIsNoOp
    , rule_ToIntIsNoOp
    , rule_SingletonAnd
    , rule_FlattenOf1D

    , rule_BubbleUp
    , rule_BubbleToAnd

    , rule_DontCareBool
    , rule_DontCareInt
    , rule_DontCareTuple
    , rule_DontCareMatrix
    , rule_DontCareSet

    , rule_ComplexLambda

    , rule_TupleIndex
    , rule_TupleEq
    , rule_TupleLt
    , rule_TupleLeq
    , rule_MapOverDomain_Tuple

    , rule_MatrixEq
    , rule_MatrixLt
    , rule_MatrixLeq

    , rule_SetEq
    , rule_Set_In
    , rule_SetSubsetEq
    , rule_SetLt
    , rule_SetLeq
    , rule_SetIntersect
    , rule_Set_MapInExpr_Literal

    , rule_Set_MapInExpr_Explicit
    , rule_Set_MapInExpr_ExplicitVarSizeWithMarker
    , rule_Set_MapInExpr_ExplicitVarSizeWithFlags

    , rule_Set_MapInExpr_Occurrence
    , rule_Set_In_Occurrence

    , rule_FunctionEq

    , rule_Function_Image_Function1D
    , rule_Function_MapInExpr_Function1D

    , rule_Function_Image_Function1DPartial
    , rule_Function_MapInExpr_Function1DPartial
    , rule_Function_InDefined_Function1DPartial

    , rule_Function_Image_FunctionNDPartial
    , rule_Function_MapInExpr_FunctionNDPartial
    , rule_Function_InDefined_FunctionNDPartial

    , rule_RelationEq
    , rule_Relation_In

    , rule_Relation_Image_RelationAsMatrix
    , rule_Relation_MapInExpr_RelationAsMatrix

    ] ++ rule_InlineFilterInsideMap


rule_ChooseRepr :: Config -> Rule
rule_ChooseRepr config = Rule "choose-repr" theRule where

    theRule (Reference nm (Just (DeclNoRepr forg _ inpDom))) = do
        let domOpts = reprOptions inpDom
        when (null domOpts) $
            bug $ "No representation matches this beast:" <++> pretty inpDom
        let options =
                [ (msg, const out, hook)
                | dom <- domOpts
                , let msg = "Choosing representation for" <+> pretty nm <> ":" <++> pretty dom
                , let out = Reference nm (Just (DeclHasRepr forg nm dom))
                , let hook = mkHook forg nm dom
                ]
        return $ if pickFirstReprForParams config && forg == Given
                    then [head options]
                    else options 
    theRule _ = fail "No match."

    mkHook forg     -- find or given
           name     -- name of the original declaration
           domain   -- domain with representation selected
           model =
        let

            freshNames' = freshNames model

            representations = model |> mInfo |> miRepresentations

            usedBefore = (name, domain) `elem` representations

            mstructurals = do
                refs <- downToX1 Find name domain
                gen  <- getStructurals downX1 domain
                gen freshNames' refs

            structurals = case mstructurals of
                Left err -> bug ("rule_ChooseRepr.hook.structurals" <+> err)
                Right s  -> s

            addStructurals
                | forg == Given = id
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
                , let this = Reference name (Just (DeclHasRepr forg name domain))
                , let that = Reference name (Just (DeclHasRepr forg name d))
                ]

            addChannels
                | forg == Given = id
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
rule_TrueIsNoOp = "true-is-noop" `namedRule` theRule where
    theRule (Op (MkOpTrue (OpTrue ref))) =
        case ref of
            Reference _ (Just DeclHasRepr{}) ->
                return ( "Remove the argument from true."
                       , const $ Constant $ ConstantBool True
                       )
            _ -> fail "The argument of true doesn't have a representation."
    theRule _ = fail "No match."


rule_ToIntIsNoOp :: Rule
rule_ToIntIsNoOp = "toInt-is-noop" `namedRule` theRule where
    theRule p = do
        x <- match opToInt p
        return ( "Remove the toInt wrapper, it is implicit in SR."
               , const x
               )


rule_SingletonAnd :: Rule
rule_SingletonAnd = "singleton-and" `namedRule` theRule where
    theRule p = do
        [x]      <- match opAnd p
        TypeBool <- typeOf x
        return ( "singleton and, removing the wrapper"
               , const x
               )


rule_FlattenOf1D :: Rule
rule_FlattenOf1D = "flattern-of-1D" `namedRule` theRule where
    theRule p = do
        x                   <- match opFlatten p
        TypeMatrix _ xInner <- typeOf x
        case xInner of
            TypeMatrix{} -> fail "No match." 
            _ -> return ( "1D matrices do not need a flatten."
                        , const x
                        )


rule_BubbleUp :: Rule
rule_BubbleUp = "bubble-up" `namedRule` theRule where
    theRule (WithLocals (WithLocals x locals1) locals2) =
        return ( "Bubbling up nested bubbles"
               , const $ WithLocals x (locals1 ++ locals2)
               )
    theRule p = do
        (m, x) <- match opIndexing p
        case (m, x) of
            (WithLocals m' locals1, WithLocals x' locals2) ->
                return ( "Bubbling up when the bubble is used to index something (1/3)"
                       , const $ WithLocals (make opIndexing m' x') (locals1 ++ locals2)
                       )
            (WithLocals m' locals1, x') ->
                return ( "Bubbling up when the bubble is used to index something (2/3)"
                       , const $ WithLocals (make opIndexing m' x') locals1
                       )
            (m', WithLocals x' locals2) ->
                return ( "Bubbling up when the bubble is used to index something (3/3)"
                       , const $ WithLocals (make opIndexing m' x') locals2
                       )
            _ -> fail "No match."


rule_BubbleToAnd :: Rule
rule_BubbleToAnd = "bubble-to-and" `namedRule` theRule where
    theRule (WithLocals x []) = return ("Empty bubble is no bubble", const x)
    theRule (WithLocals x locals) = do
        TypeBool <- typeOf x
        cons     <- onlyConstraints locals
        let outs = x:cons
        let out = case outs of
                    [_] -> x
                    _   -> make opAnd outs
        return ( "Converting a bubble into a conjunction."
               , const out
               )
    theRule _ = fail "No match."

    onlyConstraints :: MonadFail m => [Statement] -> m [Expression]
    onlyConstraints [] = return []
    onlyConstraints (SuchThat xs:rest) = (xs++) <$> onlyConstraints rest
    onlyConstraints _ = fail "onlyConstraints: not a SuchThat"


rule_DontCareBool :: Rule
rule_DontCareBool = "dontCare-bool" `namedRule` theRule where
    theRule p = do
        x          <- match opDontCare p
        DomainBool <- domainOf x
        return ( "dontCare value for bools is false."
               , const $ make opEq x (fromBool False)
               )


rule_DontCareInt :: Rule
rule_DontCareInt = "dontCare-int" `namedRule` theRule where
    theRule p = do
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


rule_DontCareTuple :: Rule
rule_DontCareTuple = "dontCare-tuple" `namedRule` theRule where
    theRule p = do
        x           <- match opDontCare p
        TypeTuple{} <- typeOf x
        xs          <- downX1 x
        return ( "dontCare handling for tuple"
               , const $ make opAnd (map (make opDontCare) xs)
               )


rule_DontCareMatrix :: Rule
rule_DontCareMatrix = "dontCare-matrix" `namedRule` theRule where
    theRule p = do
        x                    <- match opDontCare p
        DomainMatrix index _ <- domainOf x
        return ( "dontCare handling for matrix"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0) TypeInt
                    in  [essence| forAll &iPat : &index . dontCare(&x[&i]) |]
               )


rule_DontCareSet :: Rule
rule_DontCareSet = "dontCare-set" `namedRule` theRule where
    theRule p = do
        x         <- match opDontCare p
        TypeSet{} <- typeOf x
        hasRepresentation x
        xs        <- downX1 x
        return ( "dontCare handling for tuple"
               , const $ make opAnd (map (make opDontCare) xs)
               )


rule_ComplexLambda :: Rule
rule_ComplexLambda = "complex-lamba" `namedRule` theRule where
    theRule (Lambda pat@AbsPatTuple{} body) = do
        tyPat <- typeOf pat
        return
            ( "complex lambda on tuple patterns"
            , \ fresh ->
                    let
                        patName = headInf fresh
                        outPat = Single patName (Just tyPat)
                        outRef = Reference patName (Just (InLambda outPat))
                        replacements = [ (p, make opIndexing' outRef (map fromInt is))
                                       | (p, is) <- genMappings pat
                                       ]
                        f x@(Reference nm _) = fromMaybe x (lookup nm replacements)
                        f x = x
                    in
                        Lambda outPat (transform f body)
            )
    theRule _ = fail "No match."

    -- i         --> i -> []
    -- (i,j)     --> i -> [1]
    --               j -> [2]
    -- (i,(j,k)) --> i -> [1]
    --               j -> [2,1]
    --               k -> [2,2]
    genMappings :: AbstractPattern -> [(Name, [Int])]
    genMappings (Single nm _) = [(nm, [])]
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


rule_InlineFilterInsideMap :: [Rule]
rule_InlineFilterInsideMap =
    [ namedRule "filter-inside-mapOverDomain-and" $ ruleGen_InlineFilterInsideMap opMapOverDomain opAnd opAndSkip
    , namedRule "filter-inside-mapOverDomain-or"  $ ruleGen_InlineFilterInsideMap opMapOverDomain opOr  opOrSkip
    , namedRule "filter-inside-mapOverDomain-sum" $ ruleGen_InlineFilterInsideMap opMapOverDomain opSum opSumSkip
    , namedRule "filter-inside-mapInExpr-and"     $ ruleGen_InlineFilterInsideMap opMapInExpr     opAnd opAndSkip
    , namedRule "filter-inside-mapInExpr-or"      $ ruleGen_InlineFilterInsideMap opMapInExpr     opOr  opOrSkip
    , namedRule "filter-inside-mapInExpr-sum"     $ ruleGen_InlineFilterInsideMap opMapInExpr     opSum opSumSkip
    ]
    where
        opAndSkip x y = make opImply x y
        opOrSkip  x y = make opAnd [x,y]
        opSumSkip b x = make opTimes (make opToInt b) x

ruleGen_InlineFilterInsideMap
    :: MonadFail m
    => (forall m1 . MonadFail m1 => Proxy (m1 :: * -> *) -> ( Expression -> Expression -> Expression, Expression -> m1 (Expression, Expression) ))
    -> (forall m1 . MonadFail m1 => Proxy (m1 :: * -> *) -> ( [Expression] -> Expression            , Expression -> m1 [Expression]             ))
    -> (Expression -> Expression -> Expression)
    -> Expression
    -> m (Doc, [Name] -> Expression)
ruleGen_InlineFilterInsideMap opM opQ opSkip p = do
    [x] <- match opQ p
    x'  <- possiblyNested x
    return ( "Inlining the filter."
           , const $ make opQ [ x' ]
           )

    where
        possiblyNested x =
            case tryMatch opFlatten x of
                Nothing -> ruleGen_InlineFilterInsideMap_Inner x
                Just y  -> case tryMatch opM y of
                    Nothing -> fail "flatten, but no opM"
                    Just (Lambda l1 l2, domain) -> do
                        l2' <- possiblyNested l2
                        return $ make opFlatten $ make opM (Lambda l1 l2') domain
                    Just _ -> fail "opM doesn't contain a Lambda"

        ruleGen_InlineFilterInsideMap_Inner x = do
            (Lambda f1@(Single nm _) f2, rest  ) <- match opM x
            (Lambda g1@Single{}      g2, domain) <- match opFilter rest
            ty                                   <- typeOf domain

            let f = lambdaToFunction f1 f2
            let g = lambdaToFunction g1 g2

            return $  make opM
                (mkLambda nm ty $ \ i -> opSkip (g i) (f i))
                domain


rule_TupleIndex :: Rule
rule_TupleIndex = "tuple-index" `namedRule` theRule where
    theRule p = do
        (t,i)       <- match opIndexing p
        TypeTuple{} <- typeOf t
        iInt        <- match constantInt i
        ts          <- downX1 t
        return ( "Tuple indexing on:" <+> pretty p
               , const $ atNote "Tuple indexing" ts (iInt-1)
               )


rule_TupleEq :: Rule
rule_TupleEq = "tuple-eq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opEq p
        TypeTuple _ <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple _ <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        return ( "Horizontal rule for tuple equality"
               , const $ make opAnd (zipWith (make opEq) xs ys)
               )


rule_TupleLt :: Rule
rule_TupleLt = "tuple-lt" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opLt p
        TypeTuple _ <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple _ <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        let unroll [a]    [b]    = [essence| &a < &b |]
            unroll (a:as) (b:bs) = let rest = unroll as bs
                                   in  [essence| (&a < &b) \/ ((&a = &b) /\ &rest) |]
            unroll _ _ = bug ("arity mismatch in:" <+> pretty p)
        return ( "Horizontal rule for tuple <"
               , const $ unroll xs ys
               )


rule_TupleLeq :: Rule
rule_TupleLeq = "tuple-leq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opLeq p
        TypeTuple _ <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple _ <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        let unroll [a]    [b]    = [essence| &a <= &b |]
            unroll (a:as) (b:bs) = let rest = unroll as bs
                                   in  [essence| (&a < &b) \/ ((&a = &b) /\ &rest) |]
            unroll _ _ = bug ("arity mismatch in:" <+> pretty p)
        return ( "Horizontal rule for tuple <="
               , const $ unroll xs ys
               )


rule_MatrixEq :: Rule
rule_MatrixEq = "matrix-eq" `namedRule` theRule where
    theRule p = do
        (x,y)                <- match opEq p
        TypeMatrix{}         <- typeOf x        -- TODO: check if x and y have the same arity
        TypeMatrix{}         <- typeOf y
        DomainMatrix index _ <- domainOf x
        return ( "Horizontal rule for matrix ="
               , \ fresh ->
                   let (iPat, i) = quantifiedVar (fresh `at` 0) TypeInt
                   in  [essence| forAll &iPat : &index . &x[&i] = &y[&i] |]
               )


sliceEnoughTimes :: MonadFail m => Expression -> m Expression
sliceEnoughTimes m = do
    (n,_)  <- match opIndexing' m
    tym    <- typeOf m
    tyn    <- typeOf n
    let nestingLevel (TypeMatrix _ a) = 1 + nestingLevel a
        nestingLevel _ = 0 :: Int
    let howMany = nestingLevel tyn - nestingLevel tym
    let unroll a 0 = a
        unroll a i = opSlicing (unroll a (i-1)) Nothing Nothing
    return (unroll m howMany)


rule_MatrixLt :: Rule
rule_MatrixLt = "matrix-lt" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opLt p
        tx@TypeMatrix{} <- typeOf x        -- TODO: check if x and y have the same arity
        ty@TypeMatrix{} <- typeOf y
        unless (isPrimitiveType tx) $ fail ("not a primitive type:" <+> pretty tx)
        unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
        x' <- sliceEnoughTimes x
        y' <- sliceEnoughTimes y
        return ( "Horizontal rule for matrix <"
               , const [essence| &x' <lex &y' |]
               )


rule_MatrixLeq :: Rule
rule_MatrixLeq = "matrix-leq" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opLeq p
        tx@TypeMatrix{} <- typeOf x        -- TODO: check if x and y have the same arity
        ty@TypeMatrix{} <- typeOf y
        unless (isPrimitiveType tx) $ fail ("not a primitive type:" <+> pretty tx)
        unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
        x' <- sliceEnoughTimes x
        y' <- sliceEnoughTimes y
        return ( "Horizontal rule for matrix <="
               , const [essence| &x' <=lex &y' |]
               )


rule_MapOverDomain_Tuple :: Rule
rule_MapOverDomain_Tuple = "tuple-mapOverDomain-tuple" `namedRule` theRule where
    theRule p = do
        ( Lambda f1@(Single _ (Just (TypeTuple tys))) f2 , Domain (DomainTuple domains) ) <- match opMapOverDomain p

        let f = lambdaToFunction f1 f2

        let pats fresh     = [ Single i (Just ty)
                             | (i, ty) <- zip fresh tys ]
        let refs fresh     = [ Reference i (Just (InLambda (Single i (Just ty))))
                             | (i, ty) <- zip fresh tys ]
        let theValue fresh = AbstractLiteral (AbsLitTuple (refs fresh))

        let unroll val [pat] [dom] =
                make opMapOverDomain (Lambda pat (f val))
                                     (Domain dom)
            unroll val (pat:ps) (dom:doms) = make opFlatten $
                make opMapOverDomain (Lambda pat (unroll val ps doms))
                                     (Domain dom)
            unroll _ _ _ = bug "rule_MapOverDomain_Tuple.unroll"

        return ( ""
               , \ fresh -> unroll (theValue fresh) (pats fresh) domains
               )


rule_Set_In :: Rule
rule_Set_In = "set-in" `namedRule` theRule where
    theRule p = do
        (x,s)          <- match opIn p
        TypeSet sInner <- typeOf s
        -- x in s
        -- or([ x = i | i in s ])
        let body iName = mkLambda iName sInner (\ i -> make opEq i x)
        return ( "Horizontal rule for set-in."
               , \ fresh -> make opOr [make opMapInExpr (body (headInf fresh)) s]
               )


rule_Set_MapInExpr_Explicit :: Rule
rule_Set_MapInExpr_Explicit = "set-quantification{Explicit}" `namedRule` theRule where
    theRule p = do
        (Lambda lPat@Single{} lBody, s) <- match opMapInExpr p
        "Explicit"           <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let f = lambdaToFunction lPat lBody
        let body iName = mkLambda iName TypeInt $ \ i ->
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
    theRule p = do
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
    theRule p = do
        (x,y)                <- match opSubsetEq p
        TypeSet tyXInner     <- typeOf x
        TypeSet{}            <- typeOf y
        let body iName = mkLambda iName tyXInner (\ i -> make opIn i y)
        return ( "Horizontal rule for set subsetEq"
               , \ fresh -> make opAnd [make opMapInExpr (body (headInf fresh)) x]
               )


rule_SetLt :: Rule
rule_SetLt = "set-lt" `namedRule` theRule where
    theRule p = do
        (a,b) <- match opLt p
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- AbstractLiteral . AbsLitTuple <$> downX1 a
        mb <- AbstractLiteral . AbsLitTuple <$> downX1 b
        return ( "Horizontal rule for set <" <+> pretty (make opLt ma mb)
               , const $ make opLt ma mb
               )


rule_SetLeq :: Rule
rule_SetLeq = "set-leq" `namedRule` theRule where
    theRule p = do
        (a,b) <- match opLeq p
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- AbstractLiteral . AbsLitTuple <$> downX1 a
        mb <- AbstractLiteral . AbsLitTuple <$> downX1 b
        return ( "Horizontal rule for set <=" <+> pretty (make opLeq ma mb)
               , const $ make opLeq ma mb
               )

rule_SetIntersect :: Rule
rule_SetIntersect = "set-intersect" `namedRule` theRule where
    theRule p = do
        (lambda, intersected) <- match opMapInExpr p
        (x, y)                <- match opIntersect intersected
        tx                    <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> fail "type incompatibility in intersect operator"
        xInnerTy              <- innerTypeOf tx
        return ( "Horizontal rule for set intersection"
               , \ fresh -> make opMapInExpr lambda
                               (make opFilter
                                   (mkLambda (headInf fresh) xInnerTy $ \ i -> [essence| &i in &y |])
                                   x
                               )
               )


rule_Set_MapInExpr_Literal :: Rule
rule_Set_MapInExpr_Literal = "set-mapSetLiteral" `namedRule` theRule where
    theRule p = do
        (Lambda l1 l2, lit) <- match opMapInExpr p
        elems               <- match setLiteral lit
        let l               =  lambdaToFunction l1 l2
        return ( "Membership on set literals"
               , const $ AbstractLiteral $ AbsLitMatrix
                           (DomainInt [RangeBounded (fromInt 1) (fromInt (length elems))])
                           [ l e
                           | e <- elems
                           ]
               )


rule_Set_In_Occurrence :: Rule
rule_Set_In_Occurrence = "set-in{Occurrence}" `namedRule` theRule where
    theRule p = do
        (x,s)                <- match opIn p
        TypeSet{}            <- typeOf s
        "Occurrence"         <- representationOf s
        [m]                  <- downX1 s
        return ( "Vertical rule for set-in, Occurrence representation"
               , const $ make opIndexing m x
               )


rule_Set_MapInExpr_Occurrence :: Rule
rule_Set_MapInExpr_Occurrence = "set-quantification{Occurrence}" `namedRule` theRule where
    theRule p = do
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


rule_Set_MapInExpr_ExplicitVarSizeWithMarker :: Rule
rule_Set_MapInExpr_ExplicitVarSizeWithMarker = "set-quantification{ExplicitVarSizeWithMarker}"
                                               `namedRule` theRule where
    theRule p = do
        (Lambda lPat lBody, s)      <- match opMapInExpr p
        "ExplicitVarSizeWithMarker" <- representationOf s
        [marker, values]            <- downX1 s
        DomainMatrix index _        <- domainOf values
        let f = lambdaToFunction lPat lBody
        let mapBody    iName = mkLambda iName TypeInt $ \ i -> f (make opIndexing values i)
        let filterBody iName = mkLambda iName TypeInt $ \ i -> make opLeq i marker
        -- map_in_expr(f(i), x)
        -- map_domain(f(values[i]), filter(i <= marker, domain))
        return ( "Vertical rule for set-quantification, ExplicitVarSizeWithMarker representation"
               , \ fresh -> make opMapOverDomain
                               (mapBody (headInf fresh))
                               (make opFilter
                                    (filterBody (headInf fresh))
                                    (Domain index))
               )


rule_Set_MapInExpr_ExplicitVarSizeWithFlags :: Rule
rule_Set_MapInExpr_ExplicitVarSizeWithFlags = "set-quantification{ExplicitVarSizeWithFlags}"
                                               `namedRule` theRule where
    theRule p = do
        (Lambda lPat lBody, s)      <- match opMapInExpr p
        "ExplicitVarSizeWithFlags"  <- representationOf s
        [flags, values]             <- downX1 s
        DomainMatrix index _        <- domainOf values
        let f = lambdaToFunction lPat lBody
        let mapBody    iName = mkLambda iName TypeInt $ \ i -> f (make opIndexing values i)
        let filterBody iName = mkLambda iName TypeInt $ \ i ->    make opIndexing flags  i
        -- map_in_expr(f(i), x)
        -- map_domain(f(values[i]), filter(flags[i], domain))
        return ( "Vertical rule for set-quantification, ExplicitVarSizeWithFlags representation"
               , \ fresh -> make opMapOverDomain
                               (mapBody (headInf fresh))
                               (make opFilter
                                    (filterBody (headInf fresh))
                                    (Domain index))
               )


rule_FunctionEq :: Rule
rule_FunctionEq = "function-eq" `namedRule` theRule where
    theRule p = do
        (x,y)                    <- match opEq p
        TypeFunction xFrTy xToTy <- typeOf x
        TypeFunction yFrTy yToTy <- typeOf y
        return ( "Horizontal rule for function equality"
               , \ fresh ->
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0) (mostDefined [ TypeTuple [xFrTy, xToTy]
                                                                              , TypeTuple [yFrTy, yToTy]
                                                                              ])
                    in
                        [essence|
                            (forAll &iPat in &x . &y(&i[1]) = &i[2])
                                /\
                            (forAll &iPat in &y . &x(&i[1]) = &i[2])
                        |]
               )


rule_Function_MapInExpr_Function1D :: Rule
rule_Function_MapInExpr_Function1D = "function-quantification{Function1D}"
                                     `namedRule` theRule where
    theRule p = do
        (Lambda lPat lBody, expr) <- match opMapInExpr p
        let f                     =  matchDef opToSet expr
        "Function1D"              <- representationOf f
        TypeFunction fr _         <- typeOf f
        [values]                  <- downX1 f
        DomainMatrix index _      <- domainOf values
        let lambda = lambdaToFunction lPat lBody
        return ( "Mapping over a function, Function1D representation"
               , \ fresh ->
                    let iName = headInf fresh
                    in  make opMapOverDomain
                            (mkLambda iName fr $ \ i -> lambda [essence| (&i, &values[&i]) |])
                            (Domain index)
               )


rule_Function_Image_Function1D :: Rule
rule_Function_Image_Function1D = "function-image{Function1D}"
                                 `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        "Function1D" <- representationOf f
        [values]     <- downX1 f
        return ( "Function image, Function1D representation"
               , const [essence| &values[&x] |]
               )
    theRule _ = fail "No match."


rule_Function_MapInExpr_Function1DPartial :: Rule
rule_Function_MapInExpr_Function1DPartial = "function-quantification{Function1DPartial}"
                                     `namedRule` theRule where
    theRule p = do
        (Lambda lPat lBody, expr) <- match opMapInExpr p
        let f                     =  matchDef opToSet expr
        "Function1DPartial"       <- representationOf f
        TypeFunction fr _         <- typeOf f
        [flags,values]            <- downX1 f
        DomainMatrix index _      <- domainOf values
        let lambda = lambdaToFunction lPat lBody
        return ( "Mapping over a function, Function1DPartial representation"
               , \ fresh ->
                    let iName = headInf fresh
                    in  make opMapOverDomain
                            (mkLambda iName fr $ \ i -> lambda [essence| (&i, &values[&i]) |])
                            (make opFilter
                                (mkLambda iName fr $ \ i -> [essence| &flags[&i] |] )
                                (Domain index))
               )


rule_Function_Image_Function1DPartial :: Rule
rule_Function_Image_Function1DPartial = "function-image{Function1DPartial}"
                                 `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        "Function1DPartial" <- representationOf f
        [flags,values]      <- downX1 f
        return ( "Function image, Function1DPartial representation"
               , const [essence| { &values[&x]
                                 @ such that &flags[&x]
                                 }
                       |]
               )
    theRule _ = fail "No match."


rule_Function_InDefined_Function1DPartial :: Rule
rule_Function_InDefined_Function1DPartial = "function-in-defined{Function1DPartial}"
                                 `namedRule` theRule where
    theRule [essence| &x in defined(&f) |] = do
        "Function1DPartial" <- representationOf f
        [flags,_values]     <- downX1 f
        return ( "Function in defined, Function1DPartial representation"
               , const [essence| &flags[&x] |]
               )
    theRule _ = fail "No match."


rule_Function_Image_FunctionNDPartial :: Rule
rule_Function_Image_FunctionNDPartial = "function-image{FunctionNDPartial}"
                                 `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        "FunctionNDPartial" <- representationOf f
        [flags,values]      <- downX1 f

        TypeTuple ts        <- typeOf x
        let xArity          =  length ts
        let index m 1     = make opIndexing m                   (make opIndexing x (fromInt 1))
            index m arity = make opIndexing (index m (arity-1)) (make opIndexing x (fromInt arity))
        let flagsIndexed  = index flags  xArity
        let valuesIndexed = index values xArity

        return ( "Function image, FunctionNDPartial representation"
               , const [essence| { &valuesIndexed
                                 @ such that &flagsIndexed
                                 } |]
               )
    theRule _ = fail "No match."


rule_Function_InDefined_FunctionNDPartial :: Rule
rule_Function_InDefined_FunctionNDPartial = "function-in-defined{FunctionNDPartial}"
                                 `namedRule` theRule where
    theRule [essence| &x in defined(&f) |] = do
        "FunctionNDPartial" <- representationOf f
        [flags,_values]     <- downX1 f

        TypeTuple ts        <- typeOf x
        let xArity          =  length ts
        let index m 1     = make opIndexing m                   (make opIndexing x (fromInt 1))
            index m arity = make opIndexing (index m (arity-1)) (make opIndexing x (fromInt arity))
        let flagsIndexed  = index flags  xArity

        return ( "Function in defined, FunctionNDPartial representation"
               , const flagsIndexed
               )
    theRule _ = fail "No match."


rule_Function_MapInExpr_FunctionNDPartial :: Rule
rule_Function_MapInExpr_FunctionNDPartial = "function-quantification{FunctionNDPartial}"
                                     `namedRule` theRule where
    theRule p = do
        (Lambda lPat lBody, expr)        <- match opMapInExpr p
        let f                            = matchDef opToSet expr
        let lambda = lambdaToFunction lPat lBody
        "FunctionNDPartial"              <- representationOf f
        TypeFunction fr@(TypeTuple ts) _ <- typeOf f
        [flags,values]                   <- downX1 f
        valuesDom                        <- domainOf values
        let (indexDomain,_)              =  getIndices valuesDom

        let xArity          =  length ts
        let index x m 1     = make opIndexing m                     (make opIndexing x (fromInt 1))
            index x m arity = make opIndexing (index x m (arity-1)) (make opIndexing x (fromInt arity))
        let flagsIndexed  x = index x flags  xArity
        let valuesIndexed x = index x values xArity

        return ( "Mapping over a function, FunctionNDPartial representation"
               , \ fresh ->
                    let iName = headInf fresh
                    in  make opMapOverDomain
                            (mkLambda iName fr $ \ i -> let val = valuesIndexed i
                                                        in  lambda [essence| (&i, &val) |])
                            (make opFilter
                                (mkLambda iName fr $ \ i -> flagsIndexed i)
                                (Domain (DomainTuple indexDomain)))
               )


rule_RelationEq :: Rule
rule_RelationEq = "relation-eq" `namedRule` theRule where
    theRule p = do
        (x,y)          <- match opEq p
        TypeRelation{} <- typeOf x
        TypeRelation{} <- typeOf y
        return ( "Horizontal rule for relation equality"
               , const $ make opEq (make opToSet x)
                                   (make opToSet y)
               )

rule_Relation_In :: Rule
rule_Relation_In = "relation-in" `namedRule` theRule where
    theRule [essence| &x in &rel |] = do
        TypeRelation{} <- typeOf rel
        return ( "relation membership to existential quantification"
               , \ fresh ->
                   let (iPat, i) = quantifiedVar (fresh `at` 0) TypeInt
                   in  [essence| exists &iPat in toSet(&rel) . &i = &x |]
               )
    theRule _ = fail "No match."


rule_Relation_Image_RelationAsMatrix :: Rule
rule_Relation_Image_RelationAsMatrix = "relation-image{RelationAsMatrix}" `namedRule` theRule where
    theRule p = do
        (rel, args)         <- match opFunctionImage p
        TypeRelation{}      <- typeOf rel
        "RelationAsMatrix"  <- representationOf rel
        [m]                 <- downX1 rel
        let unroll v [] = v
            unroll v (i:is) = unroll (make opIndexing v i) is
        return ( "relation image, RelationAsMatrix representation"
               , const $ unroll m args
               )


rule_Relation_MapInExpr_RelationAsMatrix :: Rule
rule_Relation_MapInExpr_RelationAsMatrix = "relation-map_in_expr{RelationAsMatrix}" `namedRule` theRule where
    theRule p = do
        (Lambda f1 f2, s)      <- match opMapInExpr p
        let f                  =  lambdaToFunction f1 f2
        let r                  =  matchDef opToSet s
        TypeRelation{}         <- typeOf r
        "RelationAsMatrix"     <- representationOf r
        [m]                    <- downX1 r
        mDom                   <- domainOf m
        let (mIndices, _)      =  getIndices mDom

        let
            unroll
                :: Expression
                -> [Expression]
                -> [ ( (AbstractPattern, Expression)
                     , Domain () Expression
                     ) ]
                -> Expression
            unroll
                theMatrix
                theValue
                _quantifiers     @[((iPat,i),index)]
                = let nextMatrix = make opIndexing theMatrix i
                      nextValue  = theValue ++ [i]
                      finalValue = AbstractLiteral (AbsLitTuple nextValue)
                  in  make opMapOverDomain
                        (Lambda iPat (f finalValue))
                        (make opFilter
                            (Lambda iPat nextMatrix)
                            (Domain index))
            unroll
                theMatrix
                theValue
                _quantifiers     @(((iPat,i),index):rest)
                = let nextMatrix = make opIndexing theMatrix i
                      nextValue  = theValue ++ [i]
                  in  make opFlatten $ make opMapOverDomain
                        (Lambda iPat (unroll nextMatrix nextValue rest))
                        (Domain index)

            unroll _ _ _
                = bug "rule_Relation_MapInExpr_RelationAsMatrix.unroll []"

        let out fresh = unroll m [] (zip [ quantifiedVar fr TypeInt | fr <- fresh ] mIndices)
        return ( "Vertical rule for map_in_expr for relation domains, RelationAsMatrix representation."
               , out
               )

