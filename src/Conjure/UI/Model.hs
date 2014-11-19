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
import Conjure.Language.Ops hiding ( opOr, opAnd, opIn, opEq, opLt
                                   , opSubsetEq, opDontCare, opImply, opTimes, opToInt
                                   , opLeq, opFlatten, opToSet, opIntersect, opUnion, opFunctionImage )

import Conjure.Language.ModelStats ( modelInfo )
import Conjure.Process.Enums ( deenumifyModel )
import Conjure.Language.NameResolution ( resolveNames, resolveNamesX )

import Conjure.Representations ( downX1, downToX1, downD, reprOptions, getStructurals )

-- uniplate
import Data.Generics.Uniplate.Zipper ( Zipper, zipperBi, fromZipper, hole, replaceHole )

-- pipes
import Pipes ( Producer, yield, (>->) )
import qualified Pipes.Prelude as Pipes ( foldM, take )


type LogOr a = Either (LogLevel, Doc) a
type LogOrModel = LogOr Model

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
    , channelling               :: Bool
    , parameterRepresentation   :: Bool
    , limitModels               :: Maybe Int
    }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Default Config where
    def = Config
        { logLevel                  = LogNone
        , verboseTrail              = False
        , logRuleFails              = False
        , logRuleSuccesses          = False
        , logRuleAttempts           = False
        , strategyQ                 = Interactive
        , strategyA                 = Interactive
        , outputDirectory           = "conjure-output"
        , channelling               = True
        , parameterRepresentation   = True
        , limitModels               = Nothing
        }

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
    let limitModelsIfNeeded = maybe id (\ n gen -> gen >-> Pipes.take n ) (limitModels config)
    let each i logOrModel =
            case logOrModel of
                Left (l,msg) -> do
                    log l msg
                    return i
                Right eprime -> do
                    let filename = dir </> "model" ++ paddedNum i ++ ".eprime"
                    liftIO $ writeFile filename (renderWide eprime)
                    return (i+1)
    Pipes.foldM each
                (return (1 :: Int))
                (const $ return ())
                (limitModelsIfNeeded $ toCompletion config model )


toCompletion
    :: (MonadIO m, MonadFail m)
    => Config
    -> Model
    -> Producer LogOrModel m ()
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
                    yield (Right model')
                else do
                    nextModels <- driver qs
                    mapM_ loopy nextModels


inAReference :: Zipper a Expression -> Bool
inAReference x = not $ null [ () | Reference{} <- tail (ascendants x) ]


remaining
    :: MonadLog m
    => Config
    -> Model
    -> m [Question]
remaining config model = do
    let freshNames' = freshNames model
    let modelZipper = fromJustNote "Creating the initial zipper." (zipperBi model)
    let
        loopLevels :: Monad m => [m [a]] -> m [a]
        loopLevels [] = return []
        loopLevels (a:as) = do bs <- a
                               if null bs
                                   then loopLevels as
                                   else return bs

        processLevel :: MonadLog m => [Rule] -> m [(Zipper Model Expression, [(Doc, RuleResult)])]
        processLevel rulesAtLevel = do
            fmap catMaybes $ forM (allContexts modelZipper) $ \ x ->
                -- things in a reference should not be rewritten.
                -- specifically, no representation selection for them!
                if inAReference x
                    then return Nothing
                    else do
                        ys <- applicableRules config rulesAtLevel (hole x)
                        return $ if null ys
                                    then Nothing
                                    else (Just (x, ys))

    questions <- loopLevels $ map processLevel (allRules config)
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
                    Reference _ (Just (DeclHasRepr _ _ dom))
                        | inAReference x && not (isPrimitiveDomain dom) ->
                        return $ ""
                               : ("Not refined:" <+> pretty (hole x))
                               : [ nest 4 ("Context #" <> pretty i <> ":" <+> pretty c)
                                 | i <- allNats
                                 | c <- tail (ascendants x)
                                 ]
                    _ -> return []
    unless (null fails) (fail (vcat fails))


prologue :: (MonadFail m, MonadLog m) => Model -> m Model
prologue model = return model
                                    >>= logDebugId "[input]"
    >>= return . initInfo           >>= logDebugId "[initInfo]"
    >>= deenumifyModel              >>= logDebugId "[deenumifyModel]"
    >>= resolveNames                >>= logDebugId "[resolveNames]"
    >>= return . addTrueConstraints >>= logDebugId "[addTrueConstraints]"


epilogue :: MonadFail m => Model -> m Model
epilogue eprime = do
    checkIfAllRefined eprime
    eprime
        |> updateDeclarations
        |> oneSuchThat
        |> languageEprime
        |> return


isAtomic :: Expression -> Bool
isAtomic Reference{} = True
isAtomic (Op (MkOpIndexing (OpIndexing a _))) = isAtomic a
isAtomic _ = False


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
    -> [Rule]
    -> Expression
    -> m [(Doc, RuleResult)]
applicableRules Config{..} rulesAtLevel x = do
    let logAttempt = if logRuleAttempts  then logInfo else const (return ())
    let logFail    = if logRuleFails     then logInfo else const (return ())
    let logSuccess = if logRuleSuccesses then logInfo else const (return ())

    mys <- sequence [ do logAttempt ("attempting rule" <+> rName r <+> "on" <+> pretty x)
                         mres <- runExceptT (rApply r x)
                         return (rName r, mres :: Either Doc [RuleResult])
                    | r <- rulesAtLevel ]
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
                , "     message:" <+> vcat (map fst3 ys)
                ]
    return [ (name, (msg, out', hook))
           | (name, Right ress) <- mys
           , (msg, out, hook) <- ress
           , let out' fresh = out fresh |> resolveNamesX |> bugFail     -- re-resolving names
           ]


allRules :: Config -> [[Rule]]
allRules config =
    [   [ rule_ChooseRepr config ]
    ,   [ rule_TrueIsNoOp
        , rule_ToIntIsNoOp
        , rule_SingletonAnd
        , rule_FlattenOf1D

        , rule_BubbleUp
        , rule_BubbleToAnd

        , rule_Bool_DontCare
        , rule_Int_DontCare
        , rule_Tuple_DontCare
        , rule_Matrix_DontCare
        , rule_Set_DontCare

        , rule_ComplexAbsPat

        , rule_Tuple_Index
        , rule_Tuple_Eq
        , rule_Tuple_Lt
        , rule_Tuple_Leq
        , rule_Tuple_DomainComprehension

        , rule_Matrix_Eq
        , rule_Matrix_Lt
        , rule_Matrix_Leq

        , rule_Set_Eq
        , rule_Set_In
        , rule_Set_SubsetEq
        , rule_Set_Subset
        , rule_Set_Supset
        , rule_Set_SupsetEq
        , rule_Set_Lt
        , rule_Set_Leq
        , rule_Set_Intersect
        , rule_Set_Union

        , rule_Set_Comprehension_Literal
        , rule_Set_Comprehension_Explicit
        , rule_Set_Comprehension_ExplicitVarSizeWithMarker
        , rule_Set_Comprehension_ExplicitVarSizeWithFlags
        , rule_Set_Comprehension_Occurrence

        , rule_Set_In_Occurrence

        , rule_Function_Eq

        , rule_Function_Image_Function1D
        , rule_Function_Image_Function1DPartial
        , rule_Function_Image_FunctionNDPartial

        , rule_Function_Comprehension_Function1D
        , rule_Function_Comprehension_Function1DPartial
        , rule_Function_Comprehension_FunctionNDPartial

        , rule_Function_InDefined_Function1DPartial
        , rule_Function_InDefined_FunctionNDPartial

        , rule_Relation_Eq
        , rule_Relation_In

        , rule_Relation_Image_RelationAsMatrix
        , rule_Relation_Comprehension_RelationAsMatrix

        ] ++ rule_InlineFilters
    ]


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
                , let hook = mkHook (channelling config) forg nm dom
                ]
        return $ if forg == Given && parameterRepresentation config == False
                    then [head options]
                    else options 
    theRule _ = fail "No match."

    mkHook useChannelling   -- whether to use channelling or not
           forg             -- find or given
           name             -- name of the original declaration
           domain           -- domain with representation selected
           model =
        let

            freshNames' = freshNames model

            representations = model |> mInfo |> miRepresentations

            usedBefore = (name, domain) `elem` representations

            mstructurals = do
                refs <- downToX1 Find name domain
                gen  <- getStructurals downX1 domain
                gen freshNames' refs >>= mapM resolveNamesX     -- re-resolving names

            structurals = case mstructurals of
                Left err -> bug ("rule_ChooseRepr.hook.structurals" <+> err)
                Right s  -> s

            addStructurals
                | forg == Given = id
                | usedBefore = id
                | null structurals = id
                | otherwise = \ m ->
                    m { mStatements = mStatements m ++ [SuchThat structurals] }

            channels =
                [ make opEq this that
                | (n, d) <- representations
                , n == name
                , let this = Reference name (Just (DeclHasRepr forg name domain))
                , let that = Reference name (Just (DeclHasRepr forg name d))
                ]

            addChannels
                | forg == Given = id
                | usedBefore = id
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

            fixReprForOthers
                | useChannelling = id           -- no-op, if channelling=yes
                | otherwise = \ m ->
                let
                    f (Reference nm (Just DeclNoRepr{})) = Reference nm (Just (DeclHasRepr forg name domain))
                    f x = x
                in
                    m { mStatements = transformBi f (mStatements m) }

        in
            model
                |> addStructurals       -- unless usedBefore: add structurals
                |> addChannels          -- for each in previously recorded representation
                |> recordThis           -- unless usedBefore: record (name, domain) as being used in the model
                |> fixReprForOthers     -- fix the representation of this guy in the whole model, if channelling=no


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
rule_FlattenOf1D = "flatten-of-1D" `namedRule` theRule where
    theRule p = do
        x                   <- match opFlatten p
        TypeMatrix _ xInner <- typeOf x
        case xInner of
            TypeBool{} -> return ()
            TypeInt{}  -> return ()
            _ -> fail "No match."
        return ( "1D matrices do not need a flatten."
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


rule_Bool_DontCare :: Rule
rule_Bool_DontCare = "dontCare-bool" `namedRule` theRule where
    theRule p = do
        x          <- match opDontCare p
        DomainBool <- domainOf x
        return ( "dontCare value for bools is false."
               , const $ make opEq x (fromBool False)
               )


rule_Int_DontCare :: Rule
rule_Int_DontCare = "dontCare-int" `namedRule` theRule where
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


rule_Tuple_DontCare :: Rule
rule_Tuple_DontCare = "dontCare-tuple" `namedRule` theRule where
    theRule p = do
        x           <- match opDontCare p
        TypeTuple{} <- typeOf x
        xs          <- downX1 x
        return ( "dontCare handling for tuple"
               , const $ make opAnd (map (make opDontCare) xs)
               )


rule_Matrix_DontCare :: Rule
rule_Matrix_DontCare = "dontCare-matrix" `namedRule` theRule where
    theRule p = do
        x                    <- match opDontCare p
        DomainMatrix index _ <- domainOf x
        return ( "dontCare handling for matrix"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| forAll &iPat : &index . dontCare(&x[&i]) |]
               )


rule_Set_DontCare :: Rule
rule_Set_DontCare = "dontCare-set" `namedRule` theRule where
    theRule p = do
        x         <- match opDontCare p
        TypeSet{} <- typeOf x
        hasRepresentation x
        xs        <- downX1 x
        return ( "dontCare handling for tuple"
               , const $ make opAnd (map (make opDontCare) xs)
               )


rule_ComplexAbsPat :: Rule
rule_ComplexAbsPat = "complex-pattern" `namedRule` theRule where
    theRule (Comprehension body [Generator (GenDomain pat@AbsPatTuple{} domain)]) =
        return
            ( "complex pattern on tuple patterns"
            , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                        replacements = [ (p, make opIndexing' i (map fromInt is))
                                       | (p, is) <- genMappings pat
                                       ]
                        f x@(Reference nm _) = fromMaybe x (lookup nm replacements)
                        f x = x
                    in
                        Comprehension (transform f body)
                            [Generator (GenDomain iPat domain)]
            )
    theRule (Comprehension body [Generator (GenInExpr pat@AbsPatTuple{} expr)]) =
        return
            ( "complex pattern on tuple patterns"
            , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                        replacements = [ (p, make opIndexing' i (map fromInt is))
                                       | (p, is) <- genMappings pat
                                       ]
                        f x@(Reference nm _) = fromMaybe x (lookup nm replacements)
                        f x = x
                    in
                        Comprehension (transform f body)
                            [Generator (GenInExpr iPat expr)]
            )
    theRule _ = fail "No match."

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


rule_InlineFilters :: [Rule]
rule_InlineFilters =
    [ namedRule "filter-inside-and" $ ruleGen_InlineFilters opAnd opAndSkip
    , namedRule "filter-inside-or"  $ ruleGen_InlineFilters opOr  opOrSkip
    , namedRule "filter-inside-sum" $ ruleGen_InlineFilters opSum opSumSkip
    ]
    where
        opAndSkip x y = make opImply x y
        opOrSkip  x y = make opAnd [x,y]
        opSumSkip b x = make opTimes (make opToInt b) x

ruleGen_InlineFilters
    :: MonadFail m
    => (forall m1 . MonadFail m1
            => Proxy (m1 :: * -> *)
            -> ( [Expression] -> Expression , Expression -> m1 [Expression] )
       )
    -> (Expression -> Expression -> Expression)
    -> Expression
    -> m (Doc, [Name] -> Expression)
ruleGen_InlineFilters opQ opSkip p = do
    [Comprehension body gensOrFilters] <- match opQ p
    let filters    = [ x | Filter    x <- gensOrFilters ]
    let generators = [ x | Generator x <- gensOrFilters ]
    theGuard <- case filters of
        []  -> fail "No filter."
        [x] -> return x
        xs  -> return $ make opAnd xs
    return ( "Inlining filters"
           , const $ make opQ $ return $
               Comprehension (opSkip theGuard body) (map Generator generators)
           )


rule_Tuple_Index :: Rule
rule_Tuple_Index = "tuple-index" `namedRule` theRule where
    theRule p = do
        (t,i)       <- match opIndexing p
        TypeTuple{} <- typeOf t
        iInt        <- match constantInt i
        ts          <- downX1 t
        return ( "Tuple indexing on:" <+> pretty p
               , const $ atNote "Tuple indexing" ts (iInt-1)
               )


rule_Tuple_Eq :: Rule
rule_Tuple_Eq = "tuple-eq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opEq p
        TypeTuple _ <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple _ <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        return ( "Horizontal rule for tuple equality"
               , const $ make opAnd (zipWith (make opEq) xs ys)
               )


rule_Tuple_Lt :: Rule
rule_Tuple_Lt = "tuple-lt" `namedRule` theRule where
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


rule_Tuple_Leq :: Rule
rule_Tuple_Leq = "tuple-leq" `namedRule` theRule where
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


rule_Matrix_Eq :: Rule
rule_Matrix_Eq = "matrix-eq" `namedRule` theRule where
    theRule p = do
        (x,y)                <- match opEq p
        TypeMatrix{}         <- typeOf x        -- TODO: check if x and y have the same arity
        TypeMatrix{}         <- typeOf y
        DomainMatrix index _ <- domainOf x
        return ( "Horizontal rule for matrix ="
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
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


rule_Matrix_Lt :: Rule
rule_Matrix_Lt = "matrix-lt" `namedRule` theRule where
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


rule_Matrix_Leq :: Rule
rule_Matrix_Leq = "matrix-leq" `namedRule` theRule where
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


rule_Tuple_DomainComprehension :: Rule
rule_Tuple_DomainComprehension = "tuple-domain-comprehension" `namedRule` theRule where
    theRule (Comprehension body [Generator (GenDomain pat@Single{} (DomainTuple domains))]) = do
        let pats fresh     = [ Single i            | i <- fresh ]
        let refs fresh     = [ Reference i Nothing | i <- fresh ]
        let theValue fresh = AbstractLiteral (AbsLitTuple (refs fresh))
        let f              = lambdaToFunction pat body
        return ( "Tuple domain comprehension"
               , \ fresh' ->
                   let fresh = take (length domains) fresh'
                   in  Comprehension (f (theValue fresh))
                       [ Generator (GenDomain p d)
                       | (p,d) <- zip (pats fresh) domains
                       ]
               )
    theRule _ = fail "No match."


-- x in s ~~> or([ x = i | i in s ])
-- where s is a set
-- and not Occurrence
rule_Set_In :: Rule
rule_Set_In = "set-in" `namedRule` theRule where
    theRule p = do
        (x,s)     <- match opIn p
        TypeSet{} <- typeOf s
        case (isAtomic s, representationOf s) of
            (True, Just "Occurrence") -> fail "Occurrence has a better rule for set-membership."
            (True, Nothing          ) -> fail "Choose a representation first."
            _ -> return ()
        return ( "Horizontal rule for set-in."
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| exists &iPat in &s . &i = &x |]
               )


rule_Set_Comprehension_Explicit :: Rule
rule_Set_Comprehension_Explicit = "set-comprehension{Explicit}" `namedRule` theRule where
    theRule (Comprehension body [Generator (GenInExpr pat@(Single iPat) s)]) = do
        TypeSet{}            <- typeOf s
        "Explicit"           <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let i = Reference iPat Nothing
        let f = lambdaToFunction pat body
        return ( "Vertical rule for set-comprehension, Explicit representation"
               , const $
                   Comprehension (f [essence| &m[&i] |])
                       [ Generator (GenDomain pat index) ]
               )
    theRule _ = fail "No match."


rule_Set_Eq :: Rule
rule_Set_Eq = "set-eq" `namedRule` theRule where
    theRule p = do
        (x,y)     <- match opEq p
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return ( "Horizontal rule for set equality"
               , const $ make opAnd [ make opSubsetEq x y
                                    , make opSubsetEq y x
                                    ]
               )


rule_Set_SubsetEq :: Rule
rule_Set_SubsetEq = "set-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)     <- match opSubsetEq p
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return ( "Horizontal rule for set subsetEq"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| forAll &iPat in (&x) . &i in &y |]
               )


rule_Set_Subset :: Rule
rule_Set_Subset = "set-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] =
        return
            ( "Horizontal rule for set subset"
            , const [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = fail "No match."


rule_Set_Supset :: Rule
rule_Set_Supset = "set-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] =
        return
            ( "Horizontal rule for set supset"
            , const [essence| &b subset &a |]
            )
    theRule _ = fail "No match."


rule_Set_SupsetEq :: Rule
rule_Set_SupsetEq = "set-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] =
        return
            ( "Horizontal rule for set supsetEq"
            , const [essence| &b subsetEq &a |]
            )
    theRule _ = fail "No match."


rule_Set_Lt :: Rule
rule_Set_Lt = "set-lt" `namedRule` theRule where
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


rule_Set_Leq :: Rule
rule_Set_Leq = "set-leq" `namedRule` theRule where
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


rule_Set_Intersect :: Rule
rule_Set_Intersect = "set-intersect" `namedRule` theRule where
    theRule (Comprehension body [Generator (GenInExpr pat@(Single iPat) s)]) = do
        (x, y)             <- match opIntersect s
        tx                 <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> fail "type incompatibility in intersect operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for set intersection"
            , const $
                Comprehension body
                    [ Generator (GenInExpr pat x)
                    , Filter [essence| &i in &y |]
                    ]
            )
    theRule _ = fail "No match."


rule_Set_Union :: Rule
rule_Set_Union = "set-union" `namedRule` theRule where
    theRule (Comprehension body [Generator (GenInExpr pat@Single{} s)]) = do
        (x, y)             <- match opUnion s
        tx                 <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> fail "type incompatibility in intersect operator"
        return
            ( "Horizontal rule for set intersection"
            , const $ make opFlatten $ AbstractLiteral $ AbsLitList
                [ Comprehension body [ Generator (GenInExpr pat x) ]
                , Comprehension body [ Generator (GenInExpr pat y) ]
                ]
            )
    theRule _ = fail "No match."


rule_Set_Comprehension_Literal :: Rule
rule_Set_Comprehension_Literal = "set-mapSetLiteral" `namedRule` theRule where
    theRule (Comprehension body [Generator (GenInExpr pat s)]) = do
        elems <- match setLiteral s
        let f = lambdaToFunction pat body
        return ( "Membership on set literals"
               , const $ AbstractLiteral $ AbsLitMatrix
                           (DomainInt [RangeBounded (fromInt 1) (fromInt (length elems))])
                           [ f e
                           | e <- elems
                           ]
               )
    theRule _ = fail "No match."


rule_Set_In_Occurrence :: Rule
rule_Set_In_Occurrence = "set-in{Occurrence}" `namedRule` theRule where
    theRule p = do
        (x, s)       <- match opIn p
        TypeSet{}    <- typeOf s
        "Occurrence" <- representationOf s
        [m]          <- downX1 s
        return ( "Vertical rule for set-in, Occurrence representation"
               , const $ make opIndexing m x
               )


rule_Set_Comprehension_Occurrence :: Rule
rule_Set_Comprehension_Occurrence = "set-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body [Generator (GenInExpr pat@(Single iPat) s)]) = do
        TypeSet{}            <- typeOf s
        "Occurrence"         <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let i = Reference iPat Nothing
        return ( "Vertical rule for set-comprehension, Occurrence representation"
               , const $
                   Comprehension body
                       [ Generator (GenDomain pat index)
                       , Filter [essence| &m[&i] |]
                       ]
               )
    theRule _ = fail "No match."


rule_Set_Comprehension_ExplicitVarSizeWithMarker :: Rule
rule_Set_Comprehension_ExplicitVarSizeWithMarker = "set-comprehension{ExplicitVarSizeWithMarker}"
                                               `namedRule` theRule where
    theRule (Comprehension body [Generator (GenInExpr pat@(Single iPat) s)]) = do
        TypeSet{}                   <- typeOf s
        "ExplicitVarSizeWithMarker" <- representationOf s
        [marker, values]            <- downX1 s
        DomainMatrix index _        <- domainOf values
        let i = Reference iPat Nothing
        let f = lambdaToFunction pat body
        return ( "Vertical rule for set-comprehension, ExplicitVarSizeWithMarker representation"
               , const $
                    Comprehension (f [essence| &values[&i] |])
                        [ Generator (GenDomain pat index)
                        , Filter [essence| &i <= &marker |]
                        ]
               )
    theRule _ = fail "No match."


rule_Set_Comprehension_ExplicitVarSizeWithFlags :: Rule
rule_Set_Comprehension_ExplicitVarSizeWithFlags = "set-comprehension{ExplicitVarSizeWithFlags}"
                                               `namedRule` theRule where
    theRule (Comprehension body [Generator (GenInExpr pat@(Single iPat) s)]) = do
        TypeSet{}                   <- typeOf s
        "ExplicitVarSizeWithFlags"  <- representationOf s
        [flags, values]             <- downX1 s
        DomainMatrix index _        <- domainOf values
        let i = Reference iPat Nothing
        let f = lambdaToFunction pat body
        return ( "Vertical rule for set-comprehension, ExplicitVarSizeWithFlags representation"
               , const $
                    Comprehension (f [essence| &values[&i] |])
                        [ Generator (GenDomain pat index)
                        , Filter [essence| &flags[&i] |]
                        ]
               )
    theRule _ = fail "No match."


rule_Function_Eq :: Rule
rule_Function_Eq = "function-eq" `namedRule` theRule where
    theRule p = do
        (x,y)                    <- match opEq p
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return ( "Horizontal rule for function equality"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence|
                            (forAll &iPat in &x . &y(&i[1]) = &i[2])
                                /\
                            (forAll &iPat in &y . &x(&i[1]) = &i[2])
                        |]
               )


rule_Function_Comprehension_Function1D :: Rule
rule_Function_Comprehension_Function1D = "function-comprehension{Function1D}"
                                     `namedRule` theRule where
    theRule (Comprehension body [Generator (GenInExpr pat@(Single iPat) expr)]) = do
        let func             =  matchDef opToSet expr
        "Function1D"         <- representationOf func
        TypeFunction{}       <- typeOf func
        [values]             <- downX1 func
        DomainMatrix index _ <- domainOf values
        let i = Reference iPat Nothing
        let f = lambdaToFunction pat body
        return ( "Mapping over a function, Function1D representation"
               , const $
                   Comprehension
                       (f [essence| (&i, &values[&i]) |])
                       [Generator (GenDomain pat index)]
               )
    theRule _ = fail "No match."


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


rule_Function_Comprehension_Function1DPartial :: Rule
rule_Function_Comprehension_Function1DPartial = "function-comprehension{Function1DPartial}"
                                     `namedRule` theRule where
    theRule (Comprehension body [Generator (GenInExpr pat@(Single iPat) expr)]) = do
        let func             =  matchDef opToSet expr
        "Function1DPartial"  <- representationOf func
        TypeFunction{}       <- typeOf func
        [flags,values]       <- downX1 func
        DomainMatrix index _ <- domainOf values
        let i = Reference iPat Nothing
        let f = lambdaToFunction pat body
        return ( "Mapping over a function, Function1DPartial representation"
               , const $
                   Comprehension
                       (f [essence| (&i, &values[&i]) |])
                       [ Generator (GenDomain pat index)
                       , Filter [essence| &flags[&i] |]
                       ]
               )
    theRule _ = fail "No match."


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


rule_Function_Comprehension_FunctionNDPartial :: Rule
rule_Function_Comprehension_FunctionNDPartial = "function-comprehension{FunctionNDPartial}"
                                     `namedRule` theRule where

    theRule (Comprehension body [Generator (GenInExpr pat@(Single iPat) expr)]) = do
        let func                      =  matchDef opToSet expr
        "FunctionNDPartial"           <- representationOf func
        TypeFunction (TypeTuple ts) _ <- typeOf func
        [flags,values]                <- downX1 func
        valuesDom                     <- domainOf values
        let (indexDomain,_)           =  getIndices valuesDom

        let xArity          =  length ts
        let index x m 1     = make opIndexing m                     (make opIndexing x (fromInt 1))
            index x m arity = make opIndexing (index x m (arity-1)) (make opIndexing x (fromInt arity))
        let flagsIndexed  x = index x flags  xArity
        let valuesIndexed x = index x values xArity

        let i = Reference iPat Nothing
        let f = lambdaToFunction pat body
        return ( "Mapping over a function, Function1DPartial representation"
               , const $
                   Comprehension
                       (let val = valuesIndexed i
                        in  f [essence| (&i, &val) |])
                       [ Generator (GenDomain pat (DomainTuple indexDomain))
                       , Filter (flagsIndexed i)
                       ]
               )
    theRule _ = fail "No match."



rule_Relation_Eq :: Rule
rule_Relation_Eq = "relation-eq" `namedRule` theRule where
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
                   let (iPat, i) = quantifiedVar (fresh `at` 0)
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
        let unroll = foldl (make opIndexing)
        return ( "relation image, RelationAsMatrix representation"
               , const $ unroll m args
               )


rule_Relation_Comprehension_RelationAsMatrix :: Rule
rule_Relation_Comprehension_RelationAsMatrix = "relation-map_in_expr{RelationAsMatrix}" `namedRule` theRule where
    theRule (Comprehension body [Generator (GenInExpr pat expr)]) = do
        let f                  =  lambdaToFunction pat body
        let rel                =  matchDef opToSet expr
        TypeRelation{}         <- typeOf rel
        "RelationAsMatrix"     <- representationOf rel
        [m]                    <- downX1 rel
        mDom                   <- domainOf m
        let (mIndices, _)      =  getIndices mDom

        -- we need something like:
        -- Q i in rel . f(i)
        -- Q j in (indices...) , filter(f) . f(tuple)

        -- let out fresh = unroll m [] (zip [ quantifiedVar fr TypeInt | fr <- fresh ] mIndices)
        return ( "Vertical rule for map_in_expr for relation domains, RelationAsMatrix representation."
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)

                        lit = AbstractLiteral $ AbsLitTuple
                                    [ make opIndexing i (fromInt n) | n <- [1 .. length mIndices] ]
                        indexThis anyMatrix = make opIndexing' anyMatrix
                                    [ make opIndexing i (fromInt n) | n <- [1 .. length mIndices] ]

                    in  Comprehension (f lit)
                            [ Generator (GenDomain iPat (DomainTuple mIndices))
                            , Filter    (indexThis m)
                            ]
               )
    theRule _ = fail "No match."
