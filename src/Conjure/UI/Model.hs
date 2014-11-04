{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.UI.Model
    ( outputModels
    , pickFirst
    , interactive, interactiveFixedQs, interactiveFixedQsAutoA
    , allFixedQs
    , Strategy(..), LogRuleApplications(..)
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
                                   , opLeq, opFlatten, opToSet )

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
    deriving (Eq, Ord, Show, Read)

viewAuto :: Strategy -> (Strategy, Bool)
viewAuto (Auto s) = second (const True) (viewAuto s)
viewAuto s = (s, False)


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

data LogRuleApplications
    = LogNeither
    | LogSuccessful
    | LogFails
    | LogBoth
    deriving (Eq, Ord, Show, Read)


outputModels
    :: (MonadIO m, MonadFail m, MonadLog m)
    => FilePath -> Int
    -> LogRuleApplications
    -> Driver -> Model
    -> m ()
outputModels dir i ruleLog driver model = do
    liftIO $ createDirectoryIfMissing True dir
    Pipes.foldM (\ j eprime -> liftIO $ do
                        let filename = dir </> "model" ++ paddedNum j ++ ".eprime"
                        writeFile filename (renderWide eprime)
                        return (j+1)
                )
                (return i)
                (const $ return ())
                (toCompletion ruleLog driver model)


toCompletion
    :: (MonadIO m, MonadFail m, MonadLog m)
    => LogRuleApplications
    -> Driver -> Model
    -> Pipes.Producer Model m ()
toCompletion ruleLog driver m = do
    m2 <- prologue m
    logInfo $ modelInfo m2
    loopy m2
    where
        loopy model = do
            qs <- remaining ruleLog model
            if null qs
                then do
                    model' <- epilogue model
                    Pipes.yield model'
                else do
                    nextModels <- driver qs
                    mapM_ loopy nextModels


remaining
    :: (Functor m, Applicative m, Monad m, MonadLog m)
    => LogRuleApplications
    -> Model
    -> m [Question]
remaining ruleLog model = do
    let freshNames' = freshNames model
    let modelZipper = fromJustNote "Creating the initial zipper." (zipperBi model)
    questions <- fmap catMaybes $ forM (allContexts modelZipper) $ \ x -> do
        ys <- applicableRules ruleLog (hole x)
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
    logInfo ("Selecting the only option:" <+> doc)
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
    => LogRuleApplications
    -> Expression
    -> m [(Doc, RuleResult)]
applicableRules LogNeither x =
    concat <$> sequence [ do res <- runMaybeT (rApply r x)
                             return (map (rName r,) (concat (maybeToList res)))
                        | r <- allRules ]
applicableRules l x = do
    let logFail =
            case l of
                LogFails      -> logInfo
                LogBoth       -> logInfo
                _             -> const (return ())
    let logSuccess =
            case l of
                LogSuccessful -> logInfo
                LogBoth       -> logInfo
                _             -> const (return ())

    mys <- sequence [ do mres <- runExceptT (rApply r x)
                         return (rName r, mres :: Either Doc [RuleResult])
                    | r <- allRules ]
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


allRules :: [Rule]
allRules =
    [ rule_ChooseRepr

    , rule_TrueIsNoOp
    , rule_ToIntIsNoOp
    , rule_SingletonAnd

    , rule_DontCareBool
    , rule_DontCareInt
    , rule_DontCareTuple
    , rule_DontCareMatrix
    , rule_DontCareSet

    , rule_ComplexLambda
    , rule_InlineFilterInsideMap_And
    , rule_InlineFilterInsideMap_Or
    , rule_InlineFilterInsideMap_Sum

    , rule_TupleIndex
    , rule_TupleEq
    , rule_TupleLt
    , rule_TupleLeq
    , rule_MapOverDomain_Tuple

    , rule_MatrixEq
    , rule_MatrixLt
    , rule_MatrixLeq

    , rule_SetEq
    , rule_SetSubsetEq
    , rule_SetLt
    , rule_SetLeq

    , rule_Set_MapInExpr_Explicit
    , rule_Set_MapInExpr_Occurrence
    , rule_Set_MapInExpr_ExplicitVarSizeWithMarker
    , rule_Set_MapInExpr_ExplicitVarSizeWithFlags

    , rule_SetIn_Explicit
    , rule_SetIn_Occurrence
    , rule_SetIn_ExplicitVarSizeWithMarker
    , rule_SetIn_ExplicitVarSizeWithFlags

    , rule_FunctionEq

    , rule_Function_Image_Function1D
    , rule_Function_MapInExpr_Function1D

    , rule_Function_Image_Function1DPartial
    , rule_Function_MapInExpr_Function1DPartial
    , rule_Function_InDefined_Function1DPartial

    , rule_Function_Image_FunctionNDPartial

    , rule_RelationEq
    , rule_Relation_In_RelationAsMatrix
    , rule_Relation_MapInExpr_RelationAsMatrix

    ]


rule_ChooseRepr :: Rule
rule_ChooseRepr = Rule "choose-repr" theRule where

    theRule (Reference nm (Just (DeclNoRepr forg _ inpDom))) = do
        let domOpts = reprOptions inpDom
        when (null domOpts) $
            bug $ "No representation matches this beast:" <++> pretty inpDom
        return [ (msg, const out, hook)
               | dom <- domOpts
               , let msg = "Selecting representation for" <+> pretty nm <> ":" <++> pretty dom
               , let out = Reference nm (Just (DeclHasRepr forg nm dom))
               , let hook = mkHook forg nm dom
               ]
    theRule _ = return []

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


rule_InlineFilterInsideMap_And :: Rule
rule_InlineFilterInsideMap_And = "inline-filter-inside-map-and" `namedRule` theRule where
    theRule p = ruleGen_InlineFilterInsideMap opAnd (\ x y -> make opImply x y ) p

rule_InlineFilterInsideMap_Or :: Rule
rule_InlineFilterInsideMap_Or = "inline-filter-inside-map-or" `namedRule` theRule where
    theRule p = ruleGen_InlineFilterInsideMap opOr (\ x y -> make opAnd [x,y] ) p

rule_InlineFilterInsideMap_Sum :: Rule
rule_InlineFilterInsideMap_Sum = "inline-filter-inside-map-sum" `namedRule` theRule where
    theRule p = ruleGen_InlineFilterInsideMap opSum (\ b x -> make opTimes (make opToInt b) x ) p

ruleGen_InlineFilterInsideMap
    :: MonadFail m
    => (Proxy m -> (a, b -> m [Expression]))
    -> (Expression -> Expression -> Expression)
    -> b
    -> m (Doc, [Name] -> Expression)
ruleGen_InlineFilterInsideMap opQ opSkip p = do
    [x]                             <- match opQ p
    (Lambda f1@Single{} f2, rest  ) <- match opMapOverDomain x
    (Lambda g1@Single{} g2, domain) <- match opFilter rest
    ty                              <- typeOf domain

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


rule_SetIn_Explicit :: Rule
rule_SetIn_Explicit = "set-in{Explicit}" `namedRule` theRule where
    theRule p = do
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
    theRule p = do
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


rule_SetIn_Occurrence :: Rule
rule_SetIn_Occurrence = "set-in{Occurrence}" `namedRule` theRule where
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


rule_SetIn_ExplicitVarSizeWithMarker :: Rule
rule_SetIn_ExplicitVarSizeWithMarker = "set-in{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule p = do
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
    theRule p = do
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
    theRule p = do
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
    theRule p = do
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
        (Lambda lPat lBody, f) <- match opMapInExpr p
        "Function1D"           <- representationOf f
        TypeFunction fr _      <- typeOf f
        [values]               <- downX1 f
        DomainMatrix index _   <- domainOf values
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
        (Lambda lPat lBody, f) <- match opMapInExpr p
        "Function1DPartial"    <- representationOf f
        TypeFunction fr _      <- typeOf f
        [flags,values]         <- downX1 f
        DomainMatrix index _   <- domainOf values
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
        TypeTuple ts        <- typeOf x
        let xArity          =  length ts
        [flags,values]      <- downX1 f
        let index m 1     = make opIndexing m                   (make opIndexing x (fromInt 1))
            index m arity = make opIndexing (index m (arity-1)) (make opIndexing x (fromInt arity))
        let flagsIndexed  = index flags  xArity
        let valuesIndexed = index values xArity
        return ( "Function image, Function1DPartial representation"
               , const [essence| { &valuesIndexed
                                 @ such that &flagsIndexed
                                 } |]
               )
    theRule _ = fail "No match."


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

rule_Relation_In_RelationAsMatrix :: Rule
rule_Relation_In_RelationAsMatrix = "relation-in{RelationAsMatrix}" `namedRule` theRule where
    theRule [essence| &x in toSet(&rel) |] = do
        TypeRelation{} <- typeOf rel
        return ( "relation membership to existential quantification"
               , \ fresh ->
                   let (iPat, i) = quantifiedVar (fresh `at` 0) TypeInt
                   in  [essence| exists &iPat in toSet(&rel) . &i = &x |]
               )
    theRule _ = fail "No match."


rule_Relation_MapInExpr_RelationAsMatrix :: Rule
rule_Relation_MapInExpr_RelationAsMatrix = "relation-map_in_expr{RelationAsMatrix}" `namedRule` theRule where
    theRule p = do
        (Lambda f1 f2, s)      <- match opMapInExpr p
        let f                  =  lambdaToFunction f1 f2
        r                      <- match opToSet s
        TypeRelation{}         <- typeOf r
        "RelationAsMatrix"     <- representationOf r
        [m]                    <- downX1 r
        mDom                   <- domainOf m
        let (mIndices, _)      =  getIndices mDom

        let unroll val [((pat,_),index)]
                = make opMapOverDomain (Lambda pat (f val))
                                       (Domain index)
            unroll val (((pat,_),index):rest)
                = let val' = val
                  in  make opFlatten $
                          make opMapOverDomain (Lambda pat (unroll val' rest))
                                               (Domain index)
            unroll _ _ = bug "rule_Relation_MapInExpr_RelationAsMatrix.unroll []"
            
        let out fresh = unroll m (zip [ quantifiedVar fr TypeInt | fr <- fresh ] mIndices)
        return ( "Vertical rule for map_in_expr for relation domains, RelationAsMatrix representation."
               , out
               )

