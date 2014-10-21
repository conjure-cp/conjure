{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}

module Conjure.UI.Model
    ( outputOneModel
    , interactive, pickFirst
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Ops hiding ( opOr, opAnd, opIn, opEq, opLt, opMapOverDomain )
import Conjure.Language.Lenses
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.DomainOf
import Conjure.CState
import Conjure.Representations

import Data.Generics.Uniplate.Zipper as Zipper ( Zipper, zipperBi, fromZipper, hole, replaceHole, up )
import qualified Data.Text as T



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

type Driver = (forall m . (MonadIO m, MonadFail m) => [Question] -> m Model)

type RuleResult = ( Doc           -- describe this transformation
                  , Expression    -- the result
                  )
data Rule = Rule
    { rName  :: Doc
    , rApply :: forall m . (Functor m, MonadIO m) => Expression -> m [RuleResult]
    }

namedRule
    :: Doc
    -> (forall m . (Functor m, MonadIO m) => Expression -> m (Maybe RuleResult))
    -> Rule
namedRule nm f = Rule
    { rName = nm
    , rApply = \ x -> liftM maybeToList (f x)
    } 


remaining :: (Functor m, Applicative m, MonadIO m) => Model -> m [Question]
remaining model = do
    let modelZipper = fromJustNote "Creating the initial zipper." (zipperBi model)
    fmap catMaybes $ forM (allContexts modelZipper) $ \ x -> do
        ys <- applicableRules (hole x)
        return $ if null ys
            then Nothing
            else Just Question
                     { qHole = hole x
                     , qAscendants = tail (ascendants x)
                     , qAnswers =
                         [ Answer
                             { aText = ruleName <> ":" <+> ruleText
                             , aAnswer = ruleResult
                             , aFullModel = fromZipper (replaceHole ruleResult x)
                             }
                        | (ruleName, (ruleText, ruleResult)) <- ys
                        ]
                     }


toCompletion :: (MonadIO m, MonadFail m) => Driver -> Model -> m Model
toCompletion driver model = do
    qs <- remaining model
    if null qs
        then model |> oneSuchThat |> languageEprime |> return
        else do
            nextModel <- driver qs
            toCompletion driver nextModel


outputOneModel :: Driver -> FilePath -> Int -> Model -> IO ()
outputOneModel driver dir i essence = do
    createDirectoryIfMissing True dir
    eprime <- toCompletion driver (addTrueConstraints essence)
    let filename = dir </> "model" ++ show i ++ ".eprime"
    writeFile filename (renderWide eprime)


pickFirst :: MonadFail m => [Question] -> m Model
pickFirst [] = fail "pickFirst: No questions!"
pickFirst (question:_) =
    case qAnswers question of
        [] -> fail "pickFirst: No answers!"
        (answer:_) -> return (aFullModel answer)


interactive :: MonadIO m => [Question] -> m Model
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

    forM_ (zip allNats (qAnswers pickedQ)) $ \ (nA,a) -> do
        print $ nest 4 $ "Answer" <+> pretty nA <> ":" <+> vcat [ pretty (aText a)
                                                                , pretty (aAnswer a) ]
        -- print (nest 8 $ pretty (aFullModel a))
    putStr "Pick answer: "
    pickedAIndex <- readNote "Expecting an integer." <$> getLine
    let pickedA = (qAnswers pickedQ) `at` (pickedAIndex - 1)

    let pickedModel = aFullModel pickedA
    putStrLn "Current model:"
    print $ nest 8 $ pretty pickedModel
    return pickedModel


addReprToSt :: Name -> Domain HasRepresentation Expression -> CState -> CState
addReprToSt nm dom st = st { stCurrInfo = addToInfo (stCurrInfo st)
                           , stAllReprs = nub $ (nm, dom) : inners ++ stAllReprs st
                           }
    where
        addToInfo i = i { miRepresentations = nub $ (nm, dom) : miRepresentations i }
        inners = case mkInners (nm,dom) of
            Left err -> bug err
            Right res -> res
        mkInners p = do
            mmids <- downD1 p
            case mmids of
                Nothing -> return []
                Just mids -> do
                    lows <- mapM mkInners mids
                    return (concat (mids:lows))
            
addDecisionToSt :: Doc -> [Int] -> Int -> CState -> CState
addDecisionToSt doc opts selected st =
    st { stCurrInfo = addToInfo (stCurrInfo st)
       }
    where addToInfo i = i { miTrail = miTrail i ++ [dec] }
          dec = Decision (doc |> renderWide |> stringToText |> T.lines) opts selected

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
        mkTrueConstraint k nm dom = Op $ MkOpTrue $ OpTrue [Reference nm (Just (DeclNoRepr k nm dom))]
        trueConstraints = [ mkTrueConstraint k nm d
                          | Declaration (FindOrGiven k nm d) <- mStatements m
                          ]
    in
        m { mStatements = mStatements m ++ [SuchThat trueConstraints] }


oneSuchThat :: Model -> Model
oneSuchThat m = m { mStatements = others ++ [SuchThat suchThat] }
    where collect (SuchThat s) = ([], s)
          collect s = ([s], [])
          (others, suchThats) = mStatements m
                |> map collect
                |> mconcat
                |> second (filter (/= Constant (ConstantBool True)))
                |> second nub
          suchThat = if null suchThats
                      then [Constant (ConstantBool True)]
                      else suchThats


updateDeclarations :: (Functor m, MonadState CState m) => [Statement] -> m [Statement]
updateDeclarations statements = do
    reprs <- gets stAllReprs
    flip concatMapM statements $ \ st ->
        case st of
            Declaration (FindOrGiven h nm _) ->
                case [ d | (n,d) <- reprs, n == nm ] of
                    [] -> bug $ "No representation chosen for: " <+> pretty nm
                    domains -> flip concatMapM domains $ \ domain -> do
                        mouts <- runExceptT $ downD (nm, domain)
                        case mouts of
                            Left err -> bug err
                            Right outs -> return [Declaration (FindOrGiven h n (forgetRepr d)) | (n,d) <- outs]
            _ -> return [st]


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
    , rule_InlineFilterInsideMap
    , rule_TupleIndex
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
        return [ (msg, out)
               | dom <- domOpts
               , let msg = "Selecting representation for" <+> pretty nm <> ":" <+> pretty dom
               , let out = Reference nm (Just (DeclHasRepr ty nm dom))
               ]
    theRule _ = return []


rule_TrueIsNoOp :: Rule
rule_TrueIsNoOp = "true-is-noop" `namedRule` (return . theRule)
    where
        theRule (Op (MkOpTrue (OpTrue _))) = Just ( "Remove the argument from true."
                                                  , Constant $ ConstantBool True
                                                  )
        theRule _ = Nothing


rule_ToIntIsNoOp :: Rule
rule_ToIntIsNoOp = "toInt-is-noop" `namedRule` (return . theRule)
    where
        theRule (Op (MkOpToInt (OpToInt b))) = Just ( "Remove the toInt wrapper, it is implicit in SR."
                                                    , b
                                                    )
        theRule _ = Nothing


rule_InlineFilterInsideMap :: Rule
rule_InlineFilterInsideMap = "inline-filter-inside-map" `namedRule` (return . theRule)
    where
        theRule (Op (MkOpMapOverDomain (OpMapOverDomain
                        (Lambda vBody body)
                        (Op (MkOpFilter (OpFilter
                                (Lambda vGuard guard_)
                                domain)))))) =
            let
                fGuard  = lambdaToFunction vGuard guard_
                fBody   = lambdaToFunction vBody  body
                newBody = Lambda vBody (Op $ MkOpAnd $ OpAnd [fGuard vBody, fBody vBody])
            in
                Just ( "Inlining the filter."
                     , Op $ MkOpMapOverDomain $ OpMapOverDomain newBody domain
                     )
        theRule _ = Nothing


rule_TupleIndex :: Rule
rule_TupleIndex = "tuple-index" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (t,i)       <- match opIndexing p
        TypeTuple{} <- typeOf t
        iInt        <- match constantInt i
        ts          <- downX1 t
        return ( "Tuple indexing on:" <+> pretty p
               , atNote "Tuple indexing" ts (iInt-1)
               )


rule_SetIn_Explicit :: Rule
rule_SetIn_Explicit = "set-in{Explicit}" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (x,s)                <- match opIn p
        TypeSet{}            <- typeOf s
        "Explicit"           <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf (Proxy :: Proxy ()) m
        -- exists i : index . m[i] = x
        -- or([ m[i] = x | i : index ])
        -- or(map_domain(i --> m[i]))
        let body = mkLambda "i" TypeInt $ \ i ->
                        make opEq (make opIndexing m i) x
        return ( "Vertical rule for set-in, Explicit representation."
               , make opOr [make opMapOverDomain body (Domain index)]
               )


rule_SetIn_Occurrence :: Rule
rule_SetIn_Occurrence = "set-in{Occurrence}" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (x,s)                <- match opIn p
        TypeSet{}            <- typeOf s
        "Occurrence"         <- representationOf s
        [m]                  <- downX1 s
        return ( "Vertical rule for set-in, Occurrence representation"
               , make opIndexing m x
               )


rule_SetIn_ExplicitVarSizeWithMarker :: Rule
rule_SetIn_ExplicitVarSizeWithMarker = "set-in{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (x,s)                       <- match opIn p
        TypeSet{}                   <- typeOf s
        "ExplicitVarSizeWithMarker" <- representationOf s
        [marker,values]             <- downX1 s
        DomainMatrix index _        <- domainOf (Proxy :: Proxy ()) values
        -- exists i : index , i < marker. m[i] = x
        -- exists i : index . i < marker /\ m[i] = x
        -- or([ i < marker /\ m[i] = x | i : index ])
        -- or(map_domain(i --> i < marker /\ m[i] = x))
        let body = mkLambda "i" TypeInt $ \ i ->
                    make opAnd [ make opEq (make opIndexing values i) x
                               , make opLt i marker
                               ]
        return ( "Vertical rule for set-in, ExplicitVarSizeWithMarker representation"
               , make opOr [make opMapOverDomain body (Domain index)]
               )


rule_SetIn_ExplicitVarSizeWithFlags :: Rule
rule_SetIn_ExplicitVarSizeWithFlags = "set-in{ExplicitVarSizeWithFlags}" `namedRule` theRule where
    theRule p = runMaybeT $ do
        (x,s)                       <- match opIn p
        TypeSet{}                   <- typeOf s
        "ExplicitVarSizeWithFlags"  <- representationOf s
        [flags,values]              <- downX1 s
        DomainMatrix index _        <- domainOf (Proxy :: Proxy ()) values
        -- exists i : index , i < marker. m[i] = x
        -- exists i : index . i < marker /\ m[i] = x
        -- or([ i < marker /\ m[i] = x | i : index ])
        -- or(map_domain(i --> flags[i] /\ m[i] = x))
        let body = mkLambda "i" TypeInt $ \ i ->
                    make opAnd [ make opEq (make opIndexing values i) x
                               , make opIndexing flags i
                               ]
        return ( "Vertical rule for set-in, Occurrence representation"
               , make opOr [make opMapOverDomain body (Domain index)]
               )

