{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Conjure.UI.Model where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.ModelStats ( givens, finds, declarations, lettings )
import Conjure.Representations

import Data.Generics.Uniplate.Data ( rewriteBiM, uniplate, biplate )
import Data.Generics.Str ( Str )


data ModelGen = ModelGen
    { originalEssence :: Model
    , history :: [ModelInfo]
    }

initialise :: Model -> ModelGen
initialise m = ModelGen (addTrueConstraints m) []

-- | repeatedly call `nextModel` to generate all models
outputAllModels :: FilePath -> Int -> ModelGen -> IO ()
outputAllModels dir i gen = do
    putStrLn $ "Working on model #" ++ show i
    createDirectoryIfMissing True dir
    may <- nextModel gen
    case may of
        Nothing -> return ()
        Just (eprime,gen') -> do
            let filename = dir </> "model" ++ show i ++ ".eprime"
            writeFile filename (renderWide eprime)
            print $ vcat
                [ pretty sel <+> "out of" <+> pretty (show opts) <+> "~~" <+> pretty txt
                | Decision txt opts sel <- miTrail (mInfo eprime)
                ]
            outputAllModels dir (i+1) gen'

-- | given a `ModelGen`, which contains info about previously generated models,
--   generate the next model.
nextModel :: ModelGen -> IO (Maybe (Model, ModelGen))
nextModel (ModelGen essence pastInfos) = do
    meprime <- genNextModel essence pastInfos                       -- the workhorse
    case meprime of
        Nothing -> return Nothing                                   -- no more models to be generated
        Just eprime -> do
            let info = mInfo eprime
            return (Just ( languageEprime (oneSuchThat eprime)      -- return the newly generated model
                         , ModelGen essence (info:pastInfos)        -- and add its "info" to the log
                         ))

-- | given an initial essence model,
--   and a list of ModelInfo's describing previously generated models,
--   generate the next model
--   or return Nothing if no other model can be generated
genNextModel :: Model -> [ModelInfo] -> IO (Maybe Model)
genNextModel initialEssence pastInfos = do

    let lets  = lettings initialEssence
    let decls = declarations initialEssence

    let
        f :: (MonadState St m, MonadIO m) => Expression -> m Expression
        f (Reference nm) =
            case lookup nm decls of
                Nothing ->
                    case lookup nm lets of
                        Nothing -> do
                            liftIO $ putStrLn $ "what's this a reference to? " ++ show nm
                            return (Reference nm)
                        Just _  -> return (Reference nm)
                Just inpDom -> do
                    contexts <- gets stAscendants
                    explored <- gets alreadyExplored

                    let domOpts = reprOptions inpDom
                    let numOptions = [1 .. length domOpts]

                    case numOptions \\ explored of
                        [] -> do
                            modify $ \ st -> st { stExhausted = True }
                            liftIO $ putStrLn "exhausted=true"
                            -- st <- gets id
                            -- liftIO $ print st
                            return (Reference nm)
                        (numSelected:_) -> do
                            let domSelected = domOpts !! (numSelected - 1)
                            let descr = vcat
                                    $ ("Selecting representation for:" <+> pretty nm)
                                    : map (nest 4) (
                                       [ "Options: " <+> (vcat (map (nest 4 . pretty) domOpts))
                                       , "Selected:" <+> pretty domSelected
                                       , "# Options: " <+> pretty (show numOptions)
                                       , "# Explored:" <+> pretty (show explored)
                                       , "# Selected:" <+> pretty numSelected
                                       ] ++
                                       [ "Context #" <> pretty i <> ":" <+> either pretty pretty c
                                       | (i,c) <- zip allNats contexts
                                       ] )
                            modify $ addReprToSt nm domSelected
                            modify $ addDecisionToSt descr numOptions numSelected
                            liftIO $ print descr
                            return (Reference nm)
        f x = do
            exhausted <- gets stExhausted
            if exhausted
                then return x
                else do
                    gets stNbExpression >>= \ nb -> liftIO $ print $ "--" <+> pretty nb <> ":" <+> pretty (show x)
                    -- gets stAscendants   >>= \ xs -> liftIO $ print $ vcat [ "----" <+> pretty (show i) | i <- xs ]
                    -- liftIO $ putStrLn ""
                    modify $ \ st -> st { stNbExpression = 1 + stNbExpression st }
                    return x

    let initInfo = def { miGivens = map fst (givens initialEssence)
                       , miFinds  = map fst (finds  initialEssence)
                       }
    let pipeline =  tr f
                >=> ifNotExhausted (rewriteBiM $ firstOfRules [ rule_TrueIsNoOp
                                                              , rule_InlineFilterInsideMap
                                                              ]
                                   )
    (statements', st) <- runStateT (pipeline (mStatements initialEssence))
                                   (def { stCurrInfo = initInfo
                                        , stPastInfos = map miTrail pastInfos
                                        })

    if stExhausted st
        then return Nothing
        else do
            let model = initialEssence { mStatements = statements'
                                       , mInfo = stCurrInfo st
                                       }
            return (Just model)


class ExpressionContainer a where
    tr :: MonadState St m => (Expression -> m Expression) -> a -> m a

instance ExpressionContainer Statement where
    tr f x = do
        modify $ \ st -> st { stAscendants = Right x : stAscendants st }
        let (current, generate) = biplate x
        x' <- liftM generate $ mapM (tr f) (current :: Str Expression)
        modify $ \ st -> st { stAscendants = drop 1 (stAscendants st) }
        return x'

instance ExpressionContainer Expression where
    tr f x = do
        modify $ \ st -> st { stAscendants = Left x : stAscendants st }
        let (current, generate) = uniplate x
        x' <- liftM generate $ mapM (tr f) current
        modify $ \ st -> st { stAscendants = drop 1 (stAscendants st) }
        f x'

instance ExpressionContainer [Statement] where
    tr f = mapM (tr f)
        

data St = St
    { stNbExpression :: !Int
    , stReprsSoFar :: [ ( Name                                        -- for the declaration with this name
                        , ( Int                                       -- number of occurrences so far
                          , [Domain HasRepresentation Expression]     -- distinct reprs so far
                          ) ) ]
    , stAscendants :: [Either Expression Statement]
    , stCurrInfo :: !ModelInfo
    , stPastInfos :: [[Decision]]                                     -- each [Decision] is a trail of decisions
    , stExhausted :: Bool
    }
    deriving Show

instance Default St where
    def = St 0 [] [] def [] False

addReprToSt :: Name -> Domain HasRepresentation Expression -> St -> St
addReprToSt nm dom st = st { stCurrInfo = addToInfo (stCurrInfo st) }
    where addToInfo i = i { miRepresentations = nub $ (nm, dom) : miRepresentations i }

addDecisionToSt :: Doc -> [Int] -> Int -> St -> St
addDecisionToSt doc opts selected st =
    st { stCurrInfo = addToInfo (stCurrInfo st)
       , stPastInfos = advancePastInfos (stPastInfos st)
       }
    where addToInfo i = i { miTrail = miTrail i ++ [dec] }
          dec = Decision (stringToText $ renderWide doc) opts selected
          advancePastInfos trails =
              [ tail trail                      -- we drop the head to advance in the trail
              | trail <- trails
              , not (null trail)                -- check if this trail is already exhausted
              , let this = head trail
              , dDecision this == selected      -- only those which picked the same option are relevant.
              ]

alreadyExplored :: St -> [Int]
alreadyExplored st =
    [ dDecision (head trail)
    | trail <- stPastInfos st
    , not (null trail)
    ]

ifNotExhausted :: MonadState St m => (a -> m a) -> a -> m a
ifNotExhausted f x = do
    exhausted <- gets stExhausted
    if exhausted
        then return x
        else f x

-- | For every parameter and decision variable add a true-constraint.
--   A true-constraint has no effect, other than forcing Conjure to produce a representation.
--   It can be used to make sure that the decision variable doesn't get lost when it isn't mentioned anywhere.
--   It can also be used to produce "extra" representations.
--   Currently this function will add a true for every declaration, no matter if it is mentioned or not.
addTrueConstraints :: Model -> Model
addTrueConstraints m =
    let
        declarationNames = map fst (declarations m)
        mkTrueConstraint nm = Op "true" [Reference nm]
        trueConstraints = map mkTrueConstraint declarationNames
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


firstOfRules :: Monad m => [Expression -> m (Maybe Expression)] -> Expression -> m (Maybe Expression)
firstOfRules [] _ = return Nothing
firstOfRules (r:rs) x = r x >>= maybe (firstOfRules rs x) (return . Just)


rule_TrueIsNoOp :: Monad m => Expression -> m (Maybe Expression)
rule_TrueIsNoOp = return . theRule
    where
        theRule (Op "true" _) = Just $ Constant $ ConstantBool True
        theRule _ = Nothing


rule_InlineFilterInsideMap :: Monad m => Expression -> m (Maybe Expression)
rule_InlineFilterInsideMap = return . theRule
    where
        theRule (Op "map_domain" [Lambda vBody body, Op "filter" [Lambda vGuard guard_, domain]]) =
            let
                fGuard  = lambdaToFunction vGuard guard_
                fBody   = lambdaToFunction vBody  body
                newBody = Lambda vBody (Op "/\\" [fGuard vBody, fBody vBody])
            in
                Just $ Op "map_domain" [newBody, domain]
        theRule _ = Nothing


