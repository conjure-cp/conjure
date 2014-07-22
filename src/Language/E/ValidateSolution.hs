{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.ValidateSolution
    ( validateSolution
    , validateSolutionPure
    ) where

import qualified Data.HashMap.Strict as M

import Bug
import Language.E
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Language.E.Pipeline.ExplodeStructuralVars ( explodeStructuralVars )
import Language.E.Pipeline.HandlingEnums ( handleEnums )
import Language.E.Pipeline.HandlingUnnameds ( handleUnnameds )
import Language.E.Pipeline.InlineLettings ( inlineLettings )
import Language.E.Pipeline.NoTuples ( allNoTuplesSpec )

import Language.E.Pipeline.ReadIn(readSpecFromFile)

type Essence  = Spec
type Param    = Maybe Spec
type Solution = Spec
type Dom = E

validateSolution :: Essence -> Param -> Solution -> IO ()
validateSolution essence param solution =
    if validateSolutionPure essence param solution
        then return ()
        else error "Not a valid solution."


-- this will return True is it's valid, False if not
-- the validator might use error
validateSolutionPure :: Essence -> Param -> Solution -> Bool
validateSolutionPure essence param solution =
    let
        (mresult, _logs) = runCompESingle "validating solution" helper
    in
        case mresult of
            Left  x      -> userErr x
            Right result -> result

    where


        helper = do

            case param of
                Nothing -> return ()
                Just (Spec _ s) -> mapM_ introduceStuff (statementAsList s)
            -- bindersDoc >>= mkLog "binders 1"

            case solution of
                Spec _ s        -> mapM_ introduceStuff (statementAsList s)
            -- bindersDoc >>= mkLog "binders 2"

            let essenceCombined =
                    case (essence, param) of
                        (Spec l s, Just (Spec _ p)) -> Spec l (listAsStatement $ statementAsList p ++ statementAsList s)
                        _ -> essence

            let pipeline0 = recordSpec "init"
                    >=> explodeStructuralVars   >=> recordSpec "explodeStructuralVars"
                    >=> inlineLettings          >=> recordSpec "inlineLettings"
                    >=> fullyInline             >=> recordSpec "fullyInline"
                    >=> stripDecls              >=> recordSpec "stripDecls"
                    >=> handleEnums             >=> recordSpec "handleEnums"
                    >=> handleUnnameds          >=> recordSpec "handleUnnameds"
                    >=> inlineLettings          >=> recordSpec "inlineLettings"
                    >=> stripDecls              >=> recordSpec "stripDecls"
                    >=> allNoTuplesSpec         >=> recordSpec "allNoTuplesSpec"
                    >=> fullyEvaluate           >=> recordSpec "fullyEvaluate"

            Spec _ s <- pipeline0 essenceCombined

            let checks = map isPartOfValidSolution (statementAsList s)
            if all isJust checks
                then return (and $ catMaybes checks)
                else bug $ vcat [ "Cannot fully evaluate."
                                , pretty s
                                , prettyAsTree s
                                , prettyAsPaths s
                                ]


validateSolutionPureNew :: Essence -> Solution -> (Bool,LogTree)
validateSolutionPureNew essence solution =
    let
        (mresult, _logs) = runCompESingle "validating solution" helper
    in
        case mresult of
            Left  x      -> userErr x
            Right b -> (b, _logs)

    where
    helper = do
        case solution of
            Spec _ s        -> mapM_ introduceStuff (statementAsList s)
        bindersDoc >>= mkLog "binders 2"

        let pipeline0 = recordSpec "init"
                >=> explodeStructuralVars   >=> recordSpec "explodeStructuralVars"
                >=> inlineLettings          >=> recordSpec "inlineLettings"

        let pipeTest1 = recordSpec "init2"
                >=> fullyInline             >=> recordSpec "fullyInline"
                >=> stripDecls              >=> recordSpec "stripDecls"
                >=> handleEnums             >=> recordSpec "handleEnums"
                >=> handleUnnameds          >=> recordSpec "handleUnnameds"
                >=> inlineLettings          >=> recordSpec "inlineLettings"
                >=> stripDecls              >=> recordSpec "stripDecls"
                >=> allNoTuplesSpec         >=> recordSpec "allNoTuplesSpec"
                >=> fullyEvaluate           >=> recordSpec "fullyEvaluate"


        inlined <-  pipeline0 essence
        mkLog "inlined" (pretty inlined)
        validateSpec inlined


        (Spec _ s) <- pipeTest1 inlined
        let checks = map isPartOfValidSolution (statementAsList s)
        if all isJust checks
            then return (and $ catMaybes checks)
            else bug $ vcat [ "Cannot fully evaluate."
                            , pretty s
                            , prettyAsTree s
                            , prettyAsPaths s
                            ]


validateSpec :: MonadConjure m => Spec -> m ()
validateSpec spec = do
    bs <- gets binders
    let finds = pullFinds spec
    mkLog "binders" $ vcat $ map pretty bs
    mkLog "finds" $ vcat $ map pretty finds

    let validateBinder bs@(Binder name val) | Just f <- lookup name finds  = do
            mkLog "dom" $ pretty f
            mkLog "val" $ pretty val
            res <-  validateVal f val
            return $ case res of
                Just doc -> Just $ vcat [
                     "Error for: " <+> pretty bs
                    , "Domain: " <+> pretty f
                    , doc
                    , "---"]
                Nothing -> Nothing

    docs <-  mapM validateBinder bs
    case catMaybes docs of
        [] -> return ()
        xs -> bug $ vcat xs

validateVal :: MonadConjure m => Dom -> E -> m (Maybe Doc)
validateVal
    dom@[dMatch| matrix indexed by [&irDom] of &innerDom |]
    val@[xMatch| vs := value.matrix.values
           | [ir] := value.matrix.indexrange |]
    = do

    -- Checking the index range
    irSize <- domSize ir
    irDomSize <- domSize irDom
    mkLog "irSize" . pretty $ irSize
    mkLog "indexSize" . pretty $ irDomSize



    check <- toBool [eMake| &irSize = &irDomSize |]
    d1 <- case check of
        Right (True, _) -> return Nothing
        Right (False, _) -> do
           return . Just $ vcat [ "Index range difference sizes for matrix"
                        , pretty dom
                        , pretty val
                        ]

    -- TODO domain written in different ways e.g. 1,2,3 insead of 1..3
    d2 <- case ir == irDom of
       True ->  return Nothing
       False -> do
           return . Just $ vcat [ "Index ranges not the same for matrix"
                        , pretty dom
                        , pretty val
                        ]

    let vsSize = mkInt $ genericLength vs
    check2 <- toBool [eMake| &vsSize = &irDomSize |]
    d3 <- case check2 of
       Right (True, _) ->  return Nothing
       Right (False, _) -> do
           return . Just $ hang "Invaild number of matrix elements" 4 $ vcat
                        [
                          pretty dom
                        , pretty val
                        ]

    case joinDoc [d1,d2,d3] of
        Just s -> return . Just $ s
        Nothing -> do
            vsDocs <- mapM (validateVal innerDom) vs
            return $ joinDoc vsDocs

validateVal d@[xMatch| rs := domain.int.ranges |]
            v@[xMatch| [Prim (I i)] :=  value.literal |]  = do
    case any (inDomain i) rs of
        True -> return Nothing
        False -> return . Just $ vcat [
             "Value not in int domain"
            ,pretty d
            ,pretty v
            ]

    where
        inDomain k [xMatch| [Prim (I j)] := range.single.value.literal |] = j == k
        inDomain k [xMatch| [Prim (I l), Prim(I u)] := range.fromTo.value.literal |] =
            k >= l && k <= u
        inDomain _ _ = False

validateVal [xMatch| _ := domain.bool |] [xMatch| [Prim (B _)] :=  value.literal |] = return Nothing

validateVal dom es = bug $ vcat [
      "domain:" <+> pretty dom
    , "val:"    <+> pretty es
    ]

joinDoc :: [Maybe Doc] -> Maybe Doc
joinDoc ds = case catMaybes ds of
    [] -> Nothing
    xs -> Just . vcat $ xs

getInt :: E -> Integer
getInt  [xMatch| [Prim (I j)] :=  value.literal  |] = j

mkInt :: Integer -> E
mkInt j =  [xMake| value.literal := [Prim (I j)] |]

pullFinds :: Spec -> [(Text,E)]
pullFinds (Spec _ x) = mapMaybe pullFind (statementAsList x)
    where pullFind [xMatch| [name] := topLevel.declaration.find.name
                          | [dom]  := topLevel.declaration.find.domain |] = Just (getName name,dom)
          pullFind _ = Nothing

getName :: E -> Text
getName [xMatch| [Prim (S name)] := reference  |] = name
getName e = error . show $ "getName: not a name" <+> pretty e

_aa :: FilePath -> FilePath -> IO ()
_aa e s = do
    ee <- readSpecFromFile e
    ss <- readSpecFromFile s
    let (b, logs) = validateSolutionPureNew ee ss
    putStrLn . show .pretty $ b
    putStrLn . show . pretty $ logs


isPartOfValidSolution :: E -> Maybe Bool
isPartOfValidSolution [xMatch| [Prim (B b)] := topLevel.suchThat.value.literal |] = Just b
isPartOfValidSolution [xMatch| [Prim (B b)] := topLevel.where   .value.literal |] = Just b
isPartOfValidSolution [xMatch| _ := topLevel.objective   |] = Just True
isPartOfValidSolution [xMatch| _ := topLevel.branchingOn |] = Just True
isPartOfValidSolution _ = Nothing


fullyEvaluate :: MonadConjure m => Spec -> m Spec
fullyEvaluate
    = recordSpec "entering fullyEvaluate"
    >=> explodeStructuralVars           >=> recordSpec "explodeStructuralVars"
    >=> fullySimplifySpec               >=> recordSpec "fullySimplifySpec"
    >=> return . atMostOneSuchThat True >=> recordSpec "atMostOneSuchThat"


fullyInline :: MonadConjure m => Spec -> m Spec
fullyInline inp = do
    bs <- gets binders
    typesMap <- fmap M.fromList
        $ forM (getDecls inp)
        $ \ (nm, dom) -> do
            itsType <- typeOf dom
            return (nm, itsType)
    let bindingsMap = M.fromList [ (nm, val) | Binder nm val <- bs ]
    inliner typesMap bindingsMap inp

    where

        inliner typesMap bindingsMap (Spec v s) = Spec v <$> f s
            where
                f x@[xMatch| [Prim (S nm)] := reference |]
                    = case (M.lookup nm typesMap, M.lookup nm bindingsMap) of
                        (_, Just [xMatch| _ := type |]) -> return x
                        (Just theType, Just binding) -> do
                            theType' <- typeOf x
                            if typeUnify theType theType'
                                then f [xMake| typed.left               := [binding]
                                             | typed.right.domainInExpr := [theType]
                                             |]
                                else error $ show $ vcat [ "Type mismatch for" <+> pretty nm
                                                         , "    Expected:" <+> pretty theType
                                                         , "    Found:   " <+> pretty theType'
                                                         ]
                        (Nothing, Just binding) -> do
                            theType <- typeOf x
                            f [xMake| typed.left               := [binding]
                                    | typed.right.domainInExpr := [theType]
                                    |]
                        _           -> return x
                f x@[xMatch| [_] := structural.single.reference |] = return x
                f x@(Prim {}) = return x
                f (Tagged t xs) = Tagged t <$> mapM f xs
                f x@(EOF {}) = return x
                f (StatementAndNext this next) = StatementAndNext <$> f this <*> f next

        getDecls (Spec _ x) =
            [ (nm, dom)
            | [xMatch| [Prim (S nm)] := topLevel.declaration.find.name.reference
                     | [dom]         := topLevel.declaration.find.domain
                     |] <- statementAsList x
            ] ++
            [ (nm, dom)
            | [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference
                     | [dom]         := topLevel.declaration.given.domain
                     |] <- statementAsList x
            ]


stripDecls :: MonadConjure m => Spec -> m Spec
stripDecls (Spec language stmt) = return $ Spec language $ listAsStatement
    [ i
    | i <- statementAsList stmt
    , case i of
        [xMatch| _ := topLevel.declaration    |] -> False
        [xMatch| _ := topLevel.letting.domain |] -> False
        _ -> True
    ]


