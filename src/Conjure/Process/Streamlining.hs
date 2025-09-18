{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.Streamlining
    ( streamlining
    , streamliningToStdout
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Compute.DomainOf ( domainOf )
import Conjure.Representations.Common ( mkBinRelCons, mkBinRelConsSoft )

-- containers
import Data.Set as S ( singleton )


streamliningToStdout ::
    MonadFailDoc m =>
    MonadLog m =>
    MonadUserError m =>
    NameGen m =>
    MonadIO m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m ()
streamliningToStdout model = do
    let
        whitespace '\t' = ' '
        whitespace '\n' = ' '
        whitespace ch   = ch

        showStr :: String -> Doc
        showStr = pretty . show

    streamliners <- streamlining model

    liftIO $ print $ prettyList prBraces ","
        [ showStr (show i) <> ":" <+> prBraces (vcat
            [ showStr "onVariable" <> ":" <+> showStr (show (pretty nm)) <> ","
            , showStr "groups"     <> ":" <+> prettyList prBrackets "," (map showStr (nub groups)) <> ","
            , showStr "constraint" <> ":" <+> showStr (map whitespace $ show $ pretty cons)
            ])
        | (i, (nm, (cons, groups))) <- zip allNats streamliners
        ]
    traceM $ show $ "Number of streamliners: " <+> pretty (length streamliners)


streamlining ::
    MonadFailDoc m =>
    MonadLog m =>
    MonadUserError m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> m [(Name, Streamliner)]
streamlining model = do
    concatForM (mStatements model) $ \case
        Declaration (FindOrGiven Find nm domain) -> do
            let ref = Reference nm (Just (DeclNoRepr Find nm domain NoRegion))
            streamliners <- streamlinersForSingleVariable ref
            -- traceM $ show $ vcat [ "Streamliners for --" <+> pretty statement
            --                      , vcat [ nest 4 (vcat (pretty x : map pretty gs)) | (x,gs) <- streamliners ]
            --                      ]
            return [ (nm, s) | s <- streamliners ]
        _ -> noStreamliner


type StreamlinerGroup = String

type Streamliner = (Expression, [StreamlinerGroup])

type StreamlinerGen m = Expression -> m [Streamliner]

mkStreamliner
    :: Monad m
    => StreamlinerGroup         -- the group label
    -> Expression               -- the streamlining constraint
    -> m [Streamliner]
mkStreamliner grp x = return [(x, [grp])]

noStreamliner :: Monad m => m [a]
noStreamliner = return []

attachGroup
    :: Monad m
    => [StreamlinerGroup]
    -> Expression
    -> m Streamliner
attachGroup grps x = return (x, grps)


-- given a reference to a top level variable, produce a list of all applicable streamliners
streamlinersForSingleVariable ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
streamlinersForSingleVariable x = concatMapM ($ x)
    [ intOdd
    , intEven
    , intLowerHalf
    , intUpperHalf

    , onTuple 1 streamlinersForSingleVariable
    , onTuple 2 streamlinersForSingleVariable
    , onTuple 3 streamlinersForSingleVariable
    , onTuple 4 streamlinersForSingleVariable

    , matrixByRowBucket streamlinersForSingleVariable
    , matrixByColBucket streamlinersForSingleVariable

    , matrixAll streamlinersForSingleVariable
    , matrixHalf streamlinersForSingleVariable
    , matrixAtMostOne streamlinersForSingleVariable
    , matrixApproxHalf streamlinersForSingleVariable
    , matrixMoreThanHalf streamlinersForSingleVariable
    , matrixLessThanHalf streamlinersForSingleVariable

    , setAll streamlinersForSingleVariable
    , setHalf streamlinersForSingleVariable
    , setAtMostOne streamlinersForSingleVariable
    , setApproxHalf streamlinersForSingleVariable
    , setMoreThanHalf streamlinersForSingleVariable
    , setLessThanHalf streamlinersForSingleVariable

    , relationCardinality

    , binRelAttributes

    , monotonicallyIncreasing
    , monotonicallyDecreasing
    , smallestFirst
    , largestFirst
    , commutative
    , nonCommutative
    , associative
    , onRange streamlinersForSingleVariable
    , onDefined streamlinersForSingleVariable
    -- , diagonal streamlinersForSingleVariable
    -- , prefix streamlinersForSingleVariable
    -- , postfix streamlinersForSingleVariable

    , parts streamlinersForSingleVariable
    , quasiRegular

    ]


------------------------------------------------------------------------------
-- Integers
------------------------------------------------------------------------------


-- given an integer expression (which can be a reference to a decision variable),
-- generate a constraint forcing it to be odd
intOdd ::
    MonadFailDoc m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
intOdd x = do
    ty <- typeOf x
    if typeUnify ty (TypeInt TagInt)
        then mkStreamliner "IntOddEven" [essence| &x % 2 = 1 |]
        else noStreamliner


intEven ::
    MonadFailDoc m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
intEven x = do
    ty <- typeOf x
    if typeUnify ty (TypeInt TagInt)
        then mkStreamliner "IntOddEven" [essence| &x % 2 = 0 |]
        else noStreamliner


intLowerHalf ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
intLowerHalf x = do
    ty <- typeOf x
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainInt _ [RangeBounded _lower upper] -> do
            -- traceM $ show $ "DomainInt " <+> pretty (lower, upper)
            if typeUnify ty (TypeInt TagInt)
                then mkStreamliner "IntLowHigh" [essence| &x <= 1 + (&upper -1) /2 |]
                else noStreamliner
        _ -> noStreamliner


intUpperHalf ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
intUpperHalf x = do
    ty <- typeOf x
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainInt _ [RangeBounded _lower upper] -> do
            -- traceM $ show $ "DomainInt " <+> pretty (lower, upper)
            if typeUnify ty (TypeInt TagInt)
                then mkStreamliner "IntLowHigh" [essence| &x > 1 + (&upper -1) /2 |]
                else noStreamliner
        _ -> noStreamliner


------------------------------------------------------------------------------
-- Matrices
------------------------------------------------------------------------------


matrixAll ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
matrixAll innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainMatrix indexDom innerDom -> do
            nm <- nextName "q"
            -- traceM $ show $ "matrixAll nm" <+> pretty nm
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))

                liftMatrix (Reference n _) | n == nm = [essence| &x[&ref] |]
                liftMatrix p = p

            innerConstraints <- transformBi liftMatrix <$> innerStreamliner ref
            -- traceM $ show $ "maybeInnerConstraint" <+> vcat (map pretty innerConstraints)
            forM innerConstraints $ \ (innerConstraint, grps) ->
                    attachGroup ("MatrixCardinality": grps) [essence| forAll &pat : &indexDom . &innerConstraint |]
        _ -> noStreamliner


matrixAtMostOne ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
matrixAtMostOne innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainMatrix indexDom innerDom -> do
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))

                liftMatrix (Reference n _) | n == nm = [essence| &x[&ref] |]
                liftMatrix p = p

            innerConstraints <- transformBi liftMatrix <$> innerStreamliner ref
            forM innerConstraints $ \ (innerConstraint, grps) ->
                attachGroup ("MatrixCardinality": grps) [essence| 1 >= sum &pat : &indexDom . toInt(&innerConstraint) |]
        _ -> noStreamliner


matrixHalf :: MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
matrixHalf innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainMatrix indexDom innerDom -> do
            let size = [essence| |`&indexDom`| |]
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))

                liftMatrix (Reference n _) | n == nm = [essence| &x[&ref] |]
                liftMatrix p = p

            innerConstraints <- transformBi liftMatrix <$> innerStreamliner ref
            forM innerConstraints $ \ (innerConstraint, grps) ->
                attachGroup ("MatrixCardinality": grps) [essence| &size / 2 = (sum &pat : &indexDom . toInt(&innerConstraint)) |]
        _ -> noStreamliner


matrixApproxHalf ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
matrixApproxHalf innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainMatrix indexDom innerDom -> do
            let size = [essence| |`&indexDom`| |]
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))

                liftMatrix (Reference n _) | n == nm = [essence| &x[&ref] |]
                liftMatrix p = p

            innerConstraints <- transformBi liftMatrix <$> innerStreamliner ref
            forM innerConstraints $ \ (innerConstraint, grps) ->
                attachGroup ("MatrixCardinality": grps) [essence|
                    (&size/2) + 1 >= (sum &pat : &indexDom . toInt(&innerConstraint)) /\
                    (&size/2 -1) <= (sum &pat : &indexDom . toInt(&innerConstraint))
                    |]
        _ -> noStreamliner


matrixMoreThanHalf ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
matrixMoreThanHalf innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainMatrix indexDom innerDom -> do
            let size = [essence| |`&indexDom`| |]
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))

                liftMatrix (Reference n _) | n == nm = [essence| &x[&ref] |]
                liftMatrix p = p

            innerConstraints <- transformBi liftMatrix <$> innerStreamliner ref
            forM innerConstraints $ \ (innerConstraint, grps) ->
                attachGroup ("MatrixCardinality": grps) [essence|
                    (&size/2) <= (sum &pat : &indexDom . toInt(&innerConstraint))
                    |]
        _ -> noStreamliner


matrixLessThanHalf ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
matrixLessThanHalf innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainMatrix indexDom innerDom -> do
            let size = [essence| |`&indexDom`| |]
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))

                liftMatrix (Reference n _) | n == nm = [essence| &x[&ref] |]
                liftMatrix p = p

            innerConstraints <- transformBi liftMatrix <$> innerStreamliner ref
            forM innerConstraints $ \ (innerConstraint, grps) ->
                attachGroup ("MatrixCardinality": grps) [essence|
                    (&size/2) >= (sum &pat : &indexDom . toInt(&innerConstraint))
                    |]
        _ -> noStreamliner


matrixByRowBucket ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
matrixByRowBucket innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainMatrix (DomainInt _ [RangeBounded lb ub]) innerDom@(DomainMatrix _ DomainInt{}) -> do
            let size = [essence| &ub - &lb + 1 |]
            let bucketSize = [essence| &size / 10 |]
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))

                liftMatrix (Reference n _) | n == nm = [essence| &x[&ref] |]
                liftMatrix p = p

            innerConstraints <- transformBi liftMatrix <$> innerStreamliner ref
            concatForM [0..9] $ \ (bucketInt :: Integer) -> let bucket = fromInt bucketInt in
                forM innerConstraints $ \ (innerConstraint, grps) ->
                    attachGroup (("MatrixByRowBucket-" ++ show bucketInt) : grps) [essence|
                        forAll &pat : int(&lb + &bucket * &bucketSize .. min([&ub, &lb + (&bucket+1) * &bucketSize])) . &innerConstraint
                        |]
        DomainFunction _ _ (DomainInt _ [RangeBounded lb ub]) innerDom -> do
            let size = [essence| &ub - &lb + 1 |]
            let bucketSize = [essence| &size / 10 |]
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))

                liftMatrix (Reference n _) | n == nm = [essence| &x(&ref) |]
                liftMatrix p = p

            innerConstraints <- transformBi liftMatrix <$> innerStreamliner ref
            concatForM [0..9] $ \ (bucketInt :: Integer) -> let bucket = fromInt bucketInt in
                forM innerConstraints $ \ (innerConstraint, grps) ->
                    attachGroup (("FunctionByBucket-" ++ show bucketInt) : grps) [essence|
                        forAll &pat : int(&lb + &bucket * &bucketSize .. min([&ub, &lb + (&bucket+1) * &bucketSize])) . &innerConstraint
                        |]
        _ -> noStreamliner



matrixByColBucket ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
matrixByColBucket innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainMatrix outerIndex (DomainMatrix (DomainInt _ [RangeBounded lb ub]) innerDom) -> do
            let size = [essence| &ub - &lb + 1 |]
            let bucketSize = [essence| &size / 10 |]

            nmO <- nextName "q"
            let patO = Single nmO
            let refO = Reference nmO Nothing

            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))

                liftMatrix (Reference n _) | n == nm = [essence| &x[&refO, &ref] |]
                liftMatrix p = p

            innerConstraints <- transformBi liftMatrix <$> innerStreamliner ref
            concatForM [0..9] $ \ (bucketInt :: Integer) -> let bucket = fromInt bucketInt in
                forM innerConstraints $ \ (innerConstraint, grps) ->
                    attachGroup (("MatrixByColBucket-" ++ show bucketInt) : grps) [essence|
                        forAll &patO : &outerIndex .
                        forAll &pat : int(&lb + &bucket * &bucketSize .. min([&ub, &lb + (&bucket+1) * &bucketSize])) .
                        &innerConstraint
                        |]
        _ -> noStreamliner


------------------------------------------------------------------------------
-- Sets and MSets
------------------------------------------------------------------------------


setAll ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
setAll innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    let minnerDom =
            case dom of
                DomainSet _ _ innerDom -> Just (innerDom, "SetCardinality")
                DomainMSet _ _ innerDom -> Just (innerDom, "MSetCardinality")
                DomainRelation _ _ innerDoms -> Just (DomainTuple innerDoms, "RelationCardinality")
                _ -> Nothing
    case minnerDom of
        Just (innerDom, newTag) -> do
            nm <- nextName "q"
            -- traceM $ show $ "setAll nm" <+> pretty nm
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))
            innerConstraints <- innerStreamliner ref
            -- traceM $ show $ "maybeInnerConstraint" <+> vcat (map pretty innerConstraints)
            forM innerConstraints $ \ (innerConstraint, grps) ->
                    attachGroup (newTag: grps) [essence| forAll &pat in &x . &innerConstraint |]
        _ -> noStreamliner


setAtMostOne ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
setAtMostOne innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    let minnerDom =
            case dom of
                DomainSet _ _ innerDom -> Just (innerDom, "SetCardinality")
                DomainMSet _ _ innerDom -> Just (innerDom, "MSetCardinality")
                DomainRelation _ _ innerDoms -> Just (DomainTuple innerDoms, "RelationCardinality")
                _ -> Nothing
    case minnerDom of
        Just (innerDom, newTag) -> do
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))
            innerConstraints <- innerStreamliner ref
            forM innerConstraints $ \ (innerConstraint, grps) ->
                attachGroup (newTag: grps) [essence| 1 >= sum &pat in &x . toInt(&innerConstraint) |]
        _ -> noStreamliner


setHalf ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
setHalf innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    let minnerDom =
            case dom of
                DomainSet _ _ innerDom -> Just (innerDom, "SetCardinality")
                DomainMSet _ _ innerDom -> Just (innerDom, "MSetCardinality")
                DomainRelation _ _ innerDoms -> Just (DomainTuple innerDoms, "RelationCardinality")
                _ -> Nothing
    case minnerDom of
        Just (innerDom, newTag) -> do
            let size = [essence| |&x| |]
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))
            innerConstraints <- innerStreamliner ref
            forM innerConstraints $ \ (innerConstraint, grps) ->
                attachGroup (newTag: grps) [essence| &size / 2 = (sum &pat in &x . toInt(&innerConstraint)) |]
        _ -> noStreamliner


setApproxHalf ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
setApproxHalf innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    let minnerDom =
            case dom of
                DomainSet _ _ innerDom -> Just (innerDom, "SetCardinality")
                DomainMSet _ _ innerDom -> Just (innerDom, "MSetCardinality")
                DomainRelation _ _ innerDoms -> Just (DomainTuple innerDoms, "RelationCardinality")
                _ -> Nothing
    case minnerDom of
        Just (innerDom, newTag) -> do
            let size = [essence| |&x| |]
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))
            innerConstraints <- innerStreamliner ref
            forM innerConstraints $ \ (innerConstraint, grps) ->
                attachGroup (newTag: grps) [essence|
                    (&size/2) + 1 >= (sum &pat in &x . toInt(&innerConstraint)) /\
                    (&size/2 -1) <= (sum &pat in &x . toInt(&innerConstraint))
                |]
        _ -> noStreamliner


setMoreThanHalf ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
setMoreThanHalf innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    let minnerDom =
            case dom of
                DomainSet _ _ innerDom -> Just (innerDom, "SetCardinality")
                DomainMSet _ _ innerDom -> Just (innerDom, "MSetCardinality")
                DomainRelation _ _ innerDoms -> Just (DomainTuple innerDoms, "RelationCardinality")
                _ -> Nothing
    case minnerDom of
        Just (innerDom, newTag) -> do
            let size = [essence| |&x| |]
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))
            innerConstraints <- innerStreamliner ref
            forM innerConstraints $ \ (innerConstraint, grps) ->
                attachGroup (newTag: grps) [essence|
                    (&size/2) <= (sum &pat in &x . toInt(&innerConstraint))
                |]
        _ -> noStreamliner


setLessThanHalf ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
setLessThanHalf innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    let minnerDom =
            case dom of
                DomainSet _ _ innerDom -> Just (innerDom, "SetCardinality")
                DomainMSet _ _ innerDom -> Just (innerDom, "MSetCardinality")
                DomainRelation _ _ innerDoms -> Just (DomainTuple innerDoms, "RelationCardinality")
                _ -> Nothing
    case minnerDom of
        Just (innerDom, newTag) -> do
            let size = [essence| |&x| |]
            nm <- nextName "q"
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))
            innerConstraints <- innerStreamliner ref
            forM innerConstraints $ \ (innerConstraint, grps) ->
                attachGroup (newTag: grps) [essence|
                    (&size/2) >= (sum &pat in &x . toInt(&innerConstraint))
                |]
        _ -> noStreamliner


relationCardinality ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
relationCardinality x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainRelation _ _ inners -> do
            let maxCard = make opProduct $ fromList [ [essence| |`&i`| |] | i <- inners ]
            sequence
                [ case lowerOrUpper of
                    "lowerBound" -> return ([essence| |&x| >= &maxCard / &slice |], [grp])
                    "upperBound" -> return ([essence| |&x| <= &maxCard / &slice |], [grp])
                    _ -> bug "relationCardinality"
                | slice <- [1,2,4,8,16,32]
                , lowerOrUpper <- ["LowerBound","UpperBound"]
                , let grp = "RelationCardinality-" ++ lowerOrUpper
                ]
        _ -> noStreamliner


binRelAttributes ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
binRelAttributes x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        -- we don't insist on inner1 == inner2 any more
        DomainRelation _ _ [inner1, inner2] -> sequence
            [ do
                out <- case softness of
                        Nothing -> mkBinRelCons (BinaryRelationAttrs (S.singleton attr)) inner x
                        Just s -> mkBinRelConsSoft maxNum s (BinaryRelationAttrs (S.singleton attr)) inner x
                return (make opAnd (fromList out), [grp])
            | inner <- [inner1, inner2]
            , (attr, maxNum) <- [ ( BinRelAttr_Reflexive     , [essence| |`&inner`| |] )
                                , ( BinRelAttr_Irreflexive   , [essence| |`&inner`| |] )
                                , ( BinRelAttr_Coreflexive   , [essence| |`&inner`| * |`&inner`| |] )
                                , ( BinRelAttr_Symmetric     , [essence| |`&inner`| * |`&inner`| |] )
                                , ( BinRelAttr_AntiSymmetric , [essence| |`&inner`| * |`&inner`| |] )
                                , ( BinRelAttr_ASymmetric    , [essence| |`&inner`| * |`&inner`| |] )
                                , ( BinRelAttr_Transitive    , [essence| |`&inner`| * |`&inner`| * |`&inner`| |] )
                                , ( BinRelAttr_Total         , [essence| |`&inner`| * |`&inner`| |] )
                                , ( BinRelAttr_Connex        , [essence| |`&inner`| * |`&inner`| |] )
                                , ( BinRelAttr_Euclidean     , [essence| |`&inner`| * |`&inner`| * |`&inner`| |] )
                                , ( BinRelAttr_Serial        , [essence| |`&inner`| |] )
                                , ( BinRelAttr_Equivalence   , [essence| |`&inner`| * |`&inner`| * |`&inner`| |] )
                                , ( BinRelAttr_PartialOrder  , [essence| |`&inner`| * |`&inner`| * |`&inner`| |] )
                                ]
            , let grp = show attr
            , softness <- [Nothing, Just 2, Just 4, Just 8, Just 16, Just 32]
            ]
        _ -> noStreamliner


------------------------------------------------------------------------------
-- Functions and Sequences
------------------------------------------------------------------------------


monotonicallyIncreasing ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
monotonicallyIncreasing x = do
    -- traceM $ show $ "Monotonically Increasing [1]" <+> pretty x
    dom <- expandDomainReference <$> domainOf x
    -- traceM $ show $ "Monotonically Increasing [2]" <+> pretty dom
    let
        applicable = case dom of
                        DomainFunction _ _ DomainInt{} DomainInt{} -> True
                        DomainSequence _ _             DomainInt{} -> True
                        _ -> False
    if applicable
        then do
            (iPat, i) <- quantifiedVar
            (jPat, j) <- quantifiedVar
            mkStreamliner "FuncIncreaseDecrease" [essence|
                forAll &iPat in defined(&x) .
                    forAll &jPat in defined(&x) .
                        &i < &j -> &x(&i) <= &x(&j)
            |]
        else noStreamliner


monotonicallyDecreasing ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
monotonicallyDecreasing x = do
    dom <- expandDomainReference <$> domainOf x
    let
        applicable = case dom of
                        DomainFunction _ _ DomainInt{} DomainInt{} -> True
                        DomainSequence _ _             DomainInt{} -> True
                        _ -> False
    if applicable
        then do
            -- traceM $ show $ "Monotonically Decreasing"
            (iPat, i) <- quantifiedVar
            (jPat, j) <- quantifiedVar
            mkStreamliner "FuncIncreaseDecrease" [essence|
                forAll &iPat in defined(&x) .
                    forAll &jPat in defined(&x) .
                        &i < &j -> &x(&i) >= &x(&j)
            |]
        else noStreamliner


smallestFirst ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
smallestFirst x = do
    dom <- expandDomainReference <$> domainOf x
    let
        applicable = case dom of
                        DomainFunction _ _ DomainInt{} DomainInt{} -> True
                        DomainSequence _ _             DomainInt{} -> True
                        _ -> False
    if applicable
        then do
            -- traceM $ show $ "Smallest First"
            (ipat, i) <- quantifiedVar
            mkStreamliner "FuncIncreaseDecrease" [essence|
                forAll &ipat in defined(&x) .
                    &x(min(defined(&x))) <= &x(&i)
            |]
         else noStreamliner


largestFirst ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
largestFirst x = do
    dom <- expandDomainReference <$> domainOf x
    let
        applicable = case dom of
                        DomainFunction _ _ DomainInt{} DomainInt{} -> True
                        DomainSequence _ _             DomainInt{} -> True
                        _ -> False
    if applicable
        then do
            -- traceM $ show $ "Largest First"
            (ipat, i) <- quantifiedVar
            mkStreamliner "FuncIncreaseDecrease" [essence|
                forAll &ipat in defined(&x) .
                    &x(max(defined(&x))) >= &x(&i)
            |]
         else noStreamliner


commutative ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
commutative x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainFunction () _ (DomainTuple [a, b]) c -> do
            if (a==b) && (b==c)
                then do
                    (ipat, i) <- quantifiedVar
                    (jpat, j) <- quantifiedVar
                    mkStreamliner "FuncCommutative" [essence|
                        forAll (&ipat,&jpat) in defined(&x) .
                            &x((&i,&j)) = &x((&j,&i))
                    |]
                else noStreamliner
        _ -> noStreamliner


nonCommutative ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
nonCommutative x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainFunction () _ (DomainTuple [a, b]) c -> do
            if (a==b) && (b==c)
                then do
                    (ipat, i) <- quantifiedVar
                    (jpat, j) <- quantifiedVar
                    mkStreamliner "FuncCommutative" [essence|
                        forAll (&ipat,&jpat) in defined(&x) .
                            &x((&i,&j)) != &x((&j,&i))
                    |]
                else noStreamliner
        _ -> noStreamliner


associative ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
associative x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainFunction () _ (DomainTuple [a, b]) c -> do
            if (a==b) && (b==c)
                then do
                    (ipat, i) <- quantifiedVar
                    (jpat, j) <- quantifiedVar
                    (kpat, k) <- quantifiedVar
                    mkStreamliner "FuncAssociative" [essence|
                        forAll &ipat, &jpat, &kpat in defined(&x) .
                            &x((&x((&i,&j)), &k)) = &x((&i, &x((&j, &k))))
                    |]
                else noStreamliner
        _ -> noStreamliner



onTuple ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Integer -> StreamlinerGen m -> StreamlinerGen m
onTuple n innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainTuple ds | n >= 1 && n <= genericLength ds -> do
            nm <- nextName "q"
            let ref = Reference nm (Just (DeclNoRepr Find nm (ds `genericIndex` (n-1)) NoRegion))
            innerConstraints <- innerStreamliner ref

            let
                nE = fromInt n
                replaceRef (Reference nm2 _) | nm2 == nm = [essence| &x[&nE] |]
                replaceRef p = p

            return [ (transform replaceRef cons, grps)
                   | (cons, grps) <- innerConstraints
                   ]

        _ -> noStreamliner


onRange ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
onRange innerStreamliner x = do
    -- traceM $ show $ "onRange" <+> pretty x
    dom <- expandDomainReference <$> domainOf x
    -- traceM $ show $ "onRange dom" <+> pretty dom
    let
        minnerDomTo = case dom of
                        DomainFunction _ _ DomainInt{} innerDomTo -> return innerDomTo
                        DomainSequence _ _             innerDomTo -> return innerDomTo
                        _ -> Nothing
    case minnerDomTo of
        Just innerDomTo -> do

            let rangeSetDomain = DomainSet () def innerDomTo

            nm <- nextName "q"
            let ref = Reference nm (Just (DeclNoRepr Find nm rangeSetDomain NoRegion))
            innerConstraints <- innerStreamliner ref

            let
                replaceWithRangeOfX (Reference n _) | n == nm = [essence| range(&x) |]
                replaceWithRangeOfX p = p

            -- traceM $ show $ "innerConstraints 1" <+> vcat (map pretty innerConstraints)
            -- traceM $ show $ "innerConstraints 2" <+> vcat (map pretty (transformBi replaceWithRangeOfX innerConstraints))

            return [ (transform replaceWithRangeOfX cons, grps)
                   | (cons, grps) <- innerConstraints
                   ]

        _ -> noStreamliner


onDefined ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
onDefined innerStreamliner x = do
    -- traceM $ show $ "Defined" <+> pretty x
    dom <- expandDomainReference <$> domainOf x
    -- traceM $ show $ "Defined dom" <+> pretty dom
    -- So we get the range and then we apply and then apply the rule to the range of the function
    let
        minnerDomFr = case dom of
                        DomainFunction _ _ innerDomFr _ -> return innerDomFr
                        _ -> Nothing

    case minnerDomFr of
        Just innerDomFr -> do

            let rangeSetDomain = DomainSet () def innerDomFr

            nm <- nextName "q"
            let ref = Reference nm (Just (DeclNoRepr Find nm rangeSetDomain NoRegion))
            innerConstraints <- innerStreamliner ref

            let
                replaceWithRangeOfX (Reference n _) | n == nm = [essence| defined(&x) |]
                replaceWithRangeOfX p = p

            -- traceM $ show $ "innerConstraints 1" <+> vcat (map pretty innerConstraints)
            -- traceM $ show $ "innerConstraints 2" <+> vcat (map pretty (transformBi replaceWithRangeOfX innerConstraints))

            return [ (transform replaceWithRangeOfX cons, grps)
                   | (cons, grps) <- innerConstraints
                   ]

        _ -> noStreamliner


-- diagonal :: (MonadFailDoc m, NameGen m) => StreamlinerGen m -> StreamlinerGen m
-- diagonal innerStreamliner x = do
--     traceM $ show $ "diagnoal" <+> pretty x
--     dom <- expandDomainReference <$> domainOf x
--     case dom of
--         DomainFunction () _ (DomainTuple [a, b]) domto-> do
--             case (a == b)  of
--                 True -> do
--                     nm <- nextname "fx"
--                     let auxfunction = domainfunction () def a domto
--                         ref =  reference nm (just (declnorepr find nm auxfunction noregion))

--                     i <- nextname "q"
--                     (ipat, i) <- quantifiedvar

--                     innerconstraints <- innerstreamliner ref
--                     return $ attachGroup grps [essence| find &ref: function &a --> &domto such that forall &ipat: &a. (&i, &i) in defined(&x) -> &ref(&i) = &x((&i,&i)),
--                                     forall &ipat: &a . &i in defined(&ref) -> &ref(&i) = &x((&i,&i)) |]
--                 False -> do
--                     noStreamliner
--         _ -> noStreamliner

--
-- prefix :: (MonadFailDoc m, NameGen m) => StreamlinerGen m ->  StreamlinerGen m
-- prefix innerStreamliner x = do
--     traceM $ show $ "prefix"
--     dom <- expandDomainReference <$> domainOf x
--     case dom of
--         DomainFunction () _ (DomainInt [RangeBounded lb up]) innerDomTo -> do
--             case x of
--                 Reference nm (Just (DeclNoRepr Find _ domain NoRegion)) -> do

--                     innerConstraints <- innerStreamliner x

--                     let
--                         replaceWithRangeOfX (Reference n _) | n == nm = [essence| restrict(&x,`int(&lb..(&up-1))`) |]
--                         replaceWithRangeOfX p = p

--                 --  traceM $ show $ "innerConstraints 1" <+> vcat (map pretty innerConstraints)
--                 -- traceM $ show $ "innerConstraints 2" <+> vcat (map pretty (transformBi replaceWithRangeOfX innerConstraints))

--                     return (transformBi replaceWithRangeOfX innerConstraints)
--                 _ -> noStreamliner

--         _ -> noStreamliner
--

-- postfix :: (MonadFailDoc m, NameGen m) => StreamlinerGen m ->  StreamlinerGen m
-- postfix innerStreamliner x = do
--     traceM $ show $ "postfix"
--     dom <- expandDomainReference <$> domainOf x
--     case dom of
--         DomainFunction () _ (DomainInt [RangeBounded lb up]) innerDomTo -> do
--             case x of
--                 Reference nm (Just (DeclNoRepr Find _ domain NoRegion)) -> do

--                     innerConstraints <- innerStreamliner x

--                     let
--                         replaceWithRangeOfX (Reference n _) | n == nm = [essence| restrict(&x,`int((&lb+1)..&up)`) |]
--                         replaceWithRangeOfX p = p

--                 --  traceM $ show $ "innerConstraints 1" <+> vcat (map pretty innerConstraints)
--                 -- traceM $ show $ "innerConstraints 2" <+> vcat (map pretty (transformBi replaceWithRangeOfX innerConstraints))

--                     return (transformBi replaceWithRangeOfX innerConstraints)
--                 _ -> noStreamliner

--         _ -> noStreamliner


------------------------------------------------------------------------------
-- Partitions
------------------------------------------------------------------------------


parts ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m -> StreamlinerGen m
parts innerStreamliner x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainPartition _ _ partitionDomain -> do
            -- traceM $ show $ "partition"
            nm <- nextName "q"
            let setDomain = DomainSet () def (DomainSet () def partitionDomain)
                ref =  Reference nm (Just (DeclNoRepr Find nm setDomain NoRegion))

            innerConstraints <- innerStreamliner ref

            let
                replaceWithRangeOfX (Reference n _) | n == nm = [essence| parts(&x) |]
                replaceWithRangeOfX p = p

            return [ (transform replaceWithRangeOfX cons, grps)
                   | (cons, grps) <- innerConstraints
                   ]

        _ -> noStreamliner


quasiRegular ::
    MonadFailDoc m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    StreamlinerGen m
quasiRegular x = do
    dom <- expandDomainReference <$> domainOf x
    case dom of
        DomainPartition{} -> do
            mkStreamliner "PartitionRegular" [essence|
                minPartSize(&x, |participants(&x)| / |parts(&x)| - 1) /\
                maxPartSize(&x, |participants(&x)|/ |parts(&x)| + 1)
            |]
        _ -> noStreamliner
