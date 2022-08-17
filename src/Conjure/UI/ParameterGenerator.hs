{-# LANGUAGE QuasiQuotes #-}

module Conjure.UI.ParameterGenerator ( parameterGenerator ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Language.CategoryOf ( categoryOf, Category(..) )
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Language.Instantiate ( trySimplify )
import Conjure.Process.Enumerate ( EnumerateDomain )
import Conjure.Language.Expression.DomainSizeOf ( domainSizeOf )

-- text
import Data.Text ( pack )


-- | This doesn't do anything to do with correcting categories at the moment, it should.
--   An example:
--      given n : int(1..10)
--      given s : set (size n) of int(1..10)
--   Should output:
--      find n : int(1..10)
--      find s : set (minSize 1, maxSize 10) of int(1..10)
--      such that n = |s|
--   (Just dropping wrong category stuff from attribute list isn't acceptable, because mset.)
parameterGenerator ::
    MonadLog m =>
    MonadFail m =>
    MonadUserError m =>
    EnumerateDomain m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Integer ->      -- MININT
    Integer ->      -- MAXINT
    Model -> m ( ( Model            -- generator model
                 , Model )          -- repair model
               , [(Name, String)]   -- classification for each given
               )
parameterGenerator minIntValue maxIntValue model =
    runStateAsWriterT $ runNameGen () (resolveNames model) >>= core >>= evaluateBounds2
    where
        core m = do
            out <- forM (mStatements m) $ \ st -> case st of
                Declaration (FindOrGiven Given nm dom) -> do
                    (dom', genDecls, genCons, repairCons) <- pgOnDomain (Reference nm Nothing) nm dom
                    let repairDecls = [ Declaration (FindOrGiven Find ("repaired_" `mappend` genDeclNm) genDeclDom)
                                      | Declaration (FindOrGiven Given genDeclNm genDeclDom) <- genDecls
                                      ]
                    let repairObjectiveParts =
                                [ [essence| |&b-&a| |]
                                | Declaration (FindOrGiven Given genDeclNm _) <- genDecls
                                , let a = Reference genDeclNm Nothing
                                , let b = Reference ("repaired_" `mappend` genDeclNm) Nothing
                                ]
                    let prependRepair (Reference n _) = Reference ("repaired_" `mappend` n) Nothing
                        prependRepair x = x
                    return  ( genDecls
                                ++ [ Declaration (FindOrGiven Find nm dom') ]
                                ++ [ SuchThat genCons | not (null genCons) ]
                            , genDecls
                                ++ repairDecls
                                ++ [SuchThat (map (transform prependRepair) repairCons) | not (null repairCons)]
                            , repairObjectiveParts
                            )
                Declaration (FindOrGiven Find  _  _  ) -> return ([], [], [])
                Declaration (Letting _ _)              -> return ([st], [], [])
                Declaration       {}                   -> return ([st], [], [])
                SearchOrder       {}                   -> return ([], [], [])
                SearchHeuristic   {}                   -> return ([], [], [])
                Where             xs                   -> do
                    xs' <- mapM (transformM fixQuantified) xs
                    return ([SuchThat xs'], [], [])
                Objective         {}                   -> return ([], [], [])
                SuchThat          {}                   -> return ([], [], [])

            let (generatorStmts, repairStmts, repairObjectiveParts) = mconcat out

            return ( m { mStatements = generatorStmts }
                   , m { mStatements = repairStmts
                                    ++ [Objective Minimising (make opSum (fromList repairObjectiveParts))] }
                   )

        evaluateBounds2 (m1, m2) = do
            m1' <- evaluateBounds m1
            m2' <- evaluateBounds m2
            return (inlineLettings m1', inlineLettings m2')

        evaluateBounds m = do
            let symbols = [("MININT", fromInt minIntValue), ("MAXINT", fromInt maxIntValue)]
            let eval = transformBiM (trySimplify symbols)
            stmtsEvaluated <- mapM eval (mStatements m)
            return m { mStatements = stmtsEvaluated }


inlineLettings :: Model -> Model
inlineLettings model =
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
                Declaration (Letting nm x) -> modify ((nm,x) :) >> return Nothing
                _ -> Just <$> transformBiM inline st
    in
        model { mStatements = statements }


fixQuantified ::
    MonadUserError m =>
    NameGen m => 
    Expression ->
    m Expression
fixQuantified (Comprehension body gocs) = do
    gocs' <- forM gocs $ \ goc -> case goc of
        Generator (GenDomainNoRepr (Single pat) domain) -> do
            let go x d =
                    case d of
                        DomainInt t ranges -> do
                            boundsAndCons <- forM ranges $ \ range ->
                                case range of
                                    RangeSingle s -> do
                                        (fr', frCons) <-
                                            if categoryOf s < CatParameter
                                                then return (s, [])
                                                else do
                                                    bound <- lowerBoundOfIntExpr s
                                                    return (bound, return [essence| &x = &s |])
                                        (to', _) <-
                                            if categoryOf s < CatParameter
                                                then return (s, [])
                                                else do
                                                    bound <- upperBoundOfIntExpr s
                                                    return (bound, [])
                                        return ([fr'], [to'], frCons)
                                    RangeBounded fr to -> do
                                        (fr', frCons) <-
                                            if categoryOf fr < CatParameter
                                                then return (fr, [])
                                                else do
                                                    bound <- lowerBoundOfIntExpr fr
                                                    return (bound, return [essence| &x >= &fr |])
                                        (to', toCons) <-
                                            if categoryOf to < CatParameter
                                                then return (to, [])
                                                else do
                                                    bound <- upperBoundOfIntExpr to
                                                    return (bound, return [essence| &x <= &to |])
                                        return ([fr'], [to'], frCons ++ toCons)
                                    _ -> userErr1 $ vcat [ "Open ranges are not supported:" <+> pretty range
                                                         , "In domain:" <+> pretty d
                                                         ]
                            let (froms, tos, cons) = mconcat boundsAndCons
                            let fr' = make opMin $ fromList froms
                            let to' = make opMax $ fromList tos
                            return (DomainInt t [RangeBounded fr' to'], cons)
                        DomainFunction r attr innerFr innerTo -> do
                            (jPat, j) <- quantifiedVar
                            (innerFr', consFr) <- go [essence| &j[1] |] innerFr
                            (innerTo', consTo) <- go [essence| &j[2] |] innerTo
                            let innerCons = make opAnd $ fromList $ consFr ++ consTo
                            return ( DomainFunction r attr innerFr' innerTo'
                                   , return [essence| forAll &jPat in &x . &innerCons |]
                                   )
                        _ -> return (d, [])

            let patX = Reference pat Nothing
            (dom', cons) <- go patX (expandDomainReference domain)
            return $ [Generator (GenDomainNoRepr (Single pat) dom')]
                   ++ map Condition cons
        _ -> return [goc]
    return (Comprehension body (concat gocs'))
fixQuantified x = return x


pgOnDomain ::
    MonadUserError m =>
    NameGen m =>
    MonadState [(Name, String)] m =>
    Expression ->                       -- how do we refer to this top level variable
    Name ->                             -- its name
    Domain () Expression ->             -- its domain
    m ( Domain () Expression            -- its modified domain for the find version
      , [Statement]                     -- statements that define the necessary givens
      , [Expression]                    -- constraints for the generator model
      , [Expression]                    -- constraints for the repair model
      )
pgOnDomain x nm (expandDomainReference -> dom) =

    case dom of

        DomainBool -> return4 dom [] [] []

        DomainInt t _ -> do
            lbX <- minOfIntDomain dom
            ubX <- maxOfIntDomain dom
            lb  <- lowerBoundOfIntExpr lbX
            ub  <- upperBoundOfIntExpr ubX

            let nmMin = nm `mappend` "_min"
            sawTell [(nmMin, "i")]
            let rmin = Reference nmMin Nothing

            let nmMax  = nm `mappend` "_max"
            sawTell [(nmMax, "i")]
            let rmax = Reference nmMax Nothing

            return4
                (DomainInt t [RangeBounded lb ub])
                [ Declaration (FindOrGiven Given nmMin
                        (DomainInt t [RangeBounded lb ub]))
                , Declaration (FindOrGiven Given nmMax
                        -- (DomainInt t [RangeBounded 0 [essence| min([5, (&ub - &lb) / 2]) |]]))
                        (DomainInt t [RangeBounded lb ub]))
                ]
                ( [ [essence| &x >= &rmin |]
                  , [essence| &x <= &rmax |]
                  ] ++
                  [ [essence| &x >= &lbX |]
                  | lb /= lbX
                  ] ++
                  [ [essence| &x <= &ubX |]
                  | ub /= ubX
                  ] )
                ( [ [essence| &rmin <= &rmax |]
                  ] )

        DomainRecord ds -> do
            inners <- forM ds $ \ (nmRec, domRec) -> do
                let iE = Reference nmRec Nothing
                let ref = [essence| &x[&iE] |]
                pgOnDomain ref (nm `mappend` (Name $ pack $ "_" ++ show (pretty nmRec))) domRec
            return4
                (DomainRecord (zip (map fst ds) (map fst4 inners)))
                (concatMap snd4 inners)
                (concatMap thd4 inners)
                (concatMap fourth4 inners)

        DomainTuple ds -> do
            inners <- forM (zip [1..] ds) $ \ (i, d) -> do
                let iE = fromInt i
                let ref = [essence| &x[&iE] |]
                pgOnDomain ref (nm `mappend` (Name $ pack $ "_tuple" ++ show i)) d
            return4
                (DomainTuple (map fst4 inners))
                (concatMap snd4 inners)
                (concatMap thd4 inners)
                (concatMap fourth4 inners)

        DomainMatrix indexDomain innerDomain | categoryOf indexDomain <= CatConstant -> do
            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat : &indexDomain . &c |]
            let ref = [essence| &x[&i] |]
            (innerDomain', declInner, consInner, repairStmts) <- pgOnDomain ref (nm `mappend` "_inner") innerDomain
            return4
                (DomainMatrix indexDomain innerDomain')
                declInner
                (map liftCons consInner)
                repairStmts

        DomainSequence r attr innerDomain -> do

            let nmCardMin = nm `mappend` "_cardMin"
            sawTell [(nmCardMin, "i")]
            let cardMin = Reference nmCardMin Nothing

            let nmCardMax  = nm `mappend` "_cardMax"
            sawTell [(nmCardMax, "i")]
            let cardMax = Reference nmCardMax Nothing

            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]
            (domInner, declInner, consInner, repairStmts) <- pgOnDomain [essence| &i[2] |] (nm `mappend` "_inner") innerDomain

            (attrOut, sizeLb, sizeUb, cardDomain) <-
                    case attr of
                        SequenceAttr size jectivity -> do
                            (sizeOut, lb, ub, cardDomain) <-
                                case size of
                                    SizeAttr_None ->
                                        return ( SizeAttr_MaxSize maxInt, Nothing, Nothing
                                               , DomainInt TagInt [RangeBounded minInt maxInt]
                                               )
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just a
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb maxInt, Just a, Nothing
                                               , DomainInt TagInt [RangeBounded lb maxInt]
                                               )
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MaxSize ub, Nothing, Just a
                                               , DomainInt TagInt [RangeBounded 0 ub]
                                               )
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just b
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                            return (SequenceAttr sizeOut jectivity, lb, ub, cardDomain)

            let
                newDecl =
                    [ Declaration (FindOrGiven Given nmCardMin cardDomain)
                    , Declaration (FindOrGiven Given nmCardMax cardDomain)
                    ]

            let
                cardinalityCons = return $ return
                    [essence|
                        |&x| >= &cardMin /\
                        |&x| <= &cardMax
                    |]

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

            newCons <- concat <$> sequence [cardinalityCons, sizeLbCons, sizeUbCons]

            return4
                (DomainSequence r attrOut domInner)
                (newDecl ++ declInner)
                (newCons ++ map liftCons consInner)
                repairStmts

        DomainSet r attr innerDomain -> do

            let nmCardMin = nm `mappend` "_cardMin"
            sawTell [(nmCardMin, "i")]
            let cardMin = Reference nmCardMin Nothing

            let nmCardMax  = nm `mappend` "_cardMax"
            sawTell [(nmCardMax, "i")]
            let cardMax = Reference nmCardMax Nothing

            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]
            (domInner, declInner, consInner, repairStmts) <- pgOnDomain i (nm `mappend` "_inner") innerDomain

            (attrOut, sizeLb, sizeUb, cardDomain) <-
                    case attr of
                        SetAttr size -> do
                            (sizeOut, lb, ub, cardDomain) <-
                                case size of
                                    SizeAttr_None ->
                                        return ( SizeAttr_MaxSize maxInt, Nothing, Nothing
                                               , DomainInt TagInt [RangeBounded minInt maxInt]
                                               )
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just a
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb maxInt, Just a, Nothing
                                               , DomainInt TagInt [RangeBounded lb maxInt]
                                               )
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MaxSize ub, Nothing, Just a
                                               , DomainInt TagInt [RangeBounded 0 ub]
                                               )
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just b
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                            return (SetAttr sizeOut, lb, ub, cardDomain)

            let
                newDecl =
                    [ Declaration (FindOrGiven Given nmCardMin cardDomain)
                    , Declaration (FindOrGiven Given nmCardMax cardDomain)
                    ]

            let
                cardinalityCons = return $ return
                    [essence|
                        |&x| >= &cardMin /\
                        |&x| <= &cardMax
                    |]

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

            newCons <- concat <$> sequence [cardinalityCons, sizeLbCons, sizeUbCons]

            return4
                (DomainSet r attrOut domInner)
                (newDecl ++ declInner)
                (newCons ++ map liftCons consInner)
                repairStmts

        DomainMSet r attr innerDomain -> do

            let nmCardMin = nm `mappend` "_cardMin"
            sawTell [(nmCardMin, "i")]
            let cardMin = Reference nmCardMin Nothing

            let nmCardMax  = nm `mappend` "_cardMax"
            sawTell [(nmCardMax, "i")]
            let cardMax = Reference nmCardMax Nothing

            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]
            (domInner, declInner, consInner, repairStmts) <- pgOnDomain i (nm `mappend` "_inner") innerDomain

            (attrOut, sizeLb, sizeUb, cardDomain, occurLb, occurUb) <-
                    case attr of
                        MSetAttr sizeAttr occurAttr -> do
                            (sizeAttrOut, sizeLb, sizeUb, cardDomain) <-
                                case sizeAttr of
                                    SizeAttr_None ->
                                        return ( SizeAttr_MaxSize maxInt, Nothing, Nothing
                                               , DomainInt TagInt [RangeBounded minInt maxInt]
                                               )
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just a
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb maxInt, Just a, Nothing
                                               , DomainInt TagInt [RangeBounded lb maxInt]
                                               )
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MaxSize ub, Nothing, Just a
                                               , DomainInt TagInt [RangeBounded 0 ub]
                                               )
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just b
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                            (occurAttrOut, occurLb, occurUb) <-
                                case occurAttr of
                                    OccurAttr_None ->
                                        return (OccurAttr_MaxOccur maxInt, Nothing, Nothing)
                                    OccurAttr_MinOccur a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return (OccurAttr_MinMaxOccur lb maxInt, Just a, Nothing)
                                    OccurAttr_MaxOccur a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return (OccurAttr_MaxOccur ub, Nothing, Just a)
                                    OccurAttr_MinMaxOccur a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return (OccurAttr_MinMaxOccur lb ub, Just a, Just b)
                            return (MSetAttr sizeAttrOut occurAttrOut, sizeLb, sizeUb, cardDomain, occurLb, occurUb)

            let
                newDecl =
                    [ Declaration (FindOrGiven Given nmCardMin cardDomain)
                    , Declaration (FindOrGiven Given nmCardMax cardDomain)
                    ]

            let
                cardinalityCons = return $ return
                    [essence|
                        |&x| >= &cardMin /\
                        |&x| <= &cardMax
                    |]

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

                occurLbCons =
                    case occurLb of
                        Nothing -> return []
                        Just bound -> return $ return $ liftCons [essence| freq(&x, &i) >= &bound |]

                occurUbCons =
                    case occurUb of
                        Nothing -> return []
                        Just bound -> return $ return $ liftCons [essence| freq(&x, &i) <= &bound |]

            newCons <- concat <$> sequence [cardinalityCons, sizeLbCons, sizeUbCons, occurLbCons, occurUbCons]

            return4
                (DomainMSet r attrOut domInner)
                (newDecl ++ declInner)
                (newCons ++ map liftCons consInner)
                repairStmts

        DomainFunction r attr innerDomainFr innerDomainTo -> do

            let nmCardMin = nm `mappend` "_cardMin"
            sawTell [(nmCardMin, "i")]
            let cardMin = Reference nmCardMin Nothing

            let nmCardMax  = nm `mappend` "_cardMax"
            sawTell [(nmCardMax, "i")]
            let cardMax = Reference nmCardMax Nothing

            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]
            (domFr, declFr, consFr, repairStmtsFr) <- pgOnDomain [essence| &i[1] |] (nm `mappend` "_defined") innerDomainFr
            (domTo, declTo, consTo, repairStmtsTo) <- pgOnDomain [essence| &i[2] |] (nm `mappend` "_range") innerDomainTo

            -- drop total, post constraint instead
            (attrOut, sizeLb, sizeUb, cardDomain) <-
                    case attr of
                        FunctionAttr size PartialityAttr_Partial jectivity -> do
                            (sizeOut, lb, ub, cardDomain) <-
                                case size of
                                    SizeAttr_None -> do
                                        mdomSize <- runExceptT $ domainSizeOf innerDomainFr
                                        case mdomSize of
                                            Left{} ->
                                                return ( SizeAttr_MaxSize maxInt, Nothing, Nothing
                                                       , DomainInt TagInt [RangeBounded minInt maxInt]
                                                       )
                                            Right domSize -> do
                                                domSizeUpp <- upperBoundOfIntExpr domSize
                                                return ( SizeAttr_MaxSize maxInt, Nothing, Nothing
                                                       , DomainInt TagInt [RangeBounded 0 domSizeUpp]
                                                       )
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just a
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb maxInt, Just a, Nothing
                                               , DomainInt TagInt [RangeBounded lb maxInt]
                                               )
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MaxSize ub, Nothing, Just a
                                               , DomainInt TagInt [RangeBounded 0 ub]
                                               )
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just b
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                            return (FunctionAttr sizeOut PartialityAttr_Partial jectivity, lb, ub, cardDomain)
                        FunctionAttr _size PartialityAttr_Total jectivity -> do
                            return (FunctionAttr SizeAttr_None PartialityAttr_Partial jectivity, Nothing, Nothing, bug "cardDomain not needed")

            let isTotal = case attr of
                                FunctionAttr _ PartialityAttr_Total _ -> True
                                _ -> False
                isPartial = not isTotal

            let
                newDecl | isTotal = []
                        | otherwise =
                            [ Declaration (FindOrGiven Given nmCardMin cardDomain)
                            , Declaration (FindOrGiven Given nmCardMax cardDomain)
                            ]

            let
                cardinalityCons | isTotal = return []
                                | otherwise = return $ return
                    [essence|
                        |&x| >= &cardMin /\
                        |&x| <= &cardMax
                    |]

                totalityCons | isPartial = return []
                             | otherwise = do

                    let
                        go d = case d of
                            DomainInt{} -> do
                                dMin <- minOfIntDomain d
                                dMax <- maxOfIntDomain d
                                return (\ k -> [essence| (&k >= &dMin /\ &k <= &dMax) |] )
                            DomainTuple ds -> do
                                makers <- mapM go ds
                                return $ \ k -> make opAnd $ fromList
                                    [ mk [essence| &k[&nExpr] |]
                                    | (mk, n) <- zip makers [1..]
                                    , let nExpr = fromInt n
                                    ]
                            DomainSet _ _ inner -> do
                                (jPat, j) <- quantifiedVar
                                maker <- go inner
                                let innerCons = maker j
                                return $ \ k -> [essence| forAll &jPat in &k . &innerCons |]
                            DomainMSet _ _ inner -> do
                                (jPat, j) <- quantifiedVar
                                maker <- go inner
                                let innerCons = maker j
                                return $ \ k -> [essence| forAll &jPat in &k . &innerCons |]
                            _ -> bug $ "Unhandled domain (in function defined):" <+> vcat [pretty d, pretty (show d)]

                    mkCondition <- go innerDomainFr

                    let iCondition = mkCondition i

                    return $ return [essence|
                        forAll &iPat : &domFr .
                            &iCondition
                            <->
                            &i in defined(&x)
                        |]

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

            -- only for bool domains (innerDomainTo)
            let nmPercentageMax  = nm `mappend` "_percentage_max"
            let nmPercentageMin  = nm `mappend` "_percentage_min"
            let refPercentageMax = Reference nmPercentageMax Nothing
            let refPercentageMin = Reference nmPercentageMin Nothing
            sawTell [(nmPercentageMax, "i")]
            sawTell [(nmPercentageMin, "i")]

            let isToBool = case innerDomainTo of
                                DomainBool -> True
                                _ -> False

            let declToBool =
                    [ Declaration (FindOrGiven Given nmPercentageMin (DomainInt TagInt [RangeBounded 0 100]))
                    , Declaration (FindOrGiven Given nmPercentageMax (DomainInt TagInt [RangeBounded 0 100]))
                    ]
            let consToBool = make opAnd $ fromList
                    [ [essence| sum([ toInt(&i[2]) | &iPat <- &x ]) <= &refPercentageMax * |defined(&x)| / 100 |]
                    , [essence| sum([ toInt(&i[2]) | &iPat <- &x ]) >= &refPercentageMin * |defined(&x)| / 100 |]
                    ]

            newCons <- concat <$> sequence [cardinalityCons, totalityCons, sizeLbCons, sizeUbCons]
            let innerCons = concat $ concat
                    [ [consFr | isPartial ] -- only if the function is not total
                    , [consTo]
                    ]

            let
                appendToReferences suffix (Reference n _) = Reference (n `mappend` suffix) Nothing
                appendToReferences _ n = n

            let

                definedBoundCons n d = 
                    case d of
                        DomainInt{} -> do
                            let
                                defined_max = Reference (n `mappend` "_max") Nothing
                                defined_min = Reference (n `mappend` "_min") Nothing
                            defined_maxBound <- transform (appendToReferences "_max") <$> maxOfIntDomain d
                            defined_minBound <- transform (appendToReferences "_min") <$> minOfIntDomain d
                            return [ [essence| &defined_min >= &defined_minBound |]
                                   , [essence| &defined_max <= &defined_maxBound |]
                                   ]
                        DomainTuple ds ->
                            concatForM (zip allNats ds) $ \ (n', d') ->
                                definedBoundCons
                                    (mconcat [n, "_tuple", Name (stringToText $ show n')])
                                    d'
                        _ -> return []

                definedGtCard =
                    case innerDomainFr of
                        DomainTuple ds -> 
                            let
                                defined_min n =
                                    Reference
                                        (mconcat [nm, "_defined_tuple", Name (stringToText $ show n), "_min"])
                                        Nothing
                                defined_max n =
                                    Reference
                                        (mconcat [nm, "_defined_tuple", Name (stringToText $ show n), "_max"])
                                        Nothing
                                one n = let minn = defined_min n
                                            maxn = defined_max n
                                        in  [essence| &maxn - &minn + 1 |]
                                multiplied = make opProduct $ fromList $ map one [1..length ds]
                            in
                                [essence| &multiplied >= &cardMax |]
                        _ ->
                            let
                                defined_max = Reference (nm `mappend` "_defined_max") Nothing
                                defined_min = Reference (nm `mappend` "_defined_min") Nothing
                            in
                                [essence| &defined_max - &defined_min + 1 >= &cardMax |]

            definedBoundCons' <- definedBoundCons (nm `mappend` "_defined") innerDomainFr

            let repairCons = [ [essence| &cardMin <= &cardMax |] | isPartial ]
                          ++ [ definedGtCard | isPartial ]
                          ++ concat [ definedBoundCons' | isPartial ]
                          ++ [ [essence| &refPercentageMax >= &refPercentageMin |] | isToBool ]

            return4
                (DomainFunction r attrOut domFr domTo)
                (newDecl ++ concat [ declFr | isPartial ] ++ declTo ++ concat [ declToBool | isToBool ])
                (newCons ++ map liftCons innerCons ++ concat [[consToBool] | isToBool ])
                (repairCons ++ concat [ repairStmtsFr | isPartial ] ++ repairStmtsTo)

        DomainRelation r attr innerDomains -> do

            let nmCardMin = nm `mappend` "_cardMin"
            sawTell [(nmCardMin, "i")]
            let cardMin = Reference nmCardMin Nothing

            let nmCardMax  = nm `mappend` "_cardMax"
            sawTell [(nmCardMax, "i")]
            let cardMax = Reference nmCardMax Nothing

            (iPat, i) <- quantifiedVar
            let liftCons c = [essence| forAll &iPat in &x . &c |]

            inners <- forM (zip [1..] innerDomains) $ \ (n, d) -> do
                let nE = fromInt n
                let ref = [essence| &i[&nE] |]
                (d', decl, cons, repairStmts) <- pgOnDomain ref (nm `mappend` (Name $ pack $ "_relation" ++ show n)) d
                return (d', decl, map liftCons cons, repairStmts)

            let maxIntN = maxIntTimes (genericLength innerDomains)

            (attrOut, sizeLb, sizeUb, cardDomain) <-
                    case attr of
                        RelationAttr size binRelAttr -> do
                            (sizeOut, lb, ub, cardDomain) <-
                                case size of
                                    SizeAttr_None ->
                                        return ( SizeAttr_MaxSize maxIntN, Nothing, Nothing
                                               , DomainInt TagInt [RangeBounded minInt maxIntN]
                                               )
                                    SizeAttr_Size a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just a
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                                    SizeAttr_MinSize a -> do
                                        lb <- lowerBoundOfIntExpr a
                                        return ( SizeAttr_MinMaxSize lb maxIntN, Just a, Nothing
                                               , DomainInt TagInt [RangeBounded lb maxIntN]
                                               )
                                    SizeAttr_MaxSize a -> do
                                        ub <- upperBoundOfIntExpr a
                                        return ( SizeAttr_MaxSize ub, Nothing, Just a
                                               , DomainInt TagInt [RangeBounded 0 ub]
                                               )
                                    SizeAttr_MinMaxSize a b -> do
                                        lb <- lowerBoundOfIntExpr a
                                        ub <- upperBoundOfIntExpr b
                                        return ( SizeAttr_MinMaxSize lb ub, Just a, Just b
                                               , DomainInt TagInt [RangeBounded lb ub]
                                               )
                            return (RelationAttr sizeOut binRelAttr, lb, ub, cardDomain)

            let
                newDecl =
                    [ Declaration (FindOrGiven Given nmCardMin cardDomain)
                    , Declaration (FindOrGiven Given nmCardMax cardDomain)
                    ]

            let
                cardinalityCons = return $ return
                    [essence|
                        |&x| >= &cardMin /\
                        |&x| <= &cardMax
                    |]

                sizeLbCons =
                    case sizeLb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| >= &bound |]

                sizeUbCons =
                    case sizeUb of
                        Nothing -> return []
                        Just bound -> return $ return [essence| |&x| <= &bound |]

            newCons <- concat <$> sequence [cardinalityCons, sizeLbCons, sizeUbCons]

            return4
                (DomainRelation r attrOut (map fst4 inners))
                (newDecl ++ concatMap snd4 inners)
                (newCons ++ concatMap thd4 inners)
                (concatMap fourth4 inners)

        _ -> userErr1 $ "Unhandled domain:" <++> vcat [ pretty dom
                                                      , pretty (show dom)
                                                      ]


-- helper functions

return4 :: Monad m => a -> b -> c -> d -> m (a,b,c,d)
return4 x y z w = return (x,y,z,w)

minInt :: Expression
minInt = Reference "MININT" Nothing

maxInt :: Expression
maxInt = Reference "MAXINT" Nothing

maxIntTimes :: Integer -> Expression
maxIntTimes n = let m = fromInt n in [essence| &maxInt ** &m |]


minOfIntDomain :: MonadUserError m => Domain () Expression -> m Expression
minOfIntDomain (DomainInt _ rs) = do
    xs <- sortNub <$> mapM minOfIntRange rs
    case xs of
        []  -> return minInt
        [x] -> return x
        _   -> return $ make opMin $ fromList xs
minOfIntDomain d = userErr1 $ "Expected integer domain, but got:" <++> vcat [pretty d, pretty (show d)]

minOfIntRange :: Monad m => Range Expression -> m Expression
minOfIntRange (RangeSingle lb) = return lb
minOfIntRange (RangeLowerBounded lb) = return lb
minOfIntRange (RangeBounded lb _) = return lb
minOfIntRange _ = return minInt

lowerBoundOfIntExpr :: MonadUserError m => Expression -> m Expression
lowerBoundOfIntExpr x@Constant{} = return x
lowerBoundOfIntExpr x | x == minInt = return minInt
lowerBoundOfIntExpr x | x == maxInt = return maxInt
lowerBoundOfIntExpr (Reference _ (Just (DeclNoRepr Given _ dom _))) = minOfIntDomain dom
lowerBoundOfIntExpr (Reference _ (Just (Alias x))) = lowerBoundOfIntExpr x
lowerBoundOfIntExpr (Reference _ _) = return minInt
lowerBoundOfIntExpr (Op (MkOpMinus (OpMinus a b))) = do
    aLower <- lowerBoundOfIntExpr a
    bUpper <- upperBoundOfIntExpr b
    return $ make opMinus aLower bUpper
lowerBoundOfIntExpr (Op (MkOpSum (OpSum x))) | Just xs <- listOut x = do
    bounds <- mapM lowerBoundOfIntExpr xs
    return $ make opSum $ fromList bounds
-- TODO: check for negatives
lowerBoundOfIntExpr (Op (MkOpProduct (OpProduct x))) | Just xs <- listOut x = do
    bounds <- mapM lowerBoundOfIntExpr xs
    return $ make opProduct $ fromList bounds
lowerBoundOfIntExpr (Op (MkOpMin (OpMin x))) | Just xs <- listOut x = do
    bounds <- mapM lowerBoundOfIntExpr xs
    return $ make opMin $ fromList bounds
lowerBoundOfIntExpr (Op (MkOpMin (OpMin (Comprehension body gocs)))) = do
    body' <- lowerBoundOfIntExpr body
    return $ make opMin $ Comprehension body' gocs
lowerBoundOfIntExpr (Op (MkOpMax (OpMax x))) | Just xs <- listOut x = do
    bounds <- mapM lowerBoundOfIntExpr xs
    return $ make opMin $ fromList bounds
lowerBoundOfIntExpr (Op (MkOpMax (OpMax (Comprehension body gocs)))) = do
    body' <- lowerBoundOfIntExpr body
    return $ make opMin $ Comprehension body' gocs
lowerBoundOfIntExpr (Op (MkOpNegate (OpNegate x))) = do
    bound <- upperBoundOfIntExpr x
    return (make opNegate bound)
lowerBoundOfIntExpr x = userErr1 $ "Cannot compute lower bound of integer expression:" <++> vcat [pretty x, pretty (show x)]


maxOfIntDomain :: MonadUserError m => Domain () Expression -> m Expression
maxOfIntDomain (DomainInt _ rs) = do
    xs <- sortNub <$> mapM maxOfIntRange rs
    case xs of
        []  -> return maxInt
        [x] -> return x
        _   -> return $ make opMax $ fromList xs
maxOfIntDomain d = userErr1 $ "Expected integer domain, but got:" <++> pretty d

maxOfIntRange :: Monad m => Range Expression -> m Expression
maxOfIntRange (RangeSingle ub) = return ub
maxOfIntRange (RangeUpperBounded ub) = return ub
maxOfIntRange (RangeBounded _ ub) = return ub
maxOfIntRange _ = return maxInt

upperBoundOfIntExpr :: MonadUserError m => Expression -> m Expression
upperBoundOfIntExpr x@Constant{} = return x
upperBoundOfIntExpr x | x == minInt = return minInt
upperBoundOfIntExpr x | x == maxInt = return maxInt
upperBoundOfIntExpr (Reference _ (Just (DeclNoRepr Given _ dom _))) = maxOfIntDomain dom
upperBoundOfIntExpr (Reference _ (Just (Alias x))) = upperBoundOfIntExpr x
upperBoundOfIntExpr (Reference _ _) = return maxInt
upperBoundOfIntExpr (Op (MkOpMinus (OpMinus a b))) = do
    aUpper <- upperBoundOfIntExpr a
    bLower <- lowerBoundOfIntExpr b
    return $ make opMinus aUpper bLower
upperBoundOfIntExpr (Op (MkOpSum (OpSum x))) | Just xs <- listOut x = do
    bounds <- mapM upperBoundOfIntExpr xs
    return $ make opSum $ fromList bounds
-- TODO: check for negatives
upperBoundOfIntExpr (Op (MkOpProduct (OpProduct x))) | Just xs <- listOut x = do
    bounds <- mapM upperBoundOfIntExpr xs
    return $ make opProduct $ fromList bounds
upperBoundOfIntExpr (Op (MkOpMin (OpMin x))) | Just xs <- listOut x = do
    bounds <- mapM upperBoundOfIntExpr xs
    return $ make opMax $ fromList bounds
upperBoundOfIntExpr (Op (MkOpMin (OpMin (Comprehension body gocs)))) = do
    body' <- lowerBoundOfIntExpr body
    return $ make opMax $ Comprehension body' gocs
upperBoundOfIntExpr (Op (MkOpMax (OpMax x))) | Just xs <- listOut x = do
    bounds <- mapM upperBoundOfIntExpr xs
    return $ make opMax $ fromList bounds
upperBoundOfIntExpr (Op (MkOpMax (OpMax (Comprehension body gocs)))) = do
    body' <- lowerBoundOfIntExpr body
    return $ make opMax $ Comprehension body' gocs
upperBoundOfIntExpr (Op (MkOpNegate (OpNegate x))) = do
    bound <- lowerBoundOfIntExpr x
    return (make opNegate bound)
upperBoundOfIntExpr x = userErr1 $ "Cannot compute upper bound of integer expression:" <++> vcat [pretty x, pretty (show x)]
