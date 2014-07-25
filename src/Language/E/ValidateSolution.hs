{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.ValidateSolution
    ( validateSolution
    , validateSolutionPure
    , validateSolutionPureNew
    , satisfied
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
import Language.E.NormaliseSolution(normaliseSolutionE)

type Essence  = Spec
type Param    = Maybe Spec
type Solution = Spec
type Dom = E

validateSolution :: Essence -> Param -> Solution -> IO ()
validateSolution essence param solution =
    if validateSolutionPure essence param solution
        then return ()
        else userErr "Not a valid solution."


-- this will return True is it's valid, False if not the validator might use error


validateSolutionPure :: Essence -> Param -> Solution -> Bool
validateSolutionPure essence param solution =
    fst $ validateSolutionPureNew essence param solution

validateSolutionPureNew :: Essence -> Param ->  Solution -> (Bool,LogTree)
validateSolutionPureNew essence param solution =
    let
        (mresult, _logs) = runCompESingle "validating solution" helper
    in
        case mresult of
            Left  x      -> userErr x
            Right b -> (b, _logs)

    where
    helper = do

        case param of
            Nothing -> return ()
            Just (Spec _ s) -> mapM_ introduceStuff (statementAsList s)
        -- bindersDoc >>= mkLog "binders 1"

        case solution of
            Spec _ s        -> mapM_ introduceStuff (statementAsList s)
        bindersDoc >>= mkLog "binders 2"

        let essenceCombined =
                case (essence, param) of
                    (Spec l s, Just (Spec _ p)) ->
                        Spec l (listAsStatement $ statementAsList p ++ statementAsList s)
                    _ -> essence



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


        inlined <-  pipeline0 essenceCombined
        mkLog "inlined" (pretty inlined)


        (Spec _ s) <- pipeTest1 inlined
        let checks = map isPartOfValidSolution (statementAsList s)
        if all isJust checks
            then do
                c <- validateSpec inlined
                return $ (and $ catMaybes checks) && c

            else bug $ vcat [ "Cannot fully evaluate."
                            , pretty s
                            , prettyAsTree s
                            , prettyAsPaths s
                            ]




validateSpec :: MonadConjure m => Spec -> m Bool
validateSpec spec = do
    bs <- gets binders
    let finds = pullFinds spec
    mkLog "binders" $ vcat $ map pretty bs
    mkLog "finds" $ vcat $ map pretty finds
    -- mkLog "spec" $ vcat $ map pretty $ map (prettyAsTree  ) (statementAsList s)

    let validateFinds (name,f)  = do
            mval <- runMaybeT $ lookupReference name
            mkLog "dom" $ pretty f
            case mval of
                Just val -> do
                    mkLog "val" $ pretty val
                    res <-  validateVal (sortAttrs f) (normaliseSolutionE val)
                    return $ case res of
                        Just doc -> Just $ vcat [
                              "Error for value" <+> pretty val
                            , "Domain: " <+> pretty f
                            , hang "Details:" 2 doc
                            , "---"]
                        Nothing -> Nothing
                Nothing -> do
                    bdoc <- bindersDoc
                    bug $ vcat [ "Value not found ", bdoc, pretty spec]


    docs <-  mapM validateFinds finds
    case catMaybes docs of
        [] -> return True
        xs -> userErr $ vcat xs

-- Attributes must be listed in sorted order for dMatch
validateVal :: MonadConjure m => Dom -> E -> m (Maybe Doc)

-- Tuple
validateVal
    [xMatch| innerDoms := domain.tuple.inners|]
    [xMatch| vs        := value.tuple.values |] =

    joinDoc <$> zipWithM validateVal innerDoms vs

validateVal
    dom@[xMatch| attrs     := domain.relation.attributes.attrCollection
               | innerDoms := domain.relation.inners |]
    val@[xMatch| vs        := value.relation.values  |] = do

        let
            vsLength  = mkInt $ genericLength vs

            checkAttr :: MonadConjure m => (Text, Maybe E) -> m (Maybe Doc)
            checkAttr ("size", Just s) =
                satisfied [eMake| &vsLength = &s |]
                "Wrong number of elements in relation" errorDoc

            checkAttr ("minSize", Just s) =
                satisfied [eMake| &vsLength >= &s |]
                "Too few elements" errorDoc

            checkAttr ("maxSize", Just s) =
                satisfied [eMake| &vsLength <= &s |]
                "Too many elements" errorDoc

            checkAttr t = bug $
                vcat [
                       "Not handled, function attribute " <+> pretty t
                     , "in " <+> errorDoc
                     ]

        innerErrors <- mapM (validateVal [xMake| domain.tuple.inners := innerDoms |] ) vs
        attrChecked <- mapM (checkAttr . getAttr) attrs

        return $ joinDoc $ innerErrors ++ attrChecked

    where
    errorDoc = vcat [pretty dom, pretty val]

--Function
validateVal
    dom@[xMatch| attrs       := domain.function.attributes.attrCollection
               | [innerFrom] := domain.function.innerFrom
               | [innerTo]   := domain.function.innerTo |]
    val@[xMatch| mvs         := value.function.values  |] = do

        fromLength <- domSize innerFrom
        toLength   <- domSize innerTo

        let innerDom  = [innerFrom, innerTo]
            vsLength  = mkInt $ genericLength mvs

            checkAttr :: MonadConjure m => (Text, Maybe E) -> m (Maybe Doc)
            checkAttr ("size", Just s) =
                satisfied [eMake| &vsLength = &s |]
                "Wrong number of elements in set" errorDoc

            checkAttr ("minSize", Just s) =
                satisfied [eMake| &vsLength >= &s |]
                "Too few elements" errorDoc

            checkAttr ("maxSize", Just s) =
                satisfied [eMake| &vsLength <= &s |]
                "Too many elements" errorDoc

            checkAttr ("total", Nothing) =
                satisfied [eMake| &vsLength = &fromLength |]
                "Not total" errorDoc

            checkAttr ("injective", Nothing) =
                checkForDuplicates tos "Not injective"

            checkAttr ("surjective", Nothing) =
                let elemsLen = mkInt . genericLength $
                        nubBy ( \(_,b) (_,d) -> b == d) (zip froms tos) in
                satisfied [eMake| &elemsLen >= &toLength |]
                "Not surjective" errorDoc

            checkAttr ("bijective", Nothing) = do
                    joinDoc <$> mapM checkAttr [
                          ("surjective", Nothing)
                        , ("injective",  Nothing) ]

            checkAttr t = bug $
                vcat [
                       "Not handled, function attribute " <+> pretty t
                     , "in " <+> errorDoc
                     ]

        dupError    <- checkForDuplicates froms "Duplicates in domain of function"
        innerErrors <- mapM (checkMappings innerDom) mvs
        attrChecked <- mapM (checkAttr . getAttr) attrs

        return $ joinDoc (dupError : innerErrors ++ attrChecked)

    where
    errorDoc = vcat [pretty dom, pretty val]

    froms = map f mvs
        where f [xMatch| (x:_) := mapping |] = x
              f _ = bug $ vcat [ "function mapping" ]

    tos = map f mvs
        where f [xMatch| (_:y:_) := mapping |] = y
              f _ = bug $ vcat [ "function mapping," ]

    checkMappings innerDom [xMatch| vs := mapping |]  =
        joinDoc <$> zipWithM (\d v -> validateVal d v ) innerDom vs
    checkMappings _ _ = bug $ vcat [ "checkMappings"  ]

    checkForDuplicates :: MonadConjure m => [E] -> Doc -> m (Maybe Doc)
    checkForDuplicates elems doc = do
        case (length (nub elems) == length elems)  of
            True  -> return Nothing
            False -> return $ Just $
                hang doc 4 errorDoc



--Partition
validateVal dom@[xMatch| attrs      := domain.partition.attributes.attrCollection
                       | [innerDom] := domain.partition.inner |]
            val@[xMatch| parts      := value.partition.values |] = do

    domLength <- domSize innerDom

    let numParts = mkInt $ genericLength parts

        checkAttr :: MonadConjure m => (Text, Maybe E) -> m (Maybe Doc)
        checkAttr ("numParts", Just s) =
            satisfied [eMake| &numParts = &s |]
            "Wrong number of parts in partition" errorDoc

        checkAttr ("minNumParts", Just s) =
            satisfied [eMake| &numParts >= &s |]
            "Too few parts in partition" errorDoc

        checkAttr ("maxNumParts", Just s) =
            satisfied [eMake| &numParts <= &s |]
            "Too many parts in partition" errorDoc

        checkAttr ("partSize", Just s) =
            satisfied [eMake| forAll p in parts(&val) . |p| = &s |]
            "Part of invaild size in partition" errorDoc

        checkAttr ("minPartSize", Just s) =
            satisfied [eMake| forAll p in parts(&val) . |p| >= &s |]
            "A part is too larger in partition" errorDoc

        checkAttr ("maxPartSize", Just s) =
            satisfied [eMake| forAll p in parts(&val) . |p| <= &s |]
            "A part is too small in partition" errorDoc

        checkAttr ("complete", Nothing) =
            satisfied [eMake| (sum p in parts(&val) . |p|) = &domLength |]
            "Partition is not complete" errorDoc

        checkAttr ("size", Just s) =
            satisfied [eMake| (sum p in parts(&val) . |p|) = &s |]
            "Wrong number of elements in partition" errorDoc

        checkAttr ("minSize", Just s) =
            satisfied [eMake| (sum p in parts(&val) . |p|) >= &s |]
            "Too many elements in partition" errorDoc

        checkAttr ("maxSize", Just s) =
            satisfied [eMake| (sum p in parts(&val) . |p|) <= &s |]
            "Too few elements in partition" errorDoc

        checkAttr ("regular", Nothing) =
            case allEqual partLengths of
                True  -> return $ Nothing
                False -> return $ Just $ hang "Not a regular partition" 4 errorDoc

        checkAttr t = bug $ vcat [
               "Not handled, function attribute " <+> pretty t
             , "in " <+> errorDoc
             ]


    dupError   <- checkForDuplicates allVs  "Duplicates in partition"
    innerErrors <- mapM (checkParts innerDom) parts
    attrChecked <- mapM (checkAttr . getAttr) attrs


    return $ joinDoc $ dupError : innerErrors ++ attrChecked

    where
    errorDoc = vcat [pretty dom, pretty val]
    allVs = concatMap f parts
        where f [xMatch| vs := part|] = vs
              f _ = bug $ vcat [ "part"]

    partLengths :: [Integer]
    partLengths = map f parts
        where f [xMatch| vs := part|] = genericLength vs
              f _ = bug $ vcat [ "partLengths"]

    allEqual [] = True
    allEqual xs = all ( == head xs) (tail xs)

    checkParts :: MonadConjure m => Dom -> E -> m (Maybe Doc)
    checkParts idom [xMatch| vs := part |]  =
        joinDoc <$> mapM (validateVal idom) vs
    checkParts _ _ = bug $ vcat [ "checkParts"]


    checkForDuplicates :: MonadConjure m => [E] -> Doc -> m (Maybe Doc)
    checkForDuplicates elems doc = do
        case (length (nub elems) == length elems)  of
            True  -> return Nothing
            False -> return $ Just $
                hang doc 4 errorDoc


--set
validateVal
    dom@[xMatch| attrs      := domain.set.attributes.attrCollection
               | [innerDom] := domain.set.inner |]
    val@[xMatch| vs         := value.set.values |] = do

    let vsLength = mkInt $ genericLength vs

        checkAttr :: MonadConjure m => (Text, Maybe E) -> m (Maybe Doc)
        checkAttr ("size", Just s) =
            satisfied [eMake|  &vsLength = &s |]
            "Wrong number of elements in mset" errorDoc

        checkAttr ("minSize", Just s) =
            satisfied [eMake| &vsLength >= &s |]
            "Too many elements in mset" errorDoc

        checkAttr ("maxSize", Just s) =
            satisfied [eMake| &vsLength <= &s |]
            "Too few elements in mset" errorDoc

        checkAttr t = bug $ vcat [
               "Not handled, function attribute " <+> pretty t
             , "in " <+> errorDoc
             ]

        checkForDuplicates =
            case (length (nub vs) == length vs)  of
                True  -> Nothing
                False -> Just $ hang "Duplicates in set" 4 errorDoc

    let dupError = checkForDuplicates
    innerErrors <- mapM (validateVal innerDom) vs
    attrChecked <- mapM (checkAttr . getAttr) attrs

    return $ joinDoc $ dupError : innerErrors ++ attrChecked

    where
    errorDoc = vcat [pretty dom, pretty val]

-- mset TODO minOccur, maxOccur,
validateVal
    dom@[xMatch| attrs      := domain.mset.attributes.attrCollection
               | [innerDom] := domain.mset.inner |]
    val@[xMatch| vs         := value.mset.values |] = do

    let vsLength = mkInt $ genericLength vs

        checkAttr :: MonadConjure m => (Text, Maybe E) -> m (Maybe Doc)
        checkAttr ("size", Just s) =
            satisfied [eMake|  &vsLength = &s |]
            "Wrong number of elements in mset" errorDoc

        checkAttr ("minSize", Just s) =
            satisfied [eMake| &vsLength >= &s |]
            "Too many elements in mset" errorDoc

        checkAttr ("maxSize", Just s) =
            satisfied [eMake| &vsLength <= &s |]
            "Too few elements in mset" errorDoc

        checkAttr ("minOccur", Just [xMatch| [Prim (I j)] := value.literal |] ) =
            case all (\a -> genericLength a >= j  ) (group . sort $ vs)  of
                True -> return Nothing
                False -> return $ Just $ hang "minOccur not satisfied" 4 errorDoc

        checkAttr ("maxOccur", Just [xMatch| [Prim (I j)] := value.literal |] ) =
            case all (\a -> genericLength a <= j  ) (group . sort $ vs)  of
                True -> return Nothing
                False -> return $ Just $ hang "maxOccur not satisfied" 4 errorDoc

        checkAttr t = bug $ vcat [
               "Not handled, function attribute " <+> pretty t
             , "in " <+> errorDoc
             ]

    innerErrors <- mapM (validateVal innerDom) vs
    attrChecked <- mapM (checkAttr . getAttr) attrs

    return $ joinDoc $ innerErrors ++ attrChecked

    where
    errorDoc = vcat [pretty dom, pretty val]

-- Matrix
validateVal
    dom@[dMatch| matrix indexed by [&irDom] of &innerDom  |]
    val@[xMatch| vs   := value.matrix.values
               | [ir] := value.matrix.indexrange |]
    = do

    -- Checking the index range
    irSize <- domSize ir
    irDomSize <- domSize irDom
    mkLog "irSize" . pretty $ irSize
    mkLog "indexSize" . pretty $ irDomSize

    d1 <- satisfied [eMake| &irSize = &irDomSize |]
        "Index range difference sizes for matrix" errorDoc

    -- TODO should domain written in different ways e.g. 1,2,3 insead of 1..3
    -- be treated as being equal?
    d2 <- case ir == irDom of
       True ->  return Nothing
       False -> do
           return . Just $ hang "Index range not the same for matrix" 4 errorDoc


    let vsSize = mkInt $ genericLength vs
    d3 <- satisfied [eMake| &vsSize = &irDomSize |]
        "Invaild number of matrix elements" errorDoc

    case joinDoc [d1,d2,d3] of
        Just s -> return . Just $ s
        Nothing -> do
            vsDocs <- mapM (validateVal innerDom) vs
            return $ joinDoc vsDocs

    where errorDoc = vcat [pretty dom, pretty val]

--ints
validateVal d@[xMatch| rs := domain.int.ranges |]
            v@[xMatch| [Prim (I i)] :=  value.literal |]  = do
    case (rs, any (inDomain i) rs) of
        ([],_)   -> return Nothing
        (_,True) -> return Nothing
        (_,False) -> return . Just $ vcat [
             "Value not in int domain"
            ,pretty d
            ,pretty v
            ]

    where
        inDomain k [xMatch| [Prim (I j)] := range.single.value.literal |] = j == k
        inDomain k [xMatch| [Prim (I j)] := range.from.value.literal |]   = k >= j
        inDomain k [xMatch| [Prim (I l), Prim(I u)] := range.fromTo.value.literal |] =
            k >= l && k <= u
        inDomain _ _ = False

--bool
validateVal [xMatch| _ := domain.bool |]
            [xMatch| [Prim (B _)] :=  value.literal |] = return Nothing

-- matrix
validateVal dom@[dMatch| matrix indexed by [&_] of &_  |]
                [xMatch| vs   := value.matrix.values |] = do

    let vsSize = mkInt $ genericLength vs
    let ir = [dMake| int(1..&vsSize) |]

    let irE = [xMake|  value.matrix.values := vs
                    |  value.matrix.indexrange := [ir] |]

    validateVal dom irE

--error
-- validateVal dom es = bug $ vcat [
--      "validateVal not handled"
--     ,"domain:" <+> pretty dom
--     ,"val:"    <+> pretty es
--     ]

validateVal _ _ = return Nothing

satisfied :: MonadConjure m => E -> Doc -> Doc -> m (Maybe Doc)
satisfied e  header idoc = do
    check <- toBool e
    case check of
        Right (True, _) -> return Nothing
        Right (False, _) -> do
           return $ Just doc
        Left _ -> bug $ vcat ["Could not reduce ", pretty e, "When checking for", doc ]

    where doc = hang header 4 $ idoc


-- Sort all attributes in a domain recursively
sortAttrs :: Dom -> Dom
sortAttrs [xMatch| attrs      := domain.set.attributes.attrCollection
                 | [innerDom] := domain.set.inner |] =
     [xMake| domain.set.attributes.attrCollection := sort attrs
           | domain.set.inner                     := [sortAttrs innerDom] |]

sortAttrs [xMatch| attrs      := domain.mset.attributes.attrCollection
                 | [innerDom] := domain.mset.inner |] =
     [xMake| domain.mset.attributes.attrCollection := sort attrs
           | domain.mset.inner                     := [sortAttrs innerDom] |]

sortAttrs [xMatch| vs := domain.tuple.inners |] =
    [xMake| domain.tuple.inners := map sortAttrs vs |]

sortAttrs [xMatch| attrs      := domain.partition.attributes.attrCollection
                 | [innerDom] := domain.partition.inner |] =
     [xMake| domain.partition.attributes.attrCollection := sort attrs
           | domain.partition.inner                     := [sortAttrs innerDom] |]

sortAttrs [xMatch| attrs       := domain.function.attributes.attrCollection
                 | [innerFrom] := domain.function.innerFrom
                 | [innerTo]   := domain.function.innerTo |] =
     [xMake| domain.function.attributes.attrCollection := sort attrs
           | domain.function.innerFrom                 := [sortAttrs innerFrom]
           | domain.function.innerTo                   := [sortAttrs innerTo] |]

sortAttrs [xMatch| attrs  := domain.relation.attributes.attrCollection
                 | doms   := domain.relation.inner |] =
     [xMake| domain.relation.attributes.attrCollection := sort attrs
           | domain.relation.inner                     := map sortAttrs doms |]


sortAttrs [dMatch| matrix indexed by [&ir] of &inner |] =
    let newInner = sortAttrs inner in
    [dMake| matrix indexed by [&ir] of &newInner |]

sortAttrs d = d

getAttr :: Dom -> (Text, Maybe E)
getAttr [xMatch| [Prim (S n)] := attribute.nameValue.name.reference
               |          [v] := attribute.nameValue.value
               |] = (n,Just v)

getAttr [xMatch| [Prim (S n)] := attribute.name.reference
               |] = (n,Nothing)

getAttr _ = bug $ vcat [ "getAttr" ]

joinDoc :: [Maybe Doc] -> Maybe Doc
joinDoc ds = case catMaybes ds of
    [] -> Nothing
    xs -> Just . vcat $ xs

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
                                else userErr $
                                    vcat [ "Type mismatch for" <+> pretty nm
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
