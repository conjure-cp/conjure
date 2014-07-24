{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.ValidateSolution
    ( validateSolution
    , validateSolutionPure
    , validateSolutionPureNew
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


-- this will return True is it's valid, False if not
-- the validator might use error


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

    let validateBinder bb@(Binder name val) | Just f <- lookup name finds  = do
            mkLog "dom" $ pretty f
            mkLog "val" $ pretty val
            res <-  validateVal (sortAttrs f) (normaliseSolutionE val)
            return $ case res of
                Just doc -> Just $ vcat [
                      "Error for value" <+> pretty bb
                    , "Domain: " <+> pretty f
                    , hang "Details:" 2 doc
                    , "---"]
                Nothing -> Nothing

        validateBinder v = do
            bdoc <- bindersDoc
            bug $ vcat [ "Find not found ", pretty v,  bdoc, pretty spec ]

    docs <-  mapM validateBinder bs
    case catMaybes docs of
        [] -> return ()
        xs -> userErr $ vcat xs

-- Attributes must be listed in sorted order for dMatch
validateVal :: MonadConjure m => Dom -> E -> m (Maybe Doc)

--Function
validateVal
    dom@[xMatch| attrs       := domain.function.attributes.attrCollection
               | [innerFrom] := domain.function.innerFrom
               | [innerTo]   := domain.function.innerTo |]
    val@[xMatch| mvs         := value.function.values  |] = do
        let innerDom  = [innerFrom, innerTo]
            vsLength = mkInt $ genericLength mvs

            checkAttr :: MonadConjure m => (Text, Maybe E) -> m (Maybe Doc)
            checkAttr ("size", Just s) =
                satisfied [eMake| &vsLength = &s |]
                "Wrong number of elements in set" errorDoc

            checkAttr ("minSize", Just s) =
                satisfied [eMake| &vsLength > &s |]
                "Too few elements" errorDoc

            checkAttr ("maxSize", Just s) =
                satisfied [eMake| &vsLength < &s |]
                "Too many elements" errorDoc

            checkAttr t = bug $ vcat [
                   "Not handled, function attribute " <+> pretty t
                 , "in " <+> errorDoc
                 ]

            checkForDuplicates :: MonadConjure m => m (Maybe Doc)
            checkForDuplicates = do
                case (length (nub froms) == length froms)  of
                    True  -> return Nothing
                    False -> return $ Just $
                        hang "Duplicates in domain of function" 4 errorDoc

        dupError   <- checkForDuplicates
        innerErrors <- mapM (checkMappings innerDom) mvs
        attrChecked <- mapM (checkAttr . getAttr) attrs

        return $ joinDoc (dupError : innerErrors ++ attrChecked)

    where
    errorDoc = vcat [pretty dom, pretty val]

    froms = map f mvs
        where f [xMatch| (x:_) := mapping |] = x

    checkMappings innerDom [xMatch| vs := mapping |]  =
        joinDoc <$> zipWithM (\d v -> validateVal d v ) innerDom vs

    getAttr [xMatch| [Prim (S n)] := attribute.nameValue.name.reference
                   |           [v] := attribute.nameValue.value
                   |] = (n,Just v)

    getAttr [xMatch| [Prim (S n)] := attribute.name.reference
                   |] = (n,Nothing)



--set
validateVal
    dom@[xMatch| _          := domain.set.attributes.attrCollection
               | [innerDom] := domain.set.inner |]
    val@[xMatch| vs         := value.set.values |] = do

    let vsLength = mkInt $ genericLength vs

        checkAtts [dMatch| set (maxSize &ms) of &_ |] =
            satisfied [eMake| &vsLength <= &ms |] "Too many elements in set" errorDoc

        checkAtts [dMatch| set (minSize &ms) of &_ |] =
            satisfied [eMake| &vsLength >= &ms |] "Too few elements in set" errorDoc

        checkAtts [dMatch| set (size &s) of &_ |] =
            satisfied [eMake| &vsLength = &s |]
            "Wrong number of elements in set" errorDoc

        checkAtts [dMatch| set (maxSize &b, minSize &a) of &_ |] =
            satisfied [eMake| &vsLength >= &a /\ &vsLength <= &b |]
            "Wrong number of elements in set" errorDoc

        checkAtts [dMatch| set (maxSize &b, minSize &a, size &s) of &_ |] =
            satisfied [eMake| &vsLength >= &a /\ &vsLength <= &b /\ &vsLength = &s |]
            "Wrong number of elements in set" errorDoc

        checkAtts [dMatch| set (minSize &a, size &s) of &_ |] =
            satisfied [eMake| &vsLength >= &a /\ &vsLength = &s |]
            "Wrong number of elements in set" errorDoc

        checkAtts [dMatch| set (maxSize &b, size &s) of &_ |] =
            satisfied [eMake| &vsLength <= &b /\ &vsLength = &s |]
            "Wrong number of elements in set" errorDoc

        checkAtts [dMatch| set of &_ |] = return Nothing

        checkAtts dd = bug $ vcat ["Could not check set", pretty dd]


    let checkForDuplicates mdoc = do
            case (length (nub vs) == length vs)  of
                True  -> mdoc
                False -> joinDoc [mdoc, Just $ hang "Duplicates in set" 4 errorDoc ]

    attrCheck <- checkAtts dom
    checkForDuplicates <$> case attrCheck of
        Just d  ->  return $ Just d
        Nothing -> do
            vsDocs <- mapM (validateVal innerDom) vs
            return $ joinDoc vsDocs

    where
    errorDoc = vcat [pretty dom, pretty val]

-- mset TODO minOccur, maxOccur,
validateVal
    dom@[xMatch| _          := domain.mset.attributes.attrCollection
               | [innerDom] := domain.mset.inner |]
    val@[xMatch| vs         := value.mset.values |] = do

    let vsLength = mkInt $ genericLength vs

        checkAtts [dMatch| mset (maxSize &ms) of &_ |] =
            satisfied [eMake| &vsLength <= &ms |] "Too many elements in mset" errorDoc

        checkAtts [dMatch| mset (minSize &ms) of &_ |] =
            satisfied [eMake| &vsLength >= &ms |] "Too few elements in mset" errorDoc

        checkAtts [dMatch| mset (size &s) of &_ |] =
            satisfied [eMake| &vsLength = &s |]
            "Wrong number of elements in mset" errorDoc

        checkAtts [dMatch| mset (maxSize &b, minSize &a) of &_ |] =
            satisfied [eMake| &vsLength >= &a /\ &vsLength <= &b |]
            "Wrong number of elements in mset" errorDoc

        checkAtts [dMatch| mset (maxSize &b, minSize &a, size &s) of &_ |] =
            satisfied [eMake| &vsLength >= &a /\ &vsLength <= &b /\ &vsLength = &s |]
            "Wrong number of elements in mset" errorDoc

        checkAtts [dMatch| mset (minSize &a, size &s) of &_ |] =
            satisfied [eMake| &vsLength >= &a /\ &vsLength = &s |]
            "Wrong number of elements in mset" errorDoc

        checkAtts [dMatch| mset (maxSize &b, size &s) of &_ |] =
            satisfied [eMake| &vsLength <= &b /\ &vsLength = &s |]
            "Wrong number of elements in mset" errorDoc

        checkAtts [dMatch| mset of &_ |] = return Nothing

        checkAtts dd = bug $ vcat ["Could not check mset", pretty dd]


    attrCheck <- checkAtts dom
    case attrCheck  of
        Just d  -> return . Just $  d
        Nothing -> do
            vsDocs <- mapM (validateVal innerDom) vs
            return $ joinDoc vsDocs

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

    -- TODO domain written in different ways e.g. 1,2,3 insead of 1..3
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
validateVal dom es = bug $ vcat [
     "validateVal not handled"
    ,"domain:" <+> pretty dom
    ,"val:"    <+> pretty es
    ]


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


joinDoc :: [Maybe Doc] -> Maybe Doc
joinDoc ds = case catMaybes ds of
    [] -> Nothing
    xs -> Just . vcat $ xs

getInt :: E -> Integer
getInt  [xMatch| [Prim (I j)] :=  value.literal  |] = j
getInt e = bug $ vcat [ "Not an int" <+> pretty e ]

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


