{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.Unnameds
    ( removeUnnamedsFromModel
    , addUnnamedStructurals
    ) where

import Conjure.Prelude
import Conjure.Language
import qualified Conjure.Language.ModelStats as ModelStats ( finds )


type Unnamed = (Name, Expression)
type FindDecl = (Name, Domain () Expression)

-- | The argument is a model before nameResolution.
--   Only intended to work on problem specifications.
--   Replaces unnamed types with integers.
removeUnnamedsFromModel :: (MonadFail m, MonadLog m) => Model -> m Model
removeUnnamedsFromModel model = do
    statements' <- forM (mStatements model) $ \ st ->
            case st of
                Declaration (LettingDomainDefnUnnamed name size) -> do
                    let outDomain = mkDomainIntB 1 size
                    return $ Declaration $ Letting name $ Domain outDomain
                _ -> return st
    return model { mStatements = statements' }

addUnnamedStructurals :: (MonadFail m, MonadLog m, NameGen m) => Model -> m Model
addUnnamedStructurals model = do
    let
        -- assuming the info is ready by this point
        allUnnnameds :: [Unnamed]
        allUnnnameds = model |> mInfo |> miUnnameds

        allFinds :: [FindDecl]
        allFinds = model |> ModelStats.finds

        -- a subset of allThatCanBeBroken will have to be selected for symmetry breaking
        allThatCanBeBroken :: [(Unnamed, [FindDecl])]
        allThatCanBeBroken =
            [ (unnamed, decVars)
            | unnamed <- allUnnnameds
            , let decVars =
                    [ decVar
                    | decVar  <- allFinds
                    -- if the domain of the find contains this unnamed in it
                    , not $ null [ () | DomainReference n _ <- universe (snd decVar)
                                      , n == fst unnamed
                                      ]
                    ]
            ]

        -- TODO: the following is fairly arbitrary
        -- it keeps all finds for all unnamed types
        subsetToBeBroken :: [(Unnamed, [FindDecl])]
        subsetToBeBroken =
            [ (unnamed, decVars)
            | (unnamed, decVars) <- allThatCanBeBroken
            , not (null decVars)
            ]

    cons <- sequence [ mkUnnamedStructuralCons unnamed decVars
                     | (unnamed, decVars) <- subsetToBeBroken
                     ]
    case catMaybes cons of
        []         -> return model
        newConsYay -> return model { mStatements = mStatements model ++ [SuchThat newConsYay] }


mkUnnamedStructuralCons
    :: (MonadFail m, NameGen m)
    => Unnamed
    -> [FindDecl]
    -> m (Maybe Expression)
mkUnnamedStructuralCons (unnamedName, unnamedSize) finds = do
    (iPat, i) <- quantifiedVar
    (jPat, j) <- lettingVar
    let
        toCons :: [(Expression, Expression)] -> Expression
        toCons [(lhs, rhs)] =
                [essence|
                    and([ &lhs >=lex &rhs
                        | &iPat : int(1..&unnamedSize - 1)
                        , letting &jPat be &i + 1
                        ])
                |]
        toCons pairs =
            let
                lhs = fromList (map fst pairs)
                rhs = fromList (map snd pairs)
            in
                [essence|
                    and([ flatten(&lhs) >=lex flatten(&rhs)
                        | &iPat : int(1..&unnamedSize - 1)
                        , letting &jPat be &i + 1
                        ])
                |]
    pairs <- concat <$> sequence
        [ onDomain i j var domain
        | (name, domain) <- finds
        , let var = Reference name (Just (DeclNoRepr Find name domain Region_UnnamedSymBreaking))
        ]
    if null pairs
        then return Nothing
        else return (Just (toCons pairs))

    where

    -- current=i then j
    -- current=j then i
    -- otherwise      current
    swapIJ i j current =
        -- if current=i then j else (if current=j then i else current)
        -- (current=i) + 2 * (current=j)    current=i ----> 1    (output is j)
        --                                  current=j ----> 2    (output is i)
        --                                  otherwise ----> 0    (output is current)
        [essence| [ &current, &j, &i ; int(0..2) ] [ toInt(&current=&i) + 2 * toInt(&current=&j) ] |]

    returnPair a b = return [(a, b)]

    onDomain
        :: (NameGen m)
        => Expression
        -> Expression
        -> Expression
        -> Domain () Expression
        -> m [(Expression, Expression)]
    onDomain i j var (DomainReference n _) | n == unnamedName = do
        (k1Pat, k1) <- quantifiedVar
        (k2Pat, k2) <- lettingVar
        let k2Val = swapIJ i j k1
        returnPair
            [essence|
                [ &k1 = &var                            | &k1Pat : int(1..&unnamedSize)
                                                        ]
            |]
            [essence|
                [ &k2 = &var                            | &k1Pat : int(1..&unnamedSize)
                                                        , letting &k2Pat be &k2Val
                                                        ]
            |]

    onDomain i j var (DomainSet _ _ (DomainReference n _)) | n == unnamedName = do
        (k1Pat, k1) <- quantifiedVar
        (k2Pat, k2) <- lettingVar
        let k2Val = swapIJ i j k1
        returnPair
            [essence|
                [ &k1 in &var                           | &k1Pat : int(1..&unnamedSize)
                                                        ]
            |]
            [essence|
                [ &k2 in &var                           | &k1Pat : int(1..&unnamedSize)
                                                        , letting &k2Pat be &k2Val
                                                        ]
            |]

    onDomain i j var (DomainMSet _ _ (DomainReference n _)) | n == unnamedName = do
        (k1Pat, k1) <- quantifiedVar
        (k2Pat, k2) <- lettingVar
        let k2Val = swapIJ i j k1
        returnPair
            [essence|
                [ freq(&var, &k1)                       | &k1Pat : int(1..&unnamedSize)
                                                        ]
            |]
            [essence|
                [ freq(&var, &k2)                       | &k1Pat : int(1..&unnamedSize)
                                                        , letting &k2Pat be &k2Val
                                                        ]
            |]

    onDomain i j var (DomainFunction _ _ (DomainReference n _) domTo) | n == unnamedName = do
        (k1Pat, k1) <- quantifiedVar
        (k2Pat, k2) <- lettingVar
        let k2Val = swapIJ i j k1
        (zPat , z ) <- quantifiedVar
        returnPair
            [essence|
                [ image(&var, &k1) = &z                 | &k1Pat : int(1..&unnamedSize)
                                                        , &zPat  : &domTo
                                                        ]
            |]
            [essence|
                [ image(&var, &k2) = &z                 | &k1Pat : int(1..&unnamedSize)
                                                        , &zPat  : &domTo
                                                        , letting &k2Pat be &k2Val
                                                        ]
            |]

    onDomain i j var (DomainFunction _ _ domFr (DomainReference n _)) | n == unnamedName = do
        (k1Pat, k1) <- quantifiedVar
        (k2Pat, k2) <- lettingVar
        let k2Val = swapIJ i j k1
        (zPat , z ) <- quantifiedVar
        returnPair
            [essence|
                [ image(&var, &z) = &k1                 | &zPat  : &domFr
                                                        , &k1Pat : int(1..&unnamedSize)
                                                        ]
            |]
            [essence|
                [ image(&var, &z) = &k2                 | &zPat  : &domFr
                                                        , &k1Pat : int(1..&unnamedSize)
                                                        , letting &k2Pat be &k2Val
                                                        ]
            |]

    onDomain i j var (DomainFunction _ _ (DomainTuple [DomainReference n _, domFr2]) domTo) | n == unnamedName = do
        (k1Pat, k1) <- quantifiedVar
        (k2Pat, k2) <- lettingVar
        let k2Val = swapIJ i j k1
        (z1Pat, z1) <- quantifiedVar
        (z2Pat, z2) <- quantifiedVar
        returnPair
            [essence|
                [ image(&var, (&k1, &z1)) = &z2         | &k1Pat : int(1..&unnamedSize)
                                                        , &z1Pat : &domFr2
                                                        , &z2Pat : &domTo
                                                        ]
            |]
            [essence|
                [ image(&var, (&k2, &z1)) = &z2         | &k1Pat : int(1..&unnamedSize)
                                                        , &z1Pat : &domFr2
                                                        , &z2Pat : &domTo
                                                        , letting &k2Pat be &k2Val
                                                        ]
            |]

    onDomain i j var (DomainFunction _ _ (DomainTuple [domFr1, DomainReference n _]) domTo) | n == unnamedName = do
        (k1Pat, k1) <- quantifiedVar
        (k2Pat, k2) <- lettingVar
        let k2Val = swapIJ i j k1
        (z1Pat, z1) <- quantifiedVar
        (z2Pat, z2) <- quantifiedVar
        returnPair
            [essence|
                [ image(&var, (&z1, &k1)) = &z2         | &k1Pat : int(1..&unnamedSize)
                                                        , &z1Pat : &domFr1
                                                        , &z2Pat : &domTo
                                                        ]
            |]
            [essence|
                [ image(&var, (&z1, &k2)) = &z2         | &k1Pat : int(1..&unnamedSize)
                                                        , &z1Pat : &domFr1
                                                        , &z2Pat : &domTo
                                                        , letting &k2Pat be &k2Val
                                                        ]
            |]

    -- partial
    onDomain i j var domain = do
        (k1Pat, k1) <- quantifiedVar
        (k2Pat, k2) <- lettingVar
        let k2Val = swapIJ i j k1
        k1Count <- count var k1 domain
        k2Count <- count var k2 domain
        returnPair
            [essence|
                [ &k1Count                              | &k1Pat : int(1..&unnamedSize)
                                                        ]
            |]
            [essence|
                [ &k2Count                              | &k1Pat : int(1..&unnamedSize)
                                                        , letting &k2Pat be &k2Val
                                                        ]
            |]

    -- onDomain _ _ _ _ = return []


    isReferenced dom =
        0 < length [ () | DomainReference n _ <- universe dom, n == unnamedName ]


    count
        :: (NameGen m)
        => Expression
        -> Expression
        -> Domain () Expression
        -> m Expression
    count var val (DomainReference n _) | n == unnamedName =
        return [essence| toInt(&var = &val) |]

    count var val (DomainSet _ _ inner) | isReferenced inner = do
        (iPat, i) <- quantifiedVar
        iCount <- count i val inner
        return [essence| sum([ &iCount | &iPat <- &var ]) |]

    count var val (DomainMSet _ _ inner) | isReferenced inner = do
        (iPat, i) <- quantifiedVar
        iCount <- count i val inner
        return [essence| sum([ &iCount | &iPat <- &var ]) |]

    count var val (DomainFunction _ _ innerFr _) | isReferenced innerFr = do
        (iPat, i) <- quantifiedVar
        iCount <- count i val innerFr
        return [essence| sum([ &iCount | &iPat <- defined(&var) ]) |]

    count var val (DomainFunction _ _ _ innerTo) | isReferenced innerTo = do
        (iPat, i) <- quantifiedVar
        iCount <- count i val innerTo
        return [essence| sum([ &iCount | &iPat <- range(&var) ]) |]

    count _ _ _ = return [essence| 0 |]

