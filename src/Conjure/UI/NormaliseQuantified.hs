module Conjure.UI.NormaliseQuantified
    ( normaliseQuantifiedVariables
    , normaliseQuantifiedVariablesE
    , normaliseQuantifiedVariablesS
    , distinctQuantifiedVars
    , renameQuantifiedVarsToAvoidShadowing
    ) where

import Conjure.Prelude
import Conjure.Language


normaliseQuantifiedVariables :: Model -> Model
normaliseQuantifiedVariables m@Model{mStatements=st} =
    let stOut = map normaliseQuantifiedVariablesS st
    in  m { mStatements = stOut }

normaliseQuantifiedVariablesE :: Expression -> Expression
normaliseQuantifiedVariablesE = normX_Leveled 1

normaliseQuantifiedVariablesS :: Statement -> Statement
normaliseQuantifiedVariablesS = descendBi normaliseQuantifiedVariablesE

normX_Leveled :: Int -> Expression -> Expression
normX_Leveled nextInt p@(Comprehension _ gocs) =
    let
        quantifiedNames = getQuantifiedNames gocs
        oldNew =
            [ (qn, MachineName "q" i [])
            | (qn, i) <- zip quantifiedNames [nextInt..]
            ]
        nextInt' = nextInt + length oldNew
        f :: Name -> Name
        f nm = fromMaybe nm (lookup nm oldNew)
    in
        p |> descend (normX_Leveled nextInt')
          |> transformBi f
normX_Leveled nextInt p =
        p |> descend (normX_Leveled nextInt)


distinctQuantifiedVars :: NameGen m => Model -> m Model
distinctQuantifiedVars m@Model{mStatements=stmts} = do
    let
        -- usedOnce :: [Name]
        -- usedOnce =
        --     [ nm
        --     | (nm, nb) <- histogram
        --                     [ nm
        --                     | Comprehension _ gocs <- universeBi stmts
        --                     , nm <- getQuantifiedNames gocs
        --                     ]
        --     , nb == 1
        --     ]

        usedInALetting :: [Name]
        usedInALetting =
            [ nm
            | Declaration (Letting _ x) <- stmts
            , Comprehension _ gocs <- universe x
            , nm <- getQuantifiedNames gocs
            ]

        normX_Distinct :: NameGen m => Expression -> m Expression
        normX_Distinct p@(Comprehension _ gocs) = do
            let quantifiedNames = getQuantifiedNames gocs
            oldNew <- sequence
                    [ do
                        if qn `elem` usedInALetting
                            then do
                                new <- nextName "q"
                                return (qn, new)
                            else
                                return (qn, qn)
                    | qn <- quantifiedNames
                    ]
            let
                f :: Name -> Name
                f nm = fromMaybe nm (lookup nm oldNew)

            p' <- descendM normX_Distinct p
            return (transformBi f p')
        normX_Distinct p = descendM normX_Distinct p

    if null usedInALetting
        then return m
        else do
            stmtsOut <- forM stmts $ \ stmt ->
                case stmt of
                    Declaration Letting{} -> return stmt
                    _                     -> descendBiM normX_Distinct stmt
            namegenst <- exportNameGenState
            let miInfoOut = (mInfo m) { miNameGenState = namegenst }
            return m { mStatements = stmtsOut, mInfo = miInfoOut }


renameQuantifiedVarsToAvoidShadowing :: NameGen m => Model -> m Model
renameQuantifiedVarsToAvoidShadowing model = do
    let

        allDecls :: [Name]
        allDecls = concat [ case d of
                                FindOrGiven _ nm _ -> [nm]
                                Letting nm _ -> [nm]
                                GivenDomainDefnEnum nm -> [nm]
                                LettingDomainDefnEnum nm nms -> nm : nms
                                LettingDomainDefnUnnamed nm _ -> [nm]
                          | Declaration d <- mStatements model
                          ]

        rename :: NameGen m => Expression -> m Expression
        rename p@(Comprehension _ gocs) = do
            let quantifiedNames = getQuantifiedNames gocs
            oldNew <- concat <$> sequence
                    [ do
                        if qn `elem` allDecls
                            then do
                                new <- nextName "shadow"
                                return [(qn, new)]
                            else
                                return []
                    | qn <- quantifiedNames
                    ]
            let
                f :: Name -> Name
                f nm = fromMaybe nm (lookup nm oldNew)

            let p' = transformBi f p
            descendM rename p'
        rename p = descendM rename p

    stmtsOut <- descendBiM rename (mStatements model)
    namegenst <- exportNameGenState
    let miInfoOut = (mInfo model) { miNameGenState = namegenst }
    return model { mStatements = stmtsOut, mInfo = miInfoOut }


getQuantifiedNames :: [GeneratorOrCondition] -> [Name]
getQuantifiedNames gocs = concat
    [ case gen of
        GenDomainNoRepr  pat _ -> universeBi pat
        GenDomainHasRepr nm  _ -> [nm]
        GenInExpr        pat _ -> universeBi pat
    | Generator gen <- gocs
    ]

