module Conjure.UI.TranslateSolution ( translateSolution ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Representations ( up )


translateSolution
    :: MonadFail m
    => Model      -- eprime model
    -> Model      -- essence param
    -> Model      -- eprime solution
    -> m Model    -- essence solution
translateSolution eprimeModel essenceParam eprimeSolution = do

    let eprimeLettings = extractLettings essenceParam ++
                         extractLettings eprimeSolution
    let essenceFindNames = eprimeModel |> mInfo |> miFinds
    let essenceFinds     = eprimeModel |> mInfo |> miRepresentations
                                       |> filter (\ (n,_) -> n `elem` essenceFindNames )

    eprimeLettings' <- forM eprimeLettings $ \ (name, val) -> do
        constant <- instantiateExpression eprimeLettings val
        return (name, constant)

    essenceFinds' <- forM essenceFinds $ \ (name, dom) ->
        case dom of
            DomainEnum nm Nothing -> do -- this is an enum domain whose value should be in the param file
                let enumsInParam =
                        [ vals
                        | Declaration (LettingDomainDefnEnum nm2 vals) <- mStatements essenceParam
                        , nm == nm2
                        ]
                case enumsInParam of
                    []     -> fail ("No value given for enumerated type:" <+> pretty nm)
                    [vals] -> return (name, DomainEnum nm (Just (vals, [])))
                    _      -> fail ("Multiple values are given for enumerated type:" <+> pretty nm)
            _ -> do -- any other domain
                constant <- instantiateDomain eprimeLettings dom
                return (name, constant)

    essenceLettings <- mapM (up eprimeLettings') essenceFinds'

    return def
        { mStatements = sortNub
            [ Declaration (Letting n (Constant x))
            | (n, x) <- essenceLettings
            ]
        }

extractLettings :: Model -> [(Name, Expression)]
extractLettings model =
    [ (n, x) | Declaration (Letting n x) <- mStatements model ]

