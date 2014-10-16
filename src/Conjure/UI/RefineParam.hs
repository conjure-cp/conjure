module Conjure.UI.RefineParam ( refineParam ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Representations ( downC )


refineParam
    :: ( Functor m
       , Applicative m
       , MonadFail m
       )
    => Model      -- eprime model
    -> Model      -- essence param
    -> m Model    -- eprime param
refineParam eprimeModel essenceParam = do

    let essenceLettings   = extractLettings essenceParam
    let essenceGivenNames = eprimeModel |> mInfo |> miGivens
    let essenceGivens     = eprimeModel |> mInfo |> miRepresentations
                                        |> filter (\ (n,_) -> n `elem` essenceGivenNames )

    -- TODO: check if for every given there is a letting (there can be more)
    -- TODO: check if the same letting has multiple values for it

    essenceLettings' <- forM essenceLettings $ \ (name, val) -> do
        constant <- instantiateExpression essenceLettings val
        return (name, constant)

    essenceGivens' <- forM essenceGivens $ \ (name, dom) -> do
        constant <- instantiateDomain essenceLettings dom
        return (name, constant)

    let essenceGivensAndLettings =
            [ case lookup n essenceLettings' of
                Nothing -> userErr $ vcat
                            [ "No value for parameter:" <+> pretty n
                            , "With domain:" <+> pretty d
                            ]
                Just v  -> (n, d, v)
            | (n, d) <- essenceGivens'
            ]

    eprimeLettings <- liftM concat $ mapM downC essenceGivensAndLettings

    return $ languageEprime def
        { mStatements =
            [ Declaration (Letting n (Constant x))
            | (n, _, x) <- eprimeLettings
            ]
        }

extractLettings :: Model -> [(Name, Expression)]
extractLettings model =
    [ (n, x) | Declaration (Letting n x) <- mStatements model ]

