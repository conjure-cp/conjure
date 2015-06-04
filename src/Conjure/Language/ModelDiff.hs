module Conjure.Language.ModelDiff
    ( modelDiff
    , modelDiffIO
    ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Pretty

-- containers
import qualified Data.Set as S


modelDiffIO :: MonadUserError m => Model -> Model -> m ()
modelDiffIO m1 m2 =
    case modelDiff m1 m2 of
        Nothing -> return ()
        Just msg -> userErr $ return $ sep ["Files differ, specifically:", msg]


-- | returns `Just msg` if the models are different, msg being an explanation of what's diff.
--   returns `Nothing` if the models are the same.
modelDiff :: Model -> Model -> Maybe Doc
modelDiff m1 m2 =
    let
        explode = concatMap $ \ st -> case st of SuchThat xs -> map (SuchThat . return) xs
                                                 Where    xs -> map (Where    . return) xs
                                                 _           -> [st]
        m1Statements = m1 |> mStatements |> explode |> S.fromList
        m2Statements = m2 |> mStatements |> explode |> S.fromList
        m1Extra = S.difference m1Statements m2Statements
        m2Extra = S.difference m2Statements m1Statements
    in
        if S.null m1Extra && S.null m2Extra
            then Nothing
            else Just $ vcat $ concat
                [ [ "These models seem to be different." ]
                , [ hang "Only in the 1st:" 8 (vcat (map pretty (S.toList m1Extra))) | not (S.null m1Extra) ]
                , [ hang "Only in the 2nd:" 8 (vcat (map pretty (S.toList m2Extra))) | not (S.null m2Extra) ]
                ]

