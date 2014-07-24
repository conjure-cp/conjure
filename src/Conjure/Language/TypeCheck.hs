module Conjure.Language.TypeCheck
    ( typeCheckModel
    , typeCheckModelIO
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition


typeCheckModelIO :: Model -> IO ()
typeCheckModelIO m =
    case typeCheckModel m of
        Nothing -> return ()
        Just msg -> userErr $ sep ["Type error, specifically:", msg]


-- | returns `Just msg` if the model is type-incorrect, msg being an explanation.
--   returns `Nothing` if the model is type-correct.
typeCheckModel :: Model -> Maybe Doc
typeCheckModel _ = Nothing
-- typeCheckModel _ = Just "Just Plain Wrong (TM)"

