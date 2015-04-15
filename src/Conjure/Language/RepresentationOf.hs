module Conjure.Language.RepresentationOf where

-- conjure
import Conjure.Prelude
import Conjure.Language.Name
import Conjure.Language.Domain


class RepresentationOf a where
    representationTreeOf :: MonadFail m => a -> m (Tree (Maybe HasRepresentation))

representationOf :: (RepresentationOf a, MonadFail m) => a -> m Name
representationOf a = do
    tree <- representationTreeOf a
    case rootLabel tree of
        Just (HasRepresentation r) -> return r
        _ -> fail "doesn't seem to have a representation"

hasRepresentation :: (RepresentationOf a, MonadFail m) => a -> m ()
hasRepresentation x =
    case representationOf x of
        Nothing -> fail "doesn't seem to have a representation"
        Just _  -> return ()

sameRepresentation :: (RepresentationOf a, MonadFail m) => a -> a -> m ()
sameRepresentation x y =
    case (representationOf x, representationOf y) of
        (Just rx, Just ry) | rx == ry -> return ()
        _ -> fail "doesn't seem to have the same representation"
