module Conjure.Language.RepresentationOf where

-- conjure
import Conjure.Prelude
import Conjure.Language.Domain


class RepresentationOf a where
    representationTreeOf :: MonadFail m => a -> m (Tree (Maybe HasRepresentation))

representationOf :: (RepresentationOf a, MonadFail m) => a -> m HasRepresentation
representationOf a = do
    tree <- representationTreeOf a
    case rootLabel tree of
        Nothing               -> fail "doesn't seem to have a representation"
        Just NoRepresentation -> fail "doesn't seem to have a representation"
        Just r -> return r

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

sameRepresentationTree :: (RepresentationOf a, MonadFail m) => a -> a -> m ()
sameRepresentationTree x y = do
    let fails = fail "doesn't seem to have a representation"
    xTree <- representationTreeOf x
    yTree <- representationTreeOf y
    case (rootLabel xTree, rootLabel yTree) of
        (Nothing, _)               -> fails
        (_, Nothing)               -> fails
        (Just NoRepresentation, _) -> fails
        (_, Just NoRepresentation) -> fails
        (Just{}, Just{}) | xTree == yTree -> return ()
                         | otherwise -> fails
