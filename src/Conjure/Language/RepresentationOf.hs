module Conjure.Language.RepresentationOf where

-- conjure
import Conjure.Prelude
import Conjure.Language.Domain
import Conjure.Language.TypeOf ( TypeCheckerMode )


class RepresentationOf a where
    representationTreeOf
        :: (MonadFail m, ?typeCheckerMode :: TypeCheckerMode)
        => a -> m (Tree (Maybe HasRepresentation))

representationOf :: (RepresentationOf a, MonadFail m, ?typeCheckerMode :: TypeCheckerMode) => a -> m HasRepresentation
representationOf a = do
    tree <- representationTreeOf a
    case rootLabel tree of
        Nothing               -> fail "doesn't seem to have a representation"
        Just NoRepresentation -> fail "doesn't seem to have a representation"
        Just r -> return r

hasRepresentation :: (RepresentationOf a, MonadFail m, ?typeCheckerMode :: TypeCheckerMode) => a -> m ()
hasRepresentation x =
    case representationOf x of
        Nothing -> fail "doesn't seem to have a representation"
        Just _  -> return ()

sameRepresentation :: (RepresentationOf a, MonadFail m, ?typeCheckerMode :: TypeCheckerMode) => a -> a -> m ()
sameRepresentation x y =
    case (representationOf x, representationOf y) of
        (Just rx, Just ry) | rx == ry -> return ()
        _ -> fail "doesn't seem to have the same representation"

sameRepresentationTree :: (RepresentationOf a, MonadFail m, ?typeCheckerMode :: TypeCheckerMode) => a -> a -> m ()
sameRepresentationTree x y = do
    xTree <- representationTreeOf x
    yTree <- representationTreeOf y
    unless (xTree == yTree) $
        fail "doesn't seem to have the same representation tree"
