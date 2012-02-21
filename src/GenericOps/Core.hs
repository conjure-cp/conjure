{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE FlexibleContexts          #-}

module GenericOps.Core
    ( Hole(..), HoleStatus(..)
    , NodeTag(..), nodeTagData
    , GPlate(..), gplateLeaf, gplateSingle, gplateUniList, gplateError
    , GNode(..), mkG, showG, fromG, fromGs, (===)
    , liftG, liftGM, unliftG, unliftGM
    , universe, universeBi
    , descend, descendM
    , bottomUp, bottomUpM
    , topDown, topDownM
    , MatchBind(..)
    ) where

import Control.Arrow ( first, second )
import Control.Monad ( liftM, zipWithM_ )
import Control.Monad.Error ( MonadError(..) )
import Control.Monad.State ( MonadState (..), gets, modify )
import Data.Data ( Data, toConstr )
import Data.Generics ( Typeable, TypeRep, typeOf )
import Data.Maybe ( mapMaybe)
import Data.Typeable ( cast )
import Unsafe.Coerce ( unsafeCoerce )

import ParsePrint


--------------------------------------------------------------------------------
-- NodeTag ---------------------------------------------------------------------
--------------------------------------------------------------------------------

class NodeTag a where
    nodeTag :: a -> String

    default nodeTag :: Data a => a -> String
    nodeTag = nodeTagData

nodeTagData :: Data a => a -> String
nodeTagData = show . toConstr



--------------------------------------------------------------------------------
-- Hole ------------------------------------------------------------------------
--------------------------------------------------------------------------------

class Hole a where
    hole :: a -> HoleStatus
    hole _ = NotAHole

data HoleStatus = NotAHole | UnnamedHole | NamedHole String


--------------------------------------------------------------------------------
-- GPlate ----------------------------------------------------------------------
--------------------------------------------------------------------------------

class (NodeTag a, Hole a, Typeable a, Eq a, Show a, ParsePrint a, MatchBind a) => GPlate a where
    gplate :: a -> ([GNode], [GNode] -> a)
    gplate = gplateLeaf

gplateLeaf :: GPlate a => a -> ([GNode], [GNode] -> a)
gplateLeaf x = ([], const x)

gplateSingle ::
    (GPlate a, GPlate b)
    => (b -> a)
    -> b
    -> ([GNode], [GNode] -> a)
gplateSingle f x
    = ( [mkG x]
      , \ xs -> case xs of
                    [mx] -> case fromG mx of
                            Just x' -> f x'
                            _       -> gplateError
                    _    -> gplateError
      )

gplateUniList ::
    (GPlate a, GPlate b)
    => ([b] -> a)
    -> [b]
    -> ([GNode], [GNode] -> a)
gplateUniList f xs =
    ( map mkG xs
    , \ gxs' -> let xs' = fromGs gxs'
                in  if xs `sameLength` xs'
                        then f xs'
                        else gplateError
    )

gplateError :: a
gplateError = error "internal error: GPlate"


--------------------------------------------------------------------------------
-- GNode -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data GNode = forall a . (GPlate a) => GNode TypeRep a

instance Show GNode where
    show (GNode t _) = "GNode{" ++ show t ++ "}"

showG :: GNode -> String
showG (GNode _ x) = show x

mkG :: GPlate a => a -> GNode
mkG x = GNode (typeOf x) x

fromG :: Typeable a => GNode -> Maybe a
fromG (GNode t v) =
    case unsafeCoerce v of
        r | t == typeOf r -> Just r
          | otherwise     -> Nothing

fromGs :: Typeable a => [GNode] -> [a]
fromGs gs = mapMaybe fromG gs

(===) :: GNode -> GNode -> Bool
(GNode _ x) === (GNode _ y) = cast x == Just y


liftG :: GPlate a => (a -> a) -> GNode -> GNode
liftG f x = case fromG x of
                Nothing -> x
                Just y  -> mkG (f y)

liftGM :: (GPlate a, Monad m) => (a -> m a) -> GNode -> m GNode
liftGM f x = case fromG x of
                Nothing -> return x
                Just y  -> mkG `liftM` f y

unliftG :: GPlate a => (GNode -> GNode) -> a -> a
unliftG f x = case fromG $ f $ mkG x of Nothing -> x
                                        Just y  -> y

unliftGM :: (GPlate a, Monad m) => (GNode -> m GNode) -> a -> m a
unliftGM f x = do
    y <- f (mkG x)
    return $ case fromG y of Nothing -> x
                             Just z  -> z


universe :: GNode -> [GNode]
universe g@(GNode _ x) = g : concatMap universe (fst (gplate x))

universeBi :: (GPlate a, Typeable b) => a -> [b]
universeBi = fromGs . universe . mkG

descend :: (GNode -> GNode) -> GNode -> GNode
descend f (GNode _ x) = mkG $ generate $ map f current
    where (current, generate) = gplate x

bottomUp :: (GNode -> GNode) -> GNode -> GNode
bottomUp f = g
    where g = f . descend g

topDown :: (GNode -> GNode) -> GNode -> GNode
topDown f = g
    where g = descend g . f


descendM :: Monad m => (GNode -> m GNode) -> GNode -> m GNode
descendM f (GNode _ x) = do
    let (current, generate) = gplate x
    current' <- mapM f current
    return $ mkG $ generate current'

bottomUpM :: Monad m => (GNode -> m GNode) -> GNode -> m GNode
bottomUpM f = g
    where g x = f =<< descendM g x

topDownM :: Monad m => (GNode -> m GNode) -> GNode -> m GNode
topDownM f = g
    where g x = descendM g =<< f x


--------------------------------------------------------------------------------
-- MatchBind -------------------------------------------------------------------
--------------------------------------------------------------------------------


class MatchBind a where
    match ::
        ( MonadState ( [(String,GNode)]
                     , [(GNode,GNode)]
                     ) m
        , MonadError String m
        , GPlate a
        ) => a -> a -> m ()
    match p a = do
        modify $ second ((mkG p, mkG a) :)                                      -- add this node on top of the call stack.
        case hole p of                                                          -- check hole-status of the pattern.
            UnnamedHole  -> return ()                                           -- unnamed hole: matching succeeds, no bindings.
            NamedHole nm -> modify $ first ((nm,mkG a) :)                       -- named hole: matching succeeds, bind the name to rhs.
            NotAHole     -> do
                case (fst $ gplate p, fst $ gplate a) of
                    ([], []) | p == a    -> return ()                                           -- if this is a leaf, must check for equality.
                             | otherwise -> gmatchError "Leafs inequal."                        -- otherwise matching fails.
                    (ps, as) | nodeTag p /= nodeTag a   -> gmatchError "Constructor mismatch."  -- node tags must match.
                             | not (ps `sameLength` as) -> gmatchError "Shape mismatch."        -- with equal number of children.
                             | otherwise                -> zipWithM_ gmatch ps as               -- recursive call.
        modify $ second tail

    bind ::
        ( MonadState ( [(String,GNode)]
                     , [GNode]
                     ) m
        , MonadError String m
        , GPlate a
        ) => a -> m a
    bind t = do
        modify $ second (mkG t :)                                       -- add this node on top of the call stack.
        result <- case hole t of                                        -- check hole-status of the pattern.
            UnnamedHole  -> gbindError "Unnamed hole in template."      -- unnamed hole in a template is just nonsense.
            NamedHole nm -> do
                bindings <- gets fst
                case lookup nm bindings of
                    Nothing -> gbindError ("Not found: " ++ nm)                                                         -- if the name cannot be found in te list of bindings.
                    Just (GNode ty_b b) | typeOf t == ty_b -> return (unsafeCoerce b)                                   -- the name is bound to something of the expected type. great.
                                        | otherwise        -> gbindError $ "Type mismatch for: " ++ nm ++ "\n"          -- name is bound, but wrong type.
                                                                        ++ "\tExpected: " ++ show (typeOf t) ++ "\n"
                                                                        ++ "\tBut got:  " ++ show ty_b
            NotAHole -> case gplate t of
                            ([], _  ) -> return t                       -- if this is not a hole, and is a leaf, just return it.
                            (ts, gen) -> do
                                ts' <- mapM gbind ts                    -- otherwise, apply gbind recursively to immediate children.
                                return $ gen ts'
        modify $ second tail
        return result



gmatch ::
    ( MonadState ( [(String,GNode)]
                 , [(GNode,GNode)]
                 ) m
    , MonadError String m
    ) => GNode -> GNode -> m ()
gmatch (GNode ty_a a) (GNode ty_b b) | ty_a == ty_b = match a (unsafeCoerce b)
gmatch p a = do
    modify $ second ((p, a) :) 
    gmatchError "Type mismatch."
    modify $ second tail

gbind ::
    ( MonadState ( [(String,GNode)]
                 , [GNode]
                 ) m
    , MonadError String m
    ) => GNode -> m GNode
gbind (GNode _ t) = mkG `liftM` bind t



gmatchError ::
    ( MonadState ( [(String,GNode)]
                 , [(GNode,GNode)]
                 ) m
    , MonadError String m
    ) => String -> m ()
gmatchError msg = do
    stack <- gets snd
    let msgs = map (\ (a,b) -> showG a ++ " ~~ " ++ showG b ) stack
    let combinedMsg = unlines (msg : map ("\t"++) msgs)
    throwError combinedMsg

gbindError ::
    ( MonadState ( [(String,GNode)]
                 , [GNode]
                 ) m
    , MonadError String m
    ) => String -> m a
gbindError msg = do
    stack <- gets snd
    let msgs = map showG stack
    let combinedMsg = unlines (msg : map ("\t"++) msgs)
    throwError combinedMsg



--------------------------------------------------------------------------------
-- Helper functions ------------------------------------------------------------
--------------------------------------------------------------------------------

sameLength :: [a] -> [b] -> Bool
sameLength []     []     = True
sameLength (_:is) (_:js) = sameLength is js
sameLength _      _      = False
