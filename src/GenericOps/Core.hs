{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

module GenericOps.Core
    ( Hole(..), HoleStatus(..)
    , NodeTag(..), nodeTagData
    , GPlate(..), gplateLeaf, gplateSingle, gplateTwo, gplateUniList, gplateError
    , GNode(..), mkG, showG, fromG, fromGs, (===)
    , liftG, liftGM, unliftG, unliftGM
    , universe, universeBi
    , descend, descendM
    , bottomUp, bottomUpM
    , bottomUpRewrite, bottomUpRewriteM
    , topDown, topDownM
    , topDownRewrite, topDownRewriteM
    , MatchBind(..)
    , gapply, gapplyDeep
    ) where

import Control.Monad ( liftM, zipWithM_ )
import Control.Monad.Error ( ErrorT(..), throwError, Error(..) )
import Control.Monad.State ( StateT(..), evalStateT, get, modify )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Writer ( MonadWriter(..) )
import Data.Data ( Data, toConstr )
import Data.Generics ( Typeable, TypeRep, typeOf )
import Data.Maybe ( mapMaybe)
import Data.Typeable ( cast )
import Unsafe.Coerce ( unsafeCoerce )
import qualified Data.Map as M

import ParsecUtils ( choiceTry )
import ParsePrint
import PrintUtils ( Doc, (<+>), ($$), brackets, nest, text, vcat )
import Utils ( padLeft )


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



instance (Data a, Data b) => NodeTag (Either a b)
instance Hole (Either a b)
instance MatchBind (Either a b)
instance (ParsePrint a, ParsePrint b) => ParsePrint (Either a b) where
    parse = choiceTry [ liftM Left  parse
                      , liftM Right parse
                      ]
    pretty (Left  x) = pretty x
    pretty (Right x) = pretty x
instance (GPlate a, GPlate b, Data a, Data b) => GPlate (Either a b) where
    gplate (Left  x) = gplateSingle Left  x
    gplate (Right x) = gplateSingle Right x



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
                            _       -> gplateError "gplateSingle[1]"
                    _    -> gplateError "gplateSingle[2]"
      )

gplateTwo ::
    (GPlate a, GPlate b, GPlate c)
    => (a -> b -> c)
    -> a
    -> b
    -> ([GNode], [GNode] -> c)
gplateTwo f x y =
    ( [mkG x, mkG y]
    , \ zs -> let xs = fromGs $ take 1 zs
                  ys = fromGs $ take 1 $ drop 1 zs
              in case (xs, ys) of
                  ([x'],[y']) -> f x' y'
                  _ -> gplateError "gplateTwo"
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
                        else gplateError "gplateUniList"
    )

gplateError :: String -> a
gplateError msg = error $ "GPlate internal error: " ++ msg


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

bottomUpRewrite :: (GNode -> Maybe GNode) -> GNode -> GNode
bottomUpRewrite f = bottomUp g
    where g x = maybe x (bottomUpRewrite f) (f x)

topDown :: (GNode -> GNode) -> GNode -> GNode
topDown f = g
    where g = descend g . f

topDownRewrite :: (GNode -> Maybe GNode) -> GNode -> GNode
topDownRewrite f = topDown g
    where g x = maybe x (topDownRewrite f) (f x)


descendM :: Monad m => (GNode -> m GNode) -> GNode -> m GNode
descendM f (GNode _ x) = do
    let (current, generate) = gplate x
    current' <- mapM f current
    return $ mkG $ generate current'

bottomUpM :: Monad m => (GNode -> m GNode) -> GNode -> m GNode
bottomUpM f = g
    where g x = f =<< descendM g x

bottomUpRewriteM :: Monad m => (GNode -> m (Maybe GNode)) -> GNode -> m GNode
bottomUpRewriteM f = bottomUpM g
    where g x = f x >>= maybe (return x) (bottomUpRewriteM f)

topDownM :: Monad m => (GNode -> m GNode) -> GNode -> m GNode
topDownM f = g
    where g x = descendM g =<< f x

topDownRewriteM :: Monad m => (GNode -> m (Maybe GNode)) -> GNode -> m GNode
topDownRewriteM f = topDownM g
    where g x = f x >>= maybe (return x) (topDownRewriteM f)



--------------------------------------------------------------------------------
-- MatchBind -------------------------------------------------------------------
--------------------------------------------------------------------------------

instance Error Doc where
    strMsg = text

type BindingsMap = M.Map String GNode
type CommonMonad m a l = ErrorT Doc (StateT BindingsMap (StateT [l] m)) a
type MatchMonad m a = CommonMonad m a (GNode,GNode)
type BindMonad  m a = CommonMonad m a GNode

-- runMatchBind :: CommonMonad m a l -> Either String ()
-- runMatchBind comp = do
-- 	res <- runErrorT comp
-- 	return res

class MatchBind a where
    match :: (Monad m, GPlate a) => a -> a -> MatchMonad m ()
    match p a = do
        lift $ lift $ modify ((mkG p, mkG a) :)                                 -- add this node on top of the call stack.
        case hole p of                                                          -- check hole-status of the pattern.
            UnnamedHole  -> return ()                                           -- unnamed hole: matching succeeds, no bindings.
            NamedHole nm -> addBinding nm a                                     -- named hole: matching succeeds, bind the name to rhs.
            NotAHole     -> do
                case (fst $ gplate p, fst $ gplate a) of
                    ([], []) | p == a    -> return ()                                           -- if this is a leaf, must check for equality.
                             | otherwise -> gmatchError "Leafs inequal."                        -- otherwise matching fails.
                    (ps, as) | nodeTag p /= nodeTag a   -> gmatchError "Constructor mismatch."  -- node tags must match.
                             | not (ps `sameLength` as) -> gmatchError "Shape mismatch."        -- with equal number of children.
                             | otherwise                -> zipWithM_ gmatch ps as               -- recursive call.
        lift $ lift $ modify tail

    bind :: (Monad m, GPlate a) => a -> BindMonad m a
    bind t = do
        lift $ lift $ modify (mkG t :)                                          -- add this node on top of the call stack.
        result <- case hole t of                                                -- check hole-status of the pattern.
            UnnamedHole  -> return t -- gbindError "Unnamed hole in template."  -- unnamed hole in a template is just nonsense.
            NamedHole nm -> do
                bindings <- lift get
                case M.lookup nm bindings of
                    Nothing -> return t -- gbindError ("Not found: " <+> text nm)                                                    -- if the name cannot be found in te list of bindings.
                    Just (GNode ty_b b) | typeOf t == ty_b -> return (unsafeCoerce b)                                                -- the name is bound to something of the expected type. great.
                                        | otherwise        -> gbindError $ vcat [ "Type mismatch for: " <+> text nm                  -- name is bound, but wrong type.
                                                                                , nest 4 "Expected: "   <+> text (show (typeOf t))
                                                                                , nest 4 "But got:  "   <+> text (show ty_b)
                                                                                ]
            NotAHole -> case gplate t of
                            ([], _  ) -> return t                       -- if this is not a hole, and is a leaf, just return it.
                            (ts, gen) -> do
                                ts' <- mapM gbind ts                    -- otherwise, apply gbind recursively to immediate children.
                                return $ gen ts'
        lift $ lift $ modify tail
        return result


addBinding :: (Monad m, GPlate a) => String -> a -> MatchMonad m ()
addBinding nm x = do
    mp <- lift get
    case M.lookup nm mp of
        Nothing -> lift $ modify $ M.insert nm (mkG x)
        Just _  -> throwError ("Name is already bound: " <+> text nm)


gmatch :: Monad m => GNode -> GNode -> MatchMonad m ()
gmatch (GNode ty_a a) (GNode ty_b b) | ty_a == ty_b = match a (unsafeCoerce b)
gmatch p a = do
    lift $ lift $ modify $ ((p, a) :) 
    gmatchError "Type mismatch."
    lift $ lift $ modify $ tail

gbind :: Monad m => GNode -> BindMonad m GNode
gbind (GNode _ t) = mkG `liftM` bind t



gmatchError :: Monad m => Doc -> MatchMonad m ()
gmatchError msg = do
    stack <- lift $ lift get
    let msgs = map (\ (GNode _ a, GNode _ b) -> pretty a <+> "~~" <+> pretty b ) stack
    let combinedMsg = vcat (msg : map (nest 4) msgs)
    throwError combinedMsg

gbindError :: Monad m => Doc -> BindMonad m a
gbindError msg = do
    stack <- lift $ lift get
    let msgs = map (text . showG) stack
    let combinedMsg = vcat (msg : map (nest 4) msgs)
    throwError combinedMsg


data ApplResult = Applied    String GNode GNode
                | NotApplied String GNode Doc
                | Str Doc

instance Show ApplResult where
    show (Applied nm (GNode _ old) (GNode _ new))
        = show $ brackets ("APPLIED:" <+> text nm)
             <+> pretty old
             <+> text "~~>"
             <+> pretty new
    show (NotApplied nm (GNode _ node) msg)
        = show $ brackets ("NOT APPLIED:" <+> text nm)
             <+> pretty node
             $$  nest 4 msg
    show (Str s) = show s


gapply :: (MonadWriter [ApplResult] m, GPlate a) => String -> a -> a -> a -> m a
gapply name pattern template param = do
    (i, binds) <- flip evalStateT [] $ flip runStateT M.empty $ runErrorT $ match pattern param
    case i of
        Left msg -> tell [ NotApplied name (mkG param) msg ] >> return param
        Right () -> do
            tell [ Str $ text (padLeft ' ' 10 nm) <+> ":" <+> pretty n
                 | (nm, GNode _ n) <- M.toList binds
                 ]
            j <- flip evalStateT [] $ flip evalStateT binds $ runErrorT $ bind template
            case j of
                Left msg -> tell [ NotApplied name (mkG param) msg     ] >> return param
                Right k  -> tell [ Applied    name (mkG param) (mkG k) ] >> return k

gapplyDeep :: (MonadWriter [ApplResult] m, GPlate a) => String -> a -> a -> a -> m a
gapplyDeep name a b = unliftGM $ bottomUpM $ liftGM $ gapply name a b



--------------------------------------------------------------------------------
-- Helper functions ------------------------------------------------------------
--------------------------------------------------------------------------------

sameLength :: [a] -> [b] -> Bool
sameLength []     []     = True
sameLength (_:is) (_:js) = sameLength is js
sameLength _      _      = False
