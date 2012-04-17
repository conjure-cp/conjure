{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GenericOps.Core
    ( Hole(..), HoleStatus(..)
    , NodeTag(..), nodeTagData
    , GPlate(..), gplateLeaf, gplateSingle, gplateTwo, gplateUniList, gplateError
    , GNode(..), mkG, showG, fromG, fromGs, (===)
    -- , liftG, liftGM, liftGM_Maybe
    -- , unliftG, unliftGM, unliftGM_Maybe
    , universe
    , descend, descendM
    , bottomUp , bottomUpRewrite
    , bottomUpM, bottomUpRewriteM
    , topDown , topDownRewrite
    , topDownM, topDownRewriteM
    , MatchBind(..), runMatch, runBind, gbind, inScope
    , BindingsMap, addBinding, getBinding
    , gapply, gapplyDeep
    ) where

import Control.Applicative
import Control.Monad ( forM, liftM, zipWithM_ )
import Control.Monad.Error ( MonadError, ErrorT(..), throwError )
import Control.Monad.State ( MonadState, StateT(..), evalStateT )
import Control.Monad.Writer ( MonadWriter(..) )
import Data.Data ( Data, toConstr )
import Data.Generics ( Typeable, TypeRep, typeOf )
import Data.Maybe ( mapMaybe)
import Data.Typeable ( cast )
import Unsafe.Coerce ( unsafeCoerce )
import qualified Data.Map as M

import Nested
import Constants
import Has
import ParsePrint
import PrintUtils ( Doc, (<+>), ($$), brackets, nest, text )
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
    parse = liftM Left  parse <|> liftM Right parse
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

{-# INLINE mkG #-}
mkG :: GPlate a => a -> GNode
mkG x = GNode (typeOf x) x

{-# INLINE fromG #-}
fromG :: Typeable a => GNode -> Maybe a
fromG (GNode t v) =
    case unsafeCoerce v of
        r | t == typeOf r -> Just r
          | otherwise     -> Nothing

{-# INLINE fromGs #-}
fromGs :: Typeable a => [GNode] -> [a]
fromGs gs = mapMaybe fromG gs

{-# INLINE (===) #-}
(===) :: GNode -> GNode -> Bool
(GNode _ x) === (GNode _ y) = cast x == Just y


{-# INLINE liftG #-}
liftG :: GPlate a => (a -> a) -> GNode -> GNode
liftG f x = case fromG x of Nothing -> x
                            Just y  -> mkG (f y)

{-# INLINE liftG_Maybe #-}
liftG_Maybe :: GPlate a => (a -> Maybe a) -> GNode -> Maybe GNode
liftG_Maybe f x = case fromG x of Nothing -> Nothing
                                  Just y  -> liftM mkG (f y)

{-# INLINE liftGM #-}
liftGM :: (GPlate a, Monad m) => (a -> m a) -> GNode -> m GNode
liftGM f x = case fromG x of Nothing -> return x
                             Just y  -> liftM mkG (f y)

{-# INLINE liftGM_Maybe #-}
liftGM_Maybe :: (GPlate a, Monad m) => (a -> m (Maybe a)) -> GNode -> m (Maybe GNode)
liftGM_Maybe f x = case fromG x of Nothing -> return Nothing
                                   Just y  -> liftM (liftM mkG) (f y)


{-# INLINE unliftG #-}
unliftG :: GPlate a => (GNode -> GNode) -> a -> a
unliftG f x = case fromG $ f $ mkG x of Nothing -> x
                                        Just y  -> y

-- {-# INLINE unliftG_Maybe #-}
-- unliftG_Maybe :: GPlate a => (GNode -> Maybe GNode) -> a -> Maybe a
-- unliftG_Maybe f x = case f (mkG x) of Nothing -> Nothing
--                                       Just y  -> fromG y

{-# INLINE unliftGM #-}
unliftGM :: (GPlate a, Monad m) => (GNode -> m GNode) -> a -> m a
unliftGM f x = do
    y <- f (mkG x)
    return $ case fromG y of Nothing -> x
                             Just z  -> z

-- {-# INLINE unliftGM_Maybe #-}
-- unliftGM_Maybe :: (GPlate a, Monad m) => (GNode -> m (Maybe GNode)) -> a -> m (Maybe a)
-- unliftGM_Maybe f x = do
--     y <- f (mkG x)
--     return $ case y of Nothing -> Nothing
--                        Just z  -> fromG z



{-# INLINE gUniverse #-}
gUniverse :: GNode -> [GNode]
gUniverse g@(GNode _ x) = g : concatMap gUniverse (fst (gplate x))

{-# INLINE universe #-}
universe :: (GPlate a, Typeable b) => a -> [b]
universe = fromGs . gUniverse . mkG



{-# INLINE gDescend #-}
gDescend :: (GNode -> GNode) -> GNode -> GNode
gDescend f (GNode _ x) = mkG $ generate $ map f current
    where (current, generate) = gplate x

{-# INLINE descend #-}
descend :: (GPlate a, GPlate b) => (a -> a) -> b -> b
descend = unliftG . gDescend . liftG

{-# INLINE gDescendM #-}
gDescendM :: Monad m => (GNode -> m GNode) -> GNode -> m GNode
-- gDescendM f (GNode _ x) = trace ("gDescendM: " ++ show (pretty x)) $ do
gDescendM f (GNode _ x) = do
    let (current, generate) = gplate x
    current' <- mapM f current
    return $ mkG $ generate current'

{-# INLINE descendM #-}
descendM :: (GPlate a, GPlate b, Monad m) => (a -> m a) -> b -> m b
descendM = unliftGM . gDescendM . liftGM




{-# INLINE gBottomUp #-}
gBottomUp :: (GNode -> GNode) -> GNode -> GNode
gBottomUp f = g
    where g = f . gDescend g

{-# INLINE bottomUp #-}
bottomUp :: (GPlate a, GPlate b) => (a -> a) -> b -> b
bottomUp = unliftG . gBottomUp . liftG

{-# INLINE gBottomUpRewrite #-}
gBottomUpRewrite :: (GNode -> Maybe GNode) -> GNode -> GNode
gBottomUpRewrite f = gBottomUp g
    where g x = maybe x (gBottomUpRewrite f) (f x)

{-# INLINE bottomUpRewrite #-}
bottomUpRewrite :: (GPlate a, GPlate b) => (a -> Maybe a) -> b -> b
bottomUpRewrite f = unliftG (gBottomUpRewrite (liftG_Maybe f))

{-# INLINE gBottomUpM #-}
gBottomUpM :: Monad m => (GNode -> m GNode) -> GNode -> m GNode
gBottomUpM f = g
    where g x = f =<< gDescendM g x

{-# INLINE bottomUpM #-}
bottomUpM :: (GPlate a, GPlate b, Monad m) => (a -> m a) -> b -> m b
bottomUpM = unliftGM . gBottomUpM . liftGM

{-# INLINE gBottomUpRewriteM #-}
gBottomUpRewriteM :: Monad m => (GNode -> m (Maybe GNode)) -> GNode -> m GNode
gBottomUpRewriteM f = gBottomUpM g
    where g x = f x >>= maybe (return x) (gBottomUpRewriteM f)

{-# INLINE bottomUpRewriteM #-}
bottomUpRewriteM :: (GPlate a, GPlate b, Monad m) => (a -> m (Maybe a)) -> b -> m b
bottomUpRewriteM f = unliftGM (gBottomUpRewriteM (liftGM_Maybe f))


{-# INLINE gTopDown #-}
gTopDown :: (GNode -> GNode) -> GNode -> GNode
gTopDown f = g
    where g = gDescend g . f

{-# INLINE topDown #-}
topDown :: (GPlate a, GPlate b) => (a -> a) -> b -> b
topDown = unliftG . gTopDown . liftG

{-# INLINE gTopDownRewrite #-}
gTopDownRewrite :: (GNode -> Maybe GNode) -> GNode -> GNode
gTopDownRewrite f = gTopDown g
    where g x = maybe x (gTopDownRewrite f) (f x)

{-# INLINE topDownRewrite #-}
topDownRewrite :: (GPlate a, GPlate b) => (a -> Maybe a) -> b -> b
topDownRewrite f = unliftG (gTopDownRewrite (liftG_Maybe f))

{-# INLINE gTopDownM #-}
gTopDownM :: Monad m => (GNode -> m GNode) -> GNode -> m GNode
gTopDownM f = g
    where g x = gDescendM g =<< f x

{-# INLINE topDownM #-}
topDownM :: (GPlate a, GPlate b, Monad m) => (a -> m a) -> b -> m b
topDownM = unliftGM . gTopDownM . liftGM

{-# INLINE gTopDownRewriteM #-}
gTopDownRewriteM :: Monad m => (GNode -> m (Maybe GNode)) -> GNode -> m GNode
gTopDownRewriteM f = gTopDownM g
    where g x = f x >>= maybe (return x) (gTopDownRewriteM f)

{-# INLINE topDownRewriteM #-}
topDownRewriteM :: (GPlate a, GPlate b, Monad m) => (a -> m (Maybe a)) -> b -> m b
topDownRewriteM f = unliftGM (gTopDownRewriteM (liftGM_Maybe f))



--------------------------------------------------------------------------------
-- MatchBind -------------------------------------------------------------------
--------------------------------------------------------------------------------

type BindingsMap = M.Map String GNode
-- type MatchMonad m a = ErrorT Doc (StateT (BindingsMap, [(GNode,GNode)]) m) a
-- type BindMonad  m a = ErrorT Doc (StateT (BindingsMap, ([GNode],[String])) m) a

-- runMatchBind :: CommonMonad m a l -> Either String ()
-- runMatchBind comp = do
-- 	res <- runErrorT comp
-- 	return res



runMatch ::
    ( GPlate a
    , Monad m
    , MonadError (Nested Doc) m
    , MonadState st m
    , Has st BindingsMap
    ) => a -> a -> m ()
runMatch patt curr = do
    oldBindings <- getM
    let initSt = ( oldBindings :: BindingsMap
                 , []          :: [(GNode,GNode)]
                 )
    (i, (newBindings,_)) <- flip runStateT initSt $ runErrorT $ match patt curr
    case i of
        Left msg -> throwError msg
        Right () -> putM newBindings

runBind ::
    ( GPlate a
    , Monad m
    , MonadError (Nested Doc) m
    , MonadState st m
    , Has st BindingsMap
    ) => a -> m a
runBind template = do
    oldBindings <- getM
    let initSt = ( oldBindings :: BindingsMap
                 , []          :: [GNode]
                 , []          :: [String]
                 )
    i <- flip evalStateT initSt $ runErrorT $ bind template
    case i of
        Left msg       -> throwError msg
        Right Nothing  -> return template
        Right (Just j) -> runBind j


inScope :: forall a b st m . (MonadState st m, Has st [a]) => a -> m b -> m b
inScope a comp = do
    modifyM ((a:) :: ([a] -> [a]))
    res <- comp
    modifyM (tail :: ([a] -> [a]))
    return res


class MatchBind a where
    match ::
        ( GPlate a
        , MonadError (Nested Doc) m
        , MonadState st m
        , Has st BindingsMap
        , Has st [(GNode,GNode)]
        ) => a -> a -> m ()
    match p a = inScope (mkG p, mkG a) $ do
        traceM PatternMatching $ "match " ++ show (pretty p) ++ " ~~ " ++ show (pretty a)
        -- add this node on top of the call stack.
        case hole p of                                                          -- check hole-status of the pattern.
            UnnamedHole  -> return ()                                           -- unnamed hole: matching succeeds, no bindings.
            NamedHole nm -> addBinding nm a                                     -- named hole: matching succeeds, bind the name to rhs.
            NotAHole     -> do
                case (fst $ gplate p, fst $ gplate a) of
                    ([], []) | p == a    -> return ()                                                                                           -- if this is a leaf, must check for equality.
                             | otherwise -> gmatchError $ text $ "Leafs inequal: " ++ nodeTag p ++ " " ++ nodeTag a                             -- otherwise matching fails.
                    (ps, as) | nodeTag p /= nodeTag a   -> gmatchError $ text $ "Constructor mismatch in: " ++ nodeTag p ++ " " ++ nodeTag a    -- node tags must match.
                             | not (ps `sameLength` as) -> gmatchError $ text $ "Shape mismatch in: "       ++ nodeTag p ++ " " ++ nodeTag a    -- with equal number of children.
                             | otherwise                -> zipWithM_ gmatch ps as               -- recursive call.

    -- returning a (Just value) means an update took place.
    -- returning a (Nothing)    means the input hasn't been changed.
    bind ::
        ( GPlate a
        , MonadError (Nested Doc) m
        , MonadState st m
        , Has st BindingsMap
        , Has st [GNode]
        , Has st [String]           -- to check for cyclic definition
        ) => a -> m (Maybe a)
    bind t = inScope (mkG t) $ do
        case hole t of                                                                -- check hole-status of the pattern.
            UnnamedHole  -> return Nothing -- gbindError "Unnamed hole in template."            -- unnamed hole in a template is just nonsense.
            NamedHole nm -> do
                oldNames <- getM
                if nm `elem` oldNames
                    then error $ "cyclic definition of something: " ++ nm ++ " " ++ show oldNames
                    else inScope nm $ do
                        bindings <- getM
                        res <- case M.lookup nm bindings of
                            Nothing -> return Nothing -- gbindError ("Not found: " <+> text nm)                                                     -- if the name cannot be found in te list of bindings.
                            Just (GNode ty_b b) | typeOf t == ty_b -> return (Just (unsafeCoerce b))                                                -- the name is bound to something of the expected type. great.
                                                | otherwise        -> return Nothing
                                                -- | otherwise        -> gbindError $ vcat [ "Type mismatch for: " <+> text nm                         -- name is bound, but wrong type.
                                                --                                         , nest 4 "Expected: "   <+> text (show (typeOf t))
                                                --                                         , nest 4 "But got:  "   <+> text (show ty_b)
                                                --                                         ]
                        return res
            NotAHole -> case gplate t of
                            ([], _  ) -> return Nothing                       -- if this is not a hole, and is a leaf, just return it.
                            (ts, gen) -> do
                                -- ts'  <- mapM gbind ts                    -- otherwise, apply gbind recursively to immediate children.
                                (bools,ts') <- liftM unzip $ forM ts $ \ i -> do
                                            j <- gbind i
                                            case j of Nothing -> return (False, i)
                                                      Just k  -> return (True , k)
                                return $ if or bools
                                             then Just (gen ts')         -- degisen var.
                                             else Nothing

    -- bind :: (Monad m, GPlate a) => a -> BindMonad m a
    -- bind t = do
    --     lift $ lift $ modify (mkG t :)                                          -- add this node on top of the call stack.
    --     result <- case hole t of                                                -- check hole-status of the pattern.
    --         UnnamedHole  -> return t -- gbindError "Unnamed hole in template."  -- unnamed hole in a template is just nonsense.
    --         NamedHole nm -> do
    --             bindings <- lift get
    --             case M.lookup nm bindings of
    --                 Nothing -> return t -- gbindError ("Not found: " <+> text nm)                                                    -- if the name cannot be found in te list of bindings.
    --                 Just (GNode ty_b b) | typeOf t == ty_b -> return (unsafeCoerce b)                                                -- the name is bound to something of the expected type. great.
    --                                     | otherwise        -> gbindError $ vcat [ "Type mismatch for: " <+> text nm                  -- name is bound, but wrong type.
    --                                                                             , nest 4 "Expected: "   <+> text (show (typeOf t))
    --                                                                             , nest 4 "But got:  "   <+> text (show ty_b)
    --                                                                             ]
    --         NotAHole -> case gplate t of
    --                         ([], _  ) -> return t                       -- if this is not a hole, and is a leaf, just return it.
    --                         (ts, gen) -> do
    --                             ts' <- mapM gbind ts                    -- otherwise, apply gbind recursively to immediate children.
    --                             return $ gen ts'
    --     lift $ lift $ modify tail
    --     return result


addBinding ::
    ( GPlate a
    , MonadError (Nested Doc) m
    , MonadState st m
    , Has st BindingsMap
    , Has st [(GNode,GNode)]
    ) => String -> a -> m ()
addBinding nm x = do
    mp :: BindingsMap <- getM
    case M.lookup nm mp of
        Nothing -> modifyM $ M.insert nm (mkG x)
        Just _  -> throwErrorSingle ("Name is already bound: " <+> text nm)


getBinding ::
    ( GPlate a
    , MonadState st m
    , Has st BindingsMap
    ) => String -> m (Maybe a)
getBinding nm = do
    bindings <- getM
    return $ case M.lookup nm bindings of
        Nothing -> Nothing
        Just gr -> fromG gr

gmatch ::
    ( MonadError (Nested Doc) m
    , MonadState st m
    , Has st BindingsMap
    , Has st [(GNode,GNode)]
    ) => GNode -> GNode -> m ()
gmatch (GNode ty_a a) (GNode ty_b b) | ty_a == ty_b = match a (unsafeCoerce b)
gmatch p a = inScope (p,a) $ do
    gmatchError "Type mismatch."

gbind ::
    ( MonadError (Nested Doc) m
    , MonadState st m
    , Has st BindingsMap
    , Has st [GNode]
    , Has st [String]
    ) => GNode -> m (Maybe GNode)
gbind (GNode _ t) = liftM (liftM mkG) (bind t)
    -- mresult <- bind t
    -- case mresult of
    --     Nothing -> return Nothing
    --     Just res -> Just mkG



gmatchError ::
    ( MonadError (Nested Doc) m
    , MonadState st m
    , Has st [(GNode,GNode)]
    ) => Doc -> m ()
gmatchError msg = do
    stack <- getM
    let msgs = map (\ (GNode _ a, GNode _ b) -> pretty a <+> "~~" <+> pretty b ) stack
    throwError $ Nested (Just msg) $ map (flip Nested [] . Just) msgs

-- gbindError :: Monad m => Doc -> BindMonad m a
-- gbindError msg = do
--     stack <- lift $ lift $ gets fst
--     let msgs = map (text . showG) stack
--     let combinedMsg = vcat (msg : map (nest 4) msgs)
--     throwError combinedMsg


data ApplResult = Applied    String GNode GNode
                | NotApplied String GNode (Nested Doc)
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
             $$  nest 4 (nestedToDoc msg)
    show (Str s) = show s


gapply :: (MonadWriter [ApplResult] m, GPlate a) => String -> a -> a -> a -> m a
gapply name pattern template param = do
    (i, (binds,_)) <- flip runStateT ( M.empty :: BindingsMap
                                     , []      :: [(GNode,GNode)]
                                     ) $ runErrorT $ match pattern param
    case i of
        Left msg -> tell [ NotApplied name (mkG param) msg ] >> return param
        Right () -> do
            tell [ Str $ text (padLeft ' ' 10 nm) <+> ":" <+> pretty n
                 | (nm, GNode _ n) <- M.toList binds
                 ]
            j <- flip evalStateT ( binds :: BindingsMap
                                 , []    :: [GNode]
                                 , []    :: [String]
                                 ) $ runErrorT $ bind template
            case j of
                Left  msg      -> tell [ NotApplied name (mkG param) msg            ] >> return param
                Right Nothing  -> tell [ Applied    name (mkG param) (mkG template) ] >> return template
                Right (Just k) -> tell [ Applied    name (mkG param) (mkG k)        ] >> return k

gapplyDeep :: (MonadWriter [ApplResult] m, GPlate a) => String -> a -> a -> a -> m a
gapplyDeep name a b = unliftGM $ gBottomUpM $ liftGM $ gapply name a b



--------------------------------------------------------------------------------
-- Helper functions ------------------------------------------------------------
--------------------------------------------------------------------------------

sameLength :: [a] -> [b] -> Bool
sameLength []     []     = True
sameLength (_:is) (_:js) = sameLength is js
sameLength _      _      = False
