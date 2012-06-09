{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Language.Core.Definition where

import Language.Core.Imports
import Language.EssenceLexer ( Lexeme(..) )

import Control.Monad.RWS ( RWST, runRWST )
import Data.Generics ( Data, Typeable )
import Data.String ( IsString(..) )
import GHC.Generics ( Generic )
import Text.PrettyPrint as Pr

import Data.Generics.Uniplate.Data ( transform )


data Spec = Spec Version [Core]
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

type Version = (String,[Int])

data Core
    = L Literal
    | R Reference
    | Expr
        Tag             -- node tag
        [Core]          -- arguments
    -- | Let
    --     Reference
    --     Core            -- type of binder
    --     Core            -- context in which it is defined
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

type RuleRefn = (Text, Maybe Int, Core)

view :: Core -> ([Tag],[Core])
view (Expr t [x]) = let (ts,c) = view x
                    in  (t:ts,c)
view (Expr t xs) = ([t],xs)
view x = ([],[x])

viewDeep :: [Tag] -> Core -> Maybe [Core]
viewDeep [] _ = Nothing
viewDeep [t] (Expr t' xs) | t == t' = Just xs
viewDeep (t:ts) (Expr t' xs) | t == t' = listToMaybe $ catMaybes [ viewDeep ts x | x <- xs ]
viewDeep _ _ = Nothing

data Literal = B Bool | I Integer
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

newtype Reference = Reference Text
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, IsString, Monoid)

newtype Tag = Tag Text
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, IsString)

data Category = Constant | Parameter | Decision | Quantified
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Binder = Binder { ref :: Reference, refVal :: Core }
    deriving (Show)

type    NestedDoc  = Nested Doc
newtype CompConfig = CompConfig ()
newtype CompLog    = CompLog [(Tag,Doc)]
    deriving ( Show, Monoid )
data    CompState  = CompState { binders :: [Binder]
                               , uniqueNameInt :: Integer
                               }
    deriving ( Show )
newtype CompT m a  = CompT (ErrorT NestedDoc (RWST CompConfig CompLog CompState m) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError  NestedDoc
             , MonadReader CompConfig
             , MonadWriter CompLog
             , MonadState  CompState
             , MonadPlus
             , MonadIO
             )

instance MonadTrans CompT where
    lift = CompT . lift . lift

type Comp a = CompT Identity a

instance Default CompConfig where
    def = CompConfig ()

instance Default CompState where
    def = CompState { binders = [], uniqueNameInt = 1 }

mkLog :: Monad m => Tag -> Doc -> CompT m ()
-- mkLog _ _ = return ()
mkLog t d
    | t `elem` suppress = return ()
    | otherwise         = tell (CompLog [(t,d)])
    where suppress = drop 1 [ ""
                            , "toLit"
                            , "match"
                            , "bind"
                            , "rule-fail"
                            , "simplify"
                            , "simplify-step"
                            , "simplify-generic-case"
                            , "domainUnify"
                            , "typeUnify"
                            , "typeOf"
                            , "ApplyTransformation.tryAgain"
                            , "ApplyTransformation.worker"
                            ]

nextUniqueName :: Monad m => CompT m Text
nextUniqueName = do
    !i <- gets uniqueNameInt
    modify $ \ st -> st { uniqueNameInt = i + 1 }
    return $ "__" `mappend` (stringToText $ show i)

addBinder :: Monad m => Reference -> Core -> CompT m ()
addBinder r c = modify $ \ st -> 
    let !newBinders = Binder r c : binders st
    in  st { binders = newBinders }

removeLastBinder :: Monad m => CompT m ()
removeLastBinder = modify $ \ st ->
    let !newBinders = tailNote "removeLastBinder" $ binders st
    in  st { binders = newBinders }

localState :: Monad m => CompT m a -> CompT m a
localState comp = do
    before <- gets binders
    result <- comp
    modify $ \ st -> st { binders = before }
    return result

withLog :: Monad m => Tag -> Doc -> CompT m a -> CompT m a
withLog t d a = mkLog t d >> a

prettyLog :: CompLog -> [Doc]
prettyLog (CompLog xs) = [ Pr.brackets (stringToDoc $ padRight 7 ' ' $ textToString t) <+> d | (Tag t,d) <- xs ]

runCompT :: CompConfig -> CompState -> CompT m a -> m (Either (Nested Doc) a, CompState, CompLog)
runCompT r s (CompT a) = runRWST (runErrorT a) r s

runCompIO :: CompConfig -> CompState -> CompT IO a -> IO a
runCompIO r s a = do
    (result, _finalSt, logs) <- runCompT r s a
    mapM_ (print . nest 4) (prettyLog logs)
    -- ppPrint finalSt
    case result of
        Left  e -> error $ show $ nestedToDoc e
        Right b -> return b

runComp :: CompConfig -> CompState -> Comp a -> (Either (Nested Doc) a, CompState, CompLog)
runComp r s a = runIdentity $ runCompT r s a

lookUpRef :: Monad m => Reference -> CompT m Core
lookUpRef r@(Reference t) = do
    bs <- gets binders
    case listToMaybe [ c | Binder r' c <- bs, r == r' ] of
        Nothing  -> do
            throwError $ Nested Nothing
                $ singletonNested ("Identifier not bound:" <+> textToDoc t)
                : map (\ (Binder (Reference s) _) -> singletonNested $ textToDoc s ) bs
            -- err $ "Identifier not bound:" <+> textToDoc t
        Just val -> return val

lookUpInExpr :: Text -> [Core] -> Maybe [Core]
lookUpInExpr t cs = listToMaybe [ xs | Expr (Tag t') xs <- cs, t == t' ]


data Fixity = FNone | FLeft | FRight
    deriving Show

operators :: [(Lexeme,Fixity,Int)]
operators =
    [ ( L_Plus      , FLeft  ,  600 )
    , ( L_Minus     , FLeft  ,  600 )
    , ( L_Times     , FLeft  ,  700 )
    , ( L_Div       , FLeft  ,  700 )
    , ( L_Mod       , FLeft  ,  700 )
    , ( L_Pow       , FRight ,  800 )
    , ( L_Lt        , FNone  ,  400 )
    , ( L_Leq       , FNone  ,  400 )
    , ( L_Gt        , FNone  ,  400 )
    , ( L_Geq       , FNone  ,  400 )
    , ( L_Neq       , FNone  ,  400 )
    , ( L_Eq        , FNone  ,  400 )
    , ( L_Or        , FLeft  ,  110 )
    , ( L_And       , FLeft  ,  120 )
    , ( L_Imply     , FNone  ,   50 )
    , ( L_Iff       , FNone  ,   50 )
    , ( L_union     , FLeft  ,  600 )
    , ( L_intersect , FLeft  ,  700 )
    , ( L_subset    , FNone  ,  400 )
    , ( L_subsetEq  , FNone  ,  400 )
    , ( L_supset    , FNone  ,  400 )
    , ( L_supsetEq  , FNone  ,  400 )
    , ( L_in        , FNone  ,  550 )
    -- , ( L_Colon     , FNone  ,   10 )
    , ( L_HasType   , FNone  ,   10 )
    , ( L_HasDomain , FNone  ,   10 )
    ]

functionals :: [Lexeme]
functionals =
    [ L_toInt
    , L_min
    , L_max
    , L_allDiff
    , L_hist

    , L_toSet
    , L_toMSet
    , L_toRelation
    , L_defined
    , L_range
    , L_image
    , L_preImage
    , L_inverse
    , L_together
    , L_apart
    , L_party
    , L_participants
    , L_parts
    , L_freq
    , L_hist
    , L_allDiff
    , L_toInt
    , L_flatten
    , L_normIndices
    ]





processStatement :: Monad m => Core -> CompT m ()
processStatement s@( viewDeep [":toplevel",":declaration",":find"]
                      -> Just [ Expr ":find-name"   [R name]
                              , Expr ":find-domain" [_]
                              ]
                   ) = addBinder name s
processStatement s@( viewDeep [":toplevel",":declaration",":given"]
                      -> Just [ Expr ":given-name"   [R name]
                              , Expr ":given-domain" [_]
                              ]
                   ) = addBinder name s
processStatement   ( viewDeep [":toplevel",":letting"]
                      -> Just [ Expr ":letting-name" [R name]
                              , Expr ":letting-expr" [expression]
                              ]
                   ) = addBinder name expression
processStatement   ( viewDeep [":toplevel",":suchthat"]
                      -> Just _
                   ) = return ()
processStatement   ( viewDeep [":toplevel",":objective"]
                      -> Just _
                   ) = return ()
processStatement s@( viewDeep [":toplevel"]
                      -> Just _
                   ) = mkLog "not handled in processStatement" (stringToDoc $ show s)
processStatement _s = do
    -- mkLog "default case" (stringToDoc $ show c)
    return ()



replaceCore :: Core -> Core -> Core -> Core
replaceCore old new = transform f
    where f i | i == old  = new
              | otherwise = i



---

valueBool :: Bool -> Core
valueBool x = Expr ":value"
            [ Expr ":value-literal"
            [ L $ B x
            ]]

valueTrue :: Core
valueTrue = valueBool True

valueFalse :: Core
valueFalse = valueBool False

valueInt :: Integer -> Core
valueInt x = Expr ":value"
            [ Expr ":value-literal"
            [ L $ I x
            ]]

