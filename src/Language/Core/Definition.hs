{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Definition where

import Language.Core.Imports
-- import Control.Applicative
import Control.Monad.Error ( ErrorT, runErrorT )
import Control.Monad.Identity ( Identity, runIdentity )
import Control.Monad.RWS ( RWST, runRWST )
-- 
-- import Data.Default ( Default(def) )
-- 
import Data.String ( IsString(..) )

import Data.Generics ( Data, Typeable )
import GHC.Generics ( Generic )

import Language.EssenceLexer ( Lexeme(..) )


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

view :: Core -> ([Tag],[Core])
view (Expr t [x]) = let (ts,c) = view x
                    in  (t:ts,c)
view (Expr t xs) = ([t],xs)
view x = ([],[x])

data Literal = B Bool | I Integer
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

newtype Reference = Reference Text
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, IsString)

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
data    CompState  = CompState { binders :: [Binder] }
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
             )

instance MonadTrans CompT where
    lift = CompT . lift . lift

type Comp a = CompT Identity a

instance Default CompConfig where
    def = CompConfig ()

instance Default CompState where
    def = CompState { binders = [] }

mkLog :: Monad m => Tag -> Doc -> CompT m ()
mkLog t d = tell (CompLog [(t,d)])

withLog :: Monad m => Tag -> Doc -> CompT m a -> CompT m a
withLog t d a = mkLog t d >> a

prettyLog :: CompLog -> [Doc]
prettyLog (CompLog xs) = [ textToDoc t <+> d | (Tag t,d) <- xs ]

runCompT :: CompConfig -> CompState -> CompT m a -> m (Either (Nested Doc) a, CompState, CompLog)
runCompT r s (CompT a) = runRWST (runErrorT a) r s

runCompIO :: CompConfig -> CompState -> CompT IO a -> IO a
runCompIO r s a = do
    (result, finalSt, logs) <- runCompT r s a
    mapM_ (print . nest 4) (prettyLog logs)
    ppPrint finalSt
    case result of
        Left  e -> error $ show $ nestedToDoc e
        Right b -> return b

runComp :: CompConfig -> CompState -> Comp a -> (Either (Nested Doc) a, CompState, CompLog)
runComp r s a = runIdentity $ runCompT r s a

lookUpRef :: Monad m => Reference -> CompT m Core
lookUpRef r = do
    bs <- gets binders
    case listToMaybe [ c | Binder r' c <- bs, r == r' ] of
        Nothing  -> throwErrorSingle "Identifier not bound."
        Just val -> return val

lookUpInExpr :: Text -> [Core] -> Maybe [Core]
lookUpInExpr t cs = listToMaybe [ xs | Expr (Tag t') xs <- cs, t == t' ]

boolValue :: Bool -> Core
boolValue = L . B

intValue :: Integer -> Core
intValue = L . I

absolute :: Core -> Core
absolute a = Expr "Abs" [a]

plus :: Core -> Core -> Core
plus a b = Expr "Plus" [a,b]

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
    , ( L_Or        , FRight ,  110 )
    , ( L_And       , FRight ,  120 )
    , ( L_Imply     , FNone  ,   50 )
    , ( L_Iff       , FNone  ,   50 )
    , ( L_union     , FLeft  ,  600 )
    , ( L_intersect , FLeft  ,  700 )
    , ( L_subset    , FNone  ,  400 )
    , ( L_subsetEq  , FNone  ,  400 )
    , ( L_supset    , FNone  ,  400 )
    , ( L_supsetEq  , FNone  ,  400 )
    , ( L_in        , FNone  ,  550 )
    , ( L_HasType   , FNone  ,   10 )
    , ( L_HasDomain , FNone  ,   10 )
    ]
