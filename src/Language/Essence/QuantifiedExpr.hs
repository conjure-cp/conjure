{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.QuantifiedExpr where

import Control.Applicative
import Control.Monad ( unless )
import Control.Monad.Error ( throwError )
import qualified Control.Monad.State as S
import Data.Foldable ( forM_ )
import Data.Generics ( Data )
import Data.Maybe ( isNothing, mapMaybe, maybeToList )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import qualified  Data.Map as M

import Constants ( traceM )
import Has
import GenericOps.Core ( NodeTag, Hole
                       , GPlate, gplate, gplateError, gplateUniList
                       , mkG, fromGs, showG
                       , MatchBind, match, bind
                       , addBinding, getBinding, BindingsMap
                       , inScope )
import ParsecUtils
import ParsePrint
import PrintUtils ( (<>), (<+>), text )
import qualified PrintUtils as Pr
import Utils ( ppShow )

import Language.Essence.Domain
import Language.Essence.Expr
import Language.Essence.Identifier
import Language.Essence.Op
import Language.Essence.QuantifierDecl
import Language.Essence.StructuredVar
import Language.Essence.Type
import Language.Essence.Value



data QuantifiedExpr
    = QuantifiedExpr
    { quanName     :: Identifier
    , quanVar      :: Either Identifier StructuredVar
    , quanOverDom  :: Maybe Domain
    , quanOverExpr :: Maybe (Op, Expr)
    , quanGuard    :: QuanGuard
    , quanBody     :: Expr
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag QuantifiedExpr

instance Hole QuantifiedExpr

instance GPlate QuantifiedExpr where
    gplate p@(QuantifiedExpr qnName qnVar qnOverDom qnOverOpExpr qnGuard qnBody) =
        (  mkG (EHole qnName)
        :  ( mkG $ case qnVar of Left x -> Left (EHole x); Right x -> Right x )
        :  mkG qnGuard
        :  mkG qnBody
        :  map mkG (maybeToList qnOverDom)
        ++ concat [ [mkG op, mkG x] | (op,x) <- maybeToList qnOverOpExpr ]

        , \ xs -> let idenOut   x = case x of EHole i -> Just i; _ -> Nothing
                      qnVarOut  x = case x of Left (EHole i) -> Just (Left i); Right i -> Just (Right i); _ -> Nothing
                      qnName'       = mapMaybe idenOut  $ fromGs $ take 1 $ drop 0 xs
                      qnVar'        = mapMaybe qnVarOut $ fromGs $ take 1 $ drop 1 xs
                      qnGuard'      =                     fromGs $ take 1 $ drop 2 xs
                      qnBody'       =                     fromGs $ take 1 $ drop 3 xs
                      qnOverDom'    = if isNothing qnOverDom
                                            then []
                                            else fromGs $ take 1 $ drop 4 xs
                      qnOverOp'   = let y = if isNothing qnOverDom then 4 else 5
                                    in  if isNothing qnOverOpExpr
                                            then []
                                            else fromGs $ take 1 $ drop y xs
                      qnOverExpr' = let y = if isNothing qnOverDom then 5 else 6
                                    in  if isNothing qnOverOpExpr
                                            then []
                                            else fromGs $ take 1 $ drop y xs
                  in  if length qnName'       == 1 &&
                         length qnVar'        == 1 &&
                         length qnGuard'      == 1 &&
                         length qnBody'       == 1 &&
                         length qnOverDom'    == (if isNothing qnOverDom    then 0 else 1) &&
                         length qnOverOp'     == (if isNothing qnOverOpExpr then 0 else 1) &&
                         length qnOverExpr'   == length qnOverOp'
                         then QuantifiedExpr
                                (head qnName')
                                (head qnVar')
                                (if isNothing qnOverDom
                                    then Nothing
                                    else Just (head qnOverDom')
                                )
                                (if isNothing qnOverOpExpr
                                    then Nothing
                                    else Just ( head qnOverOp'
                                              , head qnOverExpr' )
                                )
                                (head qnGuard')
                                (head qnBody')
                        else gplateError $ "QuantifiedExpr: " ++ show (pretty p) ++ "\n"
                                        ++ ppShow ( qnName'
                                                  , qnVar'
                                                  , take 1 $ drop 1 xs
                                                  , map showG $ take 1 $ drop 1 xs
                                                  , fromGs $ take 1 $ drop 1 xs :: [Either Expr StructuredVar]
                                                  , mapMaybe qnVarOut $ (fromGs $ take 1 $ drop 1 xs :: [Either Expr StructuredVar])
                                                  , qnGuard'
                                                  , qnBody'
                                                  , qnOverDom'
                                                  , qnOverOp'
                                                  , qnOverExpr'
                                                  ) ++ "\n"
                                        ++ unlines (map showG xs)
                                        ++ unlines (map show [ length qnName'       == 1
                                                             , length qnVar'        == 1
                                                             , length qnGuard'      == 1
                                                             , length qnBody'       == 1
                                                             , length qnOverDom'    == (if isNothing qnOverDom    then 0 else 1)
                                                             , length qnOverOp'     == (if isNothing qnOverOpExpr then 0 else 1)
                                                             , length qnOverExpr'   == length qnOverOp'
                                                             ])
        )

    -- gplate (QuantifiedExpr qnName qnVar Nothing Nothing qnGuard qnBody) =
    --     ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnGuard ]
    --     , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
    --                   qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
    --                   qnBody'   = fromGs $ take 1 $ drop 2 xs
    --                   qnGuard'  = fromGs $ take 1 $ drop 3 xs
    --               in  if length qnName'   == 1 &&
    --                      length qnVar'    == 1 &&
    --                      length qnBody'   == 1 &&
    --                      length qnGuard'  == 1
    --                      then QuantifiedExpr (head qnName')
    --                                          (head qnVar')
    --                                          Nothing
    --                                          Nothing
    --                                          (head qnGuard')
    --                                          (head qnBody')
    --                      else gplateError "QuantifiedExpr[1]"
    --     )
    -- gplate (QuantifiedExpr qnName qnVar Nothing (Just (qnOp, qnExpr)) qnGuard qnBody) =
    --     ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnGuard, mkG qnOp, mkG qnExpr ]
    --     , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
    --                   qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
    --                   qnBody'   = fromGs $ take 1 $ drop 2 xs
    --                   qnGuard'  = fromGs $ take 1 $ drop 3 xs
    --                   qnOp'     = fromGs $ take 1 $ drop 4 xs
    --                   qnExpr'   = fromGs $ take 1 $ drop 5 xs
    --               in  if length qnName'   == 1 &&
    --                      length qnVar'    == 1 &&
    --                      length qnBody'   == 1 &&
    --                      length qnOp'     == 1 &&
    --                      length qnExpr'   == 1 &&
    --                      length qnGuard'  == 1
    --                      then QuantifiedExpr (head qnName')
    --                                          (head qnVar')
    --                                          Nothing
    --                                          (Just (head qnOp', head qnExpr'))
    --                                          (head qnGuard')
    --                                          (head qnBody')
    --                      else gplateError "QuantifiedExpr[2]"
    --     )
    -- gplate (QuantifiedExpr qnName qnVar (Just qnDom) Nothing qnGuard qnBody) =
    --     ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnGuard, mkG qnDom ]
    --     , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
    --                   qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
    --                   qnBody'   = fromGs $ take 1 $ drop 2 xs
    --                   qnGuard'  = fromGs $ take 1 $ drop 3 xs
    --                   -- qnDom'    = mapMaybe (\ j -> case j of D     i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 3 xs
    --                   qnDom'    = fromGs $ take 1 $ drop 4 xs
    --               in  if length qnName'   == 1 &&
    --                      length qnVar'    == 1 &&
    --                      length qnBody'   == 1 &&
    --                      length qnDom'    == 1 &&
    --                      length qnGuard'  == 1
    --                      then QuantifiedExpr (head qnName')
    --                                          (head qnVar')
    --                                          (Just (head qnDom'))
    --                                          Nothing
    --                                          (head qnGuard')
    --                                          (head qnBody')
    --                      else gplateError "QuantifiedExpr[3]"
    --     )
    -- gplate (QuantifiedExpr qnName qnVar (Just qnDom) (Just (qnOp, qnExpr)) qnGuard qnBody) =
    --     ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnGuard, mkG qnDom, mkG qnOp, mkG qnExpr ]
    --     , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
    --                   qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
    --                   qnBody'   = fromGs $ take 1 $ drop 2 xs
    --                   qnGuard'  = fromGs $ take 1 $ drop 3 xs
    --                   -- qnDom'    = mapMaybe (\ j -> case j of D     i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 3 xs
    --                   qnDom'    = fromGs $ take 1 $ drop 4 xs
    --                   qnOp'     = fromGs $ take 1 $ drop 5 xs
    --                   qnExpr'   = fromGs $ take 1 $ drop 6 xs
    --               in  if length qnName'   == 1 &&
    --                      length qnVar'    == 1 &&
    --                      length qnBody'   == 1 &&
    --                      length qnDom'    == 1 &&
    --                      length qnOp'     == 1 &&
    --                      length qnExpr'   == 1 &&
    --                      length qnGuard'  == 1
    --                      then QuantifiedExpr (head qnName')
    --                                          (head qnVar')
    --                                          (Just (head qnDom'))
    --                                          (Just (head qnOp', head qnExpr'))
    --                                          (head qnGuard')
    --                                          (head qnBody')
    --                      else gplateError "QuantifiedExpr[4]"
    --     )

instance MatchBind QuantifiedExpr

instance ParsePrint QuantifiedExpr where
    parse = do
        qnName   <- parse
        qnVars   <- parse `sepBy1` comma
        qnDom    <- optionMaybe (colon *> parse)
        qnExpr   <- optionMaybe ((,) <$> parse <*> parse)
        qnGuard  <- optionMaybe (comma *> parse)
        qnBody   <- dot *> parse
        let
            f []     = error "The Impossible has happenned. in QuantifiedExpr.parse.f"
            f [i]    = QuantifiedExpr qnName i qnDom qnExpr (QuanGuard (maybeToList qnGuard)) qnBody
            f (i:is) = QuantifiedExpr qnName i qnDom qnExpr (QuanGuard []) (Q $ f is)
        return (f qnVars)
    pretty (QuantifiedExpr qnName qnVar qnDom qnExpr qnGuard qnBody)
        = let header =  pretty qnName
                    <+> pretty qnVar
                    <+> ( case qnDom of
                            Nothing -> Pr.empty
                            Just i  -> Pr.colon  <+> pretty i
                        )
                    <+> ( case qnExpr of
                            Nothing     -> Pr.empty
                            Just (op,i) -> pretty op <+> pretty i
                        )
                    <>  ( case qnGuard of
                            QuanGuard []  -> Pr.empty
                            QuanGuard [i] -> Pr.comma <+> pretty i
                            _   -> error "Multiple guards, what the hell?"
                        )
                    <+> Pr.dot
              body = pretty qnBody
          in  Pr.hang header 4 body

instance TypeOf QuantifiedExpr where
    typeOf p@(QuantifiedExpr (Identifier qnName)
                             qnVar
                             qnOverDom
                             qnOverOpExpr
                             (QuanGuard qnGuards)
                             qnBody ) = inScope (mkG p) $ do
        qd :: Maybe QuantifierDecl <- getBinding qnName
        case qd of
            Nothing -> throwError $ "Quantifier not declared:" <+> text qnName
            Just q  -> do

                -- type check thr quantifier decl, and return the type of the identity.
                ti <- typeOf q

                -- add the quantified variable
                st <- S.get
                tForQnVar <- case (qnOverDom,qnOverOpExpr) of
                                ( Just dom, Nothing        ) -> typeOf dom
                                ( Nothing , Just (In,expr) ) -> do
                                    tExpr <- typeOf expr
                                    case tExpr of
                                        AnyType TSet  [j] -> return j
                                        AnyType TMSet [j] -> return j
                                        _ -> throwError $ "Quantification over nsopported type: " <+> pretty tExpr
                                _ -> error $ "not handled in QuantifiedExpr.typeOf: " ++ show (qnOverDom,qnOverOpExpr)
                case qnVar of
                    Left (Identifier nm) -> addBinding nm tForQnVar
                    _ -> error $ "not handled in QuantifiedExpr.typeOf: " ++ show qnVar

                traceM $ "adding quantified variable in context: " ++ show (qnVar,tForQnVar)

                -- check guards are bools
                forM_ qnGuards $ \ x -> inScope (mkG x) $ do 
                    tx <- typeOf x
                    unless (tx==TBool) $ typeError "Quantifier guard is not of type boolean."

                -- check the inner expr.
                tb <- typeOf qnBody
                unless (ti==tb) $ typeError "Type mismatch in the inner expression of quantified expression."

                S.put st
                return ti



newtype QuanGuard = QuanGuard [Expr]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag QuanGuard

instance Hole QuanGuard

instance GPlate QuanGuard where
    gplate (QuanGuard xs) = gplateUniList QuanGuard xs

instance MatchBind QuanGuard where
    match (QuanGuard [] ) (QuanGuard [] ) = return ()
    match (QuanGuard [p]) (QuanGuard [] ) = match p (V (VBool True))
    match (QuanGuard [p]) (QuanGuard [a]) = match p a
    match _ _ = throwError "while pattern matching a QuanGuard"

    bind (QuanGuard []) = return Nothing
    bind (QuanGuard [EHole (Identifier nm)]) = do
        bindings :: BindingsMap <- getM
        case M.lookup nm bindings of
            Nothing -> return Nothing -- $ Just $ QuanGuard []
            Just _  -> do
                res <- bind (EHole (Identifier nm))
                case res of
                    Nothing -> return Nothing
                    Just x  -> return $ Just $ QuanGuard [x]
    bind (QuanGuard [x]) = do
        res <- bind x
        case res of
            Nothing -> return Nothing -- $ Just $ QuanGuard []
            Just x' -> return $ Just $ QuanGuard [x']
    bind _ = error "Multiple guards, what the hell?"

instance ParsePrint QuanGuard where
    parse = error "QuanGuard parse"
    pretty = text . show

