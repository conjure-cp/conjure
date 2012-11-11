{-# LANGUAGE OverloadedStrings #-}

module Language.E.TH where

import Stuff.Generic
import Language.E.Definition
import Language.E.Parser

-- template-haskell
import Language.Haskell.TH ( Q, Exp(..), Pat(..), Lit(..), mkName )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )

import qualified Data.Text as T


-- match by parsing an expression
eMatch :: QuasiQuoter
eMatch = mkMatchLike parseExpr


-- match by parsing a domain
dMatch :: QuasiQuoter
dMatch = mkMatchLike parseDomain


mkMatchLike :: Parser E -> QuasiQuoter
mkMatchLike parser = qq {
    quotePat = \ inp -> do
        let
            buildP' :: BuiltIn -> Pat
            buildP' (B False)
                = ConP (mkName "B")
                    [ ConP (mkName "False") [] ]
            buildP' (B True)
                = ConP (mkName "B")
                    [ ConP (mkName "True") [] ]
            buildP' (I i)
                = ConP (mkName "I")
                    [ LitP (IntegerL i) ]
            buildP' (S s)
                = ConP (mkName "S")
                    [ LitP (StringL s) ]

            buildP :: E -> Q Pat
            buildP (Prim p) =
                return $ ConP (mkName "Prim") [buildP' p]
            buildP (Tagged "metavar" [Prim (S s)]) = return $
                if s == "_"
                    then WildP
                    else VarP (mkName s)
            buildP (Tagged t xs) = do
                ys <- mapM buildP xs
                return $ ConP (mkName "Tagged")
                              [ ConP (mkName $ show t) []
                              , ListP ys
                              ]

        case runLexerAndParser (inCompleteFile parser) "" (T.pack inp) of
            Left  e -> error $ show e
            Right x -> buildP x
    }


eMake :: QuasiQuoter
eMake = qq {
    quoteExp = \ inp -> do
        let
            build' :: BuiltIn -> Exp
            build' (B False)
                = ConE (mkName "B")
                    `AppE`
                  ConE (mkName "False")
            build' (B True)
                = ConE (mkName "B")
                    `AppE`
                  ConE (mkName "True")
            build' (I i)
                = ConE (mkName "I")
                    `AppE`
                  LitE (IntegerL i)
            build' (S s)
                = ConE (mkName "S")
                    `AppE`
                  LitE (StringL s)

            build :: E -> Q Exp
            build (Prim p) =
                return $ ConE (mkName "Prim") `AppE` build' p
            build (Tagged "metavar" [Prim (S s)]) =
                return $ VarE (mkName s)
            build (Tagged t xs) = do
                ys <- mapM build xs
                return $ (ConE $ mkName "Tagged")
                            `AppE`
                         (ConE $ mkName $ show t)
                            `AppE`
                         ListE ys

        case runLexerAndParser (inCompleteFile parseExpr) "" (T.pack inp) of
            Left  e -> error $ show e
            Right x -> build x
    }

