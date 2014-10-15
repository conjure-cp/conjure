module Language.E.TH ( eMatch, eMake ) where

import Conjure.Prelude
import Stuff.Generic.Tag
import Language.E.Definition
import Language.E.Parser
import Language.E.Parser.EssenceFile.Value () -- helping GHC's dependency resolution a a bit
                                              -- mutually recursive modules and TH do not play well, apparently.
                                              -- see. http://hackage.haskell.org/trac/ghc/ticket/1012

-- template-haskell
import Language.Haskell.TH ( Q, Exp(..), Pat(..), Lit(..), mkName )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )

import Data.Text as T


-- match by parsing an expression
eMatch :: QuasiQuoter
eMatch = mkMatchLike parseExpr


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
                    [ LitP (StringL $ T.unpack s) ]

            buildP :: E -> Q Pat
            buildP (Tagged (Tag "metavar") [Prim (S s)])
                = return $ if s == "_" then WildP else VarP (mkName $ T.unpack s)
            buildP (D (DomainHack (Tagged (Tag "metavar") [Prim (S s)]))) -- oh boy, what a hack!
                = return $ if s == "_" then WildP else VarP (mkName $ T.unpack s)
            buildP (Prim p)
                = return $ ConP (mkName "Prim") [buildP' p]
            buildP (Tagged (Tag t) xs) = do
                ys <- mapM buildP xs
                return $ ConP (mkName "Tagged")
                              [ ConP (mkName "Tag") [LitP (StringL (T.unpack t))]
                              , ListP ys
                              ]
            buildP p = error ("buildP: " ++ show p)

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
                  ( VarE (mkName "stringToText")
                    `AppE`
                    LitE (StringL $ T.unpack s)
                  )

            build :: E -> Q Exp
            build (D (DomainHack (Tagged (Tag "metavar") [Prim (S s)]))) -- oh boy, what a hack!
                = return $ VarE (mkName $ T.unpack s)
            build (Tagged (Tag "metavar") [Prim (S s)])
                = return $ VarE (mkName $ T.unpack s)
            build (Prim p)
                = return $ ConE (mkName "Prim") `AppE` build' p
            build (Tagged (Tag t) xs) = do
                ys <- mapM build xs
                return $ (ConE $ mkName "Tagged")
                            `AppE`
                         (ConE (mkName "Tag") `AppE` LitE (StringL $ T.unpack t))
                            `AppE`
                         ListE ys
            build p = error ("eMake.build: " ++ show p)

        case runLexerAndParser (inCompleteFile parseExpr) "" (T.pack inp) of
            Left  e -> error $ show e
            Right x -> build x
    }

