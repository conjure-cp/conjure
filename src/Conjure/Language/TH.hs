{-# LANGUAGE TemplateHaskell #-}

module Conjure.Language.TH
    ( essence, essenceDomain, essenceStmts
    , fixAfterParsing
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Expression.Op.FrameUpdate
import Conjure.Language.Domain
import Conjure.Language.Parser
import Conjure.Language.Pretty
import Conjure.Language.Lenses ( fixRelationProj )


-- megaparsec
import Text.Megaparsec.Prim ( setPosition )
import Text.Megaparsec.Pos ( SourcePos, newPos )

-- template-haskell
import Language.Haskell.TH ( Q, Loc(..), location, mkName, ExpQ, varE, appE, PatQ, varP, wildP )
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

-- syb
import Data.Generics.Aliases ( extQ )


essenceStmts :: QuasiQuoter
essenceStmts = QuasiQuoter
    { quoteExp = \ str -> do
        l <- locationTH
        e <- parseIO (setPosition l *> parseTopLevels) str
        let e' = dataToExpQ (const Nothing `extQ` expE `extQ` expD `extQ` expAP `extQ` expName) e
        appE [| $(varE (mkName "fixAfterParsing")) |] e'
    , quotePat  = \ str -> do
        l <- locationTH
        e <- parseIO (setPosition l *> parseTopLevels) str
        dataToPatQ (const Nothing `extQ` patE `extQ` patD `extQ` patAP `extQ` patName) e
    , quoteType = bug "quoteType"
    , quoteDec  = bug "quoteDec"
    }

essence :: QuasiQuoter
essence = QuasiQuoter
    { quoteExp = \ str -> do
        l <- locationTH
        e <- parseIO (setPosition l *> parseExpr) str
        let e' = dataToExpQ (const Nothing `extQ` expE `extQ` expD `extQ` expAP `extQ` expName) e
        appE [| $(varE (mkName "fixAfterParsing")) |] e'
    , quotePat  = \ str -> do
        l <- locationTH
        e <- parseIO (setPosition l *> parseExpr) str
        dataToPatQ (const Nothing `extQ` patE `extQ` patD `extQ` patAP `extQ` patName) e
    , quoteType = bug "quoteType"
    , quoteDec  = bug "quoteDec"
    }

essenceDomain :: QuasiQuoter
essenceDomain = QuasiQuoter
    { quoteExp = \ str -> do
        l <- locationTH
        e <- parseIO (setPosition l *> parseDomain) str
        let e' = dataToExpQ (const Nothing `extQ` expE `extQ` expD `extQ` expAP `extQ` expName) e
        appE [| $(varE (mkName "fixAfterParsing")) |] e'
    , quotePat  = \ str -> do
        l <- locationTH
        e <- parseIO (setPosition l *> parseDomain) str
        dataToPatQ (const Nothing `extQ` patE `extQ` patD `extQ` patAP `extQ` patName) e
    , quoteType = bug "quoteType"
    , quoteDec  = bug "quoteDec"
    }

locationTH :: Q SourcePos
locationTH = do
    loc <- location
    let file = loc_filename loc
    let (line, col) = loc_start loc
    return (newPos file line col)

expE :: Expression -> Maybe ExpQ
expE (ExpressionMetaVar x) = Just [| $(varE (mkName x)) |]
expE _ = Nothing

expD :: Domain () Expression -> Maybe ExpQ
expD (DomainMetaVar x) = Just (appE [| $(varE (mkName "forgetRepr")) |]
                                    [| $(varE (mkName x)) |])
expD _ = Nothing

expAP :: AbstractPattern -> Maybe ExpQ
expAP (AbstractPatternMetaVar x) = Just [| $(varE (mkName x)) |]
expAP _ = Nothing

expName :: Name -> Maybe ExpQ
expName (NameMetaVar x) = Just [| $(varE (mkName x)) |]
expName _ = Nothing


patE :: Expression -> Maybe PatQ
patE (ExpressionMetaVar x) = toPat x
patE _ = Nothing

patD :: Domain () Expression -> Maybe PatQ
patD (DomainMetaVar x) = toPat x
patD _ = Nothing

patAP :: AbstractPattern -> Maybe PatQ
patAP (AbstractPatternMetaVar x) = toPat x
patAP _ = Nothing

patName :: Name -> Maybe PatQ
patName (NameMetaVar x) = toPat x
patName _ = Nothing


toPat :: String -> Maybe PatQ
toPat "_" = Just wildP
toPat x = Just (varP (mkName x))



fixAfterParsing :: (Data a, Show a) => a -> a
fixAfterParsing = fixFrameUpdate . fixRelationProj

fixFrameUpdate :: (Data a, Show a) => a -> a
fixFrameUpdate = transformBi f
    where

        convert :: OpFrameUpdate Expression -> Either [Name] Expression -> [Name]
        convert p focus =
            case focus of
                Left xs -> xs
                Right x ->
                    case listOut x of
                        Just xs -> [nm | Reference nm _ <- xs]
                        Nothing -> 
                            bug $ vcat [ "focus:" <+> pretty (show focus)
                                       , "whole expr:" <+> pretty (show p)
                                       ]
        f :: OpFrameUpdate Expression -> OpFrameUpdate Expression
        f p@OpFrameUpdate{} = p
        f p@OpFrameUpdateEprime{} = p
        f p@(OpFrameUpdateInternal source target sourceFocus targetFocus constraint) =
            let
                sourceFocusOut = convert p sourceFocus
                targetFocusOut = convert p targetFocus
            in
                OpFrameUpdate source target sourceFocusOut targetFocusOut constraint
        f p@(OpFrameUpdateEprimeInternal source target sourceFocus targetFocus) =
            let
                sourceFocusOut = convert p sourceFocus
                targetFocusOut = convert p targetFocus
            in
                OpFrameUpdateEprime source target sourceFocusOut targetFocusOut
