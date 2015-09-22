{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.UI.LogFollow
    ( logFollow
    , storeChoice
    , refAnswers
    ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Rules.Definition
import Conjure.Language.Domain
import Conjure.Language.Parser

import Conjure.Bug ( bug )
import Conjure.UserError
import Conjure.UI.IO ( readModelFromFile )

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as A
import qualified Data.ByteString.Lazy as B

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Text.PrettyPrint as Pr
-- import Text.Read(read)

import qualified Data.IntSet as I
import Data.Set(Set)
import qualified Data.Set as S

import Data.Map(Map)
import qualified Data.Map as M
import Data.List(maximumBy)
import System.FilePath(takeExtension)

import Data.Global(declareIORef)
import Data.IORef(readIORef, IORef, writeIORef)

type HoleHash    = Int
type GenOrd      = Int
type Pref         = Int
type AnswerStore =  Map HoleHash (Set (QuestionAnswered, GenOrd) )

answeredRef :: IORef AnswerStore
answeredRef = declareIORef "answeredRefGlobal" M.empty

refAnswers :: (MonadIO m) => FilePath ->  m ()
refAnswers fp = do
  answered <- liftIO $ getAnswersFromFile fp
  liftIO $ writeIORef answeredRef answered

giveAnswers :: (MonadIO m) => m AnswerStore
giveAnswers = liftIO $ readIORef answeredRef

putAnswers :: (MonadIO m) => AnswerStore -> m ()
putAnswers store = liftIO $ writeIORef answeredRef store



logFollow  :: (MonadIO m, MonadLog m)
          => Config
          -> Question -> [(Doc, Answer)] -> m [(Int, Doc, Answer)]
logFollow config q@Question{..} options = do
  logWarn ("-----")
  logWarn ( "qhole       " <+>  pretty  qHole <+> (pretty . holeHash) qHole )
  logWarn ( "qAscendants" <+> vcat (map (pretty . holeHash) qAscendants) )

  logWarn $ hang "Ans" 4 $
    vcat $ [ hang ("Answer" <+> pretty i) 8 $
                  vcat ["text" <+> pretty aText, "ansE" <+> pretty aAnswer]
           |  (_,Answer{..}) <- options | i <- allNats ]

  before <- giveAnswers

  res <- case matching before of
    Just (a, newStore)  -> do
      logWarn (vcat ["Matched with previous data"
                    , "Question" <+> (pretty  qHole) <+> (pretty . holeHash $ qHole)
                    , "Answer" <+> (prettyAns  a) ])
      putAnswers newStore
      mapM (storeChoice config q ) [a]

    Nothing  -> do
        logWarn (vcat ["No match for "
                      , "question hole"  <+> (pretty  qHole) <+> (pretty . holeHash $ qHole)
                      , "AsQuestionAnswered" <+> (pretty . show)
                                                 [ makeChoice q a |  (_,a) <- options ]
                      ])
        mapM (storeChoice config q . snd) options

  logWarn ("-----")
  return [ (0, "logfollow", r) | r <- res ]

  where
    prettyAns :: Answer -> Doc
    prettyAns Answer{..} =  hang "Answer" 4 $ vcat [
                             "aText"     <+> aText
                           , "aAnswer"   <+> pretty aAnswer
                           , "aRuleName" <+> pretty aRuleName
                           ]

    matching :: AnswerStore -> Maybe (Answer, AnswerStore)
    matching before = do
        qsMatches   <- M.lookup (holeHash qHole) before
        ascMatches  <- nullSetMay $ setMapMaybe (ascMatch) qsMatches
        optsMatches <- nullListMay $ mapMaybe (optionMatch ascMatches) (map snd options)

        let maxAsc        = maximumBy (compare `on` fou4) optsMatches
            maxAscMatches = filter (\t -> fou4 t == fou4 maxAsc ) optsMatches
            minMatch      = minimumBy (compare `on` fou3) maxAscMatches
            newSet        = S.delete (fou23 minMatch) qsMatches
            newStore      = M.insert (holeHash qHole) newSet before


        Just (fou1 minMatch, newStore)

    ascMatch :: (QuestionAnswered, GenOrd) -> Maybe (QuestionAnswered,  GenOrd, Pref)
    ascMatch x = case f x of
                   (_,_,0) -> Nothing
                   y       -> Just y

      where
      qAsSet = (I.fromList . map holeHash) qAscendants

      f :: (QuestionAnswered, GenOrd) -> (QuestionAnswered, GenOrd, Pref)
      f (a,ord) | (I.null (qAscendants_ a) && I.null qAsSet) = (a, ord, 1)
      f (a,ord) = (a, ord, I.size (qAscendants_ a `I.intersection` qAsSet))

    optionMatch :: Set (QuestionAnswered, GenOrd, Pref) -> Answer
                -> Maybe (Answer, QuestionAnswered, GenOrd, Pref)
    optionMatch ls a =  minMaySet $ S.filter f ls


      where
      f :: (QuestionAnswered, GenOrd, Pref) -> Bool
      f (qa,_,_) | (aRuleName_ qa) /= (show $ aRuleName a) = False

      f (AnsweredRule{},_,_)  = True
      f (AnsweredRepr{aDom_=dom},_,_) = compareDoms dom (getReprFromAnswer a)
      f t@(AnsweredReprStored{},_,_) = bug ("Got unexpected " <+> (pretty . show ) t)

      minMaySet :: Set (QuestionAnswered, GenOrd, Pref)
                -> Maybe (Answer, QuestionAnswered, GenOrd, Pref)
      minMaySet s | S.null s = Nothing
      minMaySet s =
          let maxAsc        = maximumBy (compare `on` thd3) (S.toList s)
              maxAscMatches = filter (\t -> thd3 t == thd3 maxAsc ) (S.toList s)
              (b,c,d)      = minimumBy (compare `on` snd3) maxAscMatches
          in  Just (a, b,c,d)



    nullSetMay :: Set a -> Maybe (Set a)
    nullSetMay s | S.null s = Nothing
    nullSetMay s = Just s

    nullListMay  :: [a] -> Maybe [a]
    nullListMay  [] = Nothing
    nullListMay  xs = Just xs

    setMapMaybe :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
    setMapMaybe f = S.map (fromJustNote "setMapMaybe") . S.filter isJust . S.map f



compareDoms :: Domain HasRepresentation Expression
            -> Domain HasRepresentation Expression -> Bool
-- compareDoms d1 d2 = d1 == d2
compareDoms d1 d2 = getReps d1 == getReps d2
-- compareDoms d1 d2 = error . show . vcat $ [ pretty  $ d1
--                                           , pretty  $ d2
--                                           , pretty . show . getReps $ d1
--                                           , pretty . show . getReps $ d2
--                                           ]

    where
    getReps :: Domain HasRepresentation Expression -> [Name]
    getReps d = catMaybes [ getRep n | n <- universe d ]

    getRep :: Domain HasRepresentation Expression -> Maybe Name
    getRep (DomainSet (HasRepresentation r) _ _)         = Just r
    getRep (DomainMSet (HasRepresentation r) _ _)        = Just r
    getRep (DomainFunction (HasRepresentation r) _ _ _)  = Just r
    getRep (DomainRelation (HasRepresentation r) _ _)    = Just r
    getRep (DomainPartition (HasRepresentation r) _ _)   = Just r
    getRep _ = Nothing


storeChoice :: MonadLog m => Config -> Question -> Answer -> m Answer
storeChoice config q a = do
  let c = makeChoice q a
  logWarn $ vcat ["storedChoice:"
                 ,  (pretty . qHole)  q <+> (pretty . holeHash . qHole) q
                 ,  pretty . show $ c]
  saveToLog $ "LF: " <+> jsonToDoc c  <+> "END:"
  let a' = a { aFullModel = updateModelWIPInfo (addQuestionAnswered (logChoices config) c) (aFullModel a) }
  return $ a'

addQuestionAnswered :: Bool -> QuestionAnswered -> ModelInfo -> ModelInfo
addQuestionAnswered False _ m =  m
addQuestionAnswered _ qa oldInfo = oldInfo { miQuestionAnswered = miQuestionAnswered oldInfo ++ [qa] }


makeChoice :: Question -> Answer -> QuestionAnswered
makeChoice q a =  case (aRuleName a) of
                 "choose-repr" ->
                     AnsweredReprStored
                     { qHole_       = holeHash . qHole $  q
                     , qAscendants_ = I.fromList . map holeHash . qAscendants $ q
                     , aDomStored_  = renderNormal . pretty $ getReprFromAnswer a
                     , aRuleName_   = show $ aRuleName a
                     }
                 _ ->
                     AnsweredRule
                     { qHole_       = holeHash . qHole $  q
                     , qAscendants_ = I.fromList . map holeHash . qAscendants $ q
                     , aRuleName_   = show $ aRuleName a
                     }

getReprFromAnswer ::   Answer -> Domain HasRepresentation Expression
getReprFromAnswer = unErr . (runLexerAndParser parseDomainWithRepr "getReprFromAnswer")
                          . getReprDomText


  where
  unErr (Right r) = r
  unErr (Left r)  = bug ("getReprFromAnswer unErr" <+> r)

  -- the expected format is always the following:
  -- "choose-repr: Choosing representation for <VARNAME>: <DOMAIN>"
  getReprDomText :: Answer -> Text
  getReprDomText = T.pack                                       -- convert to text
                 . intercalate ":" . tail . tail . splitOn ":"  -- drop the first two things seperated by :'s
                 . renderNormal . aText                         -- aText into String


getAnswersFromFile :: (MonadIO m, MonadFail m, MonadUserError m)
              => FilePath -> m AnswerStore

 -- Read from a json file
getAnswersFromFile fp | takeExtension fp  == ".json" = do
  liftIO $ fmap A.decode (B.readFile fp) >>= \case
    Just (vs ::  [QuestionAnswered])  -> do
        -- putStrLn $ "BeforeToSet: " ++  (show vs)
        return $ M.fromListWith (S.union) [ ((qHole_ v) , S.singleton (v, i))
                                                | v <- map convertBack vs
                                                | i <- [0..]
                                          ]
    Nothing -> userErr1 $ "Error parsing" <+> pretty fp

-- Read from a eprime file
getAnswersFromFile fp = do
  Model{mInfo=ModelInfo{miQuestionAnswered=vs}} <- readModelFromFile fp
  return $ M.fromListWith (S.union) [ (( qHole_ v) , S.singleton (v,i) )
                                          | v <- map convertBack vs
                                          | i <- [0..]
                                    ]

convertBack :: QuestionAnswered -> QuestionAnswered
convertBack AnsweredReprStored{..} = AnsweredRepr{..}
  where
    unErr (Right r) = r
    unErr (Left r)  = bug ("convertBack unErr" <+> r)

    aDom_ = unErr . (runLexerAndParser parseDomainWithRepr "convertBack")
                  . stringToText
                  $ aDomStored_

convertBack a = a


saveToLog :: MonadLog m => Doc -> m ()
saveToLog = log LogFollow


jsonToDoc :: ToJSON a => a -> Doc
jsonToDoc  = Pr.text . L.unpack . L.toLazyText . A.encodeToTextBuilder . toJSON


holeHash :: (Show x, Pretty x) =>x  -> HoleHash
holeHash = hash . show . pretty

fou1 :: (a,b,c,d) -> a
fou1 (a,_,_,_) = a

fou4 :: (a,b,c,d) -> d
fou4 (_,_,_,d) = d

fou3 :: (a,b,c,d) -> c
fou3 (_,_,c,_) = c

fou23 :: (a,b,c,d) -> (b,c)
fou23 (_,b,c,_) = (b,c)
