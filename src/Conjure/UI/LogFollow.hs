{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}



module Conjure.UI.LogFollow
    ( logFollow
    , storeChoice
    , getAnswers
    ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Rules.Definition

import Conjure.Bug(userErr)
-- import Conjure.UI.IO ( readModelFromFile )

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as A
import qualified Data.ByteString.Lazy as B

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Text.PrettyPrint as Pr
-- import Text.Read(read)

import qualified Data.IntSet as I

import Data.Map(Map)
import qualified Data.Map as M


logFollow :: (MonadIO m, MonadLog m)
          => Map (String,HoleHash) QuestionAnswered
          -> Question -> [(Doc, Answer)] -> m [Answer]
logFollow before q@Question{..} options = do
  logWarn ("-----")
  logWarn ( "qhole       " <+>  pretty  qHole )
  logWarn ( "qAscendants" <+> vcat (map (pretty . hash) qAscendants) )

  logWarn $ hang "Ans" 4 $
    vcat $ [ hang ("Answer" <+> pretty i) 8 $
                  vcat ["text" <+> pretty aText, "ansE" <+> pretty aAnswer]
           |  (_,Answer{..}) <- options | i <- allNats ]



  res <- case matching of
    Just a  -> do
      logWarn (vcat ["Matched with previous data"
                    , "Question" <+> (pretty  qHole)
                    , "Answer" <+> (pretty . aAnswer $ a) ])
      c <- storeChoice q a
      return [c]
    Nothing  -> do
        logWarn (vcat ["No match for ", "question" <+> (pretty  qHole)])
        mapM (storeChoice q . snd) options

  logWarn ("-----")
  return res

  where
    matching :: Maybe Answer
    matching =
      case (catMaybes $ map f (map snd options))  of
           []    -> Nothing
           (x:_) -> return x

    qAsSet = (I.fromList . map hash) qAscendants

    f ans@Answer{..}
        | Just a <- M.lookup (show aRuleName, hash qHole) before
        , I.null (qAscendants_ a) == I.null qAsSet
        || (not . I.null) (qAscendants_ a `I.intersection` qAsSet) =
           if process a ans then
               Just ans
           else
               Nothing

    f _ =  Nothing

    -- FIXME compare domains
    process AnsweredRepr{..} ans = aDom_ == getReprDomText ans
    process AnsweredRule{} _     = True



storeChoice :: MonadLog m => Question -> Answer -> m Answer
storeChoice q a = do
  let qa = case (aRuleName a) of
                 "choose-repr" ->
                     AnsweredRepr
                     { qHole_       = hash $ qHole q
                     , qAscendants_ = I.fromList . map hash . qAscendants $ q
                     , aDom_        = getReprDomText a
                     , aRuleName_   = show $ aRuleName a
                     }
                 _ ->
                     AnsweredRule
                     { qHole_       = hash $ qHole q
                     , qAscendants_ = I.fromList . map hash . qAscendants $ q
                     , aRuleName_   = show $ aRuleName a
                     }


  saveToLog $ "LF: " <+> jsonToDoc qa  <+> "END:"
  return a


getReprDomText :: Answer -> Text
getReprDomText a =  T.split (== '˸') (T.pack . renderNormal . aText $ a) `at` 1

-- Read from a json file
type HoleHash = Int
getAnswers :: (MonadIO m, MonadFail m ) => FilePath -> m (Map (String,HoleHash) QuestionAnswered)
getAnswers fp = do
  liftIO $ fmap A.decode (B.readFile fp) >>= \case
    Just (vs :: [QuestionAnswered])  -> return $ M.fromList [ ((aRuleName_ v, qHole_ v) ,v)  | v <- vs ]
    Nothing -> userErr $ "Error parsing" <+> pretty fp


saveToLog :: MonadLog m => Doc -> m ()
saveToLog = log LogFollow


jsonToDoc :: ToJSON a => a -> Doc
jsonToDoc  = Pr.text . L.unpack . L.toLazyText . A.encodeToTextBuilder . toJSON
