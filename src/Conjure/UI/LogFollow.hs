{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}


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

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import qualified Text.PrettyPrint as Pr
-- import Text.Read(read)


logFollow :: (MonadIO m, MonadLog m)
          => [QuestionAnswered] -> Question -> [(Doc, Answer)] -> m [Answer]
logFollow before q@Question{..} options = do
  logWarn ("-----")
  logWarn ( "qhole       " <+>  pretty  qHole )
  logWarn ( "qAscendants" <+> vcat (map pretty qAscendants) )

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


    f ans@Answer{..} = if or $ map match before then
              Just ans
          else
              Nothing
      where
        match QuestionAnswered{..} = and
          [ qHole_        == hash qHole
          , qAscendants_  == hash  qAscendants
          , aText_        == (hash $ show aText)
          , aAnswer_      == hash aAnswer
          ]


storeChoice :: MonadLog m => Question -> Answer -> m Answer
storeChoice q a@Answer{aFullModel=m} = do
  let qa = QuestionAnswered{ qHole_       = hash $ qHole q
                           , qAscendants_ = hash $ qAscendants q
                           , aText_       = hash $ show $ aText a
                           , aAnswer_     = hash $ aAnswer a
                           }
      -- newInfo = (mInfo m){miFollow= qa : (miFollow . mInfo $ m) }
      newInfo = (mInfo m){miFollow= ( qa : (miFollow . mInfo $ m)) }
  saveToLog $ "LF: " <+> jsonToDoc qa  <+> "END:"
  return $ a{aFullModel=m{mInfo=newInfo}}


getAnswers :: (MonadIO m, MonadFail m ) => FilePath -> m [QuestionAnswered]
getAnswers fp = do
  -- the double encoded Q&A
  -- let ans :: [QuestionAnswered] = concatMap  (\a ->
  --         fromMaybe (userErr $ "logParseError" <+> (pretty $ show a) <+> "---\n" )
  --                       . A.decode . read $ a ) logStrings
  -- return ans

  -- Reading from the eprime
  -- Model{mInfo=ModelInfo{miFollow=logStrings} }  <- readModelFromFile fp
  -- return logStrings

  -- Read from a json file
  liftIO $ fmap A.decode (B.readFile fp) >>= \case
    Just v  -> return v
    Nothing -> userErr $ "Error parsing" <+> pretty fp


saveToLog :: MonadLog m => Doc -> m ()
saveToLog = log LogFollow


jsonToDoc :: ToJSON a => a -> Doc
jsonToDoc  = Pr.text . T.unpack . T.toLazyText . A.encodeToTextBuilder . toJSON
