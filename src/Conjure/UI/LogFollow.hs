{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}


module Conjure.UI.LogFollow where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty

import Conjure.Rules.Definition

import Text.Read(read)
import Conjure.UI.IO ( readModelFromFile )
import Conjure.Bug
import qualified Data.Aeson as A


logFollow :: (MonadIO m, MonadLog m)
          => [QuestionAnswered] -> Question -> [(Doc, Answer)] -> m [Answer]
logFollow before Question{..} options = do
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
      return [a]
    Nothing  -> do
        logWarn (vcat ["No match for ", "question" <+> (pretty  qHole)])
        return (map snd options)

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


storeChoice :: Question -> Answer -> Answer
storeChoice q a@Answer{aFullModel=m} =
  let qa = QuestionAnswered{ qHole_       = hash $ qHole q
                           , qAscendants_ = hash $ qAscendants q
                           , aText_       = hash $ show $ aText a
                           , aAnswer_     = hash $ aAnswer a
                           }
      -- newInfo = (mInfo m){miFollow= qa : (miFollow . mInfo $ m) }
      newInfo = (mInfo m){miFollow= ( qa : (miFollow . mInfo $ m)) }
  in a{aFullModel=m{mInfo=newInfo}}


getAnswers :: (MonadIO m, MonadFail m ) => FilePath -> m [QuestionAnswered]
getAnswers fp = do
  Model{mInfo=ModelInfo{miFollow=logStrings} }  <- readModelFromFile fp
  -- let ans :: [QuestionAnswered] = concatMap  (\a ->
  --         fromMaybe (userErr $ "logParseError" <+> (pretty $ show a) <+> "---\n" )
  --                       . A.decode . read $ a ) logStrings
  -- return ans
  return logStrings
