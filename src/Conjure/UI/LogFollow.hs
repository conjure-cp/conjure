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
import Conjure.Language.Domain
import Conjure.Language.Parser

import Conjure.Bug(userErr,bug)
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
import qualified Data.Set as S


import Data.Map(Map)
import qualified Data.Map as M
import Data.List(maximumBy)
import System.FilePath(takeExtension)


logFollow :: (MonadIO m, MonadLog m)
          => Config
          -> Question -> [(Doc, Answer)] -> m [Answer]
logFollow config q@Question{..} options = do
  logWarn ("-----")
  logWarn ( "qhole       " <+>  pretty  qHole <+> (pretty . holeHash) qHole )
  logWarn ( "qAscendants" <+> vcat (map (pretty . holeHash) qAscendants) )

  logWarn $ hang "Ans" 4 $
    vcat $ [ hang ("Answer" <+> pretty i) 8 $
                  vcat ["text" <+> pretty aText, "ansE" <+> pretty aAnswer]
           |  (_,Answer{..}) <- options | i <- allNats ]



  res <- case matching of
    Just a  -> do
      logWarn (vcat ["Matched with previous data"
                    , "Question" <+> (pretty  qHole) <+> (pretty . holeHash $ qHole)
                    , "Answers" <+> (vcat $ map prettyAns  a) ])
      -- c <- storeChoice q a
      -- return [c]
      mapM (storeChoice config q ) a
    Nothing  -> do
        logWarn (vcat ["No match for "
                      , "question hole"  <+> (pretty  qHole) <+> (pretty . holeHash $ qHole)
                      , "AsQuestionAnswered" <+> (pretty .groom)
                                                 [ makeChoice q a |  (_,a) <- options ]
                      ])
        mapM (storeChoice config q . snd) options

  logWarn ("-----")
  return res

  where
    before :: Map (String,HoleHash) [QuestionAnswered]
    before = questionAnswers config
    matching :: Maybe [Answer]
    matching =
      case (catMaybes $ map haveMapping (map snd options))  of
           []        -> Nothing
           [(x,_)]   -> Just [x]
           xs        -> case maxes xs of
                          []      -> error "matching no max"
                          -- [(x,_)] -> Just [x]
                          xx -> Just . map fst $ xx
                          -- xx      -> error . show .vcat $ "matching multiple mappings"
                          --            : map ( \(r,t) -> pretty (prettyAns r, pretty t)
                                           -- ) xx
           -- xs    -> error . show .vcat $ "matching multiple mappings" : map (prettyAns) xs

    -- maxes :: (Ord x, Eq x) => [x] -> [x]
    maxes xs = let (_,maxSize) = maximumBy (compare `on` snd) xs
               in  filter (\(_,i) -> i == maxSize ) xs

    prettyAns :: Answer -> Doc
    prettyAns Answer{..} =  hang "Answer" 4 $ vcat [
                             "aText"     <+> aText
                           , "aAnswer"   <+> pretty aAnswer
                           , "aRuleName" <+> pretty aRuleName
                           ]

    haveMapping ans@Answer{..} = do
      previous <- M.lookup (show aRuleName, holeHash qHole) before
      mappings <- pickMapping previous
      case filter (process ans . fst) mappings of
        []  -> Nothing
        -- [_] -> Just ans
        ( (_,i) :_)   -> Just (ans,i)
        -- xs  -> error . show .vcat $ "haveMapping" : map (pretty . groom) xs

        where
          qAsSet = (I.fromList . map holeHash) qAscendants

          pickMapping :: [QuestionAnswered] -> Maybe [(QuestionAnswered,Int)]
          pickMapping =  toMaybe . filter (\(_,i) -> i /= 0 ) .  map f

          f a | (I.null (qAscendants_ a) && I.null qAsSet) = (a, 1)
          f a = (a, I.size (qAscendants_ a `I.intersection` qAsSet))

          -- toMaybe :: Show x => [x] -> Maybe x
          toMaybe []      = Nothing
          -- toMaybe [(x,_)] = Just [x]
          toMaybe [x] = Just [x]
          toMaybe xs = let (_,maxSize) = maximumBy (compare `on` snd) xs in
                       case filter (\(_,i) -> i == maxSize ) xs of
                         []      -> error "haveMapping toMaybe cannot happen"
                         xx      -> Just xx



          process a AnsweredRepr{..} = compareDoms aDom_ (getReprFromAnswer a)
          process _   AnsweredRule{}   = True







compareDoms :: Domain HasRepresentation Expression
            -> Domain HasRepresentation Expression -> Bool
-- compareDoms d1 d2 = d1 == d2
compareDoms d1 d2 = getReps d1 == getReps d2
-- compareDoms d1 d2 = error . show . vcat $ [ pretty  $ d1
--                                           , pretty  $ d2
--                                           , pretty . groom . getReps $ d1
--                                           , pretty . groom . getReps $ d2
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
                 ,  pretty . groom $ c]
  saveToLog $ "LF: " <+> jsonToDoc c  <+> "END:"
  let a' = a{aFullModel = addQuestionAnswered (logChoices config) (aFullModel a) c }
  return $ a'

addQuestionAnswered :: Bool -> Model -> QuestionAnswered -> Model
addQuestionAnswered False m _ =  m
addQuestionAnswered _  model qa = model { mInfo = newInfo }

  where
    oldInfo = mInfo model
    newInfo = oldInfo { miQuestionAnswered = miQuestionAnswered oldInfo ++ [qa]
                      }


makeChoice :: Question -> Answer -> QuestionAnswered
makeChoice q a =  case (aRuleName a) of
                 "choose-repr" ->
                     AnsweredRepr
                     { qHole_       = holeHash . qHole $  q
                     , qAscendants_ = I.fromList . map holeHash . qAscendants $ q
                     , aDom_        = getReprFromAnswer a
                     , aRuleName_   = show $ aRuleName a
                     }
                 _ ->
                     AnsweredRule
                     { qHole_       = holeHash . qHole $  q
                     , qAscendants_ = I.fromList . map holeHash . qAscendants $ q
                     , aRuleName_   = show $ aRuleName a
                     }

holeHash :: (Show x, Pretty x) =>x  -> HoleHash
holeHash = hash . show . pretty

getReprFromAnswer ::   Answer -> Domain HasRepresentation Expression
getReprFromAnswer = unErr . (runLexerAndParser parseDomainWithRepr "getReprFromAnswer")
                          . getReprDomText


  where
  unErr (Right r) = r
  unErr (Left r)  = bug ("getReprFromAnswer unErr" <+> r)

  getReprDomText :: Answer -> Text
  getReprDomText a =  T.split (== '˸') (T.pack . renderNormal . aText $ a) `at` 1


type HoleHash = Int
getAnswers :: (MonadIO m, MonadFail m )
              => FilePath -> m (Map (String,HoleHash) [QuestionAnswered])

getAnswers fp | takeExtension fp  == ".json" = do -- Read from a json file
  liftIO $ fmap A.decode (B.readFile fp) >>= \case
    Just (vs :: [QuestionAnswered])  -> do
        -- putStrLn $ "BeforeToSet: " ++  (groom vs)
        return $ M.fromListWith (++) [ ((aRuleName_ v, qHole_ v) ,[v])  | v <- nub2 vs ]
    Nothing -> userErr $ "Error parsing" <+> pretty fp


getAnswers fp = do
  Model{mInfo=ModelInfo{miQuestionAnswered=vs}} <- readModelFromFile fp
  return $ M.fromListWith (++) [ ((aRuleName_ v, qHole_ v) ,[v])  | v <- nub2 vs ]

saveToLog :: MonadLog m => Doc -> m ()
saveToLog = log LogFollow


jsonToDoc :: ToJSON a => a -> Doc
jsonToDoc  = Pr.text . L.unpack . L.toLazyText . A.encodeToTextBuilder . toJSON


nub2 :: (Ord a, Eq a) => [a] -> [a]
nub2 l = go S.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `S.member` s then go s xs
                                      else x : go (S.insert x s) xs
