{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.GatherInfomation(
     getVariables
    ,getEssenceVariables
    ,getSolVariables
    ,getEnumMapping
    ,getEnumsAndUnamed
) where

import Bug
import Language.E
import Language.E.Up.Data
import Language.E.Up.Debug


import qualified Data.Text as T
import qualified Data.Map as M


-- get info about the bounds, names and indexes of the variables.
getVariables :: Spec -> M.Map String VarInfo
getVariables  (Spec _ xs) = M.fromList $ mapMaybe getVariable (statementAsList xs)


getVariable :: E -> Maybe (String, VarInfo)
getVariable z = case getVariable' z of
    Nothing -> Nothing
    Just (name', domainRep) -> Just (name', VarInfo (getIndexes domainRep) (getBounds domainRep))

-- removed .matrix from check,  ?
getVariable' ::  E -> Maybe (String, Domain)
getVariable' [xMatch| [Prim (S name)] := topLevel.declaration.find.name.reference
                    | [D domain]      := topLevel.declaration.find.domain
                    |] = Just(T.unpack name, domain)

getVariable' _ =  Nothing


getBounds :: Domain -> [Integer]
getBounds DomainBool = [0,1]
getBounds (DomainInt rs) = sort $ nub $ concatMap getRangeList rs
getBounds d = bug $ "Language.E.Up.GatherInfomation.getBounds:" <+> pretty d


getIndexes :: Domain -> [[Integer]]
getIndexes = getIndexes' []
    where
    getIndexes' ::  [[Integer]] -> Domain  -> [[Integer]]
    getIndexes' arr (DomainMatrix next (DomainInt [ranges])) = getRangeList ranges : getIndexes' arr next
    getIndexes' arr _ = arr


getRangeList :: Range -> [Integer]
getRangeList (RangeSingle  [xMatch| [Prim (I x)] := value.literal |]) = [x]
getRangeList (RangeBounded [xMatch| [Prim (I x)] := value.literal |]
                           [xMatch| [Prim (I y)] := value.literal |]) = [x..y]
getRangeList r = bug $ "Language.E.Up.GatherInfomation.getRangeList:" <+> pretty r


getEssenceVariables :: M.Map String [E] -> Spec ->  M.Map String [TagT]
getEssenceVariables emap (Spec _ xs) =
    M.fromList $ mapMaybe (getEssenceVariable emap) (statementAsList xs)


getEssenceVariable :: M.Map String [E] -> E -> Maybe (String, [TagT])

getEssenceVariable emap [xMatch| arr := topLevel.declaration.find.domain.domain.tuple.inners
                            | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  [TagTuple (map (getTags emap) arr)])

getEssenceVariable _ [xMatch| [Prim (S kind)] := topLevel.declaration.find.domain.domain.enum.name.reference
                            | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  [TagEnum (T.unpack  kind)])

getEssenceVariable emap [xMatch|  _    := topLevel.declaration.find.domain.domain.function
                            | [ins] := topLevel.declaration.find.domain.domain
                                  .function.innerFrom
                            | [tos] := topLevel.declaration.find.domain.domain
                                  .function.innerTo
                            | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  [TagFunc ((getTags emap) ins) ((getTags emap) tos)] )

getEssenceVariable emap [xMatch|  _    := topLevel.declaration.find.domain.domain.partition
                            | [ins] := topLevel.declaration.find.domain.domain.partition.inner
                            | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  [TagPar ( (getTags emap) ins)] )


getEssenceVariable emap [xMatch| [Prim (S name)] := topLevel.declaration.find.name.reference
                               | [Prim (S ref)]  := topLevel.declaration.find.domain.reference |]
                               | Just _ <- M.lookup (T.unpack ref) emap  =
    Just (T.unpack name, [TagEnum (T.unpack ref)] )

getEssenceVariable emap [xMatch| [Prim (S name)] := topLevel.declaration.find.name.reference
                               | [Prim (S ref)]  := topLevel.declaration.find.domain.reference |]
                               | Just _ <- M.lookup ("__named_" ++  T.unpack ref) emap  =
    Just (T.unpack name, [TagUnamed (T.unpack ref)] )

getEssenceVariable emap [xMatch| arr := topLevel.declaration.find.domain.domain.relation.inners
                            | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  [TagRel (map (getTags emap) arr)])


getEssenceVariable emap [xMatch| [Tagged t arr]  := topLevel.declaration.find.domain.domain
                            | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  TagSingle t : concatMap (getTags emap) arr )

getEssenceVariable _   [xMatch| _ := topLevel.letting     |] = Nothing
getEssenceVariable _   [xMatch| _ := topLevel.given       |] = Nothing
getEssenceVariable _   [xMatch| _ := topLevel.where       |] = Nothing
getEssenceVariable _   [xMatch| _ := topLevel.branchingOn |] = Nothing
getEssenceVariable _ e@[xMatch| _ := topLevel.declaration |] = 
    _bug "getEssenceVariable unhandled declaration" [e]
getEssenceVariable _ e = 
    _bug "getEssenceVariable unhandled" [e]


getTags ::  M.Map String [E] -> E -> [TagT]
getTags emap [xMatch|  _    := domain.function
               | [ins] := domain.function.innerFrom
               | [tos] := domain.function.innerTo |] =
   [TagFunc (getTags emap ins) (getTags emap tos)]

getTags emap [xMatch| [ins] := domain.partition.inner |] =
     [TagPar (getTags emap ins)]

getTags emap [xMatch| arr            := domain.tuple.inners |] = [TagTuple (map (getTags emap) arr)]
getTags emap [xMatch| [dom]          := inner |]               = getTags emap dom
getTags emap [xMatch| [Tagged t arr] := domain |]              = TagSingle t : concatMap (getTags emap) arr

getTags emap (Tagged "reference" [Prim (S name)]) 
    | Just _ <- M.lookup ("__named_" ++  T.unpack name) emap = [TagUnamed (T.unpack name)]
getTags _ (Tagged "reference" [Prim (S name)])            = [TagEnum (T.unpack name)]

getTags _ (Tagged "attributes" _) = []
getTags _ (Tagged "index" _)      = []
getTags _ (Tagged "range" _)      = []
getTags _ (Tagged "ranges" _)     = []
getTags _ (Tagged "inners" _)     = []
getTags _ _                       = []
{-getTags _ e                       = errp [e]-}


getSolVariables :: Spec -> M.Map String [E]
getSolVariables (Spec _ xs) = M.fromList $ mapMaybe getSolVariable  (statementAsList xs)

getSolVariable :: E -> Maybe (String, [E])
getSolVariable [xMatch| e               := topLevel.letting
                      | [Prim (S name)] := topLevel.letting.name.reference |] =
    Just (T.unpack name, e)

getSolVariable _ = Nothing


-- Maps a enums's name to it Definition
getEnumMapping :: Spec -> M.Map String [E]
getEnumMapping  (Spec _ xs) = M.fromList $ mapMaybe getEnumMapping' (statementAsList xs)

getEnumMapping' :: E -> Maybe (String, [E])
getEnumMapping' [xMatch| es              := topLevel.letting.typeEnum.values
                       | [Prim (S name)] := topLevel.letting.name.reference |] =
   Just (T.unpack name,  es  )

getEnumMapping' _ = Nothing


getEnumsAndUnamed :: Spec  -> [E]
getEnumsAndUnamed (Spec _ xs) = filter func  (statementAsList xs)

    where
    func [xMatch| _ := topLevel.letting.typeEnum    |] = True
    func [xMatch| _ := topLevel.letting.typeUnnamed |] = True
    func _ = False


_bug :: String -> [E] -> t
_bug  s = upBug  ("GatherInfomation: " ++ s)
_bugi :: (Show a) => String -> (a, [E]) -> t
_bugi s = upBugi ("GatherInfomation: " ++ s )
_bugg :: String -> t
_bugg s = _bug s []

