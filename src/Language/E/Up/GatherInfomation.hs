{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.GatherInfomation(
     getVariables
    ,getEssenceVariables
    ,getSolVariables
    ,getEnumMapping
    ,getEnumsAndUnamed
) where

import Language.E
import Language.E.Up.Data
import Language.E.Up.Debug

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S


-- get info about the bounds, names and indexes of the variables.
getVariables :: Spec -> M.Map String VarInfo
getVariables  (Spec _ xs) = M.fromList $ mapMaybe getVariable (statementAsList xs)


getVariable :: E -> Maybe (String, VarInfo)
getVariable  z =
    case  getVariable' z of
      Nothing -> Nothing
      Just (name',domainRep) -> Just (name', VarInfo (getIndexes domainRep) (getBounds domainRep))

-- removed .matrix from check,  ?
getVariable' ::  E -> Maybe (String, [E])
getVariable' [xMatch|  [Prim (S name)] := topLevel.declaration.find.name.reference
                    | f := topLevel.declaration.find.domain
                    | _ := topLevel.declaration.find.domain.domain|] = Just(T.unpack name,f)

getVariable' _ =  Nothing


getBounds ::  [E] -> [Integer]
getBounds e = case  getBounds' e of
                [Tagged "bool" []] -> [0,1]
                f -> getRangeList f
    where
    getBounds' :: [E] -> [E]
    getBounds' [[xMatch| f := domain.matrix.inner |]] = getBounds' f
    getBounds' [[xMatch| f := domain.int |]] = f
    getBounds' [[xMatch| _ := domain.bool |]] = [Tagged "bool" []]
    getBounds' es = errpM "GatherInfomation: getBounds'" es


getIndexes :: [E]  -> [[Integer]]
getIndexes = getIndexes' []
    where
    getIndexes' ::  [[Integer]] -> [E]  -> [[Integer]]
    getIndexes' arr [[xMatch|
         next := domain.matrix.inner
       | r := domain.matrix.index.domain.int |]] = getRangeList r : getIndexes' arr next
    getIndexes' arr _ = arr


getRangeList :: [E] -> [Integer]
getRangeList =  S.toAscList . getRange

getRange :: [E] -> S.Set Integer
getRange [Tagged "ranges" arr] =
    foldr ((\ a s -> (S.union s . S.fromList) a) . getRange') S.empty arr

    where
    getRange' [xMatch| [Prim (I a),Prim (I b)] := range.fromTo.value.literal |] = [a..b]
    getRange' [xMatch| [Prim (I a)] := range.single.value.literal |] = [a]
    getRange' [xMatch| [Prim (I a)] := range.single.unaryOp.negate.value.literal |] = [-a]
    getRange' e = errpM "GatherInfomation: getRange'" [e]

getRange e = errpM "GatherInfomation: getRange" e

getEssenceVariables :: Spec -> M.Map String [TagT]
getEssenceVariables (Spec _ xs) =
    M.fromList $ mapMaybe getEssenceVariable (statementAsList xs)


getEssenceVariable :: E -> Maybe (String, [TagT])
getEssenceVariable [xMatch| arr := topLevel.declaration.find.domain.domain.tuple.inners
                          | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  [TagTuple (map getTags arr)])

getEssenceVariable [xMatch| [Prim (S kind)] := topLevel.declaration.find.domain.domain.enum.name.reference
                          | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  [TagEnum (T.unpack  kind)])

getEssenceVariable [xMatch|  _    := topLevel.declaration.find.domain.domain.function
                          | [ins] := topLevel.declaration.find.domain.domain
                                  .function.innerFrom
                          | [tos] := topLevel.declaration.find.domain.domain
                                  .function.innerTo
                          | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  [TagFunc (getTags ins) (getTags tos)]  )
    --error "d"

getEssenceVariable [xMatch| [Tagged t arr]  := topLevel.declaration.find.domain.domain
                          | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  TagSingle t : concatMap getTags arr )


getEssenceVariable [xMatch| [Prim (S name)] := topLevel.declaration.find.name.reference
                          | [Prim (S kind)] := topLevel.declaration.find.domain.type.enum |] =
   Just (T.unpack name,  [TagEnum (T.unpack  kind)]  )


-- Very silly but works for unmaned types
getEssenceVariable [xMatch| _  := topLevel.declaration.find.domain
                                        .topLevel.letting.typeUnnamed
                            | [Prim (S kind)] := topLevel.declaration.find.domain
                                                 .topLevel.letting.name
                                                 .topLevel.letting.name.reference
                            | [Prim (S name)] := topLevel.declaration.find.name.reference |] =
   Just (T.unpack name,  [TagUnamed (T.unpack kind) ] )


-- getEssenceVariable e = errb [e]
getEssenceVariable _ = Nothing


getTags ::  E -> [TagT]
getTags [xMatch|  _    := domain.function
               | [ins] := domain.function.innerFrom
               | [tos] := domain.function.innerTo |] =
   [TagFunc (getTags ins) (getTags tos)]

getTags [xMatch| arr := domain.tuple.inners |] = [TagTuple (map getTags arr)]
getTags [xMatch| [Tagged t arr] := domain |]   = TagSingle t : concatMap getTags arr
getTags [xMatch| [dom] := inner |]             = getTags dom
getTags _ = []


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
getEnumMapping' [xMatch| es := topLevel.letting.typeEnum.values
                       | [Prim (S name)] := topLevel.letting.name.reference |] =
   Just (T.unpack name,  es  )

getEnumMapping' _ = Nothing


getEnumsAndUnamed :: Spec  -> [E]
getEnumsAndUnamed (Spec _ xs) = filter func  (statementAsList xs)

    where
    func [xMatch| _ := topLevel.letting.typeEnum |]    = True
    func [xMatch| _ := topLevel.letting.typeUnnamed |] = True
    func _ = False

