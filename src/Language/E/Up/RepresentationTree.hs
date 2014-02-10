module Language.E.Up.RepresentationTree(
    createVarTree
) where

import qualified Data.Map as M

import Language.E
import Language.E.Up.Data
import Language.E.Up.Debug

createVarTree ::  M.Map String a -> [Tree String]
createVarTree varInfo =
    let keys = M.keys varInfo
        names  = M.toAscList $ orgNames keys in
    map toVarTree names


orgNames :: [String] -> M.Map String [[String]]
orgNames arr = orgNames' M.empty $ map (recombine .  splitOn "_") arr

--  list of all Representations 
rep_names :: [String]
rep_names = [
    "Function1D",
    "FunctionAsReln",
    "FunctionAsReln",
    "MSetExplicit",
    "MSetOccurrence",
    "RelationAsSet",
    "RelationIntMatrix2",
    "RelationIntMatrix3",
    "SetExplicit",
    "SetExplicitVarSize",
    "SetExplicitVarSizeWithDefault",
    "SetExplicitVarSizeWithMarker",
    "SetOccurrence",
    "AsReln",
    "Matrix1D",
    "MSetOfSets"
    ]

recombine :: [String] -> [String]
recombine a@(_:x:_) | "tuple" `isPrefixOf` x  = a
recombine (c:x:xs) | x `notElem` rep_names  = (c ++ "_" ++  x) : xs
recombine a  = a

orgNames' :: M.Map String [[String]] -> [[String]] ->  M.Map String [[String]]
orgNames' ma [x:xs]      = M.insertWith (++) x [xs] ma
orgNames' ma ((x:xs):zs) = M.insertWith (++) x [xs] (orgNames' ma zs)

orgNames' ma [] =  ma
orgNames' ma a =  orgNames' ma ( filter (not . null) a)


toVarTree :: (String,[[String]]) -> Tree String
toVarTree (k,vs) = toVarTree' (Branch k []) (k,vs)

toVarTree' :: Tree String -> (String,[[String]]) -> Tree String

toVarTree' _ (k,[]) =   Leaf k
toVarTree' _ (k,[[]]) = Leaf k

toVarTree' tree (_, vs) =
    let org aa = M.toAscList $ orgNames' M.empty aa
        f val = map func  (org val )   in
    if any tupleSplit vs then
        let (tuples,nontuples)  = partition  tupleSplit  vs in
        treeInsert tree $ Tuple (f tuples) : f nontuples
    else
        treeInsert tree (f vs)

    where
        func (a,b) = toVarTree' (Branch a []) (a,b)


treeInsert ::  Tree a -> [Tree a] -> Tree a
treeInsert (Branch val children) trees = Branch val (children ++ trees)
treeInsert (Tuple children) trees      = Tuple (children ++ trees)
treeInsert (Leaf k) ts =  Branch k ts


tupleSplit :: [String] -> Bool
tupleSplit arr  = (not . null) arr && (isPrefixOf "tuple" . head) arr

_bug :: String -> [E] -> t
_bug  s = upBug  ("RepresentationTree " ++ s)
_bugi :: (Show a) => String -> (a, [E]) -> t
_bugi s = upBugi ("RepresentationTree: " ++ s )
_bugg :: String -> t
_bugg s = _bug s []

