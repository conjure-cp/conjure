{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Stuff.Generic.Tag where
import Stuff.Pretty
import Data.String ( IsString(..) )
import GHC.Generics ( Generic )
import Data.Serialize ( Serialize(..) )
import Data.Hashable ( Hashable(..) )
import Data.Aeson ( ToJSON(..) )
import qualified Data.Text as T
newtype Tag = Tag T.Text 
    deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)
instance Serialize Tag where
    put (Tag t) = put (T.unpack t)
    get = fmap (Tag . T.pack) get
instance Pretty Tag where
    pretty (Tag t) = pretty t
instance IsString Tag where
    fromString t | t `elem` ["actual","allDiff","apart","append","arg1","args","atTopLevel","attrCollection","attribute","attributes","binOp","body","bool","branchingOn","declaration","defined","dim","dimFind","domain","domainInExpr","dontCare","emptyGuard","enum","expr","factorial","find","flatten","freq","from","fromTo","function","functionApply","given","guard","hist","identity","in","index","indexrange","indices","inner","innerFrom","inners","innerTo","int","inverse","lambda","left","letting","literal","locals","mapping","matrix","max","maximising","metavar","min","minimising","mset","name","nameValue","negate","nestedDimFind","new","next","normIndices","not","nowOrdered","objective","old","open","operator","param","part","participants","partition","parts","party","preImage","quanOverDom","quanOverExpr","quanOverOp","quantified","quantifier","quantifierDecl","quanVar","range","ranges","reference","relation","replace","right","set","single","slicer","statement","statementEOF","structural","subset","subsetEq","suchThat","this","to","together","toInt","toMSet","topLevel","toRelation","toSet","tuple","twoBars","type","typed","typeEnum","typeInt","typeUnnamed","unaryOp","unknown","value","values","where","within","withLocals"] = Tag (T.pack t)
    fromString t = error $ "Unknown tag: " ++ t
