{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Stuff.Generic.Tag where
import Conjure.Prelude
import Conjure.Language.Pretty
import Data.Serialize ( Serialize(..) )
import qualified Data.Text as T
import Test.QuickCheck ( Arbitrary(..), oneof )
allTags :: [String]
allTags = ["actual","allDiff","apart","append","arg1","args","atTopLevel","attrCollection","attribute","attributes","binOp","body","bool","branchingOn","declaration","defined","dim","dimFind","domain","domainInExpr","dontCare","emptyGuard","enum","expr","factorial","find","flatten","freq","from","fromTo","function","functionApply","given","guard","hist","identity","in","index","indexrange","indices","inner","innerFrom","inners","innerTo","int","inverse","lambda","left","letting","literal","locals","mapping","matrix","max","maximising","metavar","min","minimising","mset","name","nameValue","negate","nestedDimFind","new","normIndices","not","nowOrdered","objective","old","open","operator","param","part","participants","partition","parts","party","preImage","quanOverDom","quanOverExpr","quanOverOp","quantified","quantifier","quantifierDecl","quanVar","range","ranges","reference","relation","replace","right","set","single","slicer","structural","subset","subsetEq","suchThat","to","together","toInt","toMSet","topLevel","toRelation","toSet","tuple","twoBars","type","typed","typeEnum","typeInt","typeUnnamed","unaryOp","unknown","value","values","where","within","withLocals","matrixComprehension","generators","generator"]
newtype Tag = Tag T.Text 
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Hashable, ToJSON)
instance Serialize Tag where
    put (Tag t) = put (T.unpack t)
    get = fmap (Tag . T.pack) get
instance Pretty Tag where
    pretty (Tag t) = pretty t
instance IsString Tag where
    fromString t | t `elem` allTags = Tag (T.pack t)
    fromString t = error $ "Unknown tag: " ++ t
instance Arbitrary Tag where
    arbitrary = fmap (Tag . T.pack) $ oneof $ map return allTags
