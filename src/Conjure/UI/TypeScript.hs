{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}


module Conjure.UI.TypeScript ( tsDef ) where


import Conjure.Prelude hiding ( (<>) )
import Conjure.Language -- ( Model, Expression )

-- aeson
import Data.Aeson.Types ( defaultOptions )

-- import Conjure.UI.TypeScriptDefs (numDefs)
import Data.Traversable
-- aeson-typescript
import Data.Aeson.TypeScript.TH as TS
import qualified Data.Aeson.TypeScript.TH as TS
import qualified Data.Aeson.TypeScript.Recursive as TS





-- instance (TypeScript a,TypeScript b) => TypeScript (Domain b a) where
--   getTypeScriptType :: Proxy (Domain b a) -> String
--   getTypeScriptType _ = TS.getTypeScriptType (Proxy :: Proxy (Domain () a))
-- instance TypeScript Expression where
--   getTypeScriptType :: Proxy Expression -> String
--   getTypeScriptType _ = TS.getTypeScriptType (Proxy :: Proxy Expression)
-- deriveTypeScript defaultOptions ''AttrName
-- deriveTypeScript defaultOptions ''Name
-- deriveTypeScript defaultOptions ''OpActive
-- deriveTypeScript defaultOptions ''OpAllDiff
-- deriveTypeScript defaultOptions ''OpAllDiffExcept
-- deriveTypeScript defaultOptions ''OpAnd
-- deriveTypeScript defaultOptions ''OpApart
-- deriveTypeScript defaultOptions ''OpAtLeast
-- deriveTypeScript defaultOptions ''OpAtMost
-- deriveTypeScript defaultOptions ''OpAttributeAsConstraint
-- deriveTypeScript defaultOptions ''OpCatchUndef
-- deriveTypeScript defaultOptions ''OpDefined
-- deriveTypeScript defaultOptions ''OpDiv
-- deriveTypeScript defaultOptions ''OpDontCare
-- deriveTypeScript defaultOptions ''OpDotLeq
-- deriveTypeScript defaultOptions ''OpDotLt
-- deriveTypeScript defaultOptions ''OpEq
-- deriveTypeScript defaultOptions ''OpFactorial
-- deriveTypeScript defaultOptions ''OpFlatten
-- deriveTypeScript defaultOptions ''OpFreq
-- deriveTypeScript defaultOptions ''OpGCC
-- deriveTypeScript defaultOptions ''OpGeq
-- deriveTypeScript defaultOptions ''OpGt
-- deriveTypeScript defaultOptions ''OpHist
-- deriveTypeScript defaultOptions ''OpIff
-- deriveTypeScript defaultOptions ''OpImage
-- deriveTypeScript defaultOptions ''OpImageSet
-- deriveTypeScript defaultOptions ''OpImply
-- deriveTypeScript defaultOptions ''OpIn
-- deriveTypeScript defaultOptions ''OpIndexing
-- deriveTypeScript defaultOptions ''OpIntersect
-- deriveTypeScript defaultOptions ''OpInverse
-- deriveTypeScript defaultOptions ''OpLeq
-- deriveTypeScript defaultOptions ''OpLexLeq
-- deriveTypeScript defaultOptions ''OpLexLt
-- deriveTypeScript defaultOptions ''OpLt
-- deriveTypeScript defaultOptions ''OpMakeTable
-- deriveTypeScript defaultOptions ''OpMax
-- deriveTypeScript defaultOptions ''OpMin
-- deriveTypeScript defaultOptions ''OpMinus
-- deriveTypeScript defaultOptions ''OpMod
-- deriveTypeScript defaultOptions ''OpNegate
-- deriveTypeScript defaultOptions ''OpNeq
-- deriveTypeScript defaultOptions ''OpNot
-- deriveTypeScript defaultOptions ''OpOr
-- deriveTypeScript defaultOptions ''OpParticipants
-- deriveTypeScript defaultOptions ''OpParts
-- deriveTypeScript defaultOptions ''OpParty
-- deriveTypeScript defaultOptions ''OpPow
-- deriveTypeScript defaultOptions ''OpPowerSet
-- deriveTypeScript defaultOptions ''OpPred
-- deriveTypeScript defaultOptions ''OpPreImage
-- deriveTypeScript defaultOptions ''OpProduct
-- deriveTypeScript defaultOptions ''OpRange
-- deriveTypeScript defaultOptions ''OpRelationProj
-- deriveTypeScript defaultOptions ''OpRestrict
-- deriveTypeScript defaultOptions ''OpSlicing
-- deriveTypeScript defaultOptions ''OpSubsequence
-- deriveTypeScript defaultOptions ''OpSubset
-- deriveTypeScript defaultOptions ''OpSubsetEq
-- deriveTypeScript defaultOptions ''OpSubstring
-- deriveTypeScript defaultOptions ''OpSucc
-- deriveTypeScript defaultOptions ''OpSum
-- deriveTypeScript defaultOptions ''OpSupset
-- deriveTypeScript defaultOptions ''OpSupsetEq
-- deriveTypeScript defaultOptions ''OpTable
-- deriveTypeScript defaultOptions ''OpTildeLeq
-- deriveTypeScript defaultOptions ''OpTildeLt
-- deriveTypeScript defaultOptions ''OpTogether
-- deriveTypeScript defaultOptions ''OpToInt
-- deriveTypeScript defaultOptions ''OpToMSet
-- deriveTypeScript defaultOptions ''OpToRelation
-- deriveTypeScript defaultOptions ''OpToSet
-- deriveTypeScript defaultOptions ''OpTransform
-- deriveTypeScript defaultOptions ''OpTrue
-- deriveTypeScript defaultOptions ''OpTwoBars
-- deriveTypeScript defaultOptions ''OpUnion
-- deriveTypeScript defaultOptions ''OpXor
-- deriveTypeScript defaultOptions ''Op





-- deriveTypeScript defaultOptions ''AbstractPattern

-- deriveTypeScript defaultOptions ''BinaryRelationAttr
-- deriveTypeScript defaultOptions ''BinaryRelationAttrs
-- deriveTypeScript defaultOptions ''IntTag
-- deriveTypeScript defaultOptions ''AbstractLiteral
-- deriveTypeScript defaultOptions ''Type

-- deriveTypeScript defaultOptions ''Constant
-- deriveTypeScript defaultOptions ''Decision
-- deriveTypeScript defaultOptions ''FindOrGiven
-- deriveTypeScript defaultOptions ''Declaration
-- deriveTypeScript defaultOptions ''HasRepresentation
-- deriveTypeScript defaultOptions ''Generator
-- deriveTypeScript defaultOptions ''GeneratorOrCondition
-- deriveTypeScript defaultOptions ''SearchOrder
-- deriveTypeScript defaultOptions ''Objective
-- deriveTypeScript defaultOptions ''Statement
-- deriveTypeScript defaultOptions ''InBubble
-- deriveTypeScript defaultOptions ''LanguageVersion

-- deriveTypeScript defaultOptions ''JectivityAttr
-- deriveTypeScript defaultOptions ''OccurAttr
-- deriveTypeScript defaultOptions ''SizeAttr
-- deriveTypeScript defaultOptions ''PartialityAttr

-- deriveTypeScript defaultOptions ''Region
-- deriveTypeScript defaultOptions ''ReferenceTo
-- -- deriveTypeScript defaultOptions ''Tree -- base
-- deriveTypeScript defaultOptions ''Strategy
-- -- deriveTypeScript defaultOptions ''Expression
-- deriveTypeScript defaultOptions ''MSetAttr
-- deriveTypeScript defaultOptions ''PartitionAttr
-- deriveTypeScript defaultOptions ''Range
-- deriveTypeScript defaultOptions ''FunctionAttr
-- deriveTypeScript defaultOptions ''RelationAttr
-- deriveTypeScript defaultOptions ''SequenceAttr
-- deriveTypeScript defaultOptions ''SetAttr
-- deriveTypeScript defaultOptions ''TrailRewrites

-- deriveTypeScript defaultOptions ''ModelInfo
-- deriveTypeScript defaultOptions ''Model

-- instance TypeScript a =>  TypeScript (Tree a)  where
--   getTypeScriptType :: TypeScript a => Proxy (Tree a) -> String
--   getTypeScriptType _ = TS.getTypeScriptType (Proxy :: Proxy (Tree a)) 

-- -- $( mconcat 
-- --     <$> traverse
-- --         (deriveTypeScript defaultOptions)
-- --         [''AbstractLiteral
-- --         ,''AbstractPattern
-- --         ,''AttrName
-- --         ,''BinaryRelationAttr
-- --         ,''BinaryRelationAttrs
-- --         ,''Constant
-- --         ,''Decision
-- --         ,''Declaration
-- --         ,''Domain
-- --         ,''Expression
-- --         ,''FindOrGiven
-- --         ,''FunctionAttr
-- --         ,''Generator
-- --         ,''GeneratorOrCondition
-- --         ,''HasRepresentation
-- --         ,''InBubble
-- --         ,''IntTag
-- --         ,''JectivityAttr
-- --         ,''LanguageVersion
-- --         ,''Model
-- --         ,''ModelInfo
-- --         ,''MSetAttr
-- --         ,''Name
-- --         ,''Objective
-- --         ,''OccurAttr
-- --         ,''Op
-- --         ,''OpActive
-- --         ,''OpAllDiff
-- --         ,''OpAllDiffExcept
-- --         ,''OpAnd
-- --         ,''OpApart
-- --         ,''OpAtLeast
-- --         ,''OpAtMost
-- --         ,''OpAttributeAsConstraint
-- --         ,''OpCatchUndef
-- --         ,''OpDefined
-- --         ,''OpDiv
-- --         ,''OpDontCare
-- --         ,''OpDotLeq
-- --         ,''OpDotLt
-- --         ,''OpEq
-- --         ,''OpFactorial
-- --         ,''OpFlatten
-- --         ,''OpFreq
-- --         ,''OpGCC
-- --         ,''OpGeq
-- --         ,''OpGt
-- --         ,''OpHist
-- --         ,''OpIff
-- --         ,''OpImage
-- --         ,''OpImageSet
-- --         ,''OpImply
-- --         ,''OpIn
-- --         ,''OpIndexing
-- --         ,''OpIntersect
-- --         ,''OpInverse
-- --         ,''OpLeq
-- --         ,''OpLexLeq
-- --         ,''OpLexLt
-- --         ,''OpLt
-- --         ,''OpMakeTable
-- --         ,''OpMax
-- --         ,''OpMin
-- --         ,''OpMinus
-- --         ,''OpMod
-- --         ,''OpNegate
-- --         ,''OpNeq
-- --         ,''OpNot
-- --         ,''OpOr
-- --         ,''OpParticipants
-- --         ,''OpParts
-- --         ,''OpParty
-- --         ,''OpPow
-- --         ,''OpPowerSet
-- --         ,''OpPred
-- --         ,''OpPreImage
-- --         ,''OpProduct
-- --         ,''OpRange
-- --         ,''OpRelationProj
-- --         ,''OpRestrict
-- --         ,''OpSlicing
-- --         ,''OpSubsequence
-- --         ,''OpSubset
-- --         ,''OpSubsetEq
-- --         ,''OpSubstring
-- --         ,''OpSucc
-- --         ,''OpSum
-- --         ,''OpSupset
-- --         ,''OpSupsetEq
-- --         ,''OpTable
-- --         ,''OpTildeLeq
-- --         ,''OpTildeLt
-- --         ,''OpTogether
-- --         ,''OpToInt
-- --         ,''OpToMSet
-- --         ,''OpToRelation
-- --         ,''OpToSet
-- --         ,''OpTransform
-- --         ,''OpTrue
-- --         ,''OpTwoBars
-- --         ,''OpUnion
-- --         ,''OpXor
-- --         ,''PartialityAttr
-- --         ,''PartitionAttr
-- --         ,''Range
-- --         ,''ReferenceTo
-- --         ,''Region
-- --         ,''RelationAttr
-- --         ,''SearchOrder
-- --         ,''SequenceAttr
-- --         ,''SetAttr
-- --         ,''SizeAttr
-- --         ,''Statement
-- --         ,''Strategy
-- --         ,''TrailRewrites
-- --         ,''Tree 
-- --         ,''Type
-- --         ]
-- --     )

-- qq :: [TSDeclaration]
-- qq = getTypeScriptDeclarations (Proxy :: Proxy (AbstractLiteral Expression))

-- tsDef :: IO ()
-- tsDef = putStrLn $ formatTSDeclarations $ mconcat
--     [ getTypeScriptDeclarations (Proxy :: Proxy (AbstractLiteral Expression))
--     , getTypeScriptDeclarations (Proxy :: Proxy AbstractPattern)
--     , getTypeScriptDeclarations (Proxy :: Proxy AttrName)
--     , getTypeScriptDeclarations (Proxy :: Proxy BinaryRelationAttr)
--     , getTypeScriptDeclarations (Proxy :: Proxy BinaryRelationAttrs)
--     , getTypeScriptDeclarations (Proxy :: Proxy Constant)
--     , getTypeScriptDeclarations (Proxy :: Proxy Decision)
--     , getTypeScriptDeclarations (Proxy :: Proxy Declaration)
--     , getTypeScriptDeclarations (Proxy :: Proxy (Domain () Expression))
--     , getTypeScriptDeclarations (Proxy :: Proxy Expression)
--     , getTypeScriptDeclarations (Proxy :: Proxy FindOrGiven)
--     , getTypeScriptDeclarations (Proxy :: Proxy (FunctionAttr Expression))
--     , getTypeScriptDeclarations (Proxy :: Proxy Generator)
--     , getTypeScriptDeclarations (Proxy :: Proxy GeneratorOrCondition)
--     , getTypeScriptDeclarations (Proxy :: Proxy HasRepresentation)
--     , getTypeScriptDeclarations (Proxy :: Proxy InBubble)
--     , getTypeScriptDeclarations (Proxy :: Proxy IntTag)
--     , getTypeScriptDeclarations (Proxy :: Proxy JectivityAttr)
--     , getTypeScriptDeclarations (Proxy :: Proxy LanguageVersion)
--     , getTypeScriptDeclarations (Proxy :: Proxy Model)
--     , getTypeScriptDeclarations (Proxy :: Proxy ModelInfo)
--     , getTypeScriptDeclarations (Proxy :: Proxy (MSetAttr Expression))
--     , getTypeScriptDeclarations (Proxy :: Proxy Name)
--     , getTypeScriptDeclarations (Proxy :: Proxy Objective)
--     , getTypeScriptDeclarations (Proxy :: Proxy (OccurAttr Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (Op Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpActive Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpAllDiff Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpAllDiffExcept Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpAnd Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpApart Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpAtLeast Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpAtMost Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpAttributeAsConstraint Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpCatchUndef Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpDefined Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpDiv Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpDontCare Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpDotLeq Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpDotLt Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpEq Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpFactorial Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpFlatten Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpFreq Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpGCC Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpGeq Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpGt Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpHist Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpIff Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpImage Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpImageSet Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpImply Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpIn Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpIndexing Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpIntersect Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpInverse Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpLeq Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpLexLeq Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpLexLt Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpLt Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpMax Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpMin Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpMinus Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpMod Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpNegate Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpNeq Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpNot Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpOr Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpParticipants Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpParts Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpParty Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpPow Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpPowerSet Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpPred Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpPreImage Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpProduct Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpRange Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpRelationProj Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpRestrict Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpSlicing Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpSubsequence Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpSubset Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpSubsetEq Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpSubstring Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpSucc Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpSum Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpSupset Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpSupsetEq Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpTable Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpTildeLeq Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpTildeLt Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpTogether Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpToInt Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpToMSet Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpToRelation Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpToSet Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpTransform Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpTrue Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpTwoBars Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpUnion Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy (OpXor Expression ))
--     , getTypeScriptDeclarations (Proxy :: Proxy PartialityAttr)
--     , getTypeScriptDeclarations (Proxy :: Proxy (PartitionAttr Type))
--     , getTypeScriptDeclarations (Proxy :: Proxy (Range Type))
--     , getTypeScriptDeclarations (Proxy :: Proxy ReferenceTo)
--     , getTypeScriptDeclarations (Proxy :: Proxy Region)
--     , getTypeScriptDeclarations (Proxy :: Proxy (RelationAttr Type))
--     , getTypeScriptDeclarations (Proxy :: Proxy SearchOrder)
--     , getTypeScriptDeclarations (Proxy :: Proxy (SequenceAttr Type))
--     , getTypeScriptDeclarations (Proxy :: Proxy (SetAttr Type))
--     , getTypeScriptDeclarations (Proxy :: Proxy (SizeAttr Type))
--     , getTypeScriptDeclarations (Proxy :: Proxy Statement)
--     , getTypeScriptDeclarations (Proxy :: Proxy Strategy)
--     , getTypeScriptDeclarations (Proxy :: Proxy TrailRewrites)
--     , getTypeScriptDeclarations (Proxy :: Proxy Tree)
--     , getTypeScriptDeclarations (Proxy :: Proxy Type)
--     ]

tsDef :: IO ()
tsDef = putStrLn "<<TS-DEFS-BROKEN>>"