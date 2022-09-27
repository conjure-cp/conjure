{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Conjure.UI.TypeScript ( tsDef ) where

import Conjure.Prelude hiding ( (<>) )
import Conjure.Language -- ( Model, Expression )

-- aeson
import Data.Aeson.Types ( defaultOptions )

-- aeson-typescript
import Data.Aeson.TypeScript.TH


deriveTypeScript defaultOptions ''AbstractLiteral 
deriveTypeScript defaultOptions ''AbstractPattern
deriveTypeScript defaultOptions ''AttrName
deriveTypeScript defaultOptions ''BinaryRelationAttr
deriveTypeScript defaultOptions ''BinaryRelationAttrs
deriveTypeScript defaultOptions ''Constant
deriveTypeScript defaultOptions ''Decision
deriveTypeScript defaultOptions ''Declaration
deriveTypeScript defaultOptions ''Domain
deriveTypeScript defaultOptions ''Expression
deriveTypeScript defaultOptions ''FindOrGiven
deriveTypeScript defaultOptions ''FunctionAttr
deriveTypeScript defaultOptions ''Generator
deriveTypeScript defaultOptions ''GeneratorOrCondition
deriveTypeScript defaultOptions ''HasRepresentation
deriveTypeScript defaultOptions ''InBubble
deriveTypeScript defaultOptions ''IntTag
deriveTypeScript defaultOptions ''JectivityAttr
deriveTypeScript defaultOptions ''LanguageVersion
deriveTypeScript defaultOptions ''Model
deriveTypeScript defaultOptions ''ModelInfo
deriveTypeScript defaultOptions ''MSetAttr
deriveTypeScript defaultOptions ''Name
deriveTypeScript defaultOptions ''Objective
deriveTypeScript defaultOptions ''OccurAttr
deriveTypeScript defaultOptions ''Op
deriveTypeScript defaultOptions ''OpActive
deriveTypeScript defaultOptions ''OpAllDiff
deriveTypeScript defaultOptions ''OpAllDiffExcept
deriveTypeScript defaultOptions ''OpAnd
deriveTypeScript defaultOptions ''OpApart
deriveTypeScript defaultOptions ''OpAtLeast
deriveTypeScript defaultOptions ''OpAtMost
deriveTypeScript defaultOptions ''OpAttributeAsConstraint
deriveTypeScript defaultOptions ''OpCatchUndef
deriveTypeScript defaultOptions ''OpDefined
deriveTypeScript defaultOptions ''OpDiv
deriveTypeScript defaultOptions ''OpDontCare
deriveTypeScript defaultOptions ''OpDotLeq
deriveTypeScript defaultOptions ''OpDotLt
deriveTypeScript defaultOptions ''OpEq
deriveTypeScript defaultOptions ''OpFactorial
deriveTypeScript defaultOptions ''OpFlatten
deriveTypeScript defaultOptions ''OpFreq
deriveTypeScript defaultOptions ''OpGCC
deriveTypeScript defaultOptions ''OpGeq
deriveTypeScript defaultOptions ''OpGt
deriveTypeScript defaultOptions ''OpHist
deriveTypeScript defaultOptions ''OpIff
deriveTypeScript defaultOptions ''OpImage
deriveTypeScript defaultOptions ''OpImageSet
deriveTypeScript defaultOptions ''OpImply
deriveTypeScript defaultOptions ''OpIn
deriveTypeScript defaultOptions ''OpIndexing
deriveTypeScript defaultOptions ''OpIntersect
deriveTypeScript defaultOptions ''OpInverse
deriveTypeScript defaultOptions ''OpLeq
deriveTypeScript defaultOptions ''OpLexLeq
deriveTypeScript defaultOptions ''OpLexLt
deriveTypeScript defaultOptions ''OpLt
deriveTypeScript defaultOptions ''OpMakeTable
deriveTypeScript defaultOptions ''OpMax
deriveTypeScript defaultOptions ''OpMin
deriveTypeScript defaultOptions ''OpMinus
deriveTypeScript defaultOptions ''OpMod
deriveTypeScript defaultOptions ''OpNegate
deriveTypeScript defaultOptions ''OpNeq
deriveTypeScript defaultOptions ''OpNot
deriveTypeScript defaultOptions ''OpOr
deriveTypeScript defaultOptions ''OpParticipants
deriveTypeScript defaultOptions ''OpParts
deriveTypeScript defaultOptions ''OpParty
deriveTypeScript defaultOptions ''OpPow
deriveTypeScript defaultOptions ''OpPowerSet
deriveTypeScript defaultOptions ''OpPred
deriveTypeScript defaultOptions ''OpPreImage
deriveTypeScript defaultOptions ''OpProduct
deriveTypeScript defaultOptions ''OpRange
deriveTypeScript defaultOptions ''OpRelationProj
deriveTypeScript defaultOptions ''OpRestrict
deriveTypeScript defaultOptions ''OpSlicing
deriveTypeScript defaultOptions ''OpSubsequence
deriveTypeScript defaultOptions ''OpSubset
deriveTypeScript defaultOptions ''OpSubsetEq
deriveTypeScript defaultOptions ''OpSubstring
deriveTypeScript defaultOptions ''OpSucc
deriveTypeScript defaultOptions ''OpSum
deriveTypeScript defaultOptions ''OpSupset
deriveTypeScript defaultOptions ''OpSupsetEq
deriveTypeScript defaultOptions ''OpTable
deriveTypeScript defaultOptions ''OpTildeLeq
deriveTypeScript defaultOptions ''OpTildeLt
deriveTypeScript defaultOptions ''OpTogether
deriveTypeScript defaultOptions ''OpToInt
deriveTypeScript defaultOptions ''OpToMSet
deriveTypeScript defaultOptions ''OpToRelation
deriveTypeScript defaultOptions ''OpToSet
deriveTypeScript defaultOptions ''OpTransform
deriveTypeScript defaultOptions ''OpTrue
deriveTypeScript defaultOptions ''OpTwoBars
deriveTypeScript defaultOptions ''OpUnion
deriveTypeScript defaultOptions ''OpXor
deriveTypeScript defaultOptions ''PartialityAttr
deriveTypeScript defaultOptions ''PartitionAttr
deriveTypeScript defaultOptions ''Range
deriveTypeScript defaultOptions ''ReferenceTo
deriveTypeScript defaultOptions ''Region
deriveTypeScript defaultOptions ''RelationAttr
deriveTypeScript defaultOptions ''SearchOrder
deriveTypeScript defaultOptions ''SequenceAttr
deriveTypeScript defaultOptions ''SetAttr
deriveTypeScript defaultOptions ''SizeAttr
deriveTypeScript defaultOptions ''Statement
deriveTypeScript defaultOptions ''Strategy
deriveTypeScript defaultOptions ''TrailRewrites
deriveTypeScript defaultOptions ''Tree -- base
deriveTypeScript defaultOptions ''Type



tsDef :: IO ()
tsDef = putStrLn $ formatTSDeclarations $ mconcat
    [ getTypeScriptDeclarations (Proxy :: Proxy AbstractLiteral)
    , getTypeScriptDeclarations (Proxy :: Proxy AbstractPattern)
    , getTypeScriptDeclarations (Proxy :: Proxy AttrName)
    , getTypeScriptDeclarations (Proxy :: Proxy BinaryRelationAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy BinaryRelationAttrs)
    , getTypeScriptDeclarations (Proxy :: Proxy Constant)
    , getTypeScriptDeclarations (Proxy :: Proxy Decision)
    , getTypeScriptDeclarations (Proxy :: Proxy Declaration)
    , getTypeScriptDeclarations (Proxy :: Proxy Domain)
    , getTypeScriptDeclarations (Proxy :: Proxy Expression)
    , getTypeScriptDeclarations (Proxy :: Proxy FindOrGiven)
    , getTypeScriptDeclarations (Proxy :: Proxy FunctionAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy Generator)
    , getTypeScriptDeclarations (Proxy :: Proxy GeneratorOrCondition)
    , getTypeScriptDeclarations (Proxy :: Proxy HasRepresentation)
    , getTypeScriptDeclarations (Proxy :: Proxy InBubble)
    , getTypeScriptDeclarations (Proxy :: Proxy IntTag)
    , getTypeScriptDeclarations (Proxy :: Proxy JectivityAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy LanguageVersion)
    , getTypeScriptDeclarations (Proxy :: Proxy Model)
    , getTypeScriptDeclarations (Proxy :: Proxy ModelInfo)
    , getTypeScriptDeclarations (Proxy :: Proxy MSetAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy Name)
    , getTypeScriptDeclarations (Proxy :: Proxy Objective)
    , getTypeScriptDeclarations (Proxy :: Proxy OccurAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy Op)
    , getTypeScriptDeclarations (Proxy :: Proxy OpActive)
    , getTypeScriptDeclarations (Proxy :: Proxy OpAllDiff)
    , getTypeScriptDeclarations (Proxy :: Proxy OpAllDiffExcept)
    , getTypeScriptDeclarations (Proxy :: Proxy OpAnd)
    , getTypeScriptDeclarations (Proxy :: Proxy OpApart)
    , getTypeScriptDeclarations (Proxy :: Proxy OpAtLeast)
    , getTypeScriptDeclarations (Proxy :: Proxy OpAtMost)
    , getTypeScriptDeclarations (Proxy :: Proxy OpAttributeAsConstraint)
    , getTypeScriptDeclarations (Proxy :: Proxy OpCatchUndef)
    , getTypeScriptDeclarations (Proxy :: Proxy OpDefined)
    , getTypeScriptDeclarations (Proxy :: Proxy OpDiv)
    , getTypeScriptDeclarations (Proxy :: Proxy OpDontCare)
    , getTypeScriptDeclarations (Proxy :: Proxy OpDotLeq)
    , getTypeScriptDeclarations (Proxy :: Proxy OpDotLt)
    , getTypeScriptDeclarations (Proxy :: Proxy OpEq)
    , getTypeScriptDeclarations (Proxy :: Proxy OpFactorial)
    , getTypeScriptDeclarations (Proxy :: Proxy OpFlatten)
    , getTypeScriptDeclarations (Proxy :: Proxy OpFreq)
    , getTypeScriptDeclarations (Proxy :: Proxy OpGCC)
    , getTypeScriptDeclarations (Proxy :: Proxy OpGeq)
    , getTypeScriptDeclarations (Proxy :: Proxy OpGt)
    , getTypeScriptDeclarations (Proxy :: Proxy OpHist)
    , getTypeScriptDeclarations (Proxy :: Proxy OpIff)
    , getTypeScriptDeclarations (Proxy :: Proxy OpImage)
    , getTypeScriptDeclarations (Proxy :: Proxy OpImageSet)
    , getTypeScriptDeclarations (Proxy :: Proxy OpImply)
    , getTypeScriptDeclarations (Proxy :: Proxy OpIn)
    , getTypeScriptDeclarations (Proxy :: Proxy OpIndexing)
    , getTypeScriptDeclarations (Proxy :: Proxy OpIntersect)
    , getTypeScriptDeclarations (Proxy :: Proxy OpInverse)
    , getTypeScriptDeclarations (Proxy :: Proxy OpLeq)
    , getTypeScriptDeclarations (Proxy :: Proxy OpLexLeq)
    , getTypeScriptDeclarations (Proxy :: Proxy OpLexLt)
    , getTypeScriptDeclarations (Proxy :: Proxy OpLt)
    , getTypeScriptDeclarations (Proxy :: Proxy OpMax)
    , getTypeScriptDeclarations (Proxy :: Proxy OpMin)
    , getTypeScriptDeclarations (Proxy :: Proxy OpMinus)
    , getTypeScriptDeclarations (Proxy :: Proxy OpMod)
    , getTypeScriptDeclarations (Proxy :: Proxy OpNegate)
    , getTypeScriptDeclarations (Proxy :: Proxy OpNeq)
    , getTypeScriptDeclarations (Proxy :: Proxy OpNot)
    , getTypeScriptDeclarations (Proxy :: Proxy OpOr)
    , getTypeScriptDeclarations (Proxy :: Proxy OpParticipants)
    , getTypeScriptDeclarations (Proxy :: Proxy OpParts)
    , getTypeScriptDeclarations (Proxy :: Proxy OpParty)
    , getTypeScriptDeclarations (Proxy :: Proxy OpPow)
    , getTypeScriptDeclarations (Proxy :: Proxy OpPowerSet)
    , getTypeScriptDeclarations (Proxy :: Proxy OpPred)
    , getTypeScriptDeclarations (Proxy :: Proxy OpPreImage)
    , getTypeScriptDeclarations (Proxy :: Proxy OpProduct)
    , getTypeScriptDeclarations (Proxy :: Proxy OpRange)
    , getTypeScriptDeclarations (Proxy :: Proxy OpRelationProj)
    , getTypeScriptDeclarations (Proxy :: Proxy OpRestrict)
    , getTypeScriptDeclarations (Proxy :: Proxy OpSlicing)
    , getTypeScriptDeclarations (Proxy :: Proxy OpSubsequence)
    , getTypeScriptDeclarations (Proxy :: Proxy OpSubset)
    , getTypeScriptDeclarations (Proxy :: Proxy OpSubsetEq)
    , getTypeScriptDeclarations (Proxy :: Proxy OpSubstring)
    , getTypeScriptDeclarations (Proxy :: Proxy OpSucc)
    , getTypeScriptDeclarations (Proxy :: Proxy OpSum)
    , getTypeScriptDeclarations (Proxy :: Proxy OpSupset)
    , getTypeScriptDeclarations (Proxy :: Proxy OpSupsetEq)
    , getTypeScriptDeclarations (Proxy :: Proxy OpTable)
    , getTypeScriptDeclarations (Proxy :: Proxy OpTildeLeq)
    , getTypeScriptDeclarations (Proxy :: Proxy OpTildeLt)
    , getTypeScriptDeclarations (Proxy :: Proxy OpTogether)
    , getTypeScriptDeclarations (Proxy :: Proxy OpToInt)
    , getTypeScriptDeclarations (Proxy :: Proxy OpToMSet)
    , getTypeScriptDeclarations (Proxy :: Proxy OpToRelation)
    , getTypeScriptDeclarations (Proxy :: Proxy OpToSet)
    , getTypeScriptDeclarations (Proxy :: Proxy OpTransform)
    , getTypeScriptDeclarations (Proxy :: Proxy OpTrue)
    , getTypeScriptDeclarations (Proxy :: Proxy OpTwoBars)
    , getTypeScriptDeclarations (Proxy :: Proxy OpUnion)
    , getTypeScriptDeclarations (Proxy :: Proxy OpXor)
    , getTypeScriptDeclarations (Proxy :: Proxy PartialityAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy PartitionAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy Range)
    , getTypeScriptDeclarations (Proxy :: Proxy ReferenceTo)
    , getTypeScriptDeclarations (Proxy :: Proxy Region)
    , getTypeScriptDeclarations (Proxy :: Proxy RelationAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy SearchOrder)
    , getTypeScriptDeclarations (Proxy :: Proxy SequenceAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy SetAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy SizeAttr)
    , getTypeScriptDeclarations (Proxy :: Proxy Statement)
    , getTypeScriptDeclarations (Proxy :: Proxy Strategy)
    , getTypeScriptDeclarations (Proxy :: Proxy TrailRewrites)
    , getTypeScriptDeclarations (Proxy :: Proxy Tree)
    , getTypeScriptDeclarations (Proxy :: Proxy Type)
    ]
