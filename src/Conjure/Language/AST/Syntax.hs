{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.AST.Syntax where

import Conjure.Language.NewLexer (ETok)
import Conjure.Prelude




data LToken
    = RealToken [ETok] ETok
    | MissingToken ETok
    | SkippedToken ETok
    deriving (Eq, Ord)

instance Null LToken where
    isMissing (MissingToken _) = True
    isMissing _ = False

instance Show LToken where
    show (RealToken [] x) = show x
    show (RealToken ss x) =  "SKIPPED" ++ show ss ++ show x
    show (MissingToken x) = "MISSING[" ++ show x ++ "]"
    show (SkippedToken x) = "SKIPPED[" ++ show x ++ "]"

data ProgramTree = ProgramTree {
    langVersionInfo :: Maybe LangVersionNode,
    statements :: [StatementNode],
    eofToken :: LToken
} 

data LangVersionNode = LangVersionNode LToken NameNode (Sequence LToken)

instance Show ProgramTree where
    show (ProgramTree lv xs eof) = "ProgramTree \n" ++ 
                                intercalate "\n\n" (map show xs)
                                 ++ "\n\n" ++ show eof

data StatementNode
    = DeclarationStatement DeclarationStatementNode
    | BranchingStatement BranchingStatementNode
    | SuchThatStatement SuchThatStatementNode
    | WhereStatement WhereStatementNode
    | ObjectiveStatement ObjectiveStatementNode
    | HeuristicStatement LToken ExpressionNode
    | UnexpectedToken LToken
    deriving (Show)



data SuchThatStatementNode
    = SuchThatStatementNode
        LToken -- Such
        LToken -- That
        (Sequence ExpressionNode) -- constraints
    deriving (Show)

data WhereStatementNode
    = WhereStatementNode
        LToken -- where
        (Sequence ExpressionNode) -- expresssions
    deriving (Show)

data ObjectiveStatementNode
    = ObjectiveMin LToken ExpressionNode
    | ObjectiveMax LToken ExpressionNode
    deriving (Show)

-- Declaration statements
data DeclarationStatementNode
    = FindStatement LToken (Sequence FindStatementNode)
    | GivenStatement LToken (Sequence GivenStatementNode)
    | LettingStatement LToken (Sequence LettingStatementNode)
    deriving (Show)

data FindStatementNode
    = FindStatementNode
        (Sequence NameNode) -- names
        LToken -- colon
        DomainNode -- domain
    deriving (Show)
instance Null FindStatementNode where
    isMissing (FindStatementNode n l d) = isMissing n && isMissing l && isMissing d
data GivenStatementNode
    = GivenStatementNode
        (Sequence NameNode) -- name
        LToken -- colon
        DomainNode -- domain
    | GivenEnumNode
        (Sequence NameNode)
        LToken -- new
        LToken -- type
        LToken -- enum
    deriving (Show)

instance Null GivenStatementNode where
    isMissing (GivenStatementNode l t d) = isMissing l && isMissing t && isMissing d
    isMissing (GivenEnumNode l a b c) = isMissing l && isMissing a && isMissing b && isMissing c

data LettingStatementNode
    = LettingStatementNode
      (Sequence NameNode)
      LToken --
      LettingAssignmentNode
    deriving (Show)


instance Null LettingStatementNode where
    isMissing (LettingStatementNode l t a) = isMissing l && isMissing t && isMissing a 
data LettingAssignmentNode
    =  LettingExpr
        ExpressionNode
    | LettingDomain
        LToken -- domain
        DomainNode
    | LettingEnum
        LToken -- lNew
        LToken -- lType
        LToken -- lEnum
        (ListNode NameNode) -- nameList
    | LettingAnon
        LToken -- lNew
        LToken -- lType
        LToken -- lOf
        LToken -- lSize
        ExpressionNode -- expr
    deriving (Show)
instance Null LettingAssignmentNode where
    isMissing x = case x of
      LettingExpr en -> isMissing en
      LettingDomain lt dn -> isMissing lt && isMissing dn
      LettingEnum l1 l2 l3 ln -> all isMissing [l1,l2,l3] && isMissing ln
      LettingAnon l1 l2 l3 l4 en ->  all isMissing [l1,l2,l3,l4] && isMissing en


-- Branching on

data BranchingStatementNode
    = BranchingStatementNode
        LToken
        LToken
        (ListNode ExpressionNode)
    deriving (Show)


-- Domains

type MAttributes = Maybe (ListNode AttributeNode)

data DomainNode
    = BoolDomainNode LToken
    | RangedIntDomainNode LToken (Maybe (ListNode RangeNode))
    | RangedEnumNode NameNode (Maybe (ListNode RangeNode))
    -- | EnumDomainNode NameNode
    | MetaVarDomain LToken
    | ShortTupleDomainNode (ListNode DomainNode)
    | TupleDomainNode LToken (ListNode DomainNode)
    | RecordDomainNode LToken (ListNode NamedDomainNode)
    | VariantDomainNode LToken (ListNode NamedDomainNode)
    | MatrixDomainNode LToken (Maybe IndexedByNode) (ListNode DomainNode) LToken DomainNode
    | SetDomainNode LToken MAttributes LToken DomainNode
    | MSetDomainNode LToken MAttributes LToken DomainNode
    | FunctionDomainNode LToken MAttributes DomainNode LToken DomainNode
    | SequenceDomainNode LToken MAttributes LToken DomainNode
    | RelationDomainNode LToken MAttributes LToken (ListNode DomainNode)
    | PartitionDomainNode LToken MAttributes LToken DomainNode
    | MissingDomainNode LToken
    deriving (Show)
instance Null DomainNode where
    isMissing (MissingDomainNode {}) = True
    isMissing _ = False 

data IndexedByNode = IndexedByNode LToken LToken
    deriving (Show)
data RangeNode
    = SingleRangeNode ExpressionNode
    | OpenRangeNode DoubleDotNode
    | RightUnboundedRangeNode ExpressionNode DoubleDotNode
    | LeftUnboundedRangeNode DoubleDotNode ExpressionNode
    | BoundedRangeNode ExpressionNode DoubleDotNode ExpressionNode
    deriving (Show)
instance Null RangeNode where
    isMissing (SingleRangeNode e) = isMissing e
    isMissing _ = False

type DoubleDotNode = LToken
-- data DoubleDotNode = DoubleDotNode LToken LToken deriving (Show)

data AttributeNode 
    = NamedAttributeNode LToken (Maybe ExpressionNode)
    -- | TODO: Add dont care
    deriving (Show)

instance Null AttributeNode where
    isMissing (NamedAttributeNode n m_e) = isMissing n && isMissing m_e 
    -- isMissing (NamedExpressionAttribute n e) = isMissing n && isMissing e

data NamedDomainNode = NameDomainNode NameNode (Maybe (LToken, DomainNode))
    deriving (Show)

instance Null NamedDomainNode where
    isMissing (NameDomainNode (NameNode a) Nothing) = isMissing a
    isMissing (NameDomainNode (NameNode a) (Just (b,c))) = isMissing a && isMissing b && isMissing c
-- Common Statements

newtype NameNode = NameNode LToken
    deriving (Show)

instance Null NameNode where
    isMissing (NameNode e) = isMissing e

-- Expressions
data ExpressionNode
    = Literal LiteralNode
    | IdentifierNode NameNode
    | MetaVarExpr LToken 
    | QuantificationExpr QuantificationExpressionNode
    | OperatorExpressionNode OperatorExpressionNode
    | DomainExpression DomainExpressionNode
    | ParenExpression ParenExpressionNode
    | AbsExpression ParenExpressionNode
    | FunctionalApplicationNode LToken (ListNode ExpressionNode)
    | AttributeAsConstriant LToken (ListNode ExpressionNode)
    | MissingExpressionNode LToken
    | SpecialCase SpecialCaseNode
    deriving (Show)

instance Null ExpressionNode where
    isMissing (MissingExpressionNode _ ) = True
    isMissing _ = False

data SpecialCaseNode = ExprWithDecls LToken ExpressionNode LToken [StatementNode] LToken
    deriving (Show)


data DomainExpressionNode 
    = DomainExpressionNode LToken DomainNode LToken
    deriving (Show) 
data ParenExpressionNode = ParenExpressionNode LToken ExpressionNode LToken
    deriving (Show)

newtype ShortTuple = ShortTuple (ListNode ExpressionNode) deriving (Show)
instance Null ShortTuple where
    isMissing (ShortTuple ls ) = isMissing ls

data LongTuple = LongTuple LToken (ListNode ExpressionNode) deriving (Show)
instance Null LongTuple where
    isMissing (LongTuple s ls) = isMissing s && isMissing ls



-- Literals
data LiteralNode
    = IntLiteral LToken
    | BoolLiteral LToken
    | MatrixLiteral MatrixLiteralNode
    | TupleLiteralNode LongTuple
    | TupleLiteralNodeShort ShortTuple
    | RecordLiteral LToken (ListNode RecordMemberNode)
    | VariantLiteral LToken (ListNode RecordMemberNode) --catch later
    | SetLiteral (ListNode ExpressionNode)
    | MSetLiteral LToken (ListNode ExpressionNode)
    | FunctionLiteral LToken (ListNode ArrowPairNode)
    | SequenceLiteral LToken (ListNode ExpressionNode)
    | RelationLiteral LToken (ListNode RelationElemNode)
    | PartitionLiteral LToken (ListNode PartitionElemNode)
    deriving (Show)

data MatrixLiteralNode
    = MatrixLiteralNode 
        LToken --openBracket
        (Sequence ExpressionNode)
        (Maybe OverDomainNode) -- explicitDomain
        (Maybe ComprehensionNode) --compBody
        LToken --close
    deriving (Show)

data ComprehensionNode 
    = ComprehensionNode 
        LToken -- |
        (Sequence ComprehensionBodyNode)
    deriving (Show) 

data RecordMemberNode = RecordMemberNode NameNode LToken ExpressionNode
    deriving (Show)

instance Null RecordMemberNode where
    isMissing (RecordMemberNode (NameNode s) t e) = isMissing s && isMissing t && isMissing e

data ArrowPairNode = ArrowPairNode ExpressionNode LToken ExpressionNode
    deriving (Show)
instance Null ArrowPairNode where
    isMissing (ArrowPairNode l a b) = isMissing l && isMissing a && isMissing b

data RelationElemNode
    = RelationElemNodeLabeled LongTuple
    | RelationElemNodeShort ShortTuple
    deriving (Show)

instance Null RelationElemNode where
    isMissing (RelationElemNodeLabeled lt) = isMissing lt
    isMissing (RelationElemNodeShort st) = isMissing st

newtype PartitionElemNode = PartitionElemNode (ListNode ExpressionNode)
    deriving (Show)
instance Null PartitionElemNode where
    isMissing (PartitionElemNode l ) = isMissing l

data QuantificationExpressionNode
    = QuantificationExpressionNode
        LToken
        (Sequence AbstractPatternNode)
        QuantificationOverNode
        (Maybe QuanticationGuard)
        LToken --dot
        ExpressionNode
    deriving (Show) -- MAYBE?

data QuantificationOverNode = 
    QuantifiedSubsetOfNode LToken ExpressionNode
    | QuantifiedMemberOfNode LToken ExpressionNode
    | QuantifiedDomainNode OverDomainNode
    deriving (Show)

data OverDomainNode = OverDomainNode LToken DomainNode
    deriving (Show)
data AbstractPatternNode = 
    AbstractIdentifier NameNode
    | AbstractMetaVar LToken
    | AbstractPatternTuple (Maybe LToken) (ListNode AbstractPatternNode)
    | AbstractPatternMatrix (ListNode AbstractPatternNode)
    | AbstractPatternSet (ListNode AbstractPatternNode)
    deriving (Show)

instance Null AbstractPatternNode where
    isMissing (AbstractIdentifier (NameNode s) ) = isMissing s
    isMissing _ = False
data QuanticationGuard = QuanticationGuard LToken ExpressionNode
    deriving (Show)
data QuantificationPattern =
    QuantificationPattern ExpressionNode
    deriving (Show)

data ComprehensionExpressionNode
    = ComprehensionExpressionNode
        LToken
        ExpressionNode
        LToken
        (Sequence ComprehensionBodyNode)
        LToken
    deriving (Show)

data ComprehensionBodyNode
    = CompBodyCondition ExpressionNode
    | CompBodyDomain (Sequence AbstractPatternNode) LToken DomainNode
    | CompBodyGenExpr (Sequence AbstractPatternNode) LToken ExpressionNode
    | CompBodyLettingNode LToken AbstractPatternNode LToken ExpressionNode
    deriving (Show)

instance Null ComprehensionBodyNode where
    isMissing (CompBodyCondition a) = isMissing a
    isMissing (CompBodyDomain a b c) = isMissing a && isMissing b && isMissing c
    isMissing (CompBodyGenExpr s t e) = isMissing s && isMissing t && isMissing e
    isMissing (CompBodyLettingNode t p l e) = isMissing t && isMissing p && isMissing l && isMissing e
data OperatorExpressionNode
    = PostfixOpNode ExpressionNode PostfixOpNode
    | PrefixOpNode LToken ExpressionNode
    | BinaryOpNode ExpressionNode LToken ExpressionNode
    deriving (Show)


data PostfixOpNode
    = IndexedNode (ListNode RangeNode)
    | OpFactorial  LToken
    | ExplicitDomain LToken LToken DomainNode LToken
    | ApplicationNode (ListNode ExpressionNode)
    deriving (Show)

-- data FunctionApplicationNode 
--     = FunctionApplicationNode LToken (ListNode ExpressionNode) 

data IndexerNode
    = Indexer
    deriving (Show)
data ListNode itemType = ListNode
    { lOpBracket :: LToken
    , items :: Sequence itemType
    , lClBracket :: LToken
    }
    deriving (Show)
instance (Null a) => Null (ListNode a) where
    isMissing (ListNode l1 s l2 ) = isMissing l1 && isMissing s && isMissing l2
newtype Sequence itemType = Seq
    { elems :: [SeqElem itemType]
    }
    deriving (Show)

instance (Null a) => Null (SeqElem a) where
    isMissing (SeqElem i Nothing)= isMissing i
    isMissing (SeqElem i x) = isMissing i && isMissing x
    isMissing (MissingSeqElem _ c) = isMissing c

instance (Null a) => Null (Sequence a) where
    isMissing (Seq []) = True
    isMissing (Seq [a]) = isMissing a
    isMissing (Seq _) = False

-- deriving (Show)
-- instance (Show a) => Show (Sequence a) where
--     show (Seq e) = "Seq:\n" ++ intercalate "\n\t" (map show e) ++ "\n"

data SeqElem itemType = SeqElem
    { 
        item :: itemType,
        separator :: Maybe LToken
    }
    | MissingSeqElem LToken LToken
    deriving (Show)

class Null a where
    isMissing :: a -> Bool


instance (Null a) => Null (Maybe a) where
    isMissing Nothing = True
    isMissing (Just s) = isMissing s