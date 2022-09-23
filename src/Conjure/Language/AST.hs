module Conjure.Language.AST where
import Conjure.Language.Lexer 
import Conjure.Language.NewLexer (ETokenStream, ETok, Lexeme)
import Conjure.Prelude (Identity, Maybe)
import Text.Megaparsec
import Data.Void
import Conjure.Language.Definition (Statement, SearchOrder (BranchingOn), FindOrGiven (Given))




data LToken = RealToken ETok | MissingToken ETok

data ProgramTree = ProgramTree

data StatementNode = Declatation DeclatationStatementNode
                    | Branching BranchingStatementNode
                    | SuchThat SuchThatStatementNode
                    | Where WhereStatementNode
                    | Objective ObjectiveStatementNode

data SuchThatStatementNode = SuchThatStatementNode
    LToken -- Such
    LToken -- That
    (Sequence ExpressionNode) -- constraints


data WhereStatementNode = WhereStatementNode
    LToken --where
    (Sequence ExpressionNode) --expresssions

data ObjectiveStatementNode = ObjectiveMin LToken ExpressionNode
                            | ObjectiveMax LToken ExpressionNode

--Declaration statements
data DeclatationStatementNode = 
        GivenStatement GivenStatementNode
    |   LettingStatement LettingStatementNode

data GivenStatementNode = GivenStatementNode 
    LToken -- given
    NameNode -- name
    LToken -- colon



data LettingStatementNode = 
        LettingExpr LToken NameNode LToken ExpressionNode
    |   LettingDomain LToken NameNode LToken LToken DomainNode
    

data GivenEnumNode = GivenEnumNode
    LToken --lGivenEnum
    NameNode --name
    LToken --lNew
    LToken --lType
    LToken --lEnum

data LettingEnumNode = LettingEnumNode
    LToken --lLetting
    NameNode --name
    LToken --lBe
    LToken --lNew
    LToken --lType
    LToken --lEnum
    ListNode NameNode--nameList


data LettingUnnamedNode = LettingAnon
     LToken --lLettingAnon
     NameNode --name
     LToken --lBe
     LToken --lNew
     LToken --lType
     LToken --lOf
     LToken --lSize
     ExpressionNode --expr

--Branching on 

data BranchingStatementNode = BranchingStatementNode 
    LToken
    LToken
    (ListNode BranchingOnNode)


data  BranchingOnNode = BranchingOnName NameNode | BranchingOnExpression ExpressionNode


--Domains 

data DomainNode =   BoolDomainNode LToken
                |   RangedIntDomainNode LToken (ListNode RangeNode)
                |   NamedRangeNode NameNode (ListNode RangeNode)
                |   NamedDomainNode NameNode
                |   TupleDomainNode LToken (ListNode DomainNode)
                |   RecordDomainNode LToken (ListNode NamedDomainNode)
                |   VariantDomainNode LToken (ListNode NamedDomainNode)
                |   MatrixDomainNode LToken LToken LToken (ListNode DomainNode) LToken DomainNode
                |   SetDomainNode LToken (ListNode AttributeNode) LToken DomainNode
                |   MSetDomainNode LToken (ListNode AttributeNode) LToken DomainNode
                |   FunctionDomainNode LToken (ListNode AttributeNode) DomainNode LToken DomainNode
                |   SequenceDomainNode LToken (ListNode AttributeNode) LToken DomainNode
                |   RelationDomainNode LToken (ListNode AttributeNode) LToken (ListNode DomainNode)
                |   PartitionDomainNode LToken (ListNode AttributeNode) LToken DomainNode
                  
                  
data RangeNode =    SingleRangeNode ExpressionNode
                |   RightUnboundedRangeNode ExpressionNode LToken
                |   LeftUnboundedRangeNode LToken ExpressionNode
                |   BoundedRangeNode ExpressionNode LToken ExpressionNode              
data AttributeNode = NamedAttributeNode NameNode | NamedExpressionAttribute NameNode ExpressionNode
data NamedDomainNode  = NameDomainNode NameNode LToken DomainNode 
-- Common Statements


newtype NameNode = NameNode LToken

data ExpressionNode = BinaryOpNode ExpressionNode LToken ExpressionNode
                    | UnaryPrefixOpNode LToken ExpressionNode

data ListNode itemType= ListNode {
    lOpBracket :: LToken,
    items :: Sequence itemType,
    lClBracket :: LToken
}

newtype Sequence itemType = Seq {
    elems :: [SeqElem itemType]
}

data SeqElem itemType = SeqElem {
    item :: itemType,
    separator :: Maybe LToken
}

type Parser = Parsec Void ETokenStream




-- parseProgram :: Parser ProgramNode
-- parseProgram = do
                    

