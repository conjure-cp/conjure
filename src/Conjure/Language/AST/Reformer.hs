module Conjure.Language.AST.Reformer (Flattenable(..)) where

import Conjure.Language.AST.Syntax
import Conjure.Language.NewLexer (ETok (..))
import Conjure.Prelude
import Data.Sequence ((><))
import qualified Data.Sequence as S




class Flattenable a where
    flatten :: Flattenable a => a -> S.Seq ETok

instance Flattenable (S.Seq ETok) where
    flatten = id
instance Flattenable ProgramTree where
    flatten (ProgramTree lv sts end) = mconcat [flatten lv , mconcat $ map flatten sts , flatten end]

instance Flattenable LangVersionNode where
    flatten (LangVersionNode l1 l2 l3) = flatten l1 >< flatten l2 >< flatten l3
instance Flattenable StatementNode where
    flatten x = case x of
        DeclarationStatement dsn -> flatten dsn
        BranchingStatement bsn -> flatten bsn
        SuchThatStatement stsn -> flatten stsn
        WhereStatement wsn -> flatten wsn
        ObjectiveStatement osn -> flatten osn
        HeuristicStatement l1 ex -> flatten l1 >< flatten ex
        UnexpectedToken tok -> flatten tok

instance Flattenable DeclarationStatementNode where
    flatten x = case x of
        FindStatement f fsn -> flatten f >< flatten fsn
        GivenStatement g gsn -> flatten g >< flatten gsn
        LettingStatement t lsn -> flatten t >< flatten lsn


instance Flattenable LettingStatementNode where 
    flatten (LettingStatementNode a b c) = mconcat[ flatten a, flatten b, flatten c]

instance Flattenable LettingAssignmentNode where
    flatten x = case x of
        LettingExpr d ->  flatten d
        LettingDomain d e -> flatten d >< flatten e
        LettingEnum d e f g -> mconcat [flatten d, flatten e, flatten f, flatten g]
        LettingAnon d e f g h -> mconcat [flatten d, flatten e, flatten f, flatten g, flatten h]

instance Flattenable FindStatementNode where
    flatten (FindStatementNode a b c) = mconcat  [flatten a, flatten b, flatten c]

instance Flattenable GivenStatementNode where
    flatten x = case x of
        GivenStatementNode a b c -> mconcat [flatten a, flatten b, flatten c]
        GivenEnumNode a b c d -> mconcat [flatten a, flatten b, flatten c, flatten d]



instance Flattenable BranchingStatementNode where
    flatten (BranchingStatementNode lt lt' ln) = mconcat [flatten lt, flatten lt', flatten ln]


instance Flattenable SuchThatStatementNode where
    flatten (SuchThatStatementNode l1 l2 l3) =  flatten l1 >< flatten l2 >< flatten l3
instance Flattenable WhereStatementNode where
    flatten (WhereStatementNode l1 l2) =  flatten l1 >< flatten l2
instance Flattenable ObjectiveStatementNode where
    flatten x =  case x of
        ObjectiveMin lt en -> flatten lt >< flatten en
        ObjectiveMax lt en -> flatten lt >< flatten en

instance Flattenable LToken where
    flatten x =  case x of
        RealToken st -> flatten st
        MissingToken et ->  flatten et
        SkippedToken et ->  flatten et

instance Flattenable SToken where
    flatten (StrictToken xs e) = flatten xs >< flatten e
instance Flattenable ETok where
    flatten = pure

instance Flattenable ExpressionNode where
    flatten x =  case x of
        Literal ln -> flatten ln
        IdentifierNode nn -> flatten nn
        MetaVarExpr tk -> flatten tk
        QuantificationExpr qen -> flatten qen
        OperatorExpressionNode oen -> flatten oen
        ParenExpression pen ->flatten pen
        AbsExpression pen ->  flatten pen
        DomainExpression dex -> flatten dex
        FunctionalApplicationNode lt ln ->  flatten lt >< flatten ln
        SpecialCase nd -> flatten nd
        AttributeAsConstriant l1 exprs -> flatten l1 >< flatten exprs
        MissingExpressionNode e ->  flatten e

instance Flattenable SpecialCaseNode where 
    flatten x = case x of 
        ExprWithDecls l1 en l2 sns l3 -> mconcat [flatten l1,flatten en,flatten l2, flatten sns , flatten l3]

instance Flattenable DomainExpressionNode where
    flatten (DomainExpressionNode a b c) = flatten a >< flatten b >< flatten c
instance Flattenable QuantificationExpressionNode where
    flatten (QuantificationExpressionNode a b c d e f) = mconcat [
        flatten a, flatten b, flatten c, flatten d, flatten e, flatten f]
instance Flattenable QuantificationOverNode where
    flatten x = case x of
      QuantifiedSubsetOfNode a b -> flatten a >< flatten b
      QuantifiedMemberOfNode a b -> flatten a >< flatten b
      QuantifiedDomainNode a -> flatten a

instance Flattenable OverDomainNode where
    flatten (OverDomainNode a b) = flatten a >< flatten b

instance Flattenable QuanticationGuard where
    flatten (QuanticationGuard a b ) = flatten a >< flatten b

instance Flattenable AbstractPatternNode where
    flatten x = case x of
      AbstractIdentifier nn -> flatten nn
      AbstractMetaVar lt -> flatten lt
      AbstractPatternTuple a b -> flatten a >< flatten b
      AbstractPatternMatrix ln -> flatten ln
      AbstractPatternSet ln -> flatten ln
instance Flattenable QuantificationPattern where
    flatten (QuantificationPattern en) = flatten en

instance Flattenable LiteralNode where
    flatten x = case x of
        IntLiteral lt -> flatten lt
        BoolLiteral lt -> flatten lt
        MatrixLiteral mln -> flatten mln
        TupleLiteralNode lt -> flatten lt
        TupleLiteralNodeShort st -> flatten st
        RecordLiteral lt ln -> flatten lt >< flatten ln
        VariantLiteral lt ln -> flatten lt >< flatten ln
        SetLiteral ln -> flatten ln
        MSetLiteral lt ln -> flatten lt >< flatten ln
        FunctionLiteral lt ln -> flatten lt >< flatten ln
        SequenceLiteral lt ln -> flatten lt >< flatten ln
        RelationLiteral lt ln -> flatten lt >< flatten ln
        PartitionLiteral lt ln -> flatten lt >< flatten ln

instance Flattenable PartitionElemNode where
    flatten (PartitionElemNode ln) = flatten ln

instance Flattenable RelationElemNode where
    flatten x = case x of
        RelationElemNodeLabeled lt -> flatten lt
        RelationElemNodeShort st -> flatten st

instance Flattenable ArrowPairNode where
    flatten (ArrowPairNode a b c) = mconcat [flatten a, flatten b, flatten c]

instance Flattenable RecordMemberNode where
    flatten (RecordMemberNode nn lt en) =  mconcat [flatten nn, flatten lt, flatten en]
instance Flattenable LongTuple where
    flatten (LongTuple a b) =  flatten a >< flatten b

instance Flattenable ShortTuple where
    flatten (ShortTuple a) = flatten a

instance Flattenable MatrixLiteralNode where
    flatten ( MatrixLiteralNode a b c d e) = mconcat
            [ flatten a
            , flatten b
            , flatten c
            , flatten d
            , flatten e
            ]

instance Flattenable ComprehensionNode where
    flatten (ComprehensionNode a b) = flatten a >< flatten b
instance Flattenable ComprehensionExpressionNode where
    flatten (ComprehensionExpressionNode a b c d e) =
        mconcat
            [ flatten a
            , flatten b
            , flatten c
            , flatten d
            , flatten e
            ]

instance Flattenable ComprehensionBodyNode where
    flatten x =  case x of
        CompBodyCondition en -> flatten en
        CompBodyDomain a b c -> flatten a >< flatten b >< flatten c
        CompBodyGenExpr a b c -> flatten a >< flatten b >< flatten c
        CompBodyLettingNode a b c d -> mconcat [flatten a, flatten b, flatten c, flatten d]

instance Flattenable OperatorExpressionNode where
    flatten x =  case x of
        PostfixOpNode en pon -> flatten en >< flatten pon
        PrefixOpNode lt en -> flatten lt >< flatten en
        BinaryOpNode en lt en' -> mconcat [flatten en, flatten lt, flatten en']

instance Flattenable PostfixOpNode where
    flatten x =  case x of
        IndexedNode l -> flatten l
        OpFactorial lt -> flatten lt
        ApplicationNode ln -> flatten ln
        ExplicitDomain l1 l2 dom l3 -> mconcat  [flatten l1,flatten l2,flatten dom,flatten l3]

instance Flattenable DomainNode where
    flatten x =  case x of
        BoolDomainNode lt -> flatten lt
        RangedIntDomainNode lt ln -> flatten lt >< flatten ln
        MetaVarDomain a -> flatten a
        RangedEnumNode nn ln -> flatten nn >< flatten ln
        -- EnumDomainNode nn -> flatten nn
        ShortTupleDomainNode ln -> flatten ln
        TupleDomainNode lt ln -> flatten lt >< flatten ln
        RecordDomainNode lt ln -> flatten lt >< flatten ln
        VariantDomainNode lt ln -> flatten lt >< flatten ln
        MatrixDomainNode a m_ib b c d -> mconcat [flatten a, flatten m_ib, flatten b, flatten c, flatten d]
        SetDomainNode a b c d -> mconcat [flatten a, flatten b, flatten c, flatten d]
        MSetDomainNode a b c d -> mconcat [flatten a, flatten b, flatten c, flatten d]
        FunctionDomainNode a b c d e -> mconcat [flatten a, flatten b, flatten c, flatten d, flatten e]
        SequenceDomainNode a b c d -> mconcat [flatten a, flatten b, flatten c, flatten d]
        RelationDomainNode a b c d -> mconcat [flatten a, flatten b, flatten c, flatten d]
        PartitionDomainNode a b c d -> mconcat [flatten a, flatten b, flatten c, flatten d]
        MissingDomainNode m -> flatten m

instance Flattenable IndexedByNode where
    flatten (IndexedByNode a b ) = flatten a >< flatten b

instance (Flattenable a) => Flattenable (Maybe a) where
    flatten (Just x) = flatten x
    flatten Nothing = S.empty
instance Flattenable AttributeNode where
    flatten x =  case x of
        NamedAttributeNode nn m_e -> flatten nn >< flatten m_e
        -- NamedExpressionAttribute nn en -> flatten nn >< flatten en

instance Flattenable RangeNode where
    flatten x =  case x of
        SingleRangeNode en -> flatten en
        OpenRangeNode ddn -> flatten ddn
        RightUnboundedRangeNode en ddn -> flatten en >< flatten ddn
        LeftUnboundedRangeNode ddn en -> flatten ddn >< flatten en
        BoundedRangeNode en ddn en' -> mconcat [flatten en, flatten ddn, flatten en']

-- instance Flattenable DoubleDotNode where
--     flatten (DoubleDotNode a b) =  flatten a >< flatten b

instance Flattenable NamedDomainNode where
    flatten (NameDomainNode a Nothing) = flatten a
    flatten (NameDomainNode a (Just (b,c))) = mconcat [flatten a,flatten b,flatten c]

instance Flattenable NameNode where
    flatten (NameNode n) =  flatten n
    flatten (MissingNameNode n) =  flatten n
instance Flattenable NameNodeS where
    flatten (NameNodeS n) =  flatten n

instance Flattenable ParenExpressionNode where
    flatten (ParenExpressionNode a b c) =  flatten a >< flatten b >< flatten c

instance Flattenable b => Flattenable (ListNode b) where
    flatten (ListNode l1 seq l2) =  mconcat [flatten l1, flatten seq, flatten l2]

instance Flattenable b => Flattenable (Sequence b) where
    flatten (Seq es) = mconcat $ map flatten es

instance Flattenable b => Flattenable (SeqElem b) where
    flatten (SeqElem v s) =  flatten v >< flatten s
    flatten (MissingSeqElem v s) =  flatten v >< flatten s
instance Flattenable b => Flattenable [b] where
    flatten = mconcat . map flatten 




