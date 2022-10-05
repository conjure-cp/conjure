module Conjure.Language.AST.Reformer where

import Conjure.Language.AST.Syntax
import Conjure.Language.NewLexer (ETok (..))
import Conjure.Prelude

class Flattenable v a where
    flatten :: Flattenable v a => a -> [v]

instance Flattenable ETok ProgramTree where
    flatten (ProgramTree sts) = concatMap flatten sts

instance Flattenable ETok StatementNode where
    flatten x = case x of
        DeclarationStatement dsn -> flatten dsn
        BranchingStatement bsn -> flatten bsn
        SuchThatStatement stsn -> flatten stsn
        WhereStatement wsn -> flatten wsn
        ObjectiveStatement osn -> flatten osn
        UnexpectedToken tok -> flatten tok

instance Flattenable ETok DeclarationStatementNode where
    flatten x = case x of
        FindStatement fsn -> flatten fsn
        GivenStatement gsn -> flatten gsn
        LettingStatement lsn -> flatten lsn

instance Flattenable ETok LettingStatementNode where
    flatten x = case x of
        LettingExpr a b c d ->  concat [flatten a, flatten b, flatten c, flatten d]
        LettingDomain a b c d e -> concat [flatten a, flatten b, flatten c, flatten d, flatten e]
        LettingEnum a b c d e f g -> concat [flatten a, flatten b, flatten c, flatten d, flatten e, flatten f, flatten g]
        LettingAnon a b c d e f g h -> concat [flatten a, flatten b, flatten c, flatten d, flatten e, flatten f, flatten g, flatten h]

instance Flattenable ETok FindStatementNode where
    flatten (FindStatementNode a b c d) = concat  [flatten a, flatten b, flatten c, flatten d]

instance Flattenable ETok GivenStatementNode where
    flatten x = case x of
        GivenStatementNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        GivenEnumNode a b c d e -> concat [flatten a, flatten b, flatten c, flatten d, flatten e]



instance Flattenable ETok BranchingStatementNode where
    flatten (BranchingStatementNode lt lt' ln) = concat [flatten lt, flatten lt', flatten ln]

instance Flattenable ETok BranchingOnNode where
    flatten x = case x of
      BranchingOnName nn -> flatten nn
      BranchingOnExpression en -> flatten en

instance Flattenable ETok SuchThatStatementNode where
    flatten (SuchThatStatementNode l1 l2 l3) =  flatten l1 ++ flatten l2 ++ flatten l3
instance Flattenable ETok WhereStatementNode where
    flatten (WhereStatementNode l1 l2) =  flatten l1 ++ flatten l2
instance Flattenable ETok ObjectiveStatementNode where
    flatten x =  case x of
        ObjectiveMin lt en -> concat [flatten lt, flatten en]
        ObjectiveMax lt en -> concat [flatten lt, flatten en]

instance Flattenable ETok LToken where
    flatten x =  case x of
        RealToken et -> concat [flatten et]
        MissingToken _ -> concat []
        SkippedToken et -> concat [flatten et]

instance Flattenable ETok ETok where
    flatten = pure

instance Flattenable ETok ExpressionNode where
    flatten x =  case x of
        Literal ln -> concat [flatten ln]
        IdentifierNode nn -> concat [flatten nn]
        QuantificationExpr qen -> concat [flatten qen]
        ComprehensionExpr cen -> concat [flatten cen]
        OperatorExpressionNode oen -> concat [flatten oen]
        ParenExpression pen -> concat [flatten pen]
        AbsExpression pen -> concat [flatten pen]
        FunctionalApplicationNode lt ln -> concat [flatten lt, flatten ln]
        MissingExpressionNode _ -> concat []

instance Flattenable ETok QuantificationExpressionNode where
    flatten (QuantificationExpressionNode a b c d) = concat [flatten a, flatten b, flatten c, flatten d]

instance Flattenable ETok QuantificationPattern where
    flatten (QuantificationPattern en) = flatten en

instance Flattenable ETok LiteralNode where
    flatten x = case x of
        IntLiteral lt -> concat [flatten lt]
        BoolLiteral lt -> concat [flatten lt]
        MatrixLiteral mln -> concat [flatten mln]
        TupleLiteralNode lt -> concat [flatten lt]
        TupleLiteralNodeShort st -> concat [flatten st]
        RecordLiteral lt ln -> concat [flatten lt, flatten ln]
        VariantLiteral lt ln -> concat [flatten lt, flatten ln]
        SetLiteral ln -> concat [flatten ln]
        MSetLiteral lt ln -> concat [flatten lt, flatten ln]
        FunctionLiteral lt ln -> concat [flatten lt, flatten ln]
        SequenceLiteral lt ln -> concat [flatten lt, flatten ln]
        RelationLiteral lt ln -> concat [flatten lt, flatten ln]
        PartitionLiteral lt ln -> concat [flatten lt, flatten ln]

instance Flattenable ETok PartitionElemNode where
    flatten (PartitionElemNode ln) = flatten ln

instance Flattenable ETok RelationElemNode where
    flatten x = case x of
        RelationElemNodeLabeled lt -> flatten lt
        RelationElemNodeShort st -> flatten st

instance Flattenable ETok ArrowPairNode where
    flatten (ArrowPairNode a b c) = concat [flatten a, flatten b, flatten c]

instance Flattenable ETok RecordMemberNode where
    flatten (RecordMemberNode nn lt en) =  concat [flatten nn, flatten lt, flatten en]
instance Flattenable ETok LongTuple where
    flatten (LongTuple a b) =  flatten a ++ flatten b

instance Flattenable ETok ShortTuple where
    flatten (ShortTuple a) = flatten a

instance Flattenable ETok MatrixLiteralNode where
    flatten x = case x of
        MatrixLiteralExplicitDomain ln lt dn -> concat [flatten ln, flatten lt, flatten dn]
        MatrixLiteralImplicitDomain ln -> flatten ln

instance Flattenable ETok ComprehensionExpressionNode where
    flatten (ComprehensionExpressionNode a b c d e) =
        concat
            [ flatten a
            , flatten b
            , flatten c
            , flatten d
            , flatten e
            ]

instance Flattenable ETok ComprehensionBodyNode where
    flatten x =  case x of
        CompBodyCondition en -> flatten en
        CompBodyDomain ndn -> flatten ndn
        CompBodyLettingNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]

instance Flattenable ETok OperatorExpressionNode where
    flatten x =  case x of
        PostfixOpNode en pon -> flatten en ++ flatten pon
        PrefixOpNode lt en -> flatten lt ++ flatten en
        BinaryOpNode en lt en' -> concat [flatten en, flatten lt, flatten en']

instance Flattenable ETok PostfixOpNode where
    flatten x =  case x of
        IndexedNode lt i lt' -> concat [flatten lt, flatten i, flatten lt']
        OpFactorial lt -> flatten lt
        ApplicationNode ln -> flatten ln

instance Flattenable ETok IndexerNode where
    flatten Indexer =  []

instance Flattenable ETok DomainNode where
    flatten x =  case x of
        BoolDomainNode lt -> concat [flatten lt]
        RangedIntDomainNode lt ln -> concat [flatten lt, flatten ln]
        RangedEnumNode nn ln -> concat [flatten nn, flatten ln]
        EnumDomainNode nn -> concat [flatten nn]
        TupleDomainNode lt ln -> concat [flatten lt, flatten ln]
        RecordDomainNode lt ln -> concat [flatten lt, flatten ln]
        VariantDomainNode lt ln -> concat [flatten lt, flatten ln]
        MatrixDomainNode a b c d e f -> concat [flatten a, flatten b, flatten c, flatten d, flatten e, flatten f]
        SetDomainNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        MSetDomainNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        FunctionDomainNode a b c d e -> concat [flatten a, flatten b, flatten c, flatten d, flatten e]
        SequenceDomainNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        RelationDomainNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        PartitionDomainNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        MissingDomainNode _ -> []

instance Flattenable ETok AttributeNode where
    flatten x =  case x of
        NamedAttributeNode nn -> concat [flatten nn]
        NamedExpressionAttribute nn en -> concat [flatten nn, flatten en]

instance Flattenable ETok RangeNode where
    flatten x =  case x of
        SingleRangeNode en -> concat [flatten en]
        OpenRangeNode ddn -> concat [flatten ddn]
        RightUnboundedRangeNode en ddn -> concat [flatten en, flatten ddn]
        LeftUnboundedRangeNode ddn en -> concat [flatten ddn, flatten en]
        BoundedRangeNode en ddn en' -> concat [flatten en, flatten ddn, flatten en']

instance Flattenable ETok DoubleDotNode where
    flatten (DoubleDotNode a b) =  flatten a ++ flatten b

instance Flattenable ETok NamedDomainNode where
    flatten (NameDomainNode a b c) = concat [flatten a, flatten b, flatten c]

instance Flattenable ETok NameNode where
    flatten (NameNode n) =  flatten n

instance Flattenable ETok ParenExpressionNode where
    flatten (ParenExpressionNode a b c) =  flatten a ++ flatten b ++ flatten c

instance Flattenable ETok b => Flattenable ETok (ListNode b) where
    flatten (ListNode l1 seq l2) =  concat [flatten l1, flatten seq, flatten l2]

instance Flattenable ETok b => Flattenable ETok (Sequence b) where
    flatten (Seq es) = concatMap flatten es

instance Flattenable ETok b => Flattenable ETok (SeqElem b) where
    flatten (SeqElem v Nothing) = flatten v
    flatten (SeqElem v (Just a)) =  flatten v++ flatten a





