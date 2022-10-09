module Conjure.Language.AST.Reformer where

import Conjure.Language.AST.Syntax
import Conjure.Language.NewLexer (ETok (..))
import Conjure.Prelude

class Flattenable v a where
    flatten :: Flattenable v a => a -> [v]

instance Flattenable ETok ProgramTree where
    flatten (ProgramTree sts end) = concatMap flatten sts ++ flatten end

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
        ObjectiveMin lt en -> flatten lt ++ flatten en
        ObjectiveMax lt en -> flatten lt ++ flatten en

instance Flattenable ETok LToken where
    flatten x =  case x of
        RealToken et ->  flatten et
        MissingToken _ ->  []
        SkippedToken et ->  flatten et

instance Flattenable ETok ETok where
    flatten = pure

instance Flattenable ETok ExpressionNode where
    flatten x =  case x of
        Literal ln -> flatten ln
        IdentifierNode nn -> flatten nn
        QuantificationExpr qen -> flatten qen
        OperatorExpressionNode oen -> flatten oen
        ParenExpression pen ->flatten pen
        AbsExpression pen ->  flatten pen
        FunctionalApplicationNode lt ln ->  flatten lt ++ flatten ln
        MissingExpressionNode _ ->  []

instance Flattenable ETok QuantificationExpressionNode where
    flatten (QuantificationExpressionNode a b c d e f) = concat [
        flatten a, flatten b, flatten c, flatten d, flatten e, flatten f]
instance Flattenable ETok QuantificationOverNode where
    flatten x = case x of
      QuantifiedSubsetOfNode a b -> flatten a ++ flatten b
      QuantifiedMemberOfNode a b -> flatten a ++ flatten b
      QuantifiedDomainNode a -> flatten a

instance Flattenable ETok OverDomainNode where
    flatten (OverDomainNode a b) = flatten a ++ flatten b

instance Flattenable ETok QuanticationGuard where
    flatten (QuanticationGuard a b ) = flatten a ++ flatten b

instance Flattenable ETok AbstractPatternNode where
    flatten x = case x of
      AbstractIdentifier nn -> flatten nn
      AbstractPatternMetaVar lt -> flatten lt
      AbstractPatternTuple a b -> flatten a ++ flatten b
      AbstractPatternMatrix ln -> flatten ln
      AbstractPatternSet ln -> flatten ln
instance Flattenable ETok QuantificationPattern where
    flatten (QuantificationPattern en) = flatten en

instance Flattenable ETok LiteralNode where
    flatten x = case x of
        IntLiteral lt -> flatten lt
        BoolLiteral lt -> flatten lt
        MatrixLiteral mln -> flatten mln
        TupleLiteralNode lt -> flatten lt
        TupleLiteralNodeShort st -> flatten st
        RecordLiteral lt ln -> flatten lt ++ flatten ln
        VariantLiteral lt ln -> flatten lt ++ flatten ln
        SetLiteral ln -> flatten ln
        MSetLiteral lt ln -> flatten lt ++ flatten ln
        FunctionLiteral lt ln -> flatten lt ++ flatten ln
        SequenceLiteral lt ln -> flatten lt ++ flatten ln
        RelationLiteral lt ln -> flatten lt ++ flatten ln
        PartitionLiteral lt ln -> flatten lt ++ flatten ln

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
    flatten ( MatrixLiteralNode a b c d e) = concat
            [ flatten a
            , flatten b
            , flatten c
            , flatten d
            , flatten e
            ]

instance Flattenable ETok ComprehensionNode where
    flatten (ComprehensionNode a b) = flatten a ++ flatten b
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
        IndexedNode l -> flatten l
        OpFactorial lt -> flatten lt
        ApplicationNode ln -> flatten ln

instance Flattenable ETok DomainNode where
    flatten x =  case x of
        BoolDomainNode lt -> flatten lt
        RangedIntDomainNode lt ln -> flatten lt ++ flatten ln
        RangedEnumNode nn ln -> flatten nn ++ flatten ln
        EnumDomainNode nn -> flatten nn
        ShortTupleDomainNode ln -> flatten ln
        TupleDomainNode lt ln -> flatten lt ++ flatten ln
        RecordDomainNode lt ln -> flatten lt ++ flatten ln
        VariantDomainNode lt ln -> flatten lt ++ flatten ln
        MatrixDomainNode a b c d e f -> concat [flatten a, flatten b, flatten c, flatten d, flatten e, flatten f]
        SetDomainNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        MSetDomainNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        FunctionDomainNode a b c d e -> concat [flatten a, flatten b, flatten c, flatten d, flatten e]
        SequenceDomainNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        RelationDomainNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        PartitionDomainNode a b c d -> concat [flatten a, flatten b, flatten c, flatten d]
        MissingDomainNode _ -> []

instance (Flattenable ETok a) => Flattenable ETok (Maybe a) where
    flatten (Just x) = flatten x
    flatten Nothing = []
instance Flattenable ETok AttributeNode where
    flatten x =  case x of
        NamedAttributeNode nn -> flatten nn
        NamedExpressionAttribute nn en -> flatten nn ++ flatten en

instance Flattenable ETok RangeNode where
    flatten x =  case x of
        SingleRangeNode en -> flatten en
        OpenRangeNode ddn -> flatten ddn
        RightUnboundedRangeNode en ddn -> flatten en ++ flatten ddn
        LeftUnboundedRangeNode ddn en -> flatten ddn ++ flatten en
        BoundedRangeNode en ddn en' -> concat [flatten en, flatten ddn, flatten en']

-- instance Flattenable ETok DoubleDotNode where
--     flatten (DoubleDotNode a b) =  flatten a ++ flatten b

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
    flatten (SeqElem s v) =  flatten s ++ flatten v





