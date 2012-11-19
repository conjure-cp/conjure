module Stuff.Generic.Tag where
import Stuff.Pretty
import Data.Char ( isSpace )
import Data.String ( IsString(..) )
data Tag = Tactual
    | TallDiff
    | Tappend
    | Targ1
    | Targs
    | TatTopLevel
    | TattrCollection
    | Tattribute
    | Tattributes
    | TbinOp
    | Tbody
    | Tbool
    | Tdeclaration
    | Tdefined
    | Tdim
    | TdimFind
    | Tdomain
    | TdomainInExpr
    | TdontCare
    | TemptyGuard
    | Tenum
    | Texpr
    | Tfind
    | Tfreq
    | Tfrom
    | TfromTo
    | Tfunction
    | TfunctionApply
    | Tgiven
    | Tguard
    | Thist
    | Tidentity
    | Tin
    | Tindex
    | Tindexrange
    | Tindices
    | Tinner
    | TinnerFrom
    | Tinners
    | TinnerTo
    | Tint
    | Tinverse
    | Tlambda
    | Tleft
    | Tletting
    | Tliteral
    | Tlocals
    | Tmapping
    | Tmatrix
    | Tmax
    | Tmaximising
    | Tmetavar
    | Tmin
    | Tminimising
    | Tmset
    | Tname
    | TnameValue
    | Tnegate
    | TnestedDimFind
    | Tnew
    | Tnext
    | Tnot
    | TnowOrdered
    | Tobjective
    | Told
    | Topen
    | Toperator
    | Tparam
    | Tpart
    | Tpartition
    | Tpattern
    | TpreImage
    | TquanOverDom
    | TquanOverExpr
    | TquanOverOp
    | Tquantified
    | Tquantifier
    | TquantifierDecl
    | TquanVar
    | Trange
    | Tranges
    | Treference
    | Trelation
    | Treplace
    | Tright
    | Trulerefn
    | Tset
    | Tsingle
    | Tslicer
    | Tstatement
    | TstatementEOF
    | Tstructural
    | Tsubset
    | TsubsetEq
    | TsuchThat
    | Ttemplates
    | Tthis
    | Tto
    | TtoInt
    | TtoMSet
    | TtopLevel
    | TtoRelation
    | TtoSet
    | Ttuple
    | TtwoBars
    | Ttype
    | Ttyped
    | TtypeEnum
    | TtypeUnnamed
    | TunaryOp
    | Tunknown
    | Tvalue
    | Tvalues
    | Twhere
    | Twithin
    | TwithLocals
    deriving (Eq, Ord, Show)
instance Pretty Tag where
    pretty = pretty . drop 1 . show
instance IsString Tag where
    fromString = fromString' . filter (not . isSpace)
        where
            fromString' "actual" = Tactual
            fromString' "allDiff" = TallDiff
            fromString' "append" = Tappend
            fromString' "arg1" = Targ1
            fromString' "args" = Targs
            fromString' "atTopLevel" = TatTopLevel
            fromString' "attrCollection" = TattrCollection
            fromString' "attribute" = Tattribute
            fromString' "attributes" = Tattributes
            fromString' "binOp" = TbinOp
            fromString' "body" = Tbody
            fromString' "bool" = Tbool
            fromString' "declaration" = Tdeclaration
            fromString' "defined" = Tdefined
            fromString' "dim" = Tdim
            fromString' "dimFind" = TdimFind
            fromString' "domain" = Tdomain
            fromString' "domainInExpr" = TdomainInExpr
            fromString' "dontCare" = TdontCare
            fromString' "emptyGuard" = TemptyGuard
            fromString' "enum" = Tenum
            fromString' "expr" = Texpr
            fromString' "find" = Tfind
            fromString' "freq" = Tfreq
            fromString' "from" = Tfrom
            fromString' "fromTo" = TfromTo
            fromString' "function" = Tfunction
            fromString' "functionApply" = TfunctionApply
            fromString' "given" = Tgiven
            fromString' "guard" = Tguard
            fromString' "hist" = Thist
            fromString' "identity" = Tidentity
            fromString' "in" = Tin
            fromString' "index" = Tindex
            fromString' "indexrange" = Tindexrange
            fromString' "indices" = Tindices
            fromString' "inner" = Tinner
            fromString' "innerFrom" = TinnerFrom
            fromString' "inners" = Tinners
            fromString' "innerTo" = TinnerTo
            fromString' "int" = Tint
            fromString' "inverse" = Tinverse
            fromString' "lambda" = Tlambda
            fromString' "left" = Tleft
            fromString' "letting" = Tletting
            fromString' "literal" = Tliteral
            fromString' "locals" = Tlocals
            fromString' "mapping" = Tmapping
            fromString' "matrix" = Tmatrix
            fromString' "max" = Tmax
            fromString' "maximising" = Tmaximising
            fromString' "metavar" = Tmetavar
            fromString' "min" = Tmin
            fromString' "minimising" = Tminimising
            fromString' "mset" = Tmset
            fromString' "name" = Tname
            fromString' "nameValue" = TnameValue
            fromString' "negate" = Tnegate
            fromString' "nestedDimFind" = TnestedDimFind
            fromString' "new" = Tnew
            fromString' "next" = Tnext
            fromString' "not" = Tnot
            fromString' "nowOrdered" = TnowOrdered
            fromString' "objective" = Tobjective
            fromString' "old" = Told
            fromString' "open" = Topen
            fromString' "operator" = Toperator
            fromString' "param" = Tparam
            fromString' "part" = Tpart
            fromString' "partition" = Tpartition
            fromString' "pattern" = Tpattern
            fromString' "preImage" = TpreImage
            fromString' "quanOverDom" = TquanOverDom
            fromString' "quanOverExpr" = TquanOverExpr
            fromString' "quanOverOp" = TquanOverOp
            fromString' "quantified" = Tquantified
            fromString' "quantifier" = Tquantifier
            fromString' "quantifierDecl" = TquantifierDecl
            fromString' "quanVar" = TquanVar
            fromString' "range" = Trange
            fromString' "ranges" = Tranges
            fromString' "reference" = Treference
            fromString' "relation" = Trelation
            fromString' "replace" = Treplace
            fromString' "right" = Tright
            fromString' "rulerefn" = Trulerefn
            fromString' "set" = Tset
            fromString' "single" = Tsingle
            fromString' "slicer" = Tslicer
            fromString' "statement" = Tstatement
            fromString' "statementEOF" = TstatementEOF
            fromString' "structural" = Tstructural
            fromString' "subset" = Tsubset
            fromString' "subsetEq" = TsubsetEq
            fromString' "suchThat" = TsuchThat
            fromString' "templates" = Ttemplates
            fromString' "this" = Tthis
            fromString' "to" = Tto
            fromString' "toInt" = TtoInt
            fromString' "toMSet" = TtoMSet
            fromString' "topLevel" = TtopLevel
            fromString' "toRelation" = TtoRelation
            fromString' "toSet" = TtoSet
            fromString' "tuple" = Ttuple
            fromString' "twoBars" = TtwoBars
            fromString' "type" = Ttype
            fromString' "typed" = Ttyped
            fromString' "typeEnum" = TtypeEnum
            fromString' "typeUnnamed" = TtypeUnnamed
            fromString' "unaryOp" = TunaryOp
            fromString' "unknown" = Tunknown
            fromString' "value" = Tvalue
            fromString' "values" = Tvalues
            fromString' "where" = Twhere
            fromString' "within" = Twithin
            fromString' "withLocals" = TwithLocals
            fromString' t = error $ "Unknown tag: " ++ t
