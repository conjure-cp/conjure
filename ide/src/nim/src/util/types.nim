type
    EprimeParseException* = object of Exception
type
    MinionParseException* = object of Exception

type TreeViewNode* = ref object of RootObj
  name: string
  children: seq[TreeViewNode]

type Variable* = ref object of RootObj
  name: string
  rng: string

type Expression* = ref object of Variable

type Set* = ref object of Variable
    lower: int
    upper: int
    included: seq[int]
    excluded: seq[int]

type OccurrenceSet* = ref object of Set


type DummySet* = ref object of Set
    dummyVal : int

type MarkerSet* = ref object of Set
    markerLower : int
    markerUpper : int

type FlagSet* = ref object of Set
    # list : seq[string]
    # flags : seq[int]
    maxSetTo1 : int
    maxSetTo0 : int
    flagCount : int

proc getPrettyRange*(lower: string, upper: string): string =
    if lower == upper:
       return "int(" & $lower & ")" 
    return "int(" & $lower & ".." & $upper & ")"

proc getCardinality*(s: Set): string =
    if s of DummySet or s of OccurrenceSet:
        return getPrettyRange($len(s.included), $(s.upper - len(s.excluded))) 

    if s of MarkerSet:
        let mS = cast[MarkerSet](s)

        # if mS.markerLower == mS.markerUpper:
        return getPrettyRange($mS.markerLower, $mS.markerUpper)

        # return mS.lower
        # return getPrettyRange($mS.markerLower, $mS.markerLower)

    if s of FlagSet:
        let fS = cast[FlagSet](s)

        return getPrettyRange($max(fS.included.len(), fS.lower), $(fS.flagCount - fS.excluded.len()))
        # return getPrettyRange($fS.maxSetTo1, $fS.maxSetTo1)
    
    return "ERROR"

proc `$`*(v:Variable): string =
    if v of Expression:
        return "<Expr> " & v.name & " " & v.rng
    if v of FlagSet:
        let s = cast[MarkerSet](v)
        return "<FSet> " & getPrettyRange($s.lower, $s.upper) & " inc " & $s.included & " exc " & $s.excluded  &  " cardinality " & getCardinality(s)
    if v of MarkerSet:
        let s = cast[MarkerSet](v)
        return "<MSet> " & getPrettyRange($s.lower, $s.upper) & " inc " & $s.included & " exc " & $s.excluded &  " cardinality " & getCardinality(s)
    if v of OccurrenceSet:
        let s = cast[OccurrenceSet](v)
        return "<OSet> " & getPrettyRange($s.lower, $s.upper) & " inc " & $s.included & " exc " & $s.excluded &  " cardinality " & getCardinality(s)
    if v of DummySet:
        let s = cast[DummySet](v)
        return "<DSet> " & getPrettyRange($s.lower, $s.upper) & " dummy " & $s.dummyVal & " inc " & $s.included & " exc " & $s.excluded &  " cardinality " & getCardinality(s)
    return "<Variable> " & v.name & " " & v.rng 
