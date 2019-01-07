type TreeViewNode = ref object of RootObj
  name: string
  children: seq[TreeViewNode]

type Variable = ref object of RootObj
  name: string
  rng: string

type Expression = ref object of Variable

type Set = ref object of Variable
    lower: int
    upper: int
    included: seq[int]
    excluded: seq[int]

type OccurrenceSet = ref object of Set

type MarkerSet = ref object of Set
    cardinality : string

type DummySet = ref object of Set
    dummyVal : int

proc getPrettyRange(lower: string, upper: string): string =
    if lower == upper:
       return "int(" & $lower & ")" 
    return "int(" & $lower & ".." & $upper & ")"

proc `$`*(v:Variable): string =
    if v of Expression:
        return "<Expr> " & v.name & " " & v.rng
    if v of MarkerSet:
        let s = cast[MarkerSet](v)
        return "<MSet> " & getPrettyRange($s.lower, $s.upper) & " " & " inc " & $s.included & " exc " & $s.excluded 
    if v of OccurrenceSet:
        let s = cast[OccurrenceSet](v)
        return "<OSet> " & getPrettyRange($s.lower, $s.upper) & " " & " inc " & $s.included & " exc " & $s.excluded 
    if v of DummySet:
        let s = cast[DummySet](v)
        return "<DSet> " & getPrettyRange($s.lower, $s.upper) & " " & $s.dummyVal & " inc " & $s.included & " exc " & $s.excluded 
    return "<Variable> " & v.name & " " & v.rng 

proc getCardinality(s: Set): string =
    if s of DummySet or s of OccurrenceSet:
        return getPrettyRange($len(s.included), $(s.upper - len(s.excluded))) 

    if s of MarkerSet:
        let mS = cast[MarkerSet](s)
        return mS.cardinality
    return "ERROR"