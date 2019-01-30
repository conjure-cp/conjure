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
    lowerBound: int
    upperBound: int
    included: seq[int]
    excluded: seq[int]
    inner: Set

type OccurrenceSet* = ref object of Set


type DummySet* = ref object of Set
    dummyVal : int
    excludedCount : int

type MarkerSet* = ref object of Set
    markerLower : int
    markerUpper : int
    # cardinality : int

type FlagSet* = ref object of Set
    # flagCount : int
    maxSetTo1: int

type ExplicitSet* = ref object of Set
    cardinality : int

proc getPrettyRange*(lowerBound: string, upperBound: string): string =
    if lowerBound == upperBound:
       return "int(" & $lowerBound & ")" 
    return "int(" & $lowerBound & ".." & $upperBound & ")"

proc getCardinality*(s: Set): string =
    if s of OccurrenceSet:
        return getPrettyRange($max(len(s.included), s.lowerBound), $(s.upperBound - len(s.excluded))) 

    if s of DummySet:
        let d = cast[DummySet](s)
        return getPrettyRange($max(len(d.included), d.lowerBound), $(d.upperBound - d.excludedCount)) 

    if s of MarkerSet:
        let mS = cast[MarkerSet](s)
        return getPrettyRange($mS.markerLower, $mS.markerUpper)

    if s of FlagSet:
        let fS = cast[FlagSet](s)
        # return getPrettyRange($max(fS.included.len(), fS.lowerBound), $(fS.flagCount - fS.excluded.len()))
        return getPrettyRange($fS.maxSetTo1, $fS.maxSetTo1)
    
    if s of ExplicitSet:
        let eS = cast[ExplicitSet](s)
        return getPrettyRange($eS.cardinality, $eS.cardinality)
    
    return "SET TYPE NOT SUPPORTED"

proc `$`*(v:Variable): string =
    if v of Expression:
        return "<Expr> " & v.name & " " & v.rng


    if v of Set:
        let s = cast[Set](v)
        result = getPrettyRange($s.lowerBound, $s.upperBound) & " inc " & $s.included & " exc " & $s.excluded  &  " cardinality " & getCardinality(s)

        if not  (s.inner == nil):
            result &= " {" & $s.inner & "}"

        if s of FlagSet:
            result = "<FSet> " & result
        if s of MarkerSet:
            result = "<MSet> " & result
        if s of OccurrenceSet:
            result = "<OSet> " & result
        if s of DummySet:
            let d = cast[DummySet](v)
            result = "<DSet>  dummy " & $d.dummyVal & " " & result

    else:
        result =  "<Variable> " & v.name & " " & v.rng 
    
    # if v of FlagSet:
    #     let s = cast[MarkerSet](v)
    #     return "<FSet> " & getPrettyRange($s.lowerBound, $s.upperBound) & " inc " & $s.included & " exc " & $s.excluded  &  " cardinality " & getCardinality(s)  
    # if v of MarkerSet:
    #     let s = cast[MarkerSet](v)
    #     return "<MSet> " & getPrettyRange($s.lowerBound, $s.upperBound) & " inc " & $s.included & " exc " & $s.excluded &  " cardinality " & getCardinality(s)
    # if v of OccurrenceSet:
    #     let s = cast[OccurrenceSet](v)
    #     return "<OSet> " & getPrettyRange($s.lowerBound, $s.upperBound) & " inc " & $s.included & " exc " & $s.excluded &  " cardinality " & getCardinality(s)
    # if v of DummySet:
    #     let s = cast[DummySet](v)
    #     return "<DSet> " & getPrettyRange($s.lowerBound, $s.upperBound) & " dummy " & $s.dummyVal & " inc " & $s.included & " exc " & $s.excluded &  " cardinality " & getCardinality(s)
