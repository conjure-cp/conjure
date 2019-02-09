type
    EprimeParseException* = object of Exception
type
    MinionParseException* = object of Exception

type TreeViewNode* = ref object of RootObj
  name: string
  children: seq[TreeViewNode]
  

# type HiddenTreeViewNode* = ref object of RootObj
#   name: string
#   children: seq[TreeViewNode]

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
    children: seq[Set]

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
    maxSetTo0: int

type ExplicitSet* = ref object of Set
    cardinality : int

proc newVariable*(name, rng: string = "UNDEFINED"): Variable =
    return Variable(name: name, rng: rng)

proc newExpression*(name, rng: string = "UNDEFINED"): Expression =
    return Expression(name: name, rng: rng)

proc newOccurrenceSet*(name : string, lowerBound, upperBound: int = -1, inner: Set = nil): OccurrenceSet =
    return OccurrenceSet(name: name, rng: "UNDEFINED", upperBound: upperBound, lowerBound: lowerBound, inner: inner)

proc newDummySet*(name : string,  lowerBound, upperBound: int = -1, dummyVal : int = -1): DummySet =
    return DummySet(name: name, rng: "UNDEFINED", lowerBound: lowerBound, upperBound: upperBound, dummyVal: dummyVal, excludedCount: 0)

proc newMarkerSet*(name : string, lowerBound, upperBound: int = -1,inner: Set = nil): MarkerSet =
    return MarkerSet(name: name, rng: "UNDEFINED", lowerBound: lowerBound, upperBound: upperBound, markerLower: -1, markerUpper: -1, inner: inner)

proc newFlagSet*(name : string, lowerBound, upperBound: int = -1, inner: Set = nil): FlagSet =
    return FlagSet(name: name, rng: "UNDEFINED", lowerBound: lowerBound, upperBound: upperBound, maxSetTo1: 0,maxSetTo0: 0, inner: inner)

proc newExplicitSet*(name : string, lowerBound, upperBound: int = -1, card: int = -1, inner: Set = nil): ExplicitSet =
    return ExplicitSet(name: name, rng: "UNDEFINED", lowerBound: lowerBound, upperBound: upperBound, cardinality: card, inner: inner)


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
        result = s.name & " " & getPrettyRange($s.lowerBound, $s.upperBound) & " inc " & $s.included & " exc " & $s.excluded  &  " cardinality " & getCardinality(s)

        if (s.inner != nil):
            result &= " {" & $s.inner & "}"

        if s of ExplicitSet:
            result = "<ESet> " & result
        if s of FlagSet:
            result = "<FSet> " & result
        if s of MarkerSet:
            result = "<MSet> " & result
        if s of OccurrenceSet:
            result = "<OSet> " & result
        if s of DummySet:
            let d = cast[DummySet](v)
            result = "<DSet>  dummy " & $d.dummyVal & " " & result

        # echo s.children.len()

        if s.children.len() > 0:
            result &= "\n KIDS:{"
            for child in s.children:
                if (child.inner != nil):
                    result &= "\n inner"
                else:
                    result &= "\n" & $child
            result &= "}"

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
