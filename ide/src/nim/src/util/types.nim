import strutils

# Constants

let maxIndex* = 9
let rootNodeId* = 0

# Exceptions

type
    CannotOpenEprimeException* = object of Exception
type
    CannotOpenMinionException* = object of Exception
type
    CannotOpenDatabaseException* = object of Exception
type
    EprimeParseException* = object of Exception
type
    MinionParseException* = object of Exception

# Decision variables

type Variable* = ref object of RootObj
  name*: string
  rng*: string

type Expression* = ref object of Variable

type Set* = ref object of Variable
    markerLower* : int
    markerUpper* : int
    id* : int
    lowerBound: int
    upperBound: int
    included*: seq[int]
    excluded*: seq[int]
    inner*: Set
    children*: seq[Set]

type OccurrenceSet* = ref object of Set


type DummySet* = ref object of Set
    dummyVal* : int
    excludedCount* : int

type MarkerSet* = ref object of Set

type FlagSet* = ref object of Set

type ExplicitSet* = ref object of Set
    cardinality* : int

proc newVariable*(name, rng: string = "UNDEFINED"): Variable =
    return Variable(name: name, rng: rng)

proc newExpression*(name, rng: string = "UNDEFINED"): Expression =
    return Expression(name: name, rng: rng)

proc newOccurrenceSet*(name : string, lowerBound, upperBound: int = -1, inner: Set = nil): OccurrenceSet =
    return OccurrenceSet(name: name, rng: "UNDEFINED", upperBound: upperBound, lowerBound: lowerBound, inner: inner)

proc newDummySet*(name : string,  lowerBound, upperBound: int = -1, dummyVal : int = -1): DummySet =
    return DummySet(name: name, rng: "UNDEFINED", lowerBound: lowerBound, upperBound: upperBound, dummyVal: dummyVal, excludedCount: 0)

proc newMarkerSet*(name : string, lowerBound, upperBound: int = -1,inner: Set = nil): MarkerSet =
    return MarkerSet(name: name, rng: "UNDEFINED", lowerBound: lowerBound, upperBound: upperBound, markerLower: -1, markerUpper: -1, inner: inner, id : -1)

proc newFlagSet*(name : string, lowerBound, upperBound: int = -1, inner: Set = nil): FlagSet =
    return FlagSet(name: name, rng: "UNDEFINED", lowerBound: lowerBound, upperBound: upperBound, markerLower: -1, inner: inner)

proc newExplicitSet*(name : string, lowerBound, upperBound: int = -1, cardinality: int = -1, inner: Set = nil): ExplicitSet =
    return ExplicitSet(name: name, rng: "UNDEFINED", lowerBound: lowerBound, upperBound: upperBound, cardinality: cardinality, inner: inner)


proc getPrettyRange*(lowerBound: string, upperBound: string): string =
    if lowerBound == upperBound:
       return "int(" & $lowerBound & ")" 
    return "int(" & $lowerBound & ".." & $upperBound & ")"

proc getCardinality*(s: Set): string =
    if s of OccurrenceSet:
        return getPrettyRange($s.included.len(), $s.included.len()) 

    if s of DummySet:
        let d = DummySet(s)
        return getPrettyRange($s.included.len(), $s.included.len()) 

    if s of MarkerSet:
        let mS = MarkerSet(s)
        return getPrettyRange($mS.markerLower, $mS.markerUpper)

    if s of FlagSet:
        let fS = FlagSet(s)
        return getPrettyRange($fS.markerLower, $fS.markerLower)
    
    if s of ExplicitSet:
        let eS = ExplicitSet(s)
        return getPrettyRange($eS.cardinality, $eS.cardinality)
    
    return "SET TYPE NOT SUPPORTED"


proc getSetType*(s: Set): string =
    if s of ExplicitSet:
        result = "<ESet> "
    if s of FlagSet:
        result = "<FSet> "
    if s of MarkerSet:
        result = "<MSet> "
    if s of OccurrenceSet:
        result = "<OSet> "
    if s of DummySet:
        let d = DummySet(s)
        result = "<DSet>  dummy " & $d.dummyVal & " "


proc `$`*(v:Variable): string =
    if v of Expression:
        return "<Expr> " & v.name & " " & v.rng

    if v of Set:
        let s = Set(v)
        result = getSetType(s)
        result &= s.name
        result &= " cardinality " & getCardinality(s)

        if (s.inner != nil):
            result &= " {" & getSetType(s.inner) & "}"
        else:
            result &= " " & getPrettyRange($s.lowerBound, $s.upperBound) & " inc " & $s.included & " exc " & $s.excluded 

        if s.children.len() > 0:
            result &= "\n"
            var indent = ""
            for i in countUp(1, s.name.split("-").len()):
                indent &= "       "
            for child in s.children:
                result &= indent & $child & "\n"
    else:
        result =  "<Variable> " & v.name & " " & v.rng 


proc getSetName*(parent : Set, setId : int): string =
    return parent.name & "-" & $setId


# Responses

type SimpleDomainResponse* = ref object of RootObj
    changedNames* : seq[string]
    vars* : seq[Variable]

type ParentChild* = ref object of RootObj
    nodeId*: int
    parentId*: int
    label*: string
    children*: seq[int]

type ChildResponse* = ref object of RootObj
    nodeId*: int
    children*: seq[int]

type Node* = ref object of RootObj
    id*: int
    name*: string
    children*: seq[Node]

# Json

type TreeViewNode* = ref object of RootObj
  name*: string
  children*: seq[TreeViewNode]