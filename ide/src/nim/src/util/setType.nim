import variable, util, strutils, intsets, sequtils, algorithm, system

type Set* = ref object of Variable
    ## Represents a set
    notExcludedCount*: int
    markerLower*: int
    markerUpper*: int
    id*: int
    included: IntSet
    notExcluded: IntSet
    inner*: Set
    children*: seq[Set]

type OccurrenceSet* = ref object of Set

type DummySet* = ref object of Set
    dummyVal*: int
    excludedCount*: int

type MarkerSet* = ref object of Set

type FlagSet* = ref object of Set

type ExplicitSet* = ref object of Set
    cardinality*: int

proc initSet(name: string, s: Set, inner: Set = nil) =
    ## Initialises members in set
    s.name = name
    s.inner = inner
    s.notExcludedCount = -1
    s.markerLower = -1
    s.markerUpper = -1
    s.id = -1

proc newOSet*(name: string): OccurrenceSet =
    new result
    initSet(name, result)

proc newDSet*(name: string, dummyVal: int): DummySet =
    new result
    result.excludedCount = 0
    result.dummyVal = dummyVal
    initSet(name, result)

proc newMSet*(name: string, inner: Set = nil): MarkerSet =
    new result
    initSet(name, result, inner)

proc newFSet*(name: string, inner: Set = nil): FlagSet =
    new result
    initSet(name, result, inner)

proc newESet*(name: string, cardinality: int, inner: Set = nil): ExplicitSet =
    new result
    result.cardinality = cardinality
    initSet(name, result, inner)

proc getCardinality*(s: Set): string =
    ## Returns the cardinality of a set
    if s of OccurrenceSet:
        return getPrettyRange($s.included.len(), $s.notExcluded.len())

    if s of DummySet:
        let d = DummySet(s)
        return getPrettyRange($s.included.len(), $s.notExcluded.len())

    if s of MarkerSet:
        let mS = MarkerSet(s)
        return getPrettyRange($mS.markerLower, $mS.markerUpper)

    if s of FlagSet:
        let fS = FlagSet(s)
        return getPrettyRange($fS.markerLower, $fS.notExcludedCount)

    if s of ExplicitSet:
        let eS = ExplicitSet(s)
        return getPrettyRange($eS.cardinality, $eS.cardinality)

    return "SET TYPE NOT SUPPORTED"

proc includeInSet*(s: Set, num: int) =
    s.included.incl(num)

proc dontExclude*(s: Set, num: int) =
    s.notExcluded.incl(num)

proc getIncluded*(s: Set): seq[int] =
    return toSeq(s.included.items())

proc getNotExcluded*(s: Set): seq[int] =
    return toSeq(s.notExcluded.items())

proc getSetName*(parent: Set, setId: int): string =
    return parent.name & "-" & $setId

proc getSetType*(s: Set): string =
    if s of ExplicitSet:
        return "<ESet> "
    if s of FlagSet:
        return "<FSet> "
    if s of MarkerSet:
        return "<MSet> "
    if s of OccurrenceSet:
        return "<OSet> "
    if s of DummySet:
        let d = DummySet(s)
        return "<DSet>  dummy " & $d.dummyVal & " "

proc `$`*(s: Set): string =
    ## toString procedure
    result = getSetType(s)
    result &= s.name
    result &= " cardinality " & getCardinality(s)

    if (s.inner != nil):
        result &= " [" & getSetType(s.inner) & "]"
    else:
        result &= " " & " {" & $s.included & "} " & " {" & $s.notExcluded & "}"

    if s.children.len() > 0:
        result &= "\n"
        var indent = ""
        for i in countUp(1, s.name.split("-").len()):
            indent &= "       "
        for child in s.children:
            result &= indent & $child & "\n"


proc getPrettyIncluded*(s: Set): string =
    return prettifyIntSet(s.included)

proc getPrettyNotExcluded*(s: Set): string =
    return prettifyIntSet(s.notExcluded)