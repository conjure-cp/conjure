
import variable, util, strutils, intsets

type Set* = ref object of Variable
    notExcludedCount*: int
    markerLower*: int
    markerUpper*: int
    id*: int
    lowerBound: int
    upperBound: int
    included*: IntSet
    notExcluded*: IntSet
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

proc getSetName*(parent: Set, setId: int): string =
    return parent.name & "-" & $setId

proc newOccurrenceSet*(name: string, lowerBound, upperBound: int = -1,
        inner: Set = nil): OccurrenceSet =
    return OccurrenceSet(name: name, rng: "UNDEFINED", upperBound: upperBound,
            lowerBound: lowerBound, inner: inner)

proc newDummySet*(name: string, lowerBound, upperBound: int = -1,
        dummyVal: int = -1): DummySet =
    return DummySet(name: name, rng: "UNDEFINED", lowerBound: lowerBound,
            upperBound: upperBound, dummyVal: dummyVal, excludedCount: 0)

proc newMarkerSet*(name: string, lowerBound, upperBound: int = -1,
        inner: Set = nil): MarkerSet =
    return MarkerSet(name: name, rng: "UNDEFINED", lowerBound: lowerBound,
            upperBound: upperBound, markerLower: -1, markerUpper: -1,
            inner: inner, id: -1)

proc newFlagSet*(name: string, lowerBound, upperBound: int = -1,
        inner: Set = nil): FlagSet =
    return FlagSet(name: name, rng: "UNDEFINED", lowerBound: lowerBound,
            upperBound: upperBound, markerLower: -1, inner: inner)

proc newExplicitSet*(name: string, lowerBound, upperBound: int = -1,
        cardinality: int = -1, inner: Set = nil): ExplicitSet =
    return ExplicitSet(name: name, rng: "UNDEFINED", lowerBound: lowerBound,
            upperBound: upperBound, cardinality: cardinality, inner: inner)

proc getCardinality*(s: Set): string =
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

proc `$`(s: Set): string =

    result = getSetType(s)
    result &= s.name
    result &= " cardinality " & getCardinality(s)

    if (s.inner != nil):
        result &= " [" & getSetType(s.inner) & "]"
    else:
        result &= " " &  " {" & $s.included  & "} " & " {" & $s.notExcluded & "}"

    if s.children.len() > 0:
        result &= "\n"
        var indent = ""
        for i in countUp(1, s.name.split("-").len()):
            indent &= "       "
        for child in s.children:
            result &= indent & $child & "\n"
