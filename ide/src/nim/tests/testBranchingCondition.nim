import unittest, intsets
import util/types
import util/branchingCondition

suite "O":
    test "OExc":
        let s = newOSet("s")
        check(getLabel(@[Variable(s)], "s_Occurrence_00001", "1", "0") == "1 was excluded from s")
        
    test "ONotExc":
        let s = newOSet("s")
        check(getLabel(@[Variable(s)], "s_Occurrence_00001", "0", "0") == "1 was not excluded from s")

    test "OInc":
        let s = newOSet("s")
        check(getLabel(@[Variable(s)], "s_Occurrence_00001", "1", "1") == "1 was included in s")

    test "ONotInc":
        let s = newOSet("s")
        check(getLabel(@[Variable(s)], "s_Occurrence_00001", "0", "1") == "1 was not included in s")

suite "E":
    test "Ein":
        let s = newEset("s", 4)
        check(getLabel(@[Variable(s)], "s_Explicit_00001", "1", "2") == "2 was included in s")

    test "ENotin":
        let s = newEset("s", 4)
        check(getLabel(@[Variable(s)], "s_Explicit_00001", "0", "2") == "2 was not included in s")

suite "D":
    test "DDummy":
        let s = newDSet("s", 10)
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithDummy_00001", "1", "10") == "Cardinality of s was reduced by 1")

    test "DNotDummy":
        let s = newDSet("s", 10)
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithDummy_00001", "0", "10") == "Cardinality of s was not reduced by 1")

    test "Din":
        let s = newEset("s", 10)
        check(getLabel(@[Variable(s)], "s_Explicit_00001", "1", "2") == "2 was included in s")

    test "DNotin":
        let s = newEset("s", 10)
        check(getLabel(@[Variable(s)], "s_Explicit_00001", "0", "2") == "2 was not included in s")

suite "M":
    test "MCard":
        let s = newMSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarker_Marker", "1", "2") == "Cardinality of s was set to 2")

    test "MNotCard":
        let s = newMSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarker_Marker", "0", "2") == "Cardinality of s was not set to 2")

    test "Min":
        let s = newMSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarker_Values_00001", "1", "2") == "2 was included in s")

    test "MNotin":
        let s = newMSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarker_Values_00001", "0", "2") == "2 was not included in s")