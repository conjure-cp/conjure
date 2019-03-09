import unittest, intsets
import util/types
import util/branchingCondition

suite "O":
    test "OExc":
        let oSet = newOSet("s")
        check(getLabel(@[Variable(oSet)], "s_Occurrence_00008", "1", "0", true) == "8 was excluded from s")
        
    test "ONotExc":
        let oSet = newOSet("s")
        check(getLabel(@[Variable(oSet)], "s_Occurrence_00008", "0", "0", true) == "8 was included in s")

    test "OInc":
        let oSet = newOSet("s")
        check(getLabel(@[Variable(oSet)], "s_Occurrence_00008", "1", "1", true) == "8 was included in s")

    test "ONotInc":
        let oSet = newOSet("s")
        check(getLabel(@[Variable(oSet)], "s_Occurrence_00008", "0", "1", true) == "8 was excluded from s")

suite "E":
    test "Ein":
        let s = newEset("s", 4)
        check(getLabel(@[Variable(s)], "s_Explicit_00001", "1", "2", true) == "2 was included in s")

    test "ENotin":
        let s = newEset("s", 4)
        check(getLabel(@[Variable(s)], "s_Explicit_00001", "0", "2", true) == "2 was not included in s")

suite "D":
    test "DDummy":
        let s = newDSet("s", 10)
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithDummy_00001", "1", "10", true) == "Cardinality of s was reduced by 1")

    test "DNotDummy":
        let s = newDSet("s", 10)
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithDummy_00001", "0", "10", true) == "Cardinality of s was not reduced by 1")

    test "Din":
        let s = newEset("s", 10)
        check(getLabel(@[Variable(s)], "s_Explicit_00001", "1", "2", true) == "2 was included in s")

    test "DNotin":
        let s = newEset("s", 10)
        check(getLabel(@[Variable(s)], "s_Explicit_00001", "0", "2", true) == "2 was not included in s")

suite "M":
    test "MCard":
        let s = newMSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarker_Marker", "1", "2", true) == "Cardinality of s was set to 2")

    test "MNotCard":
        let s = newMSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarker_Marker", "0", "2", true) == "Cardinality of s was not set to 2")

    test "Min":
        let s = newMSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarker_Values_00001", "1", "2", true) == "2 was included in s")

    test "MNotin":
        let s = newMSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarker_Values_00001", "0", "2", true) == "2 was not included in s")

suite "getLabel":
    test "simple":
        check(getLabel(@[Variable()],"x", "0", "0") == "x != 0")
        check(getLabel(@[Variable()], "y", "1", "4") == "y = 4")