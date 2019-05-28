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
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithDummy_00001", "1", "10", true) == "Cardinality of s is at most 1")

    test "DNotDummy":
        let s = newDSet("s", 10)
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithDummy_00001", "0", "10", true) == "Cardinality of s is at least 1")

    test "Din":
        let s = newDset("s", 10)
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithDummy_00001", "1", "2", true) == "2 was included in s")

    test "DNotin":
        let s = newDset("s", 10)
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithDummy_00001", "0", "2", true) == "2 was not included in s")

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

    test "M":
        let s = newMSet("s")
        s.inner = newOSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarkerR2_Marker", "1", "1", true) == "Cardinality of s was set to 1")

    test "MM":
        let s = newMSet("s")
        s.inner = newMSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarkerR5R2_Values_ExplicitVarSizeWithMarkerR2_Marker_00001",
                                    "1", "1", true) == "Cardinality of s-1 was set to 1")
    test "MMMmarker":
        let s = newMSet("s")
        s.inner = newMSet("")
        s.inner.inner = newMSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarkerR4R5_Values_ExplicitVarSizeWithFlagsR5_Values_ExplicitVarSizeWithMarker_Marker_00001_00001",
                                    "1", "0", true) == "Cardinality of s-1-1 was set to 0")

    test "MMMvalues":
        let s = newMSet("s")
        s.inner = newMSet("")
        s.inner.inner = newMSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarkerR4R5_Values_ExplicitVarSizeWithFlagsR5_Values_ExplicitVarSizeWithMarker_Values_00001_00001_00001",
                                    "1", "1", true) == "1 was included in s-1-1")
    test "MO":
        let s = newMSet("s")
        s.inner = newOSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarkerR2_Values_Occurrence_00001_00001", "1", "0", true) == "1 was excluded from s-1")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarkerR2_Values_Occurrence_00001_00001", "0", "0", true) == "1 was included in s-1")

    test "MMO":
        let s = newMSet("s")
        s.inner = newMSet("")
        s.inner.inner = newOSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithMarkerR5R2_Values_ExplicitVarSizeWithMarkerR2_Values_Occurrence_00001_00001_00001",
                                    "1", "0", true) == "1 was excluded from s-1-1")

suite "F":
    test "FCard":
        let s = newFSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithFlags_Flags_00001", "1", "1", true) == "Cardinality of s is at least 1")

    test "FNotCard":
        let s = newFSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithFlags_Flags_00001", "0", "1", true) == "Cardinality of s is less than 1")

    test "Fin":
        let s = newFSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithFlags_Values_00001", "1", "2", true) == "2 was included in s")

    test "FNotin":
        let s = newFSet("s")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithFlags_Values_00001", "0", "2", true) == "2 was not included in s")

    test "Fflags":
        let s = newFSet("s")
        s.inner = newOSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithFlagsR2_Flags_00001",
                                    "0", "0", true) == "Cardinality of s is at least 1")
    test "FFflags":
        let s = newFSet("s")
        s.inner = newFSet("")
        s.inner.inner = newFSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithFlagsR4R4_Values_ExplicitVarSizeWithFlagsR4_Values_ExplicitVarSizeWithFlags_Flags_00001_00001",
                                    "0", "0", true) == "Cardinality of s-1 is at least 1")
    test "FFFflags":
        let s = newFSet("s")
        s.inner = newFSet("")
        s.inner.inner = newFSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithFlagsR4R4_Values_ExplicitVarSizeWithFlagsR4_Values_ExplicitVarSizeWithFlags_Flags_00001_00001_00001",
                                    "0", "0", true) == "Cardinality of s-1-1 is at least 1")
    test "FFFvalues":
        let s = newFSet("s")
        s.inner = newFSet("")
        s.inner.inner = newFSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithFlagsR4R4_Values_ExplicitVarSizeWithFlagsR4_Values_ExplicitVarSizeWithFlags_Values_00001_00001_00001",
                                    "1", "5", true) == "5 was included in s-1-1")
    test "FO":
        let s = newFSet("s")
        s.inner = newOSet("")
        check(getLabel(@[Variable(s)], "s_ExplicitVarSizeWithFlagsR2_Values_Occurrence_00001_00001",
                                    "1", "0", true) == "1 was excluded from s-1")

suite "getLabel":
    test "simple":
        check(getLabel(@[Variable()],"x", "0", "0") == "x != 0")
        check(getLabel(@[Variable()], "y", "1", "4") == "y = 4")






        

