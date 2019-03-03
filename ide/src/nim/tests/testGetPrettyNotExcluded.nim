import unittest, intsets
import util/setType


suite "1":
    test "1..10":
        let oSet = newOSet("s")

        for i in countUp(1,10):
            oSet.notExcluded.incl(i)
        
        check(oSet.getPrettyNotExcluded() == "int(1..10)")

    test "1,3,5":
        let oSet = newOSet("s")
        oSet.notExcluded.incl(1)
        oSet.notExcluded.incl(3)
        oSet.notExcluded.incl(5)
        check(oSet.getPrettyNotExcluded() == "int(1,3,5)")

    test "1..5,8,11":
        let oSet = newOSet("s")

        for i in countUp(1,5):
            oSet.notExcluded.incl(i)
        oSet.notExcluded.incl(8)
        oSet.notExcluded.incl(11)
        check(oSet.getPrettyNotExcluded() == "int(1..5,8,11)")

    test "1..5,10..20":
        let oSet = newOSet("s")

        for i in countUp(1,5):
            oSet.notExcluded.incl(i)
        for i in countUp(10,20):
            oSet.notExcluded.incl(i)
        check(oSet.getPrettyNotExcluded() == "int(1..5,10..20)")

    test "1..5,8,10..20,69":
        let oSet = newOSet("s")

        for i in countUp(1,5):
            oSet.notExcluded.incl(i)
        oSet.notExcluded.incl(8)
        for i in countUp(10,20):
            oSet.notExcluded.incl(i)
        oSet.notExcluded.incl(69)
        check(oSet.getPrettyNotExcluded() == "int(1..5,8,10..20,69)")
