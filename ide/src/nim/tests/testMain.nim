import unittest
import util/types
include util/main

suite "init":
    test "initValidPath":
        let validPath = "../test/testData/sets/dummy"
        init(validPath)


    test "initNoDBFile":
        let badPath = "../test/testData/extension/noDBFile"
        expect(CannotOpenDatabaseException):
            init(badPath)

    test "initNoEprimeFile":
        let badPath = "../test/testData/extension/noEprimeFile"
        expect(IOError):
            init(badPath)
    
    test "initNoMinionFile":
        let badPath = "../test/testData/extension/noMinionFile"
        expect(IOError):
            init(badPath)

suite "loadNodes":
    let validPath = "../test/testData/sets/recursive/markerMarkerMarker"
    init(validPath)

    test "fromZero":
        let nodes = loadNodes("3", "0")
        check(nodes.len() == 3)
        check(nodes[0].nodeId == 1)
        check(nodes[0].parentId == -1)
        check(nodes[0].label == "x = 1")

        check(nodes[1].nodeId == 2)
        check(nodes[1].parentId == 1)
        check(nodes[1].label == "y = 1")

        check(nodes[2].nodeId == 3)
        check(nodes[2].parentId == 2)
        check(nodes[2].label == "z = 1")
        

    test "fromTwo":
        let nodes = loadNodes("3", "2")
        check(nodes.len() == 3)
        check(nodes[0].nodeId == 3)
        check(nodes[1].nodeId == 4)
        check(nodes[2].nodeId == 5)

suite "loadChildren":
    let validPath = "../test/testData/sets/recursive/markerMarkerMarker"
    init(validPath)

    test "oneChild":
        let response = loadChildren("1")
        check(response.nodeId == 1)
        check(response.children == @[2])

suite "loadCore":
    let validPath = "../test/testData/sets/recursive/markerMarkerMarker"
    init(validPath)

    test "1":
        let core = loadCore()
        check(core.len() == 9)

        for i in countUp(0, 7):
            check(core[i].nodeId == i+1)
            check(core[i].children[0] == i+2)

        check(core[0].label == "x = 1")
        check(core[1].label == "y = 1")
        check(core[2].label == "z = 1")

suite "getLabel":
    test "simple":
        check(getLabel("x", "0", "0") == "x != 0")
        check(getLabel("y", "4", "1") == "y = 4")

suite "loadSimpleDomains":
    let validPath = "../test/testData/sets/recursive/markerMarkerMarker"
    init(validPath)
    test "1":
        for i in countUp(1, 10):
            let response = loadSimpleDomains("100", "1", $i)
            # echo response.vars
            # echo response.changedNames


        # for node in core:


# suite "Test for Main":
#     echo "suite setup: run once before the tests"

#     init(path)

#     test "loadNodes":
#         let nodes = loadNodes("10", "0")
#         check(nodes.len() == 10)

#     test "loadSimpleDomains":
#         let ls =  loadSimpleDomains("1", "0", "2" )

#     test "loadPrettyDomains":
#         let lp =  loadPrettyDomains("1", "")

#     test "getLongestBranchingVarName":
#         let longest = getLongestBranchingVarName()

#     test "expandSet":
#         init("../test/testData/sets/recursive/markerMarkerOccurrence")
#         let lp =  loadPrettyDomains("2", "")
#         check(getExpandedSetChild("2", "s.s-1").name == "s-1")
#         check(getExpandedSetChild("2", "s.s-1.s-1-1").name == "s-1-1")
#         check(getExpandedSetChild("2", "s.s-2.s-2-1").name == "s-2-1")

#     test "loadSetChild":
#         init("../test/testData/sets/recursive/markerMarkerOccurrence")
#         let lp =  loadPrettyDomains("2", "")
#         echo loadSetChild("2", "s.1")



        # let domains = getPrettyDomainsOfNode(db, "2")
        # echo (%getExpandedSetChildren("2", "s", "1:1")).pretty()
        # echo (%getExpandedSetChildren("2", "s", "1:2")).pretty()
        # echo (%getExpandedSetChildren("2", "s", "1:1.Children.s")).pretty()



