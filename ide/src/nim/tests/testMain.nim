import unittest
include util/main

let path = "../test/testData/sets/dummy"

suite "Test for Main":
    echo "suite setup: run once before the tests"

    init(path)

    test "loadNodes":
        let nodes = loadNodes("10", "0")
        check(nodes.len() == 10)

    test "loadSimpleDomains":
        let ls =  loadSimpleDomains("1", "0", "2" )

    test "loadPrettyDomains":
        let lp =  loadPrettyDomains("1", "")

    test "getLongestBranchingVarName":
        let longest = getLongestBranchingVarName()

    test "expandSet":
        init("../test/testData/sets/recursive/markerMarkerOccurrence")
        let lp =  loadPrettyDomains("2", "")
        echo getExpandedSetChild("2", "s.s-1")

    test "loadSetChild":
        init("../test/testData/sets/recursive/markerMarkerOccurrence")
        let lp =  loadPrettyDomains("2", "")
        echo loadSetChild("2", "s.1")



        # let domains = getPrettyDomainsOfNode(db, "2")
        # echo (%getExpandedSetChildren("2", "s", "1:1")).pretty()
        # echo (%getExpandedSetChildren("2", "s", "1:2")).pretty()
        # echo (%getExpandedSetChildren("2", "s", "1:1.Children.s")).pretty()



