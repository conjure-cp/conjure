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
        let ls =  loadSimpleDomains("1", "0", "2")

    test "loadPrettyDomains":
        let lp =  loadPrettyDomains("1", "0", "1")

    test "getLongestBranchingVarName":
        let longest = getLongestBranchingVarName()



