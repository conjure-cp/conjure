# doAssert(1 + 1 == 2)
import unittest
include util/process


let eprimePath = "../test/testData/sets/occurrence/model000001.eprime"
let minionPath = "../test/testData/sets/occurrence/model000001.eprime-minion"
let dbPath =     "../test/testData/sets/occurrence/test.db"

let db = open(dbPath, "", "", "") 

suite "Test for occurrence sets":
    echo "suite setup: run once before the tests"

    initParser(minionPath, eprimePath)

    test "Parsing minion":
        let t = parseAux(minionPath)
      # echo t

        for v in t.values:
            require(not v.name.contains("aux"))

    test "Parsing eprime":


        var containsOccurrenceSet = false

        for p in parseEprime(eprimePath).values():
            if p of OccurrenceSet:
                containsOccurrenceSet = true
          # echo d
        require(containsOccurrenceSet)


    test "Pretty domains":

        var containsOccurrenceSet = false

        for d in getPrettyDomainsOfNode(db, "1"):
            if d of OccurrenceSet:
                containsOccurrenceSet = true
        # echo d
        require(containsOccurrenceSet)
