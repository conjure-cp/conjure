
# doAssert(1 + 1 == 2)
import unittest
include util/process


let eprimePath = "../test/testData/sets/recursive/markerOccurrence/model000001.eprime"
let minionPath = "../test/testData/sets/recursive/markerOccurrence/model000001.eprime-minion"
let dbPath =     "../test/testData/sets/recursive/markerOccurrence/test.db"

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


        var containsMarkerOccurrenceSet = false

        for p in parseEprime(eprimePath).values():
            if p of MarkerSet:
                # echo p
                # discard
                containsMarkerOccurrenceSet = true
          # echo d
        require(containsMarkerOccurrenceSet)


    test "Pretty domains":

        for d in getPrettyDomainsOfNode(db, "1"):
            echo d
        # echo d
