# doAssert(1 + 1 == 2)
import unittest
import util/main
include util/process


let eprimePath = "../test/testData/sets/recursive/markerDummy/model000001.eprime"
let minionPath = "../test/testData/sets/recursive/markerDummy/model000001.eprime-minion"
let dbPath =     "../test/testData/sets/recursive/markerDummy/test.db"

let db = open(dbPath, "", "", "") 

suite "Test for occurrence sets":
    echo "suite setup: run once before the tests"

    initParser(minionPath, eprimePath)

    # test "Parsing minion":
    #     let t = parseAux(minionPath)
    #   # echo t

    #     for v in t.values:
    #         require(not v.name.contains("aux"))

    # test "Parsing eprime":


    #     var containsMarkerOccurrenceSet = false

    #     for p in parseEprime(eprimePath).values():
    #         if p of MarkerSet:
    #             # echo p
    #             # discard
    #             containsMarkerOccurrenceSet = true
    #       # echo d
    #     require(containsMarkerOccurrenceSet)


    test "Pretty domains":

        let pretty = getPrettyDomainsOfNode(db, "5")

        for d in pretty:
            if (d of MarkerSet):
                let ms = cast[MarkerSet](d)
                # echo ms.inner
                echo ms
                discard

        # echo domainsToJson(pretty).pretty()
                #  d
        # echo d
