
import unittest
import util/main
include util/process

let eprimePath = "../test/testData/sets/recursive/explicitExplicit/model000001.eprime"
let minionPath = "../test/testData/sets/recursive/explicitExplicit/model000001.eprime-minion"
let dbPath =     "../test/testData/sets/recursive/explicitExplicit/test.db"

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

    #     for p in parseEprime(eprimePath).values():
    #         if p of Set:
    #             echo p
                # discard
          # echo d


    test "Pretty domains":

        let pretty = getPrettyDomainsOfNode(db, "2")

        for d in pretty:
            if (d of ExplicitSet):
                let fs = cast[ExplicitSet](d)
                # echo ms.inner
                echo fs
                discard

        # echo domainsToJson(pretty).pretty()
        #          d
        # echo d