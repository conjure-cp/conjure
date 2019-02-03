import unittest
include util/process


let eprimePath = "../test/testData/sets/marker/model000001.eprime"
let minionPath = "../test/testData/sets/marker/model000001.eprime-minion"
let dbPath =     "../test/testData/sets/marker/test.db"

let db = open(dbPath, "", "", "") 

suite "Test for Flag sets":
    echo "suite setup: run once before the tests"

    initParser(minionPath, eprimePath)

    test "Parsing minion":
        let t = parseAux(minionPath)
      # echo t
        for v in t.values:
            require(not v.name.contains("aux"))

    test "Parsing eprime":


        for p in parseEprime(eprimePath).values():
            if p of MarkerSet:
                discard


    test "Pretty domains":

        for d in getPrettyDomainsOfNode(db, "15"):
            if d of MarkerSet:
                let mS = cast[MarkerSet](d)
                check(mS.included == @[2, 4])
                check(mS.excluded.len() == 0)
            
                # echo d
        # echo d
