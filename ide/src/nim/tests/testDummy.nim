# doAssert(1 + 1 == 2)
import unittest
include util/process


let eprimePath = "../test/testData/sets/dummy/model000001.eprime"
let minionPath = "../test/testData/sets/dummy/model000001.eprime-minion"
let dbPath =     "../test/testData/sets/dummy/test.db"

let db = open(dbPath, "", "", "") 

suite "Test for dummy sets":
    echo "suite setup: run once before the tests"

    initParser(minionPath, eprimePath)

    test "Parsing minion":
        let t = parseAux(minionPath)
      # echo t

        for v in t.values:
            require(not v.name.contains("aux"))

    test "Parsing eprime":

        for p in parseEprime(eprimePath).values():
            if p of DummySet:
                discard
          # echo d


    test "Pretty domains":
        for d in getPrettyDomainsOfNode(db, "10"):
            if d of DummySet:
                let dS = cast[DummySet](d)
                check(dS.included == @[6, 7, 1])
                check(dS.excluded.len() == 0)
        # echo d
