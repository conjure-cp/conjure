# doAssert(1 + 1 == 2)
import unittest
include util/process


let eprimePath = "../test/testData/sets/explicit/model000001.eprime"
let minionPath = "../test/testData/sets/explicit/model000001-p.eprime-minion"
let dbPath =     "../test/testData/sets/explicit/test.db"

let db = open(dbPath, "", "", "") 

suite "Test for explicit sets":
    echo "suite setup: run once before the tests"

    initParser(minionPath, eprimePath)

    test "Parsing minion":
        let t = parseAux(minionPath)
      # echo t

        for v in t.values:
            require(not v.name.contains("aux"))

    test "Parsing eprime":

        var containsExplicit = false

        for p in parseEprime(eprimePath).values():
            if p of ExplicitSet:
                containsExplicit = true
          # echo d
        require(containsExplicit)


    test "Pretty domains":

        var containsExplicit = false

        for d in getPrettyDomainsOfNode(db, "1"):
            if d of ExplicitSet:
                containsExplicit = true
        # echo d
        require(containsExplicit)
