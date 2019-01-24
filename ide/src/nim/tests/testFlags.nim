import unittest
include util/process


let eprimePath = "../test/testData/sets/flags/model000001.eprime"
let minionPath = "../test/testData/sets/flags/model000001.eprime-minion"
let dbPath =     "../test/testData/sets/flags/test.db"

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

        var containsDummy = false

        for p in parseEprime(eprimePath).values():
            if p of FlagSet:
                containsDummy = true
          # echo d
        require(containsDummy)


    test "Pretty domains":

        var containsDummy = false

        for d in getPrettyDomainsOfNode(db, "1"):
            if d of FlagSet:
                containsDummy = true
        # echo d
        require(containsDummy)
