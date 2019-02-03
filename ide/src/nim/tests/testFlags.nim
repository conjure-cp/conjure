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


        for p in parseEprime(eprimePath).values():
            if p of FlagSet:
                discard


    test "Pretty domains":

        for d in getPrettyDomainsOfNode(db, "21"):
            if d of FlagSet:
                let fS = cast[FlagSet](d)
                check(fS.included == @[1,2,3,7])
                check(fS.excluded.len() == 0)
            
                # echo d
        # echo d
