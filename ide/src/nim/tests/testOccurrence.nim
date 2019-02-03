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

        for p in parseEprime(eprimePath).values():
            if p of OccurrenceSet:
                discard
          # echo d


    test "Pretty domains":



        for d in getPrettyDomainsOfNode(db, "11"):
            if d of OccurrenceSet:
                let oS = cast[OccurrenceSet](d)
                check(oS.included == @[9, 4])
                check(oS.excluded == @[2, 8, 3])
                
        # echo d
        
