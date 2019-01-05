# doAssert(1 + 1 == 2)
import unittest
include util/parser


let eprimePath = "../test/testData/sets/occurrence/model000001.eprime"
let minionPath = "../test/testData/sets/occurrence/model000001.eprime-minion"
let dbPath =     "../test/testData/sets/occurrence/test.db"

let db = open(dbPath, "", "", "") 

suite "description for this stuff":
    echo "suite setup: run once before the tests"
    initParser(minionPath, eprimePath)

    
    setup:
      echo "run before each test"
    
    teardown:
      echo "run after each test"
    

    test "Parsing minion":
      let t = parseAux(minionPath)
      # echo t

      for v in t.values:
        require(not v.name.contains("aux"))

    test "Parsing eprime":

        let t = parseEprime(eprimePath)

        for p in t.values():
          echo p


    test "Pretty domains":
      for d in getPrettyDomainsOfNode(db, "1"):
        echo d

    test "Pretty domains Json":
      echo domainsToJson(getPrettyDomainsOfNode(db, "1")).pretty()

      
    
    

      
        # echo t.contains("x")
        # echo t["x"]
        # echo correct
        # check($parseEprime(testPath) == 

      # give up and stop if this fails
    #   require(true)
    
    # test "slightly less obvious stuff":
    #   # print a nasty message and move on, skipping
    #   # the remainder of this block
    #   check(1 != 1)
    #   check("asd"[2] == 'd')
    
    # test "out of bounds error is thrown on bad access":
    #   let v = @[1, 2, 3]  # you can do initialization here
    #   expect(IndexError):
    #     discard v[4]
    
    # echo "suite teardown: run once after the tests"