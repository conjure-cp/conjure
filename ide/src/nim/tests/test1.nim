# doAssert(1 + 1 == 2)
import unittest
import "../src/parser.nim"


let testPath = "../test/testData/conjure-output/model000001.eprime"

suite "description for this stuff":
    echo "suite setup: run once before the tests"
    
    setup:
      echo "run before each test"
    
    teardown:
      echo "run after each test"
    
    test "Parsing eprime":
        # echo "ASDA"
        # var correct = {"y" : " <Variable> ", "u" : " <Variable> ", "s" : " <DSet> (1..9) ", "z" : " <Variable> ", "x" : " <Variable> "}.toTable()

        # let t = parseEprime(testPath)
        # echo t
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
    
    echo "suite teardown: run once after the tests"