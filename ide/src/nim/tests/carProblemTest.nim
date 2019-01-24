import unittest
# include util/process
import util/main

let path = "/home/tom/EssenceCatalog/problems/csplib-prob001/conjure-output";
let badPath = "/home/tom/EssenceCatalog/problems/csplib-prob001/"
        

# let db = open(dbPath, "", "", "") 

suite "Test for dummy sets":
    test "Valid path":
        init(path)

    test "invaid path":
        init(badPath)

    # initParser(minionPath, eprimePath)

    # test "Parsing minion":
    #     let t = parseAux(minionPath)
    #   # echo t

    #     for v in t.values:
    #         require(not v.name.contains("aux"))

    # test "Parsing eprime":


    #     var containsDummy = false

    #     for p in parseEprime(eprimePath).values():
    #         if p of DummySet:
    #             containsDummy = true
    #       # echo d
    #     require(containsDummy)


    # test "Pretty domains":

    #     var containsDummy = false

    #     for d in getPrettyDomainsOfNode(db, "1"):
    #         if d of DummySet:
    #             containsDummy = true
    #     # echo d
    #     require(containsDummy)
