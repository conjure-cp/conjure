# import unittest
# import util/main
# # include util/process

# import times, os, strutils

# template benchmark(benchmarkName: string, code: untyped) =
#   block:
#     let t0 = epochTime()
#     code
#     let elapsed = epochTime() - t0
#     let elapsedStr = elapsed.formatFloat(format = ffDecimal, precision = 3)
#     echo "CPU Time [", benchmarkName, "] ", elapsedStr, "s"

# suite "Test for occurrence sets":
#     echo "suite setup: run once before the tests"

#     # initParser(minionPath, eprimePath)
#     # init("../test/testData/sets/dummy")
#     init("/home/tom/EssenceCatalog/problems/csplib-prob053-GracefulGears/conjure-output")

    # test "blah":
        # echo loadAllNodes()
        # benchmark "my benchmark":
            # echo loadAllNodes()
            # discard loadAllNodes()
        
    #     let t = parseAux(minionPath)
    #   # echo t

    #     for v in t.values:
    #         require(not v.name.contains("aux"))

    # test "Parsing eprime":


    #     var containsMarkerOccurrenceSet = false

    #     for p in parseEprime(eprimePath).values():
    #         if p of MarkerSet:
    #             # echo p
    #             # discard
    #             containsMarkerOccurrenceSet = true
    #       # echo d
    #     require(containsMarkerOccurrenceSet)


    # test "Pretty domains":

    #     for d in getPrettyDomainsOfNode(db, "1"):
    #         echo d
    #     # echo d
