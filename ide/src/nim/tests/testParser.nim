import unittest, os, tables, strutils
import util/types, util/parser

suite "parser":

    test "aux":
        let path = absolutePath("../test/testData/sets/recursive/flagsFlagsFlags/model000001.eprime-minion")

        for expression in parseAux(path).values():

            check(not expression.name.contains("aux"))

    test "eprimeFFF":
        let path = absolutePath("../test/testData/sets/recursive/flagsFlagsFlags/model000001.eprime")
        let varList = @["x", "y", "z", "s"]

        for variable in parseEprime(path).values():

            echo variable
            check(variable.name in varList)

            if (variable.name == "s"):
                check(variable of FlagSet)
                let s = cast[FlagSet](variable)
                check(s.inner of FlagSet)
                check(s.inner.inner of FlagSet)

    test "eprimeEMF":
        let path = absolutePath("../test/testData/sets/recursive/explicitMarkerFlags/model000001.eprime")
        let varList = @["x", "y", "z", "s"]

        for variable in parseEprime(path).values():

            echo variable
            check(variable.name in varList)

            if (variable.name == "s"):
                check(variable of ExplicitSet)
                let s = cast[ExplicitSet](variable)
                check(s.cardinality == 2)

                check(s.inner of MarkerSet)

                check(s.inner.inner of FlagSet)



    test "eprimeFFO":
        let path = absolutePath("../test/testData/sets/recursive/flagsFlagsOccurrence/model000001.eprime")
        let varList = @["x", "y", "z", "s"]

        for variable in parseEprime(path).values():

            echo variable
            check(variable.name in varList)

            if (variable.name == "s"):
                check(variable of FlagSet)
                let s = cast[FlagSet](variable)

                check(s.inner of FlagSet)

                check(s.inner.inner of OccurrenceSet)

    test "eprimeMMD":
        let path = absolutePath("../test/testData/sets/recursive/markerMarkerDummy/model000001.eprime")
        let varList = @["x", "y", "z", "s"]

        for variable in parseEprime(path).values():

            echo variable
            check(variable.name in varList)

            if (variable.name == "s"):
                check(variable of MarkerSet)
                let s = cast[MarkerSet](variable)

                check(s.inner of MarkerSet)

                check(s.inner.inner of DummySet)

    test "eprimeGolomb":
        let path = "/home/tom/EssenceCatalog/problems/csplib-prob006/conjure-output/model000001.eprime";
        let varList = @["x", "y", "z", "s"]

        for variable in parseEprime(path).values():

            echo variable
            check(variable.name in varList)

            if (variable.name == "s"):
                check(variable of MarkerSet)
                let s = cast[MarkerSet](variable)

                check(s.inner of MarkerSet)

                check(s.inner.inner of DummySet)