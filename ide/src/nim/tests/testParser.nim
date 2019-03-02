import unittest, os, tables, strutils, db_sqlite, constants
import util/types, util/parser

suite "parser":

    var db = open("", "", "", "")

    test "aux":
        let path = testDataPath & "/sets/recursive/flagsFlagsFlags/model000001.eprime-minion"

        for expression in parseAux(path).values():

            check(not expression.name.contains("aux"))

    test "eprimeFFF":
        let path = testDataPath & "/sets/recursive/flagsFlagsFlags/model000001.eprime"
        let varList = @["x", "y", "z", "s"]

        for variable in parseEprime(db, path).values():

            # echo variable
            check(variable.name in varList)

            if (variable.name == "s"):
                check(variable of FlagSet)
                let s = FlagSet(variable)
                check(s.inner of FlagSet)
                check(s.inner.inner of FlagSet)

    test "eprimeEMF":
        let path = testDataPath & "/sets/recursive/explicitMarkerFlags/model000001.eprime"
        let varList = @["x", "y", "z", "s"]

        for variable in parseEprime(db,path).values():

            # echo variable
            check(variable.name in varList)

            if (variable.name == "s"):
                check(variable of ExplicitSet)
                let s = ExplicitSet(variable)
                check(s.cardinality == 2)

                check(s.inner of MarkerSet)

                check(s.inner.inner of FlagSet)



    test "eprimeFFO":
        let path = testDataPath & "/sets/recursive/flagsFlagsOccurrence/model000001.eprime"
        let varList = @["x", "y", "z", "s"]

        for variable in parseEprime(db,path).values():

            # echo variable
            check(variable.name in varList)

            if (variable.name == "s"):
                check(variable of FlagSet)
                let s = FlagSet(variable)

                check(s.inner of FlagSet)

                check(s.inner.inner of OccurrenceSet)

    test "eprimeMMD":
        db = open(testDataPath & "/sets/recursive/markerMarkerDummy/test.db", "", "", "")
        let path = testDataPath & "/sets/recursive/markerMarkerDummy/model000001.eprime"
        let varList = @["x", "y", "z", "s"]

        for variable in parseEprime(db, path).values():

            # echo variable
            check(variable.name in varList)

            if (variable.name == "s"):
                check(variable of MarkerSet)
                let s = MarkerSet(variable)

                check(s.inner of MarkerSet)

                check(s.inner.inner of DummySet)