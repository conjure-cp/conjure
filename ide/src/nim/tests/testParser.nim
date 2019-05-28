import unittest, os, tables, strutils, db_sqlite, constants, sequtils
import util/types, util/parser

suite "parser":

    var db : DbConn

    test "aux":
        let path = testDataPath & "golomb/model000001-05.eprime-minion"
        let auxVars = toSeq(parseAux(path).values())
        
        check((auxVars).len() == 495)

        # echo auxVars.len()
        for expression in parseAux(path).values():
            check(not expression.name.contains("aux"))

    test "eprimeFFF":
        let path = testDataPath & "/sets/recursive/flagsFlagsFlags/model000001.eprime"
        db = open(testDataPath & "/sets/recursive/flagsFlagsFlags/test.db", "", "", "")
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
        db = open(testDataPath & "/sets/recursive/explicitMarkerFlags/test.db", "", "", "")
        let varList = @["x", "y", "z", "s"]

        for variable in parseEprime(db, path).values():

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
        db = open(testDataPath & "/sets/recursive/flagsFlagsOccurrence/test.db", "", "", "")
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