import unittest, os, json, constants
import util/types
import util/init
import util/main

suite "findFiles":
    test "Missing files":
        expect(InitException):
            let path = testDataPath & "extension/noDBFile"
            discard findFiles(path)

        expect(InitException):
            let path = testDataPath & "extension/noEprimeFile"
            discard findFiles(path)

        expect(InitException):
            let path = testDataPath & "extension/noMinionFile"
            discard findFiles(path)

    test "Multiple DB":
        expect(InitException):
            let path = testDataPath & "extension/multipleDBFiles"
            discard findFiles(path)

    test "Multiple Eprime":
        expect(InitException):
            let path = testDataPath & "extension/multipleEprimeFiles"
            discard findFiles(path)

    test "Multiple Minion":
        expect(InitException):
            let path = testDataPath & "extension/multipleMinionFiles"
            discard findFiles(path)