import unittest, os, json, constants
import util/types
import util/init
import util/main



suite "init":
    let validPath = testDataPath & "gears"
    let db = findFiles(validPath)

    test "getCore":
        echo getCore(db).pretty()
