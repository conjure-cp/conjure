import unittest, os, json, constants
import util/types
import util/init
import util/main



suite "init":

    test "gears93":
        let validPath = testDataPath & "gears93"
        let db = findFiles(validPath)
        echo getCore(db, getDecendants(db))["tree"]

    test "gears94":
        let validPath = testDataPath & "gears94"
        let db = findFiles(validPath)
        echo getCore(db, getDecendants(db))["tree"]

    
