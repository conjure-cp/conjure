import unittest, json, constants, strutils
import ../src/util/types
import ../src/util/types
import ../src/util/main

suite "diff":
    test "firstDiff":
        let leftPath = testDataPath & "/diff/default-sacbounds/normal"
        let rightPath = testDataPath & "/diff/default-sacbounds/sacbounds"
        discard init(leftPath)
        discard init(rightPath)
        let nodeIds = getFirstDiffPoint(leftPath, rightPath)

        echo nodeIds

        check(nodeIds == @[(3, 3), (17, 6), (27, 9)])