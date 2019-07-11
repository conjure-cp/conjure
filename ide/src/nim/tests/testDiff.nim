import unittest, json, constants, strutils
import ../src/util/types
import ../src/util/types
import ../src/util/main

suite "diff":
    test "8":
        let leftPath = testDataPath & "/diff/default-sacbounds-8/normal"
        let rightPath = testDataPath & "/diff/default-sacbounds-8/sacbounds"
        discard init(leftPath)
        discard init(rightPath)
        let nodeIds = diff(leftPath, rightPath)

        echo nodeIds

        check(nodeIds == @[@[3, 3], @[17, 6], @[27, 9]])

    test "10":
        let leftPath = testDataPath & "/diff/default-sacbounds-10/normal"
        let rightPath = testDataPath & "/diff/default-sacbounds-10/sacbounds"
        discard init(leftPath)
        discard init(rightPath)
        let nodeIds = diff(leftPath, rightPath)

        echo nodeIds

        # check(nodeIds == @[@[3, 3], @[17, 6], @[27, 9]])