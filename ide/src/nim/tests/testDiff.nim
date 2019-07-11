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

        check(nodeIds == @[@[4, 4], @[21, 8], @[34, 11], @[44, 13], @[123, 35], @[137, 38], @[147, 41], @[192, 54], @[202, 57], @[224, 61]])

    test "12":
        let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
        let rightPath = testDataPath & "/diff/default-sacbounds-12/sacbounds"
        discard init(leftPath)
        discard init(rightPath)
        # let nodeIds = diff(leftPath, rightPath)
        let nodeIds = diff(rightPath, leftPath)

        echo nodeIds

        # check(nodeIds == @[@[4, 4], @[21, 8], @[34, 11], @[44, 13], @[123, 35], @[137, 38], @[147, 41], @[192, 54], @[202, 57], @[224, 61]])