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
        # check(nodeIds == @[@[5, 5], @[26, 10], @[41, 14], @[53, 17], @[62, 19], @[72, 22], @[89, 26], @[102, 29], @[112, 31], @[122, 34], @[135, 37], @[145, 40], @[155, 43], @[165, 46], @[175, 49], @[190, 53], @[208, 57], @[221, 60], @[231, 63], @[241, 66], @[255, 69], @[265, 72], @[275, 75], @[285, 78], @[295, 81]])


        # check(nodeIds == @[@[4, 4], @[21, 8], @[34, 11], @[44, 13], @[123, 35], @[137, 38], @[147, 41], @[192, 54], @[202, 57], @[224, 61]])