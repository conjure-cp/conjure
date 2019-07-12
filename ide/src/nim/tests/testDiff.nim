import unittest, json, constants, strutils, sequtils, sugar
import ../src/util/types
import ../src/util/types
import ../src/util/main

suite "diff":

    test "same":
        let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
        discard init(leftPath)
        let nodeIds = diff(leftPath, leftPath)
        check(nodeIds == newSeq[seq[int]]())

    test "8":
        let leftPath = testDataPath & "/diff/default-sacbounds-8/normal"
        let rightPath = testDataPath & "/diff/default-sacbounds-8/sacbounds"
        discard init(leftPath)
        discard init(rightPath)
        let nodeIds = diff(leftPath, rightPath)


        check(nodeIds == @[@[3, 3], @[17, 6], @[28, 10]])

    test "10":
        let leftPath = testDataPath & "/diff/default-sacbounds-10/normal"
        let rightPath = testDataPath & "/diff/default-sacbounds-10/sacbounds"
        discard init(leftPath)
        discard init(rightPath)
        let nodeIds = diff(leftPath, rightPath)

        check(nodeIds == @[@[4, 4], @[21, 8], @[34, 11], @[44, 13], @[54, 16], @[68, 19], @[78, 22], @[88, 25], @[98, 28], @[108, 31], @[123, 35], @[137, 38], @[147, 41], @[157, 44], @[167, 47], @[177, 50], @[192, 54], @[202, 57], @[212, 60], @[227, 64]])

    test "12":
        let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
        let rightPath = testDataPath & "/diff/default-sacbounds-12/sacbounds"
        discard init(leftPath)
        discard init(rightPath)
        let answer = @[@[5, 5], @[26, 10], @[41, 14], @[53, 17], @[62, 19], @[72, 22], @[89, 26], @[102, 29], @[112, 31], @[122, 34], @[135, 37], @[145, 40], @[155, 43], @[165, 46], @[175, 49], @[190, 53], @[208, 57], @[221, 60], @[231, 63], @[241, 66], @[255, 69], @[265, 72], @[275, 75], @[285, 78], @[296, 82]]
        # let nodeIds = diff(leftPath, rightPath)
        var nodeIds = diff(leftPath, rightPath)

        check(nodeIds == answer)
    
    test "flipped":
        let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
        let rightPath = testDataPath & "/diff/default-sacbounds-12/sacbounds"
        discard init(leftPath)
        discard init(rightPath)
        let answer = @[@[5, 5], @[26, 10], @[41, 14], @[53, 17], @[62, 19], @[72, 22], @[89, 26], @[102, 29], @[112, 31], @[122, 34], @[135, 37], @[145, 40], @[155, 43], @[165, 46], @[175, 49], @[190, 53], @[208, 57], @[221, 60], @[231, 63], @[241, 66], @[255, 69], @[265, 72], @[275, 75], @[285, 78], @[296, 82]]
        let flipped = answer.map(x => @[x[1], x[0]])

        let nodeIds = diff(rightPath, leftPath)
    
        check(nodeIds == flipped)

    test "contrived":
        let leftPath = testDataPath & "/diff/contrived/normal"
        let rightPath = testDataPath & "/diff/contrived/modified"
        discard init(leftPath)
        discard init(rightPath)
        let nodeIds = diff(leftPath, rightPath)
        check(nodeIds == @[@[32,32]])


        # check(nodeIds == @[@[3, 3], @[17, 6], @[28, 10]]