import unittest, json, constants, strutils, sequtils, sugar, os, times, strformat
import ../src/util/main

template benchmark(benchmarkName: string, code: untyped) =
  block:
    let t0 = epochTime()
    code
    let elapsed = epochTime() - t0
    let elapsedStr = elapsed.formatFloat(format = ffDecimal, precision = 3)
    echo "CPU Time [", benchmarkName, "] ", elapsedStr, "s"


suite "diffHandler":
  test "handleNotCached":
    let leftPath = testDataPath & "diff/default-sacbounds-8/normal"
    let rightPath = testDataPath & "diff/default-sacbounds-8/sacbounds"
    discard init(leftPath)
    discard init(rightPath)
    let leftHash = "leftHash"
    let rightHash = "rightHash"
    discard diffHandler(leftPath, rightPath, leftHash, rightHash)
    let fileName = fmt"{testDataPath}/diff/default-sacbounds-8/diffCaches/{leftHash}~{rightHash}.json"
    check(fileExists(fileName))

    let json = parseJson(readAll(open(fileName)))
    # echo json
    check (json == %*{"diffLocations": [[3, 3], [17, 6], [27, 9]],
        "augmentedIds": [[], []]})

    removeFile(fileName)

  test "handleCachedFlipped":
    let leftPath = testDataPath & "diff/default-sacbounds-8/normal"
    let rightPath = testDataPath & "diff/default-sacbounds-8/sacbounds"
    discard init(leftPath)
    discard init(rightPath)
    let leftHash = "leftHash"
    let rightHash = "rightHash"
    discard diffHandler(leftPath, rightPath, leftHash, rightHash)
    let fileName = fmt"{testDataPath}/diff/default-sacbounds-8/diffCaches/{leftHash}~{rightHash}.json"

    check(fileExists(fileName))

    let expected = %*{"diffLocations": [[3, 3], [6, 17], [9, 27]],
            "augmentedIds": []}

    check(expected["diffLocations"] == diffHandler(leftPath, rightPath,
            rightHash, leftHash))

    let flippedFileName = fmt"{testDataPath}/diff/default-sacbounds-8/diffCaches/{rightHash}~{leftHash}.json"
    check(not fileExists(flippedFileName))

    removeFile(fileName)


suite "domainsAreEqual":
  test "equal":
    let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
    discard init(leftPath)
    for i in countup(1, 100):
      check checkDomainsAreEqual([leftPath, leftPath], [$i, $i]) == true

  test "notequal":
    let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-10/normal"
    discard init(leftPath)
    discard init(rightPath)
    for i in countup(1, 100):
      check checkDomainsAreEqual([leftPath, rightPath], [$i, $i]) == false

suite "findAugNodes":
  test "10aug":
    let leftPath = testDataPath & "/diff/default-sacbounds-10/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-10/sacbounds"
    discard init(leftPath)
    discard init(rightPath)

    let diffLocs = @[@[4, 4], @[21, 8], @[34, 11], @[44, 13], @[54, 16], @[
            68, 19], @[78, 22], @[88, 25], @[98, 28], @[108, 31], @[123,
                    35], @[
            137, 38], @[147, 41], @[157, 44], @[167, 47], @[177, 50], @[192,
            54], @[202, 57], @[212, 60], @[227, 64]]


    let res = getAugs(leftPath, rightPath, diffLocs)
    echo res

    check(res[0] == @[49, 83, 103, 113, 116, 152, 172, 182, 185, 207, 217, 220,
        232, 235, 239])
    check(res[1].len() == 0)


suite "diff":

  test "same":
    let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
    discard init(leftPath)
    # let nodeIds = diff(leftPath, leftPath).diffLocations
    # check(nodeIds == newSeq[seq[int]]())

  test "8":
    let leftPath = testDataPath & "/diff/default-sacbounds-8/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-8/sacbounds"
    discard init(leftPath)
    discard init(rightPath)

    let answers = @[@[3, 3], @[17, 6], @[27, 9]]
    let d = findDiffLocationsBoyo(leftPath, rightPath)
    echo d
    # echo d.diffLocations

    # check(d.diffLocations == answers)
    # check(d.augmentedIds == newSeq[seq[int]](2))
    # check(d.augmentedIds == newSeq[int]())

  test "10":
    let leftPath = testDataPath & "/diff/default-sacbounds-10/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-10/sacbounds"
    discard init(leftPath)
    discard init(rightPath)

    let answer = @[@[4, 4], @[21, 8], @[34, 11], @[44, 13], @[54, 16], @[68,
            19], @[78, 22], @[88, 25], @[98, 28], @[108, 31], @[123, 35], @[
            137, 38], @[147, 41], @[157, 44], @[167, 47], @[177, 50], @[192,
            54], @[202, 57], @[212, 60], @[227, 64]]

    let d = findDiffLocationsBoyo(leftPath, rightPath)
    echo d

    

    # check(d.diffLocations == answer)
    # check(d.augmentedIds == @[@[49, 83, 103, 113, 116, 152, 172, 182, 185, 207,
    #     217, 220, 232, 235, 239], newSeq[int]()])


  test "12":
    let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-12/sacbounds"
    discard init(leftPath)
    discard init(rightPath)
    let answer = @[@[5, 5], @[26, 10], @[41, 14], @[53, 17], @[62, 19], @[
            72, 22], @[89, 26], @[102, 29], @[112, 31], @[122, 34], @[135,
            37], @[145, 40], @[155, 43], @[165, 46], @[175, 49], @[190, 53],
            @[208, 57], @[221, 60], @[231, 63], @[241, 66], @[255, 69], @[
            265, 72], @[275, 75], @[285, 78], @[295, 81]]
    # let nodeIds = diff(leftPath, rightPath)
    # let d = diff(leftPath, rightPath)
    # check(d.diffLocations == answer)
    # check(d.augmentedIds == @[@[67, 117, 150, 170, 180, 183, 236, 270, 290], @[]])

  test "16":
    let leftPath = testDataPath & "/diff/default-sacbounds-16/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-16/sacbounds"
    discard init(leftPath)
    discard init(rightPath)

    let answer = @[@[7, 7], @[39, 13], @[65, 18], @[86, 23], @[101, 27], @[
            111, 30], @[121, 33], @[147, 39], @[168, 44], @[183, 48], @[193,
            51], @[203, 54], @[224, 59], @[239, 63], @[249, 66], @[260, 69],
            @[275, 73], @[285, 76], @[298, 79], @[308, 82], @[323, 85], @[
            341, 89], @[356, 93], @[383, 99], @[404, 104], @[419, 108], @[
            429, 111], @[440, 114], @[461, 119], @[476, 123], @[486, 126],
            @[499, 129], @[514, 133], @[524, 136], @[539, 139], @[549, 142],
            @[567, 146], @[576, 148], @[586, 151], @[601, 155], @[622, 160],
            @[637, 164], @[647, 167], @[662, 170], @[677, 174], @[687, 177],
            @[705, 181], @[717, 184], @[726, 186], @[736, 189], @[746, 192],
            @[756, 195], @[771, 199], @[786, 203], @[798, 206], @[807, 208],
            @[817, 211], @[830, 214], @[840, 217], @[850, 220], @[860, 223],
            @[870, 226], @[885, 230], @[899, 234], @[909, 237], @[919, 240],
            @[929, 243], @[939, 246], @[954, 250], @[964, 253], @[974, 256],
            @[989, 260], @[1010, 265], @[1037, 271], @[1058, 276], @[1073,
            280], @[1083, 283], @[1096, 286], @[1117, 291], @[1132, 295], @[
            1142, 298], @[1157, 301], @[1172, 305], @[1182, 308], @[1200,
            312], @[1212, 315], @[1221, 317], @[1231, 320], @[1241, 323], @[
            1251, 326], @[1266, 330], @[1287, 335], @[1302, 339], @[1312,
            342], @[1330, 346], @[1345, 350], @[1357, 353], @[1366, 355], @[
            1376, 358], @[1389, 361], @[1399, 364], @[1409, 367], @[1419,
            370], @[1429, 373], @[1444, 377], @[1461, 381], @[1474, 384], @[
            1484, 386], @[1494, 389], @[1508, 392], @[1518, 395], @[1528,
            398], @[1538, 401], @[1548, 404], @[1563, 408], @[1577, 412], @[
            1587, 415], @[1597, 418], @[1607, 421], @[1617, 424], @[1632,
            428], @[1642, 431], @[1652, 434], @[1667, 438], @[1688, 443], @[
            1709, 448], @[1724, 452], @[1736, 455], @[1745, 457], @[1755,
            460], @[1772, 464], @[1785, 467], @[1795, 469], @[1805, 472], @[
            1818, 475], @[1828, 478], @[1838, 481], @[1848, 484], @[1858,
            487], @[1873, 491], @[1890, 495], @[1903, 498], @[1913, 501], @[
            1923, 504], @[1937, 508], @[1947, 511], @[1957, 514], @[1967,
            517], @[1977, 520], @[1992, 524], @[2007, 528], @[2017, 531], @[
            2027, 534], @[2037, 537], @[2047, 540], @[2062, 544], @[2072,
            547], @[2082, 550], @[2097, 554], @[2118, 559], @[2136, 563], @[
            2150, 566], @[2160, 569], @[2170, 572], @[2184, 576], @[2194,
            579], @[2204, 582], @[2214, 585], @[2224, 588], @[2239, 592], @[
            2254, 596], @[2264, 599], @[2274, 602], @[2284, 605], @[2294,
            608], @[2309, 612], @[2319, 615], @[2329, 618], @[2344, 622], @[
            2365, 627], @[2380, 631], @[2390, 634], @[2400, 637], @[2410,
            640], @[2420, 643]]

    benchmark "poop":
        let d = findDiffLocationsBoyo(leftPath, rightPath)
        echo d

    # echo d.diffLocations[35..^1]

    # check(d.diffLocations == answer)

    # let correctAugs = @[@[290, 313, 346, 349, 434, 491, 529, 556, 581, 591, 594,
    #     694, 731, 751, 761, 764, 812, 845, 865, 875, 878, 914, 934, 944, 969,
    #     979, 982, 994, 997, 1001, 1088, 1147, 1189, 1226, 1246, 1256, 1259,
    #     1319, 1371, 1404, 1424, 1434, 1437, 1489, 1523, 1543, 1553, 1556, 1592,
    #     1612, 1622, 1625, 1647, 1657, 1660, 1672, 1675, 1679, 1750, 1800, 1833,
    #     1853, 1863, 1866, 1918, 1952, 1972, 1982, 1985, 2022, 2042, 2052, 2055,
    #     2077, 2087, 2090, 2102, 2105, 2109, 2165, 2199, 2219, 2229, 2232, 2269,
    #     2289, 2299, 2302, 2324, 2334, 2337, 2349, 2352, 2356, 2395, 2415], @[
    #     143, 178, 309, 343]]

    # check(d.augmentedIds == correctAugs)
      # check(d.augmentedIds == newSeq[int]())

  test "flipped":
    let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-12/sacbounds"
    discard init(leftPath)
    discard init(rightPath)
    let answer = @[@[5, 5], @[26, 10], @[41, 14], @[53, 17], @[62, 19], @[
            72, 22], @[89, 26], @[102, 29], @[112, 31], @[122, 34], @[135,
            37], @[145, 40], @[155, 43], @[165, 46], @[175, 49], @[190, 53],
            @[208, 57], @[221, 60], @[231, 63], @[241, 66], @[255, 69], @[
            265, 72], @[275, 75], @[285, 78], @[295, 81]]
    let flipped = answer.map(x => @[x[1], x[0]])

    # let d = diff(rightPath, leftPath)
    # check(d.diffLocations == flipped)
    # check(d.augmentedIds == @[@[], @[67, 117, 150, 170, 180, 183, 236, 270, 290]])

    # check(d.augmentedIds == newSeq[int]())

  test "reps":
    let leftPath = testDataPath & "/diff/differentReps/ex-8"
    let rightPath = testDataPath & "/diff/differentReps/oc-8"
    discard init(leftPath)
    discard init(rightPath)
    # let d = diff(leftPath, rightPath)
    # check(d.diffLocations == @[@[-1, -1]])
    # check(d.augmentedIds == newSeq[seq[int]](2))

  test "findAllSols":
    let leftPath = testDataPath & "/diff/default-findAllSols-8/normal"
    let rightPath = testDataPath & "/diff/default-findAllSols-8/findAllSols"
    discard init(leftPath)
    discard init(rightPath)
    # let d = diff(leftPath, rightPath, true)
    # check(d.diffLocations == @[@[32, 32]])
    # check(d.augmentedIds == @[newSeq[int](), @[33, 36]])


  test "symmBreak":
    let leftPath = testDataPath & "/diff/default-symmBreak-8/normal"
    let rightPath = testDataPath & "/diff/default-symmBreak-8/symmBreakNoOptimisation"
    discard init(leftPath)
    discard init(rightPath)
    # let d = diff(leftPath, rightPath, true)
    # echo d.diffLocations
    # echo d.augmentedIds
    # check(d.diffLocations == @[@[0, 0]])
    # check(d.augmentedIds == newSeq[seq[int]](2))

    # check(d.augmentedIds == newSeq[int]())

  # test "contrived":
  #     let leftPath = testDataPath & "/diff/contrived/normal"
  #     let rightPath = testDataPath & "/diff/contrived/modified"
  #     discard init(leftPath)
  #     discard init(rightPath)
  #     let nodeIds = diff(leftPath, rightPath)
  #     check(nodeIds == @[@[32,32]])
