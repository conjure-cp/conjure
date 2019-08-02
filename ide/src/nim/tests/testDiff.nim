import unittest, json, constants, strutils, sequtils, sugar, os, times, strformat
import ../src/util/main
import ../src/util/diff

template benchmark(benchmarkName: string, code: untyped) =
  block:
    let t0 = epochTime()
    code
    let elapsed = epochTime() - t0
    let elapsedStr = elapsed.formatFloat(format = ffDecimal, precision = 3)
    echo "CPU Time [", benchmarkName, "] ", elapsedStr, "s"

# suite "diffHandler":
#   test "handleNotCached":
#     let leftPath = testDataPath & "diff/default-sacbounds-8/normal"
#     let rightPath = testDataPath & "diff/default-sacbounds-8/sacbounds"
#     discard init(leftPath)
#     discard init(rightPath)
#     let leftHash = "leftHash"
#     let rightHash = "rightHash"
#     discard diffHandler(leftPath, rightPath, leftHash, rightHash)
#     let fileName = fmt"{testDataPath}/diff/default-sacbounds-8/diffCaches/{leftHash}~{rightHash}.json"
#     check(fileExists(fileName))

#     let json = parseJson(readAll(open(fileName)))

#     echo json

#     let answer = %*[{"path": "0/1/2/3", "leftTreeId": 3, "rightTreeId": 3,
#         "descCount": 0, "highlightLeft": [4], "highlightRight": []}, {
#         "path": "0/1/2/3/7", "leftTreeId": 7, "rightTreeId": 4, "descCount": 0,
#         "highlightLeft": [8, 11], "highlightRight": []}, {"path": "0/1/2/16/17",
#         "leftTreeId": 17, "rightTreeId": 6, "descCount": 0, "highlightLeft": [
#         18], "highlightRight": []}, {"path": "0/1/2/16/17/21", "leftTreeId": 21,
#         "rightTreeId": 7, "descCount": 0, "highlightLeft": [22, 24],
#         "highlightRight": []}, {"path": "0/1/2/16/26/27", "leftTreeId": 27,
#         "rightTreeId": 9, "descCount": 1, "highlightLeft": [28, 30],
#         "highlightRight": [10]}]
#     check (json == answer)

#     removeFile(fileName)

#   test "handleCachedFlipped":
#     let leftPath = testDataPath & "diff/default-sacbounds-8/normal"
#     let rightPath = testDataPath & "diff/default-sacbounds-8/sacbounds"
#     discard init(leftPath)
#     discard init(rightPath)
#     let leftHash = "leftHash"
#     let rightHash = "rightHash"
#     discard diffHandler(leftPath, rightPath, leftHash, rightHash)
#     let fileName = fmt"{testDataPath}/diff/default-sacbounds-8/diffCaches/{leftHash}~{rightHash}.json"

#     check(fileExists(fileName))

#     let answer = %*[{"path": "", "leftTreeId": 3, "rightTreeId": 3,
#         "descCount": 0, "highlightLeft": [], "highlightRight": [4]}, {
#         "path": "", "leftTreeId": 4, "rightTreeId": 7, "descCount": 0,
#         "highlightLeft": [], "highlightRight": [8, 11]}, {"path": "",
#         "leftTreeId": 6, "rightTreeId": 17, "descCount": 0, "highlightLeft": [],
#         "highlightRight": [18]}, {"path": "", "leftTreeId": 7,
#         "rightTreeId": 21, "descCount": 0, "highlightLeft": [],
#         "highlightRight": [22, 24]}, {"path": "", "leftTreeId": 9,
#         "rightTreeId": 27, "descCount": 1, "highlightLeft": [10],
#         "highlightRight": [28, 30]}]

#     check(answer == diffHandler(leftPath, rightPath, rightHash, leftHash))

#     let flippedFileName = fmt"{testDataPath}/diff/default-sacbounds-8/diffCaches/{rightHash}~{leftHash}.json"
#     check(not fileExists(flippedFileName))

#     removeFile(fileName)


suite "domainsAreEqual":
  test "equal":
    let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
    discard init(leftPath)
    let db = getDB(leftPath)
    for i in countup(1, 100):
      check checkDomainsAreEqual([db, db], [$i, $i]) == true

  test "notequal":
    let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-10/normal"
    discard init(leftPath)
    let leftDB = getDB(leftPath)
    discard init(rightPath)
    let rightDB = getDB(rightPath)
    for i in countup(1, 100):
      check checkDomainsAreEqual([leftDB, rightDB], [$i, $i]) == false

  test "neq":
    let leftPath = testDataPath & "/diff/default-sacbounds-8/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-8/sacbounds"
    discard init(leftPath)
    let leftDB = getDB(leftPath)
    discard init(rightPath)
    let rightDB = getDB(rightPath)
    check checkDomainsAreEqual([leftDB, rightDB], [$4, $4]) == false

suite "descCountIncreases":
  test "10dc":

    let leftPath = testDataPath & "/diff/default-sacbounds-10/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-10/sacbounds"
    discard init(leftPath)
    let leftDB = getDB(leftPath)
    discard init(rightPath)
    let rightDB = getDB(rightPath)


# @[4, 4], @[9, 5], @[13, 6]
    var list = newSeq[DiffPoint]()
# @[5], @[10], @[14, 16]

    list.add(newDiffPoint("4", "4", @["5"], @[]))
    list.add(newDiffPoint("9", "5", @["10"], @[]))
    list.add(newDiffPoint("13", "6", @["14", "16"], @[]))
    # let point1 = newDiffPoint("4", "4", @[],@[])

    assignDescCountIncreases(leftDB, rightDB, list, true)

    check list[0].descCount == 0
    check list[1].descCount == 0
    check list[2].descCount == 0

  test "r10dc":

    let leftPath = testDataPath & "/diff/default-sacbounds-10/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-10/sacbounds"
    discard init(leftPath)
    let leftDB = getDB(leftPath)
    discard init(rightPath)
    let rightDB = getDB(rightPath)


# @[4, 4], @[9, 5], @[13, 6]
    var list = newSeq[DiffPoint]()
# @[5], @[10], @[14, 16]

    list.add(newDiffPoint("4", "4",@[], @["5"]))
    list.add(newDiffPoint("5", "9",  @[], @["10"]))
    list.add(newDiffPoint("6", "13",  @[], @["14", "16"]))
    # let point1 = newDiffPoint("4", "4", @[],@[])

    assignDescCountIncreases(rightDB, leftDB, list, true)

    check list[0].descCount == 4
    check list[1].descCount == 3
    check list[2].descCount == 6

suite "removeDuplicates":

  test "rd8":
    let leftPath = testDataPath & "/diff/default-sacbounds-8/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-8/sacbounds"
    let leftDB = getDB(leftPath)
    discard init(rightPath)
    let rightDB = getDB(rightPath)
    check removeDuplicates(leftDB, rightDB, [@["4", "7"], @["4"]]) == @[@["4"], @[]]

  test "rdfa":
    let leftPath = testDataPath & "/diff/default-findAllSols-8/normal"
    let rightPath = testDataPath & "/diff/default-findAllSols-8/findAllSols"
    let leftDB = getDB(leftPath)
    discard init(rightPath)
    let rightDB = getDB(rightPath)
    check removeDuplicates(leftDB, rightDB, [@["27"], @["27", "33"]]) == [@[],
        @["33"]]

suite "diff":

  test "same":
    let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
    discard init(leftPath)
    let d = diff(leftPath, leftPath)
    check(d.len() == 0)

  test "8":
    let leftPath = testDataPath & "/diff/default-sacbounds-8/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-8/sacbounds"
    discard init(leftPath)
    discard init(rightPath)

    let d = diff(leftPath, rightPath, false)
    echo d

    let diffNodeIds = d.map(x => @[x.leftTreeId, x.rightTreeId])
    let answers = @[@[3, 3], @[7, 4], @[17, 6], @[21, 7], @[27, 9]]

    check diffNodeIds == answers

    let highlightLeft = d.map(x => x.highlightLeft)
    let hL = @[@[4], @[8, 11], @[18], @[22, 24], @[28, 30]]

    check highlightLeft == hL

    let highlightRight = d.map(x => x.highlightRight)

    check highlightRight == @[@[], @[], @[], @[], @[10]]

    check d[0].descCount == 0
    check d[1].descCount == 0
    check d[2].descCount == 0
    check d[3].descCount == 0
    check d[4].descCount == 1

    # check d[0].path == "0/1/2/3"
    # check d[1].path == "0/1/2/3/7"
    # check d[2].path == "0/1/2/16/17"
    # check d[3].path == "0/1/2/16/17/21"
    # check d[4].path == "0/1/2/16/26/27"

    let flipped = diff(rightPath, leftPath, false)

    check flipped[0].descCount == 3
    check flipped[1].descCount == 8
    check flipped[2].descCount == 3
    check flipped[3].descCount == 4
    check flipped[4].descCount == 5

    # check flipped[0].path == "0/1/2/3"
    # check flipped[1].path == "0/1/2/3/4"
    # check flipped[2].path == "0/1/2/5/6"
    # check flipped[3].path == "0/1/2/5/6/7"
    # check flipped[4].path == "0/1/2/5/8/9"

  test "10":
    let leftPath = testDataPath & "/diff/default-sacbounds-10/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-10/sacbounds"
    discard init(leftPath)
    discard init(rightPath)

    let d = diff(leftPath, rightPath)
    let diffNodeIds = d.map(x => @[x.leftTreeId, x.rightTreeId])
    let answer = @[@[4, 4], @[9, 5], @[13, 6], @[21, 8], @[25, 9], @[34, 11], @[
        44, 13], @[43, 12], @[54, 16], @[58, 17], @[68, 19], @[72, 20], @[78,
        22], @[77, 21], @[88, 25], @[92, 26], @[98, 28], @[97, 27], @[108, 31],
        @[107, 30], @[106, 29], @[123, 35], @[127, 36], @[137, 38], @[141, 39],
        @[147, 41], @[146, 40], @[157, 44], @[161, 45], @[167, 47], @[166, 46],
        @[177, 50], @[176, 49], @[175, 48], @[192, 54], @[196, 55], @[202, 57],
        @[201, 56], @[212, 60], @[211, 59], @[210, 58], @[227, 64], @[226, 63],
        @[225, 62], @[224, 61]]

    check diffNodeIds == answer

    let highlightLeft = d.map(x => x.highlightLeft)
    let hL = @[@[5], @[10], @[14, 16], @[22], @[26, 28], @[35, 38], @[45, 47],
        @[49], @[55], @[59, 62], @[69], @[73, 75], @[79, 81], @[83], @[89], @[
        93, 95], @[99, 101], @[103], @[109, 111], @[113], @[116], @[124], @[128,
        131], @[138], @[142, 144], @[148, 150], @[152], @[158], @[162, 164], @[
        168, 170], @[172], @[178, 180], @[182], @[185], @[193], @[197, 199], @[
        203, 205], @[207], @[213, 215], @[217], @[220], @[228, 230], @[232], @[
        235], @[239]]

    check hL == highlightLeft

    let highlightRight = d.map(x => x.highlightRight)

    for x in highlightRight:
      check x.len() == 0




  test "12":
    let leftPath = testDataPath & "/diff/default-sacbounds-12/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-12/sacbounds"
    discard init(leftPath)
    discard init(rightPath)

    let d = diff(leftPath, rightPath)
    let diffNodeIds = d.map(x => @[x.leftTreeId, x.rightTreeId])


    let answer = @[@[5, 5], @[11, 6], @[16, 7], @[20, 8], @[26, 10], @[31, 11],
        @[35, 12], @[41, 14], @[45, 15], @[53, 17], @[62, 19], @[61, 18], @[72,
        22], @[77, 23], @[81, 24], @[89, 26], @[93, 27], @[102, 29], @[112, 31],
        @[111, 30], @[122, 34], @[126, 35], @[135, 37], @[139, 38], @[145, 40],
        @[144, 39], @[155, 43], @[159, 44], @[165, 46], @[164, 45], @[175, 49],
        @[174, 48], @[173, 47], @[190, 53], @[195, 54], @[199, 55], @[208, 57],
        @[212, 58], @[221, 60], @[225, 61], @[231, 63], @[230, 62], @[241, 66],
        @[245, 67], @[255, 69], @[259, 70], @[265, 72], @[264, 71], @[275, 75],
        @[279, 76], @[285, 78], @[284, 77], @[295, 81]]

    check diffNodeIds == answer

    let highlightLeft = d.map(x => x.highlightLeft)

    let hl = @[@[6], @[12], @[17], @[21, 23], @[27], @[32], @[36, 38], @[42], @[
        46, 48], @[54, 56], @[63, 65], @[67], @[73], @[78], @[82, 84], @[90], @[
        94, 96], @[103, 106], @[113, 115], @[117], @[123], @[127, 129], @[136],
        @[140, 142], @[146, 148], @[150], @[156], @[160, 162], @[166, 168], @[
        170], @[176, 178], @[180], @[183], @[191], @[196], @[200, 202], @[209],
        @[213, 215], @[222], @[226, 228], @[232, 234], @[236], @[242], @[246,
        249], @[256], @[260, 262], @[266, 268], @[270], @[276], @[280, 282], @[
        286, 288], @[290], @[296]]

    check hL == highlightLeft

    let highlightRight = d.map(x => x.highlightRight)

    let hR = @[@[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[82]]

    check hR == highlightRight

  test "16":
    let leftPath = testDataPath & "/diff/default-sacbounds-16/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-16/sacbounds"
    discard init(leftPath)
    discard init(rightPath)

    benchmark "poop":
      let d = diff(leftPath, rightPath)
      let diffNodeIds = d.map(x => @[x.leftTreeId, x.rightTreeId])

      let answer = @[@[7, 7], @[14, 8], @[20, 9], @[25, 10], @[29, 11], @[39,
          13], @[45, 14], @[50, 15], @[55, 16], @[65, 18], @[71, 19], @[76, 20],
          @[80, 21], @[86, 23], @[91, 24], @[95, 25], @[101, 27], @[105, 28], @[
          111, 30], @[110, 29], @[121, 33], @[127, 34], @[132, 35], @[137, 36],
          @[141, 37], @[147, 39], @[153, 40], @[158, 41], @[162, 42], @[168,
          44], @[173, 45], @[177, 46], @[183, 48], @[187, 49], @[193, 51], @[
          192, 50], @[203, 54], @[209, 55], @[214, 56], @[218, 57], @[224, 59],
          @[229, 60], @[233, 61], @[239, 63], @[243, 64], @[249, 66], @[248,
          65], @[260, 69], @[265, 70], @[269, 71], @[275, 73], @[279, 74], @[
          285, 76], @[284, 75], @[298, 79], @[302, 80], @[308, 82], @[307, 81],
          @[323, 85], @[322, 84], @[341, 89], @[340, 88], @[339, 87], @[356,
          93], @[362, 94], @[368, 95], @[373, 96], @[377, 97], @[383, 99], @[
          389, 100], @[394, 101], @[398, 102], @[404, 104], @[409, 105], @[413,
          106], @[419, 108], @[423, 109], @[429, 111], @[428, 110], @[440, 114],
          @[446, 115], @[451, 116], @[455, 117], @[461, 119], @[466, 120], @[
          470, 121], @[476, 123], @[480, 124], @[486, 126], @[485, 125], @[499,
          129], @[504, 130], @[508, 131], @[514, 133], @[518, 134], @[524, 136],
          @[523, 135], @[539, 139], @[543, 140], @[549, 142], @[548, 141], @[
          567, 146], @[576, 148], @[575, 147], @[586, 151], @[585, 150], @[584,
          149], @[601, 155], @[607, 156], @[612, 157], @[616, 158], @[622, 160],
          @[627, 161], @[631, 162], @[637, 164], @[641, 165], @[647, 167], @[
          646, 166], @[662, 170], @[667, 171], @[671, 172], @[677, 174], @[681,
          175], @[687, 177], @[686, 176], @[705, 181], @[709, 182], @[717, 184],
          @[726, 186], @[725, 185], @[736, 189], @[740, 190], @[746, 192], @[
          745, 191], @[756, 195], @[755, 194], @[754, 193], @[771, 199], @[776,
          200], @[780, 201], @[786, 203], @[790, 204], @[798, 206], @[807, 208],
          @[806, 207], @[817, 211], @[821, 212], @[830, 214], @[834, 215], @[
          840, 217], @[839, 216], @[850, 220], @[854, 221], @[860, 223], @[859,
          222], @[870, 226], @[869, 225], @[868, 224], @[885, 230], @[889, 231],
          @[893, 232], @[899, 234], @[903, 235], @[909, 237], @[908, 236], @[
          919, 240], @[923, 241], @[929, 243], @[928, 242], @[939, 246], @[938,
          245], @[937, 244], @[954, 250], @[958, 251], @[964, 253], @[963, 252],
          @[974, 256], @[973, 255], @[972, 254], @[989, 260], @[988, 259], @[
          987, 258], @[986, 257], @[1010, 265], @[1016, 266], @[1022, 267], @[
          1027, 268], @[1031, 269], @[1037, 271], @[1043, 272], @[1048, 273], @[
          1052, 274], @[1058, 276], @[1063, 277], @[1067, 278], @[1073, 280], @[
          1077, 281], @[1083, 283], @[1082, 282], @[1096, 286], @[1102, 287], @[
          1107, 288], @[1111, 289], @[1117, 291], @[1122, 292], @[1126, 293], @[
          1132, 295], @[1136, 296], @[1142, 298], @[1141, 297], @[1157, 301], @[
          1162, 302], @[1166, 303], @[1172, 305], @[1176, 306], @[1182, 308], @[
          1181, 307], @[1200, 312], @[1204, 313], @[1212, 315], @[1221, 317], @[
          1220, 316], @[1231, 320], @[1235, 321], @[1241, 323], @[1240, 322], @[
          1251, 326], @[1250, 325], @[1249, 324], @[1266, 330], @[1272, 331], @[
          1277, 332], @[1281, 333], @[1287, 335], @[1292, 336], @[1296, 337], @[
          1302, 339], @[1306, 340], @[1312, 342], @[1311, 341], @[1330, 346], @[
          1335, 347], @[1339, 348], @[1345, 350], @[1349, 351], @[1357, 353], @[
          1366, 355], @[1365, 354], @[1376, 358], @[1380, 359], @[1389, 361], @[
          1393, 362], @[1399, 364], @[1398, 363], @[1409, 367], @[1413, 368], @[
          1419, 370], @[1418, 369], @[1429, 373], @[1428, 372], @[1427, 371], @[
          1444, 377], @[1449, 378], @[1453, 379], @[1461, 381], @[1465, 382], @[
          1474, 384], @[1484, 386], @[1483, 385], @[1494, 389], @[1498, 390], @[
          1508, 392], @[1512, 393], @[1518, 395], @[1517, 394], @[1528, 398], @[
          1532, 399], @[1538, 401], @[1537, 400], @[1548, 404], @[1547, 403], @[
          1546, 402], @[1563, 408], @[1567, 409], @[1571, 410], @[1577, 412], @[
          1581, 413], @[1587, 415], @[1586, 414], @[1597, 418], @[1601, 419], @[
          1607, 421], @[1606, 420], @[1617, 424], @[1616, 423], @[1615, 422], @[
          1632, 428], @[1636, 429], @[1642, 431], @[1641, 430], @[1652, 434], @[
          1651, 433], @[1650, 432], @[1667, 438], @[1666, 437], @[1665, 436], @[
          1664, 435], @[1688, 443], @[1694, 444], @[1699, 445], @[1703, 446], @[
          1709, 448], @[1714, 449], @[1718, 450], @[1724, 452], @[1728, 453], @[
          1736, 455], @[1745, 457], @[1744, 456], @[1755, 460], @[1760, 461], @[
          1764, 462], @[1772, 464], @[1776, 465], @[1785, 467], @[1795, 469], @[
          1794, 468], @[1805, 472], @[1809, 473], @[1818, 475], @[1822, 476], @[
          1828, 478], @[1827, 477], @[1838, 481], @[1842, 482], @[1848, 484], @[
          1847, 483], @[1858, 487], @[1857, 486], @[1856, 485], @[1873, 491], @[
          1878, 492], @[1882, 493], @[1890, 495], @[1894, 496], @[1903, 498], @[
          1907, 499], @[1913, 501], @[1912, 500], @[1923, 504], @[1927, 505], @[
          1931, 506], @[1937, 508], @[1941, 509], @[1947, 511], @[1946, 510], @[
          1957, 514], @[1961, 515], @[1967, 517], @[1966, 516], @[1977, 520], @[
          1976, 519], @[1975, 518], @[1992, 524], @[1997, 525], @[2001, 526], @[
          2007, 528], @[2011, 529], @[2017, 531], @[2016, 530], @[2027, 534], @[
          2031, 535], @[2037, 537], @[2036, 536], @[2047, 540], @[2046, 539], @[
          2045, 538], @[2062, 544], @[2066, 545], @[2072, 547], @[2071, 546], @[
          2082, 550], @[2081, 549], @[2080, 548], @[2097, 554], @[2096, 553], @[
          2095, 552], @[2094, 551], @[2118, 559], @[2123, 560], @[2127, 561], @[
          2136, 563], @[2140, 564], @[2150, 566], @[2154, 567], @[2160, 569], @[
          2159, 568], @[2170, 572], @[2174, 573], @[2178, 574], @[2184, 576], @[
          2188, 577], @[2194, 579], @[2193, 578], @[2204, 582], @[2208, 583], @[
          2214, 585], @[2213, 584], @[2224, 588], @[2223, 587], @[2222, 586], @[
          2239, 592], @[2244, 593], @[2248, 594], @[2254, 596], @[2258, 597], @[
          2264, 599], @[2263, 598], @[2274, 602], @[2278, 603], @[2284, 605], @[
          2283, 604], @[2294, 608], @[2293, 607], @[2292, 606], @[2309, 612], @[
          2313, 613], @[2319, 615], @[2318, 614], @[2329, 618], @[2328, 617], @[
          2327, 616], @[2344, 622], @[2343, 621], @[2342, 620], @[2341, 619], @[
          2365, 627], @[2370, 628], @[2374, 629], @[2380, 631], @[2384, 632], @[
          2390, 634], @[2389, 633], @[2400, 637], @[2404, 638], @[2410, 640], @[
          2409, 639], @[2420, 643]]

      check diffNodeIds == answer

      let highlightLeft = d.map(x => x.highlightLeft)
      let hL = @[@[8], @[15], @[21], @[26], @[30, 33], @[40], @[46],
          @[51], @[56, 59], @[66], @[72], @[77], @[81, 83], @[87], @[92], @[96,
          98], @[102], @[106, 108], @[112, 114], @[116], @[122], @[128], @[133],
          @[138], @[142, 144], @[148], @[154], @[159], @[163, 165], @[169], @[
          174], @[178, 180], @[184], @[188, 190], @[194, 196], @[198], @[204],
          @[210], @[215], @[219, 221], @[225], @[230], @[234, 236], @[240], @[
          244, 246], @[250, 252], @[254], @[261], @[266], @[270, 272], @[276],
          @[280, 282], @[286, 288], @[290], @[299], @[303, 305], @[309, 311], @[
          313], @[324, 326], @[330], @[342, 344], @[346], @[349], @[357], @[
          363], @[369], @[374], @[378, 380], @[384], @[390], @[395], @[399,
          401], @[405], @[410], @[414, 416], @[420], @[424, 426], @[430, 432],
          @[434], @[441], @[447], @[452], @[456, 458], @[462], @[467], @[471,
          473], @[477], @[481, 483], @[487, 489], @[491], @[500], @[505], @[509,
          511], @[515], @[519, 521], @[525, 527], @[529], @[540], @[544, 546],
          @[550, 552], @[556], @[568, 570], @[577, 579], @[581], @[587, 589], @[
          591], @[594], @[602], @[608], @[613], @[617, 619], @[623], @[628], @[
          632, 634], @[638], @[642, 644], @[648, 650], @[652], @[663], @[668],
          @[672, 674], @[678], @[682, 684], @[688, 690], @[694], @[706], @[710,
          712], @[718, 720], @[727, 729], @[731], @[737], @[741, 743], @[747,
          749], @[751], @[757, 759], @[761], @[764], @[772], @[777], @[781,
          783], @[787], @[791, 793], @[799, 801], @[808, 810], @[812], @[818],
          @[822, 824], @[831], @[835, 837], @[841, 843], @[845], @[851], @[855,
          857], @[861, 863], @[865], @[871, 873], @[875], @[878], @[886], @[
          890], @[894, 896], @[900], @[904, 906], @[910, 912], @[914], @[920],
          @[924, 926], @[930, 932], @[934], @[940, 942], @[944], @[947], @[955],
          @[959, 961], @[965, 967], @[969], @[975, 977], @[979], @[982], @[990,
          992], @[994], @[997], @[1001], @[1011], @[1017], @[1023], @[1028], @[
          1032, 1034], @[1038], @[1044], @[1049], @[1053, 1055], @[1059], @[
          1064], @[1068, 1070], @[1074], @[1078, 1080], @[1084, 1086], @[1088],
          @[1097], @[1103], @[1108], @[1112, 1114], @[1118], @[1123], @[1127,
          1129], @[1133], @[1137, 1139], @[1143, 1145], @[1147], @[1158], @[
          1163], @[1167, 1169], @[1173], @[1177, 1179], @[1183, 1185], @[1189],
          @[1201], @[1205, 1207], @[1213, 1215], @[1222, 1224], @[1226], @[
          1232], @[1236, 1238], @[1242, 1244], @[1246], @[1252, 1254], @[1256],
          @[1259], @[1267], @[1273], @[1278], @[1282, 1284], @[1288], @[1293],
          @[1297, 1299], @[1303], @[1307, 1309], @[1313, 1315], @[1319], @[
          1331], @[1336], @[1340, 1342], @[1346], @[1350, 1352], @[1358, 1360],
          @[1367, 1369], @[1371], @[1377], @[1381, 1383], @[1390], @[1394,
          1396], @[1400, 1402], @[1404], @[1410], @[1414, 1416], @[1420, 1422],
          @[1424], @[1430, 1432], @[1434], @[1437], @[1445], @[1450], @[1454,
          1456], @[1462], @[1466, 1468], @[1475, 1478], @[1485, 1487], @[1489],
          @[1495], @[1499, 1502], @[1509], @[1513, 1515], @[1519, 1521], @[
          1523], @[1529], @[1533, 1535], @[1539, 1541], @[1543], @[1549, 1551],
          @[1553], @[1556], @[1564], @[1568], @[1572, 1574], @[1578], @[1582,
          1584], @[1588, 1590], @[1592], @[1598], @[1602, 1604], @[1608, 1610],
          @[1612], @[1618, 1620], @[1622], @[1625], @[1633], @[1637, 1639], @[
          1643, 1645], @[1647], @[1653, 1655], @[1657], @[1660], @[1668, 1670],
          @[1672], @[1675], @[1679], @[1689], @[1695], @[1700], @[1704, 1706],
          @[1710], @[1715], @[1719, 1721], @[1725], @[1729, 1731], @[1737,
          1739], @[1746, 1748], @[1750], @[1756], @[1761], @[1765, 1767], @[
          1773], @[1777, 1779], @[1786, 1789], @[1796, 1798], @[1800], @[1806],
          @[1810, 1812], @[1819], @[1823, 1825], @[1829, 1831], @[1833], @[
          1839], @[1843, 1845], @[1849, 1851], @[1853], @[1859, 1861], @[1863],
          @[1866], @[1874], @[1879], @[1883, 1885], @[1891], @[1895, 1897], @[
          1904], @[1908, 1910], @[1914, 1916], @[1918], @[1924], @[1928], @[
          1932, 1934], @[1938], @[1942, 1944], @[1948, 1950], @[1952], @[1958],
          @[1962, 1964], @[1968, 1970], @[1972], @[1978, 1980], @[1982], @[
          1985], @[1993], @[1998], @[2002, 2004], @[2008], @[2012, 2014], @[
          2018, 2020], @[2022], @[2028], @[2032, 2034], @[2038, 2040], @[2042],
          @[2048, 2050], @[2052], @[2055], @[2063], @[2067, 2069], @[2073,
          2075], @[2077], @[2083, 2085], @[2087], @[2090], @[2098, 2100], @[
          2102], @[2105], @[2109], @[2119], @[2124], @[2128, 2130], @[2137], @[
          2141, 2144], @[2151], @[2155, 2157], @[2161, 2163], @[2165], @[2171],
          @[2175], @[2179, 2181], @[2185], @[2189, 2191], @[2195, 2197], @[
          2199], @[2205], @[2209, 2211], @[2215, 2217], @[2219], @[2225, 2227],
          @[2229], @[2232], @[2240], @[2245], @[2249, 2251], @[2255], @[2259,
          2261], @[2265, 2267], @[2269], @[2275], @[2279, 2281], @[2285, 2287],
          @[2289], @[2295, 2297], @[2299], @[2302], @[2310], @[2314, 2316], @[
          2320, 2322], @[2324], @[2330, 2332], @[2334], @[2337], @[2345, 2347],
          @[2349], @[2352], @[2356], @[2366], @[2371], @[2375, 2377], @[2381],
          @[2385, 2387], @[2391, 2393], @[2395], @[2401], @[2405, 2407], @[2411,
          2413], @[2415], @[2421]]

      check highlightLeft == hL

      let highlightRight = d.map(x => x.highlightRight)

      let hR = @[@[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[
        ], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[86], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[143], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[
        178], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[309], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[343], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[], @[],
        @[], @[644]]

      check highlightRight == hR

  test "flipped":
    let leftPath = testDataPath & "/diff/default-sacbounds-8/normal"
    let rightPath = testDataPath & "/diff/default-sacbounds-8/sacbounds"
    discard init(leftPath)
    discard init(rightPath)

    let d = diff(leftPath, rightPath)
    let flipped = d.map(x => newDiffPoint($x.rightTreeId, $x.leftTreeId,
        x.highlightRight.map(y => $y), x.highlightLeft.map(y => $y),
            x.descCount, x.leftPath))

    let rTL = diff(rightPath, leftPath)

    echo rTL

    for i in countUp(0, rTL.len() - 1):
      check rTL[i].leftTreeId == flipped[i].leftTreeId
      check rTL[i].rightTreeId == flipped[i].rightTreeId
      check rTL[i].highlightLeft == flipped[i].highlightLeft
      check rTL[i].highlightRight == flipped[i].highlightRight

  test "reps":
    let leftPath = testDataPath & "/diff/differentReps/ex-8"
    let rightPath = testDataPath & "/diff/differentReps/oc-8"
    discard init(leftPath)
    discard init(rightPath)
    let d = diff(leftPath, rightPath)
    check(d[0].leftTreeId == -1)
    check(d[0].rightTreeId == -1)

    echo d[0].descCount == 0
    # check(d.augmentedIds == newSeq[seq[int]](2))

  test "findAllSols":
    let leftPath = testDataPath & "/diff/default-findAllSols-8/normal"
    let rightPath = testDataPath & "/diff/default-findAllSols-8/findAllSols"
    discard init(leftPath)
    discard init(rightPath)

    let d = diff(leftPath, rightPath, false)

    check(d[0].leftTreeId == 26)
    check(d[0].rightTreeId == 26)
    check(d[0].highlightLeft.len() == 0)
    check(d[0].highlightRight == @[33])
    # check(d.diffLocations == @[@[32, 32]])
    check(d[1].leftTreeId == 1)
    check(d[1].rightTreeId == 1)
    check(d[1].highlightLeft.len() == 0)
    check(d[1].highlightRight == @[36])


    check d[0].descCount == 3
    check d[1].descCount == 35

    echo d

  test "symmBreak":
    let leftPath = testDataPath & "/diff/default-symmBreak-8/normal"
    let rightPath = testDataPath & "/diff/default-symmBreak-8/symmBreakNoOptimisation"
    discard init(leftPath)
    discard init(rightPath)
    let d = diff(leftPath, rightPath, false)

    check(d[0].leftTreeId == 0)
    check(d[0].rightTreeId == 0)
    check(d[0].highlightLeft == @[1])
    check(d[0].highlightRight == @[1])

    check d[0].descCount == 10
