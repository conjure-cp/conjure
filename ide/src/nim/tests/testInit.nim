import unittest, os, json, constants, db_sqlite, tables, parseutils
import ../src/util/types
import ../src/util/init
import ../src/util/main

# suite "findFiles":
#     test "everythingOkay":
#         let path = testDataPath & "sets/dummy"
#         discard findFiles(path)

#     test "Missing files":
#         expect(InitException):
#             let path = testDataPath & "extension/noDBFile"
#             discard findFiles(path)

#         expect(InitException):
#             let path = testDataPath & "extension/noEprimeFile"
#             discard findFiles(path)

#         expect(InitException):
#             let path = testDataPath & "extension/noMinionFile"
#             discard findFiles(path)

#     test "Multiple DB":
#         expect(InitException):
#             let path = testDataPath & "extension/multipleDBFiles"
#             discard findFiles(path)

#     test "Multiple Eprime":
#         expect(InitException):
#             let path = testDataPath & "extension/multipleEprimeFiles"
#             discard findFiles(path)

#     test "Multiple Minion":
#         expect(InitException):
#             let path = testDataPath & "extension/multipleMinionFiles"
#             discard findFiles(path)

# suite "path":
#     test "1":
#         let path = testDataPath & "gears1000"
#         let (db, _) = findFiles(path)
#         expect(DbError):
#             let val = db.getValue(sql"select path from Node where nodeId = 0")
#             echo "hello boyo"

#         discard init(path)
#         let val = db.getValue(sql"select path from Node where nodeId = 0")

# suite "makePaths":
#     test "1":
#         # let path = testDataPath & "gears1000"
#         let path = "/home/tom/EssenceCatalog/problems/csplib-prob049/vscodeExtensionCache/Wed__10_Jul_2019_14:18:17_GMT_Config1_4ecgUyhgB"
#         let (db, _) = findFiles(path)
#         writePaths(db )




        

        # init(path)


# suite "descendants":
#     test "calculationIsCorrect":
#         # let path = "/cs/home/to26/EssenceCatalog/problems/csplib-prob006/GolombRuler-04-conjure-output"
#         let path = testDataPath & "golomb"
#         let (db, _ = findFiles(path)
#         let t = getDescendants(db)
#         let totalNodes = db.getValue(sql"select count(nodeId) from Node")
#         check($(t[0]+1) == totalNodes)

#         var leafId: int
#         for leaf in db.fastRows(sql"select nodeId from Node where nodeId not in (select parentId from Node) "):
#             discard leaf[0].parseInt(leafId)
#             check(t[leafId] == 0)
