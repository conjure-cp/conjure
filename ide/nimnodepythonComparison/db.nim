import re, strutils, os, tables, json, db_sqlite, parseutils, sequtils


let db = open("/home/tom/minion-private/build/gears/test.db", "", "", "")
let query = "select nodeId, parentId from Node where parentId >= 0;"

var countTable = initCountTable[int]()
var id2Parent = initTable[int, int]()
var nodeId, parentId : int

for res in db.fastRows(sql(query)):
    discard res[0].parseInt(nodeId)
    discard res[1].parseInt(parentId)

    id2Parent[nodeId] = parentId


for nodeId in id2Parent.values():

    var n = nodeId

    while true:
        countTable.inc(n)
        if not id2Parent.hasKey(n):
            break
        else:
            n = id2Parent[n]

echo countTable[0]

        # echo nodeId
            

    # echo countTable
    # for key, value in countTable.pairs():
    #     echo key, ",", value

