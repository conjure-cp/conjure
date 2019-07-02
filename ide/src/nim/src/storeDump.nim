import db_sqlite, parseutils, os

if (paramCount() != 1):
    echo "usage ./storeDump <PathToDB>"

# echo paramCount(), " ", paramStr(1)

let db = open(paramStr(1), "", "", "")

var nodeCount: int
discard db.getValue(sql"select count(nodeId) from Node").parseInt(nodeCount)

for i in countup(0, nodeCount):

  let file = open("./store/" & $i & ".domainstore", fmWrite)

  for x in db.fastRows(sql"SELECT nodeId, name, storeDump FROM Domain where nodeId = ?", i):
    file.writeLine(x[1], ",",  x[2])

  file.close()