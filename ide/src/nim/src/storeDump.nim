import db_sqlite, parseutils

let db = open("out.db", "", "", "")

var nodeCount: int
discard db.getValue(sql"select count(nodeId) from Node").parseInt(nodeCount)

for i in countup(0, nodeCount):

  let file = open("./store/" & $i & ".domainstore", fmWrite)

  for x in db.fastRows(sql"SELECT nodeId, name, storeDump FROM Domain where nodeId = ?", i):
    file.writeLine(x[1], ",",  x[2])

  file.close()

echo nodeCount