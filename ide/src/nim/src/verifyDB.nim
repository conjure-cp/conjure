import  os, db_sqlite, strutils, json

proc walkTree(obj: JsonNode, db: DbConn, wentLeft: bool, pId: string) =

    if (obj.hasKey("Node")):
        echo obj["Node"]

        let row = db.getRow(sql("select * from Node where nodeId = ? "), obj["Node"])

        let nodeId = row[0]
        let parentId = row[1]
        let isLeftChild = row[2]
        let branchingVariable = row[3]
        let value = row[4]
        let isSolution = row[5]

        assert(pId == parentId)

        assert(nodeId == $obj["Node"].getInt())

        assert(parseBool(isLeftChild) == wentLeft)

        if (obj.hasKey("branchVar")):
            assert(obj["branchVar"].getStr() == branchingVariable)

        if (obj.hasKey("branchVal")):
            assert($obj["branchVal"].getInt() == value)

        if (obj.hasKey("isSolution")):
            assert(isSolution == "1")
            
    # echo obj
    if (obj.hasKey("left")):
        walkTree(obj["left"], db, true, $obj["Node"].getInt())

    if (obj.hasKey("right")):
        walkTree(obj["right"], db, false, $obj["Node"].getInt())


if paramCount() != 2:
    echo "This program requires 2 arguments, <JSON File Path> <DB File Path>"
    quit()

let obj = parseJson(readFile(paramStr(1)))
let db = open(paramStr(2), "", "", "")

walkTree(obj, db, true, "0")
