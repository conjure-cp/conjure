import os, db_sqlite, strutils, json

proc verifyDomains(obj: JsonNode, db: DbConn) =
    for row in db.fastRows(sql("select name, lower, upper from Domain where nodeId = ? "), obj["Node"]):
        let name = row[0]
        let lower = row[1]
        let upper = row[2]

        # echo name
        # echo row
        # echo upper

        let bounds = obj["Domains"][name].getElems()

        # echo bounds

        var jsonLower: string
        var jsonUpper: string


        if (bounds.len() > 1):
            var min = high(int)
            var max = low(int)

            for subArray in bounds:
                let first = subArray[0].getInt()
                let second = subArray[1].getInt()

                if first < min:
                    min = first
                
                if second > max:
                    max = second

            jsonLower = $min
            jsonUpper = $max
        else:
            jsonLower = $bounds[0].getElems()[0].getInt()
            jsonUpper = $bounds[0].getElems()[1].getInt()

        # echo jsonLower

        assert(lower == jsonLower)
        assert(upper == jsonUpper)


proc walkTree(obj: JsonNode, db: DbConn, wentLeft: bool, pId: string) =

    if (obj.hasKey("Node")):
        echo "-------------------------------------------------Node ", obj["Node"]
        verifyDomains(obj, db)

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
