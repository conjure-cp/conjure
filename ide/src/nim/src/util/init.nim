import tables, os, db_sqlite, types, process, parseutils, times, strutils, json


proc findFiles*(dirPath: string): DbConn =

    let current = getCurrentDir()
    setCurrentDir(dirPath)

    var minionFilePath: string
    var eprimeFilePath: string
    var dbFilePath: string

    for f in walkFiles("*.eprime-minion"):
        minionFilePath = absolutePath(f)
        break;

    for f in walkFiles("*.eprime"):
        eprimeFilePath = absolutePath(f)
        break;

    for f in walkFiles("*.db"):
        dbFilePath = absolutePath(f)
        break;

    setCurrentDir(current)

    if dbFilePath == "":
        raise newException(CannotOpenDatabaseException,
                "No files with .db extension found")

    let db = open(dbFilePath, "", "", "")

    initParser(db, minionFilePath, eprimeFilePath)

    return db


proc getDecendants*(db: DbConn): CountTable[int] =

    let query = "select nodeId, parentId from Node where parentId >= 0;"

    var countTable = initCountTable[int]()
    var id2Parent = initTable[int, int]()
    var nodeId, parentId: int

    for res in db.fastRows(sql(query)):
        discard res[0].parseInt(nodeId)
        discard res[1].parseInt(parentId)

        id2Parent[nodeId] = parentId


    for n in id2Parent.keys():
        var current = n

        while true:
            countTable.inc(current)
            if id2Parent.hasKey(current):
                current = id2Parent[current]
            else:
                break

    return countTable

proc getCore*(db: DbConn): JsonNode =

    let query = sql(

                        """with recursive
        correctPath(n) as (
        select nodeId from Node where isSolution = 1  
        union 
        select parentId  from Node, correctPath
            where nodeId=correctPath.n 
                    )
        select nodeId, parentId, branchingVariable, isLeftChild, value, isSolution from Node where nodeId in correctPath;""")


    for row1 in db.fastRows(query):
        var sol = false

        if (row1[5] == "1"):
            sol = true

        discard parseInt(row1[0], nId)
        var kids: seq[int]

        for row2 in db.fastRows(sql"select nodeId from Node where parentId = ?",
                row1[0]):
            discard parseInt(row2[0], childId)
            kids.add(childId)

        discard parseInt(row1[1], pId)

        let l = getLabel(getInitialVariables(), row1[2], row1[3], row1[4])
        let pL = getLabel(getInitialVariables(), row1[2], row1[3], row1[4],
                true)

        result.add(ParentChild(parentId: pId, nodeId: nId, label: l, prettyLabel: pL, children: kids, isLeftChild: parsebool(row1[ 3]), isSolution: sol, decendantCount: decTable[nId]))

