import tables, os, db_sqlite, types, process, parseutils, times, strutils


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
        raise newException(CannotOpenDatabaseException, "No files with .db extension found")

    let db = open(dbFilePath, "", "", "")


    initParser(db, minionFilePath, eprimeFilePath)

    return db


proc getDecendants*(db: DbConn): CountTable[int] =

    let query = "select nodeId, parentId from Node where parentId >= 0;"

    var countTable = initCountTable[int]()
    var id2Parent = initTable[int, int]()
    var nodeId, parentId : int

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
