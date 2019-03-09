import tables, os, db_sqlite, types, process, parseutils, times, strutils, json, sequtils
import jsonify
import process
import branchingCondition

proc findFiles*(dirPath: string): DbConn =

    let current = getCurrentDir()
    setCurrentDir(dirPath)

    var minionFilePath: string
    var eprimeFilePath: string
    var dbFilePath: string

    let minionFiles = toSeq(walkFiles("*.eprime-minion"))
    if (minionFiles.len() == 0):
        raise newException(InitException, "No minion file found!")
    if (minionFiles.len() > 1):
        raise newException(InitException, "More than one minion file found!")

    let eprimeFiles = toSeq(walkFiles("*.eprime"))
    if (eprimeFiles.len() == 0):
        raise newException(InitException, "No eprime file found!")
    if (eprimeFiles.len() > 1):
        raise newException(InitException, "More than one eprime file found!")
    
    let dbFiles = toSeq(walkFiles("*.db"))
    if (dbFiles.len() == 0):
        raise newException(InitException, "No db file found!")
    if (dbFiles.len() > 1):
        raise newException(InitException, "More than one db file found!")

    minionFilePath = absolutePath(minionFiles[0])
    eprimeFilePath = absolutePath(eprimeFiles[0])
    dbFilePath = absolutePath(dbFiles[0])

    setCurrentDir(current)

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


let solutionQuery = sql("""with recursive
        correctPath(n) as (
        select nodeId from Node where isSolution = 1  
        union 
        select parentId  from Node, correctPath
            where nodeId=correctPath.n 
                    )
        select nodeId, parentId, branchingVariable, isLeftChild, value, isSolution from Node where nodeId in correctPath;""")

let solutionFailedQuery = sql("""with recursive
    correctPath(n) as (
    select nodeId from Node where isSolution = 1  
    union 
    select parentId  from Node, correctPath
        where nodeId=correctPath.n 
                )
    select nodeId, parentId, branchingVariable, isLeftChild, value, isSolution from Node where nodeId not in correctPath and parentId in correctPath;""")

let noSolutionQuery = sql("""with recursive
        correctPath(n) as (
        select max(nodeId) from Node 
        union 
        select parentId  from Node, correctPath
            where nodeId=correctPath.n 
                    )
        select nodeId, parentId, branchingVariable, isLeftChild, value, isSolution from Node where nodeId in correctPath;""")

let noSolutionFailedQuery = sql("""with recursive
    correctPath(n) as (
    select max(nodeId) from Node
    union 
    select parentId  from Node, correctPath
        where nodeId=correctPath.n 
                )
    select nodeId, parentId, branchingVariable, isLeftChild, value, isSolution from Node where nodeId not in correctPath and parentId in correctPath;""")


proc processQuery( db: DbConn, query: SqlQuery, list: var seq[Node], decTable: CountTable[int]): seq[int] =

    var nId, pId: int

    for row1 in db.fastRows(query):

        discard parseInt(row1[0], nId)
        discard parseInt(row1[1], pId)

        result.add(nId)

        var childCount : int
        discard db.getValue(sql"select count(nodeId) from Node where parentId = ?", row1[0]).parseInt(childCount)

        let vName = db.getValue(sql"select branchingVariable from Node where nodeId = ?", pId)
        let value = db.getValue(sql"select value from Node where nodeId = ?", pId)

        let l = getLabel(getInitialVariables(), vName, row1[3], value)
        let pL = getLabel(getInitialVariables(), vName, row1[3], value, true)

        list.add(Node(parentId: pId, id: nId, label:l, prettyLabel: pL, isLeftChild: parsebool(row1[3]), childCount: childCount, decCount: decTable[nId] - 1, isSolution: parseBool(row1[5]) ))


proc makeCore*(db: DbConn, decTable: CountTable[int]): Core =
    var coreQuery : SqlQuery
    var failedQuery : SqlQuery
    var solAncestorIds : seq[int]
    var nodeList: seq[Node]

    let firstQuery = "select nodeId from Node where isSolution = 1 limit 1"
    if db.getValue(sql(firstQuery)) == "":
        coreQuery = noSolutionQuery
        failedQuery = noSolutionFailedQuery
    else:
        failedQuery = solutionFailedQuery
        coreQuery = solutionQuery

    solAncestorIds = processQuery(db, coreQuery, nodeList, decTable)
    discard processQuery(db, failedQuery, nodeList, decTable)

    return Core(nodes: nodeList, solAncestorIds: solAncestorIds)

    # return %*{"nodes": %nodeList, "solAncestorIds": %solAncestorIds}
