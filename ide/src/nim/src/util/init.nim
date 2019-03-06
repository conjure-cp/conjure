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


proc addChild(parent, child : JsonNode, isLeftChild: bool) =

    if isLeftChild:
        parent["children"] = %(concat(@[child], parent["children"].getElems()))
    else:
        parent["children"].add(child)



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

proc processQuery(db: DbConn, query: SqlQuery, map: JsonNode, decTable: CountTable): seq[int]  =

    # var map = %*{}
    var nId, pId: int
    var solAncestorIds : seq[int]

    for row1 in db.fastRows(query):

        discard parseInt(row1[0], nId)

        solAncestorIds.add(nId)

        var childCount : int
        discard db.getValue(sql"select count(nodeId) from Node where parentId = ?", row1[0]).parseInt(childCount)

        discard parseInt(row1[1], pId)

        let l = getLabel(getInitialVariables(), row1[2], row1[3], row1[4])
        let pL = getLabel(getInitialVariables(), row1[2], row1[3], row1[4], true)

        # echo map

        let obj = %*{"id": nId, "label": l, "prettyLabel": pL, "children": %[], "childCount": childCount, "isSolution": row1[5].parseBool(), "decCount" : decTable[nId] - 1}

        if pId == -1:
            map[$nId] = obj
        else:
            map[$nId] = obj
            addChild(map[$pId], obj, parseBool(row1[3]))

    return solAncestorIds


proc getCore*(db: DbConn, decTable: CountTable[int]): JsonNode =

    var map = %*{}
    var coreQuery : SqlQuery
    var failedQuery : SqlQuery

    var solAncestorIds : seq[int]

    let firstQuery = "select nodeId from Node where isSolution = 1 limit 1"
    if db.getValue(sql(firstQuery)) == "":
        coreQuery = noSolutionQuery
        failedQuery = noSolutionFailedQuery
    else:
        failedQuery = solutionFailedQuery
        coreQuery = solutionQuery

    solAncestorIds = processQuery(db, coreQuery, map, decTable)
    discard processQuery(db, failedQuery, map, decTable)

    return %*{"solAncestorIds": %solAncestorIds, "tree" : map["0"]}


