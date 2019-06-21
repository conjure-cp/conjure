import tables, os, db_sqlite, types, process, parseutils, times, strutils, json, sequtils
import jsonify
import process
import branchingCondition

proc findFiles*(dirPath: string): (DbConn, string) =
    ## Locate files in directory pointed to by path

    let current = getCurrentDir()
    setCurrentDir(dirPath)

    var minionFilePath: string
    var eprimeFilePath: string
    var eprimeInfoFilePath: string
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

    let eprimeInfoFiles = toSeq(walkFiles("*.eprime-info"))
    if (eprimeInfoFiles.len() == 0):
        raise newException(InitException, "No eprime-info file found!")
    if (eprimeInfoFiles.len() > 1):
        raise newException(InitException, "More than one eprime-info file found!")
    
    let dbFiles = toSeq(walkFiles("*.db"))
    if (dbFiles.len() == 0):
        raise newException(InitException, "No db file found!")
    if (dbFiles.len() > 1):
        raise newException(InitException, "More than one db file found!")

    minionFilePath = absolutePath(minionFiles[0])
    eprimeFilePath = absolutePath(eprimeFiles[0])
    eprimeInfoFilePath = absolutePath(eprimeInfoFiles[0])
    dbFilePath = absolutePath(dbFiles[0])

    setCurrentDir(current)

    let db = open(dbFilePath, "", "", "")

    initParser(db, minionFilePath, eprimeFilePath)

    return (db, eprimeInfoFilePath)


proc getDescendants*(db: DbConn): Table[int, int] =
    ## Calculate the number of descendants for each node in the tree
    let query = "select nodeId, parentId from Node ;"

    var descTable = initTable[int, int]()
    var id2Parent = initTable[int, int]()
    var id2Children = initTable[int, seq[int]]()
    var leafList : seq[int]

    var nodeId, parentId: int

    for res in db.fastRows(sql(query)):
        discard res[0].parseInt(nodeId)
        discard res[1].parseInt(parentId)

        if (not id2Children.haskey(parentId)):
            id2Children[parentId] = @[]

        id2Children[parentId].add(nodeId)

    proc recursive(id: int): int = 
        if (not id2Children.hasKey(id)):
            descTable[id] = 0
            return 1
        
        var sum: int

        for childId in id2Children[id]:
            sum += recursive(childId)

        descTable[id] = sum

        return sum + 1

    discard recursive(0)

    return descTable


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


proc processQuery*( db: DbConn, query: SqlQuery, list: var seq[Node], descTable: Table[int, int]): seq[int] =
    ## Sets a list of nodes retrieved from database by executing sql query.
    ## Returns a list of node ids for ancestors of solution nodes.

    var nId, pId: int
    var l, pL: string

    for row1 in db.fastRows(query):

        discard parseInt(row1[0], nId)
        discard parseInt(row1[1], pId)

        result.add(nId)

        var childCount : int
        discard db.getValue(sql"select count(nodeId) from Node where parentId = ?", row1[0]).parseInt(childCount)

        let row2 = db.getRow(sql"select branchingVariable, value from Node where nodeId = ?", pId)

        if pId == rootNodeId:
            l = "Root Propagation" 
            pL = l
        else:
            l = getLabel(getInitialVariables(), row2[0], row1[3], row2[1])
            pL = getLabel(getInitialVariables(), row2[0], row1[3], row2[1], true)

        list.add(Node(parentId: pId,
                        id: nId,
                        label:l,
                        prettyLabel: pL,
                        isLeftChild: parsebool(row1[3]),
                        childCount: childCount,
                        descCount: descTable[nId],
                        isSolution: parseBool(row1[5])))


proc makeCore*(db: DbConn, decTable: Table[int, int]): Core =
    ## Returns data required for building the core of the tree
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
