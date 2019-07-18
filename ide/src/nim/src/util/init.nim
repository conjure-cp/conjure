import tables, os, db_sqlite, types, process, parseutils, times, strutils, json, sequtils, sugar
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

proc makePaths*(db: DbConn): Table[string, string] =
    ## Calculate the number of descendants for each node in the tree
    let query = "select nodeId, parentId from Node;"

    var pathTable = initTable[string, string]()
    var id2Children = initTable[string, (string, string)]()

    # var nodeId, parentId: int

    echo "Loading Node Table"

    for res in db.fastRows(sql(query)):

        let nodeId = res[0]
        let parentId = res[1]

        if (not id2Children.haskey(parentId)):
            id2Children[parentId] = ("-1", "-1")

        let (kid1, kid2) = id2Children[parentId]

        if kid1 == "-1":
            id2Children[parentId] = ($nodeId, "-1")
        else:
            id2Children[parentId] = (kid1, $nodeId)

    echo "Calculating Node Paths"


    proc recursive(id: string, parentPath: string) = 

        if id == "0":
            pathTable[id] = "0"
        else:
            pathTable[id] = parentPath & "/" & $id
        
        if id in id2Children:
            let (kid1, kid2) = id2Children[id]
            if (kid1 != "-1"):
                recursive(kid1, pathTable[id])
            if (kid2 != "-1"):
                recursive(kid2, pathTable[id])

    recursive("0", "/")
    
    # echo id2Children

    # for path in pathTable.values():
    #     echo path

    return pathTable


proc writePaths*(db: DBConn) =


    let colNames = db.getAllRows(sql"PRAGMA table_info(Node);").map(row => row[1])
    if (colNames.contains("path")):
        return


    let table = makePaths(db)

    db.exec(sql"BEGIN TRANSACTION;")

    db.exec(sql"ALTER TABLE NODE ADD COLUMN path TEXT;")

    echo "Building Write Paths to DB Query"

    # var query = ""
    var query = """
    UPDATE Node
        SET     path =  CASE  
    """

    for nodeId in table.keys():

        query &= "when nodeId = " & $nodeId & " then " & " '" & table[nodeId] & "' "

    query &= "END;"        

    echo "Executing Query"

    db.exec(sql(query))
        
    echo "Commiting Query"

    db.exec(sql"COMMIT;")

    # db.exec(sql"adisaodhiosah")


    echo "Done"




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


proc processQuery*( db: DbConn, query: SqlQuery, list: var seq[Node]): seq[int] =
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

        let path = db.getValue(sql"select path from Node where nodeId = ?", nId)
        let descCount = db.getValue(sql("select count(nodeId) from Node where path like '" & path & "/%'"))
        var dc: int
        discard descCount.parseInt(dc)

        list.add(Node(parentId: pId,
                        id: nId,
                        label:l,
                        prettyLabel: pL,
                        isLeftChild: parsebool(row1[3]),
                        childCount: childCount,
                        descCount: dc,
                        isSolution: parseBool(row1[5])))


proc makeCore*(db: DbConn): Core =
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

    solAncestorIds = processQuery(db, coreQuery, nodeList)
    discard processQuery(db, failedQuery, nodeList)

    return Core(nodes: nodeList, solAncestorIds: solAncestorIds)
