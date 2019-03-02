# Hello Nim!
import jester, typetraits, sequtils
import jsonify
include process

proc getDecendants(db: DbConn): CountTable[int]

var db: DbConn
var decTable: CountTable[int]


proc getLabel(varName: string, value: string, branch: string): string =

    if varName == "":
        return ""

    result &= varName

    if (branch == "1"):
        result &= " = "
    else:
        result &= " != "

    result &= value


proc init*(dirPath: string) =

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

    db = open(dbFilePath, "", "", "")

    initParser(db, minionFilePath, eprimeFilePath)


    decTable = getDecendants(db)

proc loadNodes*(amount, start: string): seq[ParentChild] =

    var nId, pId, childId: int

    let query = "select nodeId, parentId, branchingVariable, value, isLeftChild from Node where nodeId > ? limit ?"

    for row1 in db.fastRows(sql(query), start, amount):
        discard parseInt(row1[0], nId)
        var kids: seq[int]
        # for row2 in db.fastRows(sql"select nodeId from Node where parentId = ?",
        #         row1[0]):
        #     discard parseInt(row2[0], childId)
        #     kids.add(childId)

        discard parseInt(row1[1], pId)

        result.add(ParentChild(parentId: pId, nodeId: nId, label: getLabel( row1[2], row1[3], row1[4]), children: kids))


proc loadChildren*(id: string): ChildResponse =

    var nId, childId: int
    discard parseInt(id, nId)
    var kids: seq[int] 

    for row in db.fastRows(sql"select nodeId from Node where parentId = ?", id):
        discard parseInt(row[0], childId)
        kids.add(childId)
    return ChildResponse(nodeId: nId, children: kids, decendantCount: decTable[nId])



proc loadCore*(deepest : bool = false): seq[ParentChild] =
    var nId, pId, childId: int

    let solutionQuery = sql(
                """with recursive
        correctPath(n) as (
        select nodeId from Node where isSolution = 1  
        union 
        select parentId  from Node, correctPath
            where nodeId=correctPath.n 
                    )
        select nodeId, parentId, branchingVariable, value, isLeftChild, isSolution from Node where nodeId in correctPath;""")

    let deepestQuery = sql(
                """with recursive
        correctPath(n) as (
        select max(nodeId) from Node
        union 
        select parentId  from Node, correctPath
            where nodeId=correctPath.n 
                    )
        select nodeId, parentId, branchingVariable, value, isLeftChild, isSolution from Node where nodeId in correctPath;""")

    var query : SqlQuery

    if deepest:
        query = deepestQuery
    else:
        query = solutionQuery


    for row1 in db.fastRows(query):
        var sol = false
        if (row1[5] == "1"):
            sol = true

        # let decQuery = """
        # with recursive
        # decendants(n) as (
        # select nodeId from Node where parentId = ?
        # union 
        # select nodeId  from Node, decendants
        #     where parentId = decendants.n 
        #             )
        # select count(nodeId) from Node where nodeId in decendants;"""

        # var decCount : int
        # discard db.getValue(sql(decQuery), row1[0]).parseInt(decCount)

        discard parseInt(row1[0], nId)
        var kids: seq[int]
        for row2 in db.fastRows(sql"select nodeId from Node where parentId = ?", row1[0]):
            discard parseInt(row2[0], childId)
            kids.add(childId)


        discard parseInt(row1[1], pId)

        let l = getLabel(row1[2], row1[3], row1[4])


        result.add(ParentChild(parentId: pId, nodeId: nId, label: l, children: kids, isSolution: sol))
    
    if result.len() == 0:
        return loadCore(true)

proc getDecendants(db: DbConn): CountTable[int] =

    # let db = open("/home/tom/minion-private/build/gears/test.db", "", "", "")
    let query = "select nodeId, parentId from Node where parentId >= 0;"

    var countTable = initCountTable[int]()
    var id2Parent = initTable[int, int]()
    var nodeId, parentId : int

    for res in db.fastRows(sql(query)):
        discard res[0].parseInt(nodeId)
        discard res[1].parseInt(parentId)

        id2Parent[nodeId] = parentId
    
    let leaves = toSeq(id2Parent.keys).filter() do (x: int) -> bool : not countTable.hasKey(x)

    for leaf in leaves:
        var current = id2Parent[leaf]

        while true:
            countTable.inc(current)
            if id2Parent.hasKey(current):
                current = id2Parent[current]
            else:
                break

    return countTable


proc loadSimpleDomains*(nodeId: string, wantExpressions: bool = false): SimpleDomainResponse =

    var list: seq[string]
    var id: int
    var domainsAtPrev: seq[Variable]
    discard parseInt(nodeId, id)

    let domainsAtNode = getSimpleDomainsOfNode(db, nodeId, wantExpressions)

    if (id != rootNodeId):
        domainsAtPrev = getSimpleDomainsOfNode(db, $(id - 1), wantExpressions)

        for i in 0..<domainsAtNode.len():
            if (domainsAtNode[i].rng != domainsAtPrev[i].rng):
                list.add(domainsAtNode[i].name)

    return SimpleDomainResponse(changedNames: list, vars: domainsAtNode)

proc getExpandedSetChild*(nodeId, path: string): Set =

    # echo prettyLookup

    # echo nodeId
    # echo "path is " & path

    result = Set(prettyLookup[nodeId][path.split(".")[0]])

    for name in path.split(".")[1..^1]:
        for kid in result.children:
            if kid.name == name:
                result = kid
                break;

    if result.name != path.split(".")[^1]:
        return nil


proc getChildSets(paths, nodeId: string): seq[Set] =
    if paths != "":
        for path in paths.split(":"):
            result.add(getExpandedSetChild(nodeId, path))


proc getJsonVarList*(domainsAtNode: seq[Variable], nodeId: string): JsonNode =
    result = %*[]

    for v in domainsAtNode:
        if (v != nil):
            if (v of Set):
                result.add(setToJson(Set(v), nodeId, true))
            else:
                result.add(%v)

proc loadSetChild*(nodeId, path: string): JsonNode =
    let s = getExpandedSetChild(nodeId, path)
    let update = setToJson(s, nodeId, true)
    return %*{"structure": %setToTreeView(s), "update": update, "path": path}


proc temp(db: DbConn, nodeId, paths: string): JsonNode =
    var domainsAtNode: seq[Variable]
    var id: int
    var changeList: seq[string]
    discard parseInt(nodeId, id)
    domainsAtNode.deepCopy(getPrettyDomainsOfNode(db, nodeId))
    for kid in getChildSets(paths, nodeId):
        if kid != nil:
            domainsAtNode.add(kid)

    if (id != rootNodeId):
        var domainsAtPrev = getPrettyDomainsOfNode(db, $(id - 1))
        for kid in getChildSets(paths, $(id - 1)):
            if kid != nil:
                domainsAtPrev.add(kid)
        changeList = getPrettyChanges(domainsAtNode, domainsAtPrev)

    return %*{"vars": getJsonVarList(domainsAtNode, nodeId), "changed": changeList, "changedExpressions": %[]}

proc getSkeleton*(): JsonNode =
    return domainsToJson(getPrettyDomainsOfNode(db, "0"))
    # return domainsToJson(domainsAtNode)


proc loadPrettyDomains*(nodeId, paths: string): JsonNode =
    temp(db, nodeId, paths)

proc getLongestBranchingVarName*(): JsonNode =
    # return % db.getRow(sql"select max(length(branchingVariable)) from Node")[0]
    return % 15
