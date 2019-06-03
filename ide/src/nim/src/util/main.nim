# Hello Nim!
import jester, typetraits, sequtils, tables, db_sqlite, types, parseutils, strutils, json
import jsonify
import init
import process
import branchingCondition
# include process

var db: DbConn
var decTable: CountTable[int]

proc init*(dirPath: string): JsonNode =
    db = findFiles(dirPath)
    decTable = getDecendants(db)
    return getCore(db, decTable)

proc loadNodes*(start: string): seq[ParentChild] =

    var nId, pId: int

    let query = "select nodeId, parentId, branchingVariable, isLeftChild, value, isSolution from Node where parentId = ? order by nodeId asc"

    for row1 in db.fastRows(sql(query), start):
        discard parseInt(row1[0], nId)

        discard parseInt(row1[1], pId)

        var childCount : int
        discard db.getValue(sql"select count(nodeId) from Node where parentId = ?", row1[0]).parseInt(childCount)
        
        let l = getLabel(getInitialVariables(), row1[2], row1[3], row1[4])
        let pL = getLabel(getInitialVariables(), row1[2], row1[3], row1[4], true)


        result.add(ParentChild(parentId: pId, id: nId, label:l, prettyLabel: pL, isLeftChild: parsebool(row1[3]), childCount: childCount, decCount: decTable[nId] - 1))

proc getExpandedSetChild*(nodeId, path: string): Set =

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


proc loadPrettyDomains*(nodeId: string,  paths: string, wantExpressions: bool = false): JsonNode =
    temp(db, nodeId, paths)

proc loadSimpleDomains*(nodeId: string, wantExpressions: bool = true): SimpleDomainResponse =

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


proc getLongestBranchingVarName*(): JsonNode =
    return % db.getRow(sql"select max(length(branchingVariable)) from Node")[0]
