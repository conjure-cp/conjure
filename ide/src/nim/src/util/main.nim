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
    return %makeCore(db, decTable)



proc loadNodes*(start: string): seq[ParentChild] =

    var nId, pId, childId: int

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
    # echo result

proc loadSetChild*(nodeId, path: string): JsonNode =
    let s = getExpandedSetChild(nodeId, path)
    let update = setToJson(s, nodeId, true)
    return %*{"structure": %setToTreeView(s), "update": update, "path": path}


proc temp(db: DbConn, nodeId, paths: string, wantExpressions: bool = false): JsonNode =
    var domainsAtNode: seq[Variable]
    var changedExpressions: seq[Expression]
    var changedList: seq[string]
    var id: int

    discard parseInt(nodeId, id)
    domainsAtNode.deepCopy(getPrettyDomainsOfNode(db, nodeId, wantExpressions))

    for kid in getChildSets(paths, nodeId):
        if kid != nil:
            domainsAtNode.add(kid)

    if (id != rootNodeId):
        var domainsAtPrev = getPrettyDomainsOfNode(db, $(id - 1), wantExpressions)
        for kid in getChildSets(paths, $(id - 1)):
            if kid != nil:
                domainsAtPrev.add(kid)

        (changedList, changedExpressions) = getPrettyChanges(domainsAtNode, domainsAtPrev)

    # echo domainsAtNode

    return %*{"vars": getJsonVarList(domainsAtNode, nodeId), "changed": changedList, "changedExpressions": expressionsToJson(changedExpressions)}

proc getSkeleton*(): JsonNode =
    return domainsToJson(getPrettyDomainsOfNode(db, "0", true))
    # return domainsToJson(domainsAtNode)


proc loadPrettyDomains*(nodeId: string,  paths: string, wantExpressions: bool = false): JsonNode =
    temp(db, nodeId, paths, wantExpressions)

proc loadSimpleDomains*(nodeId: string, wantExpressions: bool = true): SimpleDomainResponse =

    var list: seq[string]
    var id: int
    var domainsAtPrev: seq[Variable]
    discard parseInt(nodeId, id)

    let domainsAtNode = getSimpleDomainsOfNode(db, nodeId, wantExpressions)

    if (id != rootNodeId):
        domainsAtPrev = getSimpleDomainsOfNode(db, $(id - 1), wantExpressions)

        for i in 0..<domainsAtNode.len():

            if domainsAtNode[i].name == "((!Ticks_Occurrence_00001) \\/ (!Ticks_Occurrence_00006))":
                echo domainsAtNode[i].rng, "   ", domainsAtPrev[i].rng

            # echo "old   ", "new"
            # echo domainsAtNode[i], domainsAtPrev[i]

            if (domainsAtNode[i].rng != domainsAtPrev[i].rng):
                # echo domainsAtNode[i]
                list.add(domainsAtNode[i].name)

    return SimpleDomainResponse(changedNames: list, vars: domainsAtNode)


proc getLongestBranchingVarName*(): JsonNode =
    return % db.getRow(sql"select max(length(branchingVariable)) from Node")[0]
    # return % 15
