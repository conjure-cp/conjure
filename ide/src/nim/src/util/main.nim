import jester, typetraits, sequtils, tables, db_sqlite, types, parseutils, strutils, json, strformat, sequtils, sugar

import jsonify
import init
import process
import branchingCondition


var dBTable: Table[string, DBconn]


proc init*(dirPath: string): (Core, string) =
    ## Initialises data structures 
    var eprimeInfoFilePath: string
    var db: DbConn
    (db, eprimeInfoFilePath) = findFiles(dirPath)
    dBTable[dirPath] = db

    writePaths(db)
    let infoFile = readFile(eprimeInfoFilePath)
    return (makeCore(db), infoFile)


proc diff*(leftPath, rightPath: string): seq[seq[int]] =

    var res : seq[(int, int)]

    let leftDB = dBTable[leftPath]
    let rightDB = dBTable[rightPath]

    let dbs = [leftDB, rightDb]

    let query = "select branchingVariable, value, isLeftChild from Node"

    let lRes = leftDB.getAllRows(sql(query))
    let rRes = rightDB.getAllRows(sql(query))

    var nodeIds = [0, 0]

    while true:
    # for k in countup(1,100):

        echo  ""
        echo nodeIds[0], "     ", nodeIds[1]

        let beforeIncrementingLeft = nodeIds[0]
        let beforeIncrementingRight = nodeIds[1]

        while lRes[nodeIds[0]] == rRes[nodeIds[1]]:
            nodeIds[0].inc()
            nodeIds[1].inc()

            if (nodeIds[0] >= lRes.len() or nodeIds[1] >= rRes.len()):

                echo ""
                echo "quiting"
                echo nodeIds[0], "     ", nodeIds[1]
                res.add((beforeIncrementingLeft, beforeIncrementingRight))
                return res.map(s => @[s[0], s[1]])
                
        echo nodeIds[0], "     ", nodeIds[1]

        nodeIds[0].dec()
        nodeIds[1].dec()

        echo nodeIds[0], "     ", nodeIds[1]

        if nodeIds.contains(rootNodeId):
            break

        if res.contains((nodeIds[0], nodeIds[1])):
            break

        res.add((nodeIds[0], nodeIds[1]))

        while lRes[nodeIds[0] + 1] != rRes[nodeIds[1] + 1]:
        # while lRes[nodeIds[0] + 1] != rRes[nodeIds[1] + 1]:
            for index, db in dbs:

                let path = db.getValue(sql"select path from Node where nodeId = ?", nodeIds[index])

                var nextId: int

                let query = fmt"select nodeId from Node where path not like '{path}%' and nodeId > {nodeIds[index]} limit 1"

                let check = db.getValue(sql(query)).parseInt(nextId)

                #     break

                echo query
                echo "nextId ", nextId, " nodeIds ", nodeIds

                nodeIds[index] = nextId

                if lRes[nodeIds[0]] == rRes[nodeIds[1]]:
                    break

                if check == 0:
                    return res.map(s => @[s[0], s[1]])



    return res.map(s => @[s[0], s[1]])



        
    



proc loadAncestors*(dirPath, nodeId: string): seq[Node] =
    ## Loads the children of a node
    let db = dBTable[dirPath]

    var nId : int
    discard nodeId.parseInt(nId)

    let path = db.getValue(sql"select path from Node where nodeId = ?", nodeId)

    let query = """select nodeId, parentId, branchingVariable, isLeftChild, value, isSolution from Node where 
    ( nodeId in
        (WITH split(word, str) AS (
                    SELECT '', '""" & path & """' ||'/'
                    UNION ALL SELECT
                    substr(str, 0, instr(str, '/')),
                    substr(str, instr(str, '/')+1)
                    FROM split WHERE str!=''
                ) SELECT word FROM split WHERE word!=''
        ) 
    )

    or
        
    ( parentId in
        (WITH split(word, str) AS (
                    SELECT '', '""" & path & """' ||'/'
                    UNION ALL SELECT
                    substr(str, 0, instr(str, '/')),
                    substr(str, instr(str, '/')+1)
                    FROM split WHERE str!=''
                ) SELECT word FROM split WHERE word!=''
        ) 
    )
         """

    discard processQuery(db, sql(query), result)

proc loadNodes*(dirPath, nodeId, depth: string): seq[Node] =
    ## Loads the children of a node
    # echo "path ", dirPath
    # echo "nodeId", nodeId
    # echo "depth", depth

    let db = dBTable[dirPath]

    var limit: int
    discard depth.parseInt(limit)
    # limit += 1

    var nId : int
    discard nodeId.parseInt(nId)

    let path = db.getValue(sql"select path from Node where nodeId = ?", nodeId)

    let query = "select nodeId, parentId, branchingVariable, isLeftChild, value, isSolution, path as p from Node where path like '" & 
        path & """%' and (select count(*) from
        (WITH split(word, str) AS (
                    SELECT '', p ||'/'
                    UNION ALL SELECT
                    substr(str, 0, instr(str, '/')),
                    substr(str, instr(str, '/')+1)
                    FROM split WHERE str!=''
                ) SELECT word FROM split WHERE word!=''
        ) ) <= """ & $(path.split("/").len() + limit) & " and nodeId != " & nodeId & " order by length(p)"

    discard processQuery(db, sql(query), result)

proc loadSimpleDomains*(dirPath, nodeId: string, wantExpressions: bool = false): SimpleDomainResponse =
    ## Returns the simple domains for a given node


    let db = dBTable[dirPath]

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
# proc getExpandedSetChild*(nodeId, path: string): Set =
#     ## Finds the set belonging to a path

#     result = Set(prettyLookup[nodeId][path.split(".")[0]])

#     for name in path.split(".")[1..^1]:
#         for kid in result.children:
#             if kid.name == name:
#                 result = kid
#                 break;

#     if result.name != path.split(".")[^1]:
#         return nil


# proc getChildSets(paths, nodeId: string): seq[Set] =
#     ## Returns all child sets belonging to paths
#     if paths != "":
#         for path in paths.split(":"):
#             result.add(getExpandedSetChild(nodeId, path))


# proc getJsonVarList*(domainsAtNode: seq[Variable], nodeId: string): JsonNode =
#     ## Returns a json list of variables 
#     result = %*[]

#     for v in domainsAtNode:
#         if (v != nil):
#             if (v of Set):
#                 result.add(setToJson(Set(v), nodeId, true))
#             else:
#                 result.add(%v)

# proc loadSetChild*(nodeId, path: string): JsonNode =
#     ## Returns a nested set response
#     let s = getExpandedSetChild(nodeId, path)
#     let update = setToJson(s, nodeId, true)
#     return %*{"structure": %setToTreeView(s), "update": update, "path": path}


# proc prettifyDomains(db: DbConn, nodeId, paths: string, wantExpressions: bool = false): PrettyDomainResponse =
#     ## Returns the pretty domains for the given node

#     new result
#     var domainsAtNode: seq[Variable]
#     var domainsAtPrev: seq[Variable]
#     var changedExpressions: seq[Expression]
#     var changedList: seq[string]
#     var id: int
#     discard parseInt(nodeId, id)

#     domainsAtNode.deepCopy(getPrettyDomainsOfNode(db, nodeId, wantExpressions))

#     for kid in getChildSets(paths, nodeId):
#         if kid != nil:
#             domainsAtNode.add(kid)

#     if (id != rootNodeId):
#         let oldId = $(id - 1)
#         domainsAtPrev = getPrettyDomainsOfNode(db, oldId, wantExpressions)

#         for kid in getChildSets(paths, oldId):
#             if kid != nil:
#                 domainsAtPrev.add(kid)

#         (changedList, changedExpressions) = getPrettyChanges(domainsAtNode, domainsAtPrev)

#     return PrettyDomainResponse(vars: getJsonVarList(domainsAtNode, nodeId), changed: changedList, changedExpressions: expressionsToJson(changedExpressions))

# proc getSkeleton*(): TreeViewNode =
#     ## Returns the skeleton for the treeview
#     return prettyDomainsToTreeView(getPrettyDomainsOfNode(db, "0", true))

# proc loadPrettyDomains*(nodeId: string,  paths: string, wantExpressions: bool = false): PrettyDomainResponse =
#     ## Wrapper for prettifyDomains
#     prettifyDomains(db, nodeId, paths, wantExpressions)



# proc getLongestBranchingVarName*(): string =
#     ## Returns the length of longest branching variable name
#     return db.getRow(sql"select max(length(branchingVariable)) from Node")[0]
#     # return "1000"

# proc getSet*(nodeId: string): Set =
#     ## Returns the first set found in the pretty domains of a node
#     for d in getPrettyDomainsOfNode(db, nodeId):
#         if d of Set:
#             return Set(d)
#     return nil