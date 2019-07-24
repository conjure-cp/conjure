import jester, typetraits, sequtils, tables, db_sqlite, types, parseutils, strutils, json, strformat, sequtils, sugar

import jsonify
import init
import process
import branchingCondition


var dBTable: Table[string, DBconn]

proc loadAncestors*(dirPath, nodeId: string): seq[Node] 

proc init*(dirPath: string): (Core, string) =
    ## Initialises data structures 
    var eprimeInfoFilePath: string
    var db: DbConn
    (db, eprimeInfoFilePath) = findFiles(dirPath)
    dBTable[dirPath] = db

    writePaths(db)
    let infoFile = readFile(eprimeInfoFilePath)
    return (makeCore(db), infoFile)


proc checkDomainsAreEqual*(paths: array[2, string],  nodeIds: array[2, int]) : bool =
    let query = "select group_concat(name, storeDump) from domain where nodeId = ? and name not like 'aux%'"
    let leftDB = dBTable[paths[0]]
    let rightDB = dBTable[paths[1]]

    # echo "============================================="

    let leftValue = leftDB.getValue(sql(query), nodeIds[0])
    # echo leftValue
    # echo ""
    let rightValue = rightDB.getValue(sql(query), nodeIds[1])
    # echo rightValue
    return leftValue == rightValue
    

proc nodeIdsToArray(current, other: int, leftIsMore: bool): array[2, int] =
    if leftIsMore:
        return [current, other]
    return [other, current]

proc atEndOfTree*(notFinishedTreePath: string, finishedTreeLastId: int): seq[int] = 
    let ancestors = loadAncestors(notFinishedTreePath, $finishedTreeLastId)
    let augmentedIds = ancestors.filter(x => x.id > finishedTreeLastId).map(x => x.id)
    return augmentedIds

type DiffResponse* = ref object of RootObj
    diffLocations*: seq[seq[int]]
    augmentedIds*: seq[int]



proc diff*(leftPath, rightPath: string, debug: bool = false): DiffResponse =

    var res : seq[(int, int)]

    let leftDB = dBTable[leftPath]
    let rightDB = dBTable[rightPath]

    let dbs = [leftDB, rightDb]

    let query = "select count(nodeId) from Node"

    var lCount: int
    var rCount: int

    discard leftDB.getValue(sql(query)).parseInt(lCount) 
    discard rightDB.getValue(sql(query)).parseInt(rCount) 

    let lIsMore =  lCount > rCount

    var nodeIds = [0,0]
    var current: int
    var other: int
    var db : DbConn

    if not checkDomainsAreEqual([leftPath, rightPath], nodeIds):
       return DiffResponse(diffLocations: @[@[-1,-1]], augmentedIds: @[])

    while true:

        if debug:
            echo  ""
            echo nodeIds[0], "     ", nodeIds[1]

        # Increment each tree until we get to a point where they differ

        while checkDomainsAreEqual([leftPath, rightPath], nodeIds):
            nodeIds[0].inc()
            nodeIds[1].inc()

            let leftIsFinished = nodeIds[0] >= lCount
            let rightIsFinished = nodeIds[1] >= rCount

            # If we get to the end of one of the trees then we've finished and need to return
            if (leftIsFinished or rightIsFinished):
                if debug:
                    echo nodeIds[0], "     ", nodeIds[1]
                    echo "quiting"

                if (res.len() == 0 and lCount == rCount):
                    return DiffResponse(diffLocations: newSeq[seq[int]](), augmentedIds: @[] )

                nodeIds[0].dec()    
                nodeIds[1].dec()    

                var notEndedTreePath = leftPath
                var endedTreeId = nodeIds[1] 
                if leftIsFinished:
                    notEndedTreePath = rightPath
                    endedTreeId = nodeIds[0]

                let augIds = atEndOfTree(notEndedTreePath, endedTreeId)

                res.add((nodeIds[0], nodeIds[1]))

                let diffLocations = res.map(s => @[s[0], s[1]])

                return DiffResponse(diffLocations: diffLocations, augmentedIds: augIds )
                
        if debug:
            echo nodeIds[0], "     ", nodeIds[1]

        nodeIds[0].dec()
        nodeIds[1].dec()

        if debug:
            echo nodeIds[0], "     ", nodeIds[1]

        # Add the first firr point to the res
        res.add((nodeIds[0], nodeIds[1]))

        var advanceBothTrees = true

        # Advance both trees to the next branch that is not descended from the last diff point
        while advanceBothTrees:
            var couldParse = 1

            for index, db in dbs:

                let path = db.getValue(sql"select path from Node where nodeId = ?", nodeIds[index])
                var nextId: int
                let query = fmt"select nodeId from Node where path not like '{path}%' and nodeId > {nodeIds[index]} limit 1"
                couldParse = db.getValue(sql(query)).parseInt(nextId)

                if couldParse == 0:
                    if debug:
                        echo "quitting 2"
                        echo index, " | ", query
                    let diffLocations = res.map(s => @[s[0], s[1]])
                    return DiffResponse(diffLocations: diffLocations, augmentedIds: @[] )

                nodeIds[index] = nextId
                
            if debug:
                echo nodeIds[0], "     ", nodeIds[1]

            # Initialise the variables such that they refer to the largest tree

            if lIsMore:
                current = nodeIds[0]
                other = nodeIds[1]
                db = leftDB
            else:
                current = nodeIds[1]
                other = nodeIds[0]
                db = rightDB

            #  Go through all the subsequent branches of the tree with the most nodes to see if there
            # is a node equal to that of the smaller tree
            # while lRes[nodeIds[0]] != rRes[nodeIds[1]] and couldParse != 0:

            while not checkDomainsAreEqual([leftPath, rightPath], nodeIdsToArray(current, other, lIsMore)):

                if debug:
                    echo "start of botoom llop"
                    echo nodeIds[0], "     ", nodeIds[1]

                let path = db.getValue(sql"select path from Node where nodeId = ?", current)

                var nextId: int

                let query = fmt"select nodeId from Node where path not like '{path}%' and nodeId > {current} limit 1"

                couldParse = db.getValue(sql(query)).parseInt(nextId)

                if couldParse == 0:
                    break

                current = nextId

            if couldParse != 0:
                nodeIds = nodeIdsToArray(current, other, lIsMore)
                advanceBothTrees = false

            if debug:
                echo "end of bottom loop ", nodeIds[0], "     ", nodeIds[1]

    if debug:
        echo "end"

    let diffLocations = res.map(s => @[s[0], s[1]])
    return DiffResponse(diffLocations: diffLocations, augmentedIds: @[] )


import os

proc diffHandler*(leftPath, rightPath, leftHash, rightHash: string): JsonNode =
    let diffCachesDir = fmt"{parentDir(leftPath)}/diffCaches"
    let diffCacheFile = fmt"{diffCachesDir}/{leftHash}~{rightHash}.json"

    if fileExists(diffCacheFile):
        return parseJson(readAll(open(diffCacheFile)))

    let res = diff(leftPath, rightPath)
    writeFile(diffCacheFile, $(%res))
    return %res

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