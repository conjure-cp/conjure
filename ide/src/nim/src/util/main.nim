import os, jester, typetraits, sequtils, tables, db_sqlite, types, parseutils,
        strutils, json, strformat, sequtils, sugar

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


proc checkDomainsAreEqual*(paths: array[2, string], nodeIds: array[2,
        string]): bool =
    # let query1 = "select group_concat(name, storeDump) from domain where nodeId = ? and name not like 'aux%'"
    let query1 = "select group_concat(name || ' - ' || storeDump, ' , ') from Domain where nodeId = ? and name not like 'aux%'"
    let leftDB = dBTable[paths[0]]
    let rightDB = dBTable[paths[1]]

    var leftValue = leftDB.getValue(sql(query1), nodeIds[0])

    var rightValue = rightDB.getValue(sql(query1), nodeIds[1])


    result = leftValue == rightValue

    # echo nodeIds
    # echo result

#     augmentedIds*: seq[seq[int]]

type DiffPoint* = ref object of RootObj
    leftTreeId*: int
    rightTreeId*: int
    descCount*: int
    highlightLeft*: seq[int]
    highlightRight*: seq[int]

proc `==`*(a, b: DiffPoint): bool =
    return a.leftTreeId == b.leftTreeId and 
    a.rightTreeId == b.rightTreeId and
    a.descCount == b.descCount and
    a.highlightLeft == b.highlightLeft and
    a.highlightRight == b.highlightRight 

proc newDiffPoint*(l, r: string, highlightLeft, highlightRight: seq[ string], dC: int = 0): DiffPoint =

    var lNum, rNum: int
    discard l.parseInt(lNum)
    discard r.parseInt(rNum)

    let hL = highlightLeft.map(
        proc (x: string): int =
        var num: int
        discard x.parseInt(num)
        return num
        )

    let hR = highlightRight.map(
        proc (x: string): int =
        var num: int
        discard x.parseInt(num)
        return num
        )

    return DiffPoint(leftTreeId: lNum, rightTreeId: rNum, highlightLeft: hL, highlightRight: hR, descCount: dC)

proc `$`*(d: DiffPoint): string =
    result = fmt"<({d.leftTreeId}, {d.rightTreeId}) {d.highlightLeft} {d.highlightRight} {d.descCount}>"


proc removeDuplicates*(leftPath: string, rightPath: string,
                        kids: array[2, seq[string]]):
                        array[2, seq[string]] =


    var leftKidsToSkip = newSeq[string]()
    var rightKidsToSkip = newSeq[string]()

    for i, kid in kids[0]:
        for k in kids[1]:

            if checkDomainsAreEqual([leftPath, rightPath], [kid, k]):
                leftKidsToSkip.add(kid)

    for i, kid in kids[1]:
        for k in kids[0]:

            if checkDomainsAreEqual([leftPath, rightPath], [k, kid]):
                rightKidsToSkip.add(kid)

    return [
        kids[0].filter(x => not leftKidsToSkip.contains(x)),
        kids[1].filter(x => not rightKidstoSkip.contains(x))
    ]

proc findDiffLocationsBoyo*(leftPath,
                            rightPath: string,
                            debug: bool = false):
                             seq[DiffPoint] =

#Concept of the diff point is wrong, what we need is a way of keeping track of different branches

    var res: type(result) = @[]

    let leftDB = dBTable[leftPath]
    let rightDB = dBTable[rightPath]
    let dbs = [leftDB, rightDb]

    let kidsQuery = "select nodeId from Node where parentId = ?"

    var tuples = newSeq[(string, string)]()

    proc recursive(ids: array[2, string], prevIds: array[2, string]) =
        var kids: array[2, seq[string]]

        if debug:
            echo fmt"Current ", ids
            echo checkDomainsAreEqual([leftPath, rightPath], ids)

        if not checkDomainsAreEqual([leftPath, rightPath], ids):

            let t = (prevIds[0], prevIds[1])
            if tuples.contains(t):
                return

            for i in countUp(0, 1):
                for row in dbs[i].fastRows(sql(kidsQuery), prevIds[i]):
                    kids[i].add(row[0])

            let cleanKids = removeDuplicates(leftPath, rightPath, kids)

            let diffPoint = newDiffPoint(prevIds[0], prevIds[1],
                                        cleanKids[0], cleanKids[1])

            res.add(diffPoint)
            tuples.add(t)
            return


        for i in countUp(0, 1):
            for row in dbs[i].fastRows(sql(kidsQuery), ids[i]):
                kids[i].add(row[0])

        let maxKids = kids.map(x => x.len()).max() - 1

        if debug:
            echo maxKids

        for i in countUp(0, maxKids):
            # echo fmt"Recursing on {i}", [kids[0][i], kids[1][i]]
            var nextLeft: string
            var nextRight: string

            if i >= kids[0].len():
                if kids[0].len() > 0:
                    nextLeft = kids[0][0]
                else:
                    nextLeft = ids[0]
            else:
                nextLeft = kids[0][i]

            if i >= kids[1].len():
                if kids[1].len() > 0:
                    nextRight = kids[1][0]
                else:
                    nextRight = ids[1]
            else:
                nextRight = kids[1][i]

            if debug:
                echo nextLeft
                echo nextRight

            recursive([nextLeft, nextRight], ids)

    recursive(["0", "0"], ["-1", "-1"])

    result = res

    for diffPoint in result:
        var temp1: int
        var temp2: int
        var path = leftDB.getValue(sql"select path from Node where nodeId = ?", diffPoint.leftTreeId)
        discard leftDB.getValue(sql("select count(nodeId) from Node where path like '" & path & "/%'")).parseInt(temp1)
        path = rightDB.getValue(sql"select path from Node where nodeId = ?", diffPoint.rightTreeId)
        discard rightDB.getValue(sql("select count(nodeId) from Node where path like '" & path & "/%'")).parseInt(temp2)
        diffPoint.descCount = temp1 + temp2 

        if diffPoint.highlightLeft.len() + diffPoint.highlightRight.len() < 2:
            diffPoint.descCount.dec()

    if debug:
        for d in result:
            echo d



proc diff*(leftPath, rightPath: string, debug: bool = false): seq[DiffPoint] =
    # let diffLocations = findDiffLocations(leftPath, rightPath, debug)
    # let augs = newSeq[seq[int]](2)
    # let augs = getAugs(leftPath, rightPath, diffLocations)
    # return DiffResponse(diffLocations: diffLocations, augmentedIds: augs)
    return findDiffLocationsBoyo(leftPath, rightPath, debug)

proc diffHandler*(leftPath, rightPath, leftHash, rightHash: string): JsonNode =
    let diffCachesDir = fmt"{parentDir(leftPath)}/diffCaches"
    let diffCacheFile = fmt"{diffCachesDir}/{leftHash}~{rightHash}.json"
    let flipped = fmt"{diffCachesDir}/{rightHash}~{leftHash}.json"

    if fileExists(diffCacheFile):
        return parseJson(readAll(open(diffCacheFile)))

    if fileExists(flipped):
        return %(parseJson(readAll(open(flipped))).getElems()
            .map(x => newDiffPoint($x["rightTreeId"], $x["leftTreeId"],
                                    x["highlightRight"].getElems().map(y => $y),
                                    x["highlightLeft"].getElems().map(y => $y))))

    let res = diff(leftPath, rightPath)
    writeFile(diffCacheFile, $(%res))
    return %res

proc loadAncestors*(dirPath, nodeId: string): seq[Node] =
    ## Loads the children of a node
    let db = dBTable[dirPath]

    var nId: int
    discard nodeId.parseInt(nId)

    let path = db.getValue(sql"select path from Node where nodeId = ?", nodeId)

    let query = fmt"""
    select nodeId, parentId, branchingVariable, isLeftChild, value, isSolution from Node where 
    ( nodeId in
        (WITH split(word, str) AS (
                    SELECT '', '{path}' ||'/'
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
                    SELECT '', '{path}' ||'/'
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

    var nId: int
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
        ) ) <= """ & $(path.split("/").len() + limit) & " and nodeId != " &
                nodeId & " order by length(p)"

    discard processQuery(db, sql(query), result)

proc loadSimpleDomains*(dirPath, nodeId: string,
        wantExpressions: bool = false): SimpleDomainResponse =
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
