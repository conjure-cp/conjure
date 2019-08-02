import os, jester, typetraits, sequtils, tables, db_sqlite, types, parseutils,
        strutils, json, strformat, sequtils, sugar, algorithm

import jsonify
import init
import process
import branchingCondition
import diff


# type
#   BranchPair[T] = object
#     then, otherwise: T

# # This cannot be a template yet, buggy compiler...
# proc `|`*[T](a, b: T): BranchPair[T] {.inline.} = BranchPair[T](then: a, otherwise: b)

# template `?`*[T](cond: bool; p: BranchPair[T]): T =
#   (if cond: p.then else: p.otherwise)

var dBTable: Table[string, DBconn]

proc loadAncestors*(dirPath, nodeId: string): seq[Node]

proc getDB*(dirPath: string): DbConn =
  let (db, eprimeInfoFilePath) = findFiles(dirPath)
  return db

proc init*(dirPath: string): (Core, string) =
  ## Initialises data structures
  var eprimeInfoFilePath: string
  var db: DbConn
  (db, eprimeInfoFilePath) = findFiles(dirPath)
  dBTable[dirPath] = db

  writePaths(db)
  let infoFile = readFile(eprimeInfoFilePath)
  return (makeCore(db), infoFile)


proc diff*(leftPath, rightPath: string; debug: bool = false): seq[DiffPoint] =
  # let diffLocations = findDiffLocations(leftPath, rightPath, debug)
  # let augs = newSeq[seq[int]](2)
  # let augs = getAugs(leftPath, rightPath, diffLocations)
  # return DiffResponse(diffLocations: diffLocations, augmentedIds: augs)
  let leftDB = dBTable[leftPath]
  let rightDB = dBTable[rightPath]
  return findDiffLocationsBoyo(leftDB, rightDB, debug)

proc diffHandler*(leftPath, rightPath, leftHash, rightHash: string): JsonNode =
  let diffCachesDir = fmt"{parentDir(leftPath)}/diffCaches"
  let diffCacheFile = fmt"{diffCachesDir}/{leftHash}~{rightHash}.json"
  let flipped = fmt"{diffCachesDir}/{rightHash}~{leftHash}.json"

  # if fileExists(diffCacheFile):
  #   return parseJson(readAll(open(diffCacheFile)))

  # if fileExists(flipped):
  #   return %(parseJson(readAll(open(flipped))).getElems()
  #     .map(x => newDiffPoint($x["rightTreeId"], $x["leftTreeId"],
  #                             x["highlightRight"].getElems().map(y => $y),
  #                             x["highlightLeft"].getElems().map(y => $y),
  #                             x["descCount"].getInt())))

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

proc loadSimpleDomains*(dirPath, nodeId: string;
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
