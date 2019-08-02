import os, sequtils, tables, db_sqlite, types, parseutils,
        strutils, json, strformat, sequtils, sugar, algorithm

import meta


proc checkDomainsAreEqual*(dbs: array[2, DbConn],
                    nodeIds: array[2, string]): bool =
  let query1 = "select group_concat(name || ' - ' || storeDump, ' , ') from Domain where nodeId = ? and name not like 'aux%'"
  let leftDB = dbs[0]
  let rightDB = dbs[1]

  var leftValue = leftDB.getValue(sql(query1), nodeIds[0])

  var rightValue = rightDB.getValue(sql(query1), nodeIds[1])

  result = leftValue == rightValue

type DiffPoint* = ref object of RootObj
  leftPath*: string
  rightPath*: string
  descCount*: int
  leftTreeId*: int
  rightTreeId*: int
  highlightLeft*: seq[int]
  highlightRight*: seq[int]

proc `==`*(a, b: DiffPoint): bool =
  return %a == %b

proc `$`*(d: DiffPoint): string =
  result = fmt"<({d.leftTreeId}, {d.rightTreeId}) {d.highlightLeft} {d.highlightRight} : {d.descCount}> | {d.leftPath} {d.rightPath}>"

proc `$`*(list: seq[DiffPoint]): string =
  result &= "["
  for d in list:
    result &= fmt"{$d}{'\n'}"
  result &= "]"

proc newDiffPoint*(l, r: string; highlightLeft, highlightRight: seq[string];
dC: int = 0, path: string = ""): DiffPoint =

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

  return DiffPoint(leftTreeId: lNum, rightTreeId: rNum, highlightLeft: hL,
          highlightRight: hR, descCount: dC, leftPath: path)

proc removeDuplicates*(leftDB: DBconn; rightDB: DbConn;
                        kids: array[2, seq[string]]):
                        array[2, seq[string]] =


  var leftKidsToSkip = newSeq[string]()
  var rightKidsToSkip = newSeq[string]()

  for i, kid in kids[0]:
    for k in kids[1]:

      if checkDomainsAreEqual([leftDB, rightDB], [kid, k]):
        leftKidsToSkip.add(kid)

  for i, kid in kids[1]:
    for k in kids[0]:

      if checkDomainsAreEqual([leftDB, rightDB], [k, kid]):
        rightKidsToSkip.add(kid)

  return [
      kids[0].filter(x => not leftKidsToSkip.contains(x)),
      kids[1].filter(x => not rightKidstoSkip.contains(x))
  ]


proc getNotLikeQuery(i: int, sorted: seq[DiffPoint], isLeft: bool): string =
  var notLike = ""
  if i > 0:
    if isLeft:
      notLike = fmt"and path not like '{sorted[i-1].leftPath}/%'"
    else:
      notLike = fmt"and path not like '{sorted[i-1].rightPath}/%'"
  return notLike

proc getPathQuery(diffPoint: DiffPoint, isLeft: bool): string =
  if (isLeft):
    return fmt"select path from Node where nodeId = {diffPoint.leftTreeId}"
  return fmt"select path from Node where nodeId = {diffPoint.rightTreeId}"


proc getCountQuery(path, notLikeQuery: string): string =
  return fmt"select count(nodeId) from Node where path like '{path}/%' {notLikeQuery}"


proc assignDescCountIncreases*(leftDB, rightDB: DbConn, list: seq[DiffPoint],
    debug: bool = false) =

  let sorted = list.sortedByIt(-it.leftTreeId)

  if debug:
    echo sorted

  for i, diffPoint in sorted:

    var temp1: int
    var temp2: int

    var pathQuery: string
    var notLikeQuery: string
    var countQuery: string

    pathQuery = getPathQuery(diffPoint, true)
    notLikeQuery = getNotLikeQuery(i, sorted, true)

    diffPoint.leftPath = leftDB.getValue(sql(pathQuery))

    countQuery = getCountQuery(diffPoint.leftPath, notLikeQuery)

    # echo countQuery

    discard leftDB.getValue(sql(countQuery)).parseInt(temp1)
  

    pathQuery = getPathQuery(diffPoint, false)
    notLikeQuery = getNotLikeQuery(i, sorted, false)

    diffPoint.rightPath = rightDB.getValue(sql(pathQuery))

    countQuery = getCountQuery(diffPoint.rightPath, notLikeQuery)

    discard rightDB.getValue(sql(countQuery)).parseInt(temp2)


    # if debug:
    #   echo "temp1: ", temp1
    #   echo "temp2: ", temp2

    # var difference = abs(temp2 - temp1)
    var difference = temp2 < temp1 ? temp2 | (temp2 - temp1)

    if temp2 < temp1:
      if diffPoint.highlightLeft.len() + diffPoint.highlightRight.len() < 2:
        difference.dec()
    else:
      if diffPoint.highlightLeft.len() + diffPoint.highlightRight.len() > 2:
        difference.inc()

    if debug:
      echo "difference: ", difference

    diffPoint.descCount = difference





proc findDiffLocationsBoyo*(leftDB,
                            rightDB: DBConn;
                            debug: bool = false):
                             seq[DiffPoint] =

#Concept of the diff point is wrong, what we need is a way of keeping track of different branches

  var res: type(result) = @[]

  let dbs = [leftDB, rightDb]

  let kidsQuery = "select nodeId from Node where parentId = ?"

  var tuples = newSeq[(string, string)]()

  proc recursive(ids: array[2, string]; prevIds: array[2, string]) =
    var kids: array[2, seq[string]]

    if debug:
      echo fmt"Current ", ids
      echo checkDomainsAreEqual([leftDB, rightDB], ids)

    if not checkDomainsAreEqual([leftDB, rightDB], ids):

      let t = (prevIds[0], prevIds[1])
      if tuples.contains(t):
        return

      for i in countUp(0, 1):
        for row in dbs[i].fastRows(sql(kidsQuery), prevIds[i]):
          kids[i].add(row[0])

      let cleanKids = removeDuplicates(leftDB, rightDB, kids)

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

  assignDescCountIncreases(leftDB, rightDB, result, debug)

  if debug:
    for d in result:
      echo d
