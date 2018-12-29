# Hello Nim!
import db_sqlite, json, jester, parseUtils,typetraits , re
import parser 

var db : DbConn

type Response = ref object of RootObj
  parentId: int
  children: seq[int]

proc init(dirPath: string) =
    # let minionFilePath = path & "/model000001.eprime-minion"
    # let eprimeFilePath = path & "/model000001.eprime"
    # initParser(minionFilePath, eprimeFilePath)
    let dbPath = dirPath & "/test.db"
    db = open(dbPath, "", "", "") 


proc getNParents(amount, start: string): string =

    var nodes : seq[int]
    var id : int

    for row in db.fastRows(sql"select parentId from Node where nodeId > ? limit ?", start, amount):
        discard parseInt(row[0], id)
        nodes.add(id)

    return $(%nodes)

proc getChildren(pId: string): string =

    var kids : seq[int]
    var childId : int
    var parentId : int

    discard parseInt(pId, parentId)

    for row in db.fastRows(sql"select nodeId from Node where parentId = ?", pId):
        discard parseInt(row[0], childId)
        kids.add(childId)

    return $(%Response(parentId: parentId, children: kids))


routes:

    get re"/init/(.*)":
        let path = request.matches[0]
        init(path)
        resp "OK"

    get "/domains/@nodeId":
        resp domainsToJson(getPrettyDomainsOfNode(db, @"nodeId"))
    
    get "/getNParents/@amount/@start":
        resp getNParents(@"amount", @"start") 

    get "/getChildren/@parentId":
        resp getChildren(@"parentId")


let path = "/home/tom/EssenceCatalog/problems/csplib-prob001/conjure-output"
init(path)
echo getChildren("321")
# getParent(320)
# echo getFirstN(10)