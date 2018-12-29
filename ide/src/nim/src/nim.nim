# Hello Nim!
import db_sqlite, json, jester, parseUtils,typetraits , re
import parser 

var db : DbConn

type Response = ref object of RootObj
  parentId: string
  children: seq[string]

proc init(dirPath: string) =
    # let minionFilePath = path & "/model000001.eprime-minion"
    # let eprimeFilePath = path & "/model000001.eprime"
    # initParser(minionFilePath, eprimeFilePath)
    let dbPath = dirPath & "/test.db"
    db = open(dbPath, "", "", "") 

proc getParent(childId: int): string =
    let row = db.getRow(sql"select parentId from Node where nodeId = ?", childId)
    echo row
    return row[0]

proc getNParents(amount, start: string): string =

    var nodes : seq[string]

    for nodeId in db.fastRows(sql"select parentId from Node where nodeId > ? limit ?", start, amount):
        nodes.add(nodeId)

    return $(%nodes)

proc getChildren(pId: string): string =

    var kids : seq[string]

    for childId in db.fastRows(sql"select nodeId from Node where parentId = ?", pId):
        kids.add(childId)

    return $(%Response(parentId: pid, children: kids))


routes:

    get re"/init/(.*)":
        let path = request.matches[0]
        init(path)
        resp "OK"

    get "/domains/@nodeId":
        resp domainsToJson(getPrettyDomainsOfNode(db, @"nodeId"))

    get "/getParent/@nodeId":
        var nodeId : int
        discard parseInt(@"nodeId", nodeId)
        resp getParent(nodeId)
    
    get "/getNParents/@amount/@start":
        resp getNParents(@"amount", @"start") 

    get "/getChildren/@parentId":
        resp getChildren(@"parentId")


let path = "/home/tom/EssenceCatalog/problems/csplib-prob001/conjure-output"
init(path)
echo getChildren("321")
# getParent(320)
# echo getFirstN(10)