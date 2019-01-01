# Hello Nim!
import db_sqlite, json, jester, parseUtils,typetraits , re
import parser 

var db : DbConn

type ParentChild = ref object of RootObj
  parentId: int
  children: seq[int]

type Response = ref object of RootObj
    nextNodes: seq[int]
    parentChildren: seq[ParentChild]


proc `$`(r: ParentChild): string =
    return $r.parentId & " : " & $r.children

proc `$`(r: Response): string =
    return $r.nextNodes & " : " & $r.parentChildren

proc init(dirPath: string) =
    # let minionFilePath = path & "/model000001.eprime-minion"
    # let eprimeFilePath = path & "/model000001.eprime"
    # initParser(minionFilePath, eprimeFilePath)
    let dbPath = dirPath & "/test.db"
    db = open(dbPath, "", "", "") 

proc loadNodes(amount, start: string): string =

    var list :seq[ParentChild]
    var pId, childId : int
    
    for row1 in db.fastRows(sql"select nodeId, parentId from Node where nodeId > ? limit ?", start, amount):
        var kids : seq[int]
        for row2 in db.fastRows(sql"select nodeId from Node where parentId = ?", row1[0]):
            discard parseInt(row2[0], childId)
            kids.add(childId)

        discard parseInt(row1[1], pId)
        list.add(ParentChild(parentId: pId, children: kids))

    # echo nodes
    echo list
    
    return $(%list)

routes:

    get re"/init/(.*)":
        let path = request.matches[0]
        init(path)
        resp "OK"

    get "/domains/@nodeId":
        resp domainsToJson(getPrettyDomainsOfNode(db, @"nodeId"))

    get "/loadNodes/@amount/@start":
        resp loadNodes(@"amount", @"start")

let path = "/home/tom/EssenceCatalog/problems/csplib-prob001/conjure-output"
init(path)
discard loadNodes("5", "50")
# getParent(320)
# echo getFirstN(10)