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

    return $(%ParentChild(parentId: parentId, children: kids))

proc getNChildren(amount, start: string): string =

    var res : Response
    var list :seq[ParentChild]
    var nodes : seq[int]
    var id, childId : int
    var pId : int
    var count : int

    discard parseInt(start, pId)
    # pId.inc()
    discard parseInt(amount, count)
    count.inc()

    while count > 0:
        # echo "PARENT " & $parentId 
        var kids : seq[int]
        for row in db.fastRows(sql"select nodeId from Node where parentId = ?", pId):
            # echo row
            discard parseInt(row[0], childId)
            kids.add(childId)


        list.add(ParentChild(parentId: pId, children: kids))

        pId.inc()
        count.dec()

    
    for row in db.fastRows(sql"select parentId from Node where nodeId > ? limit ?", start, amount):
        discard parseInt(row[0], id)
        nodes.add(id)
    
    res = Response(nextNodes: nodes, parentChildren: list)

    # echo "Amount: " & amount & " start " & start
    # echo res
    return $(%res)

    

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

    get "/getNChildren/@amount/@start":
        resp getNChildren(@"amount", @"start")

let path = "/home/tom/EssenceCatalog/problems/csplib-prob001/conjure-output"
init(path)
discard getNChildren("5", "50")
# getParent(320)
# echo getFirstN(10)