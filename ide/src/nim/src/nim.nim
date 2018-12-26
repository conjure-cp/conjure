# Hello Nim!
import db_sqlite, json, jester, parseUtils,typetraits , re
import parser 

var db : DbConn

routes:

    get re"/init/(.*)":
        let path = request.matches[0]
        echo path
        let minionFilePath = path & "/model000001.eprime-minion"
        let eprimeFilePath = path & "/model000001.eprime"
        let dbPath = path & "/test.db"
        db = open(dbPath, "", "", "") 
        init(minionFilePath, eprimeFilePath)
        resp "OK"

    get "/domains/@nodeId":
        resp domainsToJson(getPrettyDomainsOfNode(db, @"nodeId"))

    get "/next/@nodeId":
        var nodeId : int
        discard parseInt(@"nodeId", nodeId)
        resp db.getRow(sql"select * from Node where nodeId = ?", nodeId + 1)[0]