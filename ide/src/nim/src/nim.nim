# Hello Nim!
import db_sqlite
import json


var db : DbConn

proc connect(path: string) =
    db = open(path, nil, nil, nil)  # user, password, database name can be nil

proc getChildren(amount: int, parent: int) =
    var i = 0;
    for child in db.fastRows(sql"select nodeId, parentId, isLeftChild from Node where parentId = ? limit ?", parent, amount):
        # echo child
        var childJson = %* { "name" : child[0], "domains" : []}

        for domain in db.fastRows(sql"select name, lower, upper from Domain where nodeId = ?", child[0]):
            let domainJson = %* {"name" : domain[0], "range": getPrettyRange(domain[1], domain[2])}
            childJson["domains"].add(domainJson)

        echo childJson
            

connect("/home/tom/minion-private/build/test.db");
getChildren(2, 1);
