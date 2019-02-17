# Hello Nim!
import  jester, typetraits, sequtils 
import jsonify
include process

var db : DbConn

proc getExpandedSetChild*(nodeId, path : string): Set =

    # echo prettyLookup

    result = cast[Set](prettyLookup[nodeId][path.split(".")[0]])

    for name in path.split(".")[1..^1]:
        for kid in result.children:
            if kid.name == name:
                result = kid
                break;
                
    if result.name != path.split(".")[^1]:
        return nil

proc getJsonVarList*(domainsAtNode: seq[Variable], paths, nodeId: string): JsonNode =
    result = %*[]
    if paths != "":
        for path in paths.split(":"):
            let child = (getExpandedSetChild(nodeId, path))

            if (child == nil):
                result.add(%*{"name": path.split(".")[^1], "removed": true})
            else:
                result.add(setToJson(child, nodeId, false))

    for v in domainsAtNode:
        if (v of Set):
            result.add(setToJson(cast[Set](v), nodeId, true))
        else:
            result.add(%v)

proc loadSetChild*(nodeId, path : string): JsonNode =
    let s = getExpandedSetChild(nodeId, path)
    let update = setToJson(s, nodeId, true)
    return %*{"structure" : %setToTreeView(s), "update" : update, "path" : path}

proc getLabel(varName : string, value: string, branch : string): string =

    result &= varName

    if (branch == "1"):
        result &= " = "
    else:
        result &= " != "

    result &= value


proc init*(dirPath: string) =

    let current = getCurrentDir()
    setCurrentDir(dirPath)

    var minionFilePath : string
    var eprimeFilePath : string
    var dbFilePath : string

    for f in walkFiles("*.eprime-minion"):
        minionFilePath =  absolutePath(f)
        break;

    for f in walkFiles("*.eprime"):
        eprimeFilePath =  absolutePath(f)
        break;

    for f in walkFiles("*.db"):
        dbFilePath =  absolutePath(f)
        break;
        
    setCurrentDir(current)

    if dbFilePath == "":
        raise newException(CannotOpenDatabaseException, "No files with .db extension found")

    initParser(minionFilePath, eprimeFilePath)


    db = open(dbFilePath, "", "", "") 


proc loadNodes*(amount, start: string): seq[ParentChild] =

    var nId, pId, childId : int

    let query = "select nodeId, parentId, branchingVariable, value, isLeftChild from Node where nodeId > ? limit ?"
    
    for row1 in db.fastRows(sql(query), start, amount):
        discard parseInt(row1[0], nId)
        var kids : seq[int]
        for row2 in db.fastRows(sql"select nodeId from Node where parentId = ?", row1[0]):
            discard parseInt(row2[0], childId)
            kids.add(childId)

        discard parseInt(row1[1], pId)

        result.add(ParentChild(parentId: pId, nodeId: nId, label: getLabel(row1[2], row1[3], row1[4]), children: kids))


proc loadChildren*(id : string): ChildResponse = 

    var nId, childId : int
    discard parseInt(id, nId)
    var kids : seq[int]
    for row in db.fastRows(sql"select nodeId from Node where parentId = ?", id):
        discard parseInt(row[0], childId)
        kids.add(childId)
    return ChildResponse(nodeId: nId , children: kids)


proc loadCore*(): seq[ParentChild] = 
    var nId, pId, childId : int

    let query = sql(
        """with recursive
        correctPath(n) as (
        select max(nodeId) from Node
        union 
        select parentId  from Node, correctPath
            where nodeId=correctPath.n  and parentId > 0
        )
        select nodeId, parentId, branchingVariable, value, isLeftChild from Node where nodeId in correctPath;""")

    for row1 in db.fastRows(query):
        discard parseInt(row1[0], nId)
        var kids : seq[int]
        for row2 in db.fastRows(sql"select nodeId from Node where parentId = ?", row1[0]):
            discard parseInt(row2[0], childId)
            kids.add(childId)

        discard parseInt(row1[1], pId)

        result.add(ParentChild(parentId: pId, nodeId: nId, label: getLabel(row1[2], row1[3], row1[4]), children: kids))


proc loadSimpleDomains*(nodeId: string, wantExpressions: bool = false): SimpleDomainResponse =

    var list : seq[string]
    var id : int
    var domainsAtPrev : seq[Variable]
    discard parseInt(nodeId, id)

    let domainsAtNode = getSimpleDomainsOfNode(db, nodeId, wantExpressions)

    if (id != 1):
        domainsAtPrev = getSimpleDomainsOfNode(db,  $(id - 1), wantExpressions)
        
        for i in 0..<domainsAtNode.len():
            if (domainsAtNode[i].rng != domainsAtPrev[i].rng):
                list.add(domainsAtNode[i].name)

    return  SimpleDomainResponse(changedNames: list, vars: domainsAtNode)


proc temp(db: DbConn, nodeId, paths: string): JsonNode = 
    var domainsAtNode : seq[Variable]
    var id : int
    var changeList : seq[string]
    discard parseInt(nodeId, id)
    domainsAtNode.deepCopy(getPrettyDomainsOfNode(db, nodeId))

    if (id != 1):
        var domainsAtPrev = getPrettyDomainsOfNode(db, $(id - 1))
        changeList = getPrettyChanges(domainsAtNode, domainsAtPrev)
    
        return %*{"vars" : getJsonVarList(domainsAtNode, paths, nodeId), "changed" : changeList, "changedExpressions": %[] }

    return domainsToJson(domainsAtNode)


proc loadPrettyDomains*(nodeId, paths: string): JsonNode =
    temp(db, nodeId, paths)

proc getLongestBranchingVarName*() : JsonNode =
    return % db.getRow(sql"select max(length(branchingVariable)) from Node")[0]