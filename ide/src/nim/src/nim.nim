# Hello Nim!
import db_sqlite, json, jester, parseUtils,typetraits, re, os
include parser/parser

var db : DbConn

type SimpleDomainResponse = ref object of RootObj
    changedIds : seq[int]
    vars : seq[Variable]

type PrettyDomainResponse = ref object of RootObj
    changedIds : seq[string]
    vars : seq[Variable]

type ParentChild = ref object of RootObj
    parentId: int
    label: string
    children: seq[int]

proc `$`(r: ParentChild): string =
    return $r.parentId & " : " & $r.children

proc init(dirPath: string) =

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

    initParser(minionFilePath, eprimeFilePath)

    db = open(dbFilePath, "", "", "") 

proc loadNodes(amount, start: string): JsonNode =

    var list :seq[ParentChild]
    var pId, childId : int
    
    for row1 in db.fastRows(sql"select nodeId, parentId, branchingVariable, value from Node where nodeId > ? limit ?", start, amount):
        var kids : seq[int]
        for row2 in db.fastRows(sql"select nodeId from Node where parentId = ?", row1[0]):
            discard parseInt(row2[0], childId)
            kids.add(childId)

        discard parseInt(row1[1], pId)
        list.add(ParentChild(parentId: pId, label: row1[2] & " = " & row1[3], children: kids))

    # echo nodes
    # echo list
    
    return %list



proc loadSimpleDomains(amount, start, nodeId: string): JsonNode =

    var list : seq[int]
    var id : int
    var domainsAtPrev : seq[Variable]
    discard parseInt(nodeId, id)

    let domainsAtNode = getSimpleDomainsOfNode(db, amount, start, nodeId)
    # echo start
    # echo domainsAtNode

    if (id != 1):
        domainsAtPrev = getSimpleDomainsOfNode(db, amount, start, $(id - 1))
        
        for i in 0..<domainsAtNode.len():
            if (domainsAtNode[i].rng != domainsAtPrev[i].rng):
                list.add(i)

    # echo "3"

    return  %SimpleDomainResponse(changedIds: list, vars: domainsAtNode)


proc loadPrettyDomains(amount, start, nodeId: string): JsonNode =
    var list : seq[string]
    var id : int
    var domainsAtPrev : seq[Variable]
    
    discard parseInt(nodeId, id)

    var domainsAtNode : seq[Variable]
    domainsAtNode.deepCopy(getPrettyDomainsOfNode(db, nodeId))

    let jsonList = %domainsAtNode


    for i in 0..<domainsAtNode.len():
        if domainsAtNode[i] of Set:
            let s = cast[Set](domainsAtNode[i])

            jsonList[i]["Cardinality"] = %s.getCardinality()
            jsonList[i]["Included"] = %s.included
            jsonList[i]["Excluded"] = %s.excluded


    if (id != 1):
        domainsAtPrev = getPrettyDomainsOfNode(db, $(id - 1))

        for i in 0..<domainsAtNode.len():

            # echo domainsAtNode[i]
            # echo domainsAtPrev[i]

            if domainsAtNode[i] of Set:
                let s1 = cast[Set](domainsAtNode[i])
                let s2 = cast[Set](domainsAtPrev[i])

                if (s1.getCardinality() != s2.getCardinality()):
                    list.add("li" & s1.name & "Cardinality")

                if (s1.included != s2.included):
                    list.add("li" & s1.name & "Included")

                if (s1.excluded != s2.excluded):
                    list.add("li" & s1.name & "Excluded")

            elif domainsAtNode[i] of Expression:
                if (domainsAtNode[i].rng != domainsAtPrev[i].rng):
                    list.add("liExpressions" & domainsAtNode[i].name) 
            else:
                if (domainsAtNode[i].rng != domainsAtPrev[i].rng):
                    list.add("liVariables" & domainsAtNode[i].name )

    
    if id == 1:
        return domainsToJson(domainsAtNode)

    return %*{"vars" : jsonList, "changed" : list }

proc getCorrectPath(): JsonNode =

    var ids : seq[int]
    var id : int

    for row in db.fastRows(sql("""with recursive
	correctPath(n) as (
	select max(nodeId) from Node
	union 
	select parentId  from Node, correctPath
		where nodeId=correctPath.n  and parentId > 0
    )
    select * from correctPath;""")):
        discard parseInt(row[0], id)
        ids.add(id)

    return % ids


routes:

    get re"/init/(.*)":
        let path = request.matches[0]
        init(path)
        resp "OK"

    get "/simpleDomains/@amount/@start/@nodeId":
        resp loadSimpleDomains(@"amount", @"start", @"nodeId")
        
    get "/prettyDomains/@amount/@start/@nodeId":
        resp loadPrettyDomains(@"amount", @"start", @"nodeId")

    get "/loadNodes/@amount/@start":
        resp loadNodes(@"amount", @"start")

    get "/correctPath":
        resp getCorrectPath()


# let path = "/home/tom/EssenceCatalog/problems/csplib-prob001/conjure-output"
# let path = "/home/tom/ModRef2018-Langfords/experiment/conjure-output";
let path = "/home/tom/conjure/ide/src/test/testData/conjure-test"
init(path)
# echo getCorrectPath()
echo loadNodes("1", "8")
# echo loadSimpleDomains("1", "0", "2")
# echo loadPrettyDomains("1", "0", "2").pretty()
# getParent(320)
# echo getFirstN(10)