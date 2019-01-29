# Hello Nim!
import  jester, typetraits 
include process

var db : DbConn

type SimpleDomainResponse = ref object of RootObj
    changedNames : seq[string]
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

    initParser(minionFilePath, eprimeFilePath)

    db = open(dbFilePath, "", "", "") 

proc loadNodes*(amount, start: string): JsonNode =

    var list :seq[ParentChild]
    var pId, childId : int
    
    for row1 in db.fastRows(sql"select nodeId, parentId, branchingVariable, value, isLeftChild from Node where nodeId > ? limit ?", start, amount):
        var kids : seq[int]
        for row2 in db.fastRows(sql"select nodeId from Node where parentId = ?", row1[0]):
            discard parseInt(row2[0], childId)
            kids.add(childId)

        discard parseInt(row1[1], pId)

        var equality: string

        if (row1[4] == "1"):
            equality = " = "
        else:
            equality = " != "


        list.add(ParentChild(parentId: pId, label: row1[2] & equality & row1[3], children: kids))

    # echo nodes
    # echo list
    
    return %list



proc loadSimpleDomains*(amount, start, nodeId: string): JsonNode =

    var list : seq[string]
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
                list.add(domainsAtNode[i].name)
                # list.add(i)

    # echo "3"

    return  %SimpleDomainResponse(changedNames: list, vars: domainsAtNode)


proc loadPrettyDomains*(amount, start, nodeId: string): JsonNode =
    var list : seq[string]
    var id : int
    var domainsAtPrev : seq[Variable]
    var domainsAtNode : seq[Variable]
    var changedExpressions : seq[Expression]
    
    discard parseInt(nodeId, id)

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

                let sets = "liItemsDomain Variables"

                let s1 = cast[Set](domainsAtNode[i])
                let s2 = cast[Set](domainsAtPrev[i])

                var different = false

                if (s1.getCardinality() != s2.getCardinality()):
                    different = true
                    list.add("li" & s1.name & "Cardinality")
                    if not (sets in list):
                        list.add(sets)

                if (s1.included != s2.included):
                    different = true
                    list.add("li" & s1.name & "Included")
                    if not (sets in list):
                        list.add(sets)

                if (s1.excluded != s2.excluded):
                    different = true
                    list.add("li" & s1.name & "Excluded")
                    if not (sets in list):
                        list.add(sets)

                if different:
                    list.add("liDomain Variables" & s1.name)

            elif domainsAtNode[i] of Expression:
                let expressions = "liItemsExpressions"

                if (domainsAtNode[i].rng != domainsAtPrev[i].rng):
                    list.add("liExpressions" & domainsAtNode[i].name) 
                    let expression = cast[Expression](domainsAtNode[i])
                    changedExpressions.add(expression)
                    if not (expressions in list):
                        list.add(expressions)
            else:
                let variables = "liItemsDomain Variables"
                if (domainsAtNode[i].rng != domainsAtPrev[i].rng):
                    list.add("liDomain Variables" & domainsAtNode[i].name )
                    if not (variables in list):
                        list.add(variables)

        if list.len() > 0:
            list.add("liItems")

    
    else:
        return domainsToJson(domainsAtNode)

    return %*{"vars" : jsonList, "changed" : list, "changedExpressions": expressionsToJson(changedExpressions) }

proc getCorrectPath*(): JsonNode =

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

proc getLongestBranchingVarName*() : JsonNode =
    return % db.getRow(sql"select max(length(branchingVariable)) from Node")[0]