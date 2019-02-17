# Hello Nim!
import  jester, typetraits, sequtils 
include process

var db : DbConn

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



proc loadSimpleDomains*(amount, start, nodeId: string): SimpleDomainResponse =

    var list : seq[string]
    var id : int
    var domainsAtPrev : seq[Variable]
    discard parseInt(nodeId, id)

    let domainsAtNode = getSimpleDomainsOfNode(db, nodeId)

    if (id != 1):
        domainsAtPrev = getSimpleDomainsOfNode(db,  $(id - 1))
        
        for i in 0..<domainsAtNode.len():
            if (domainsAtNode[i].rng != domainsAtPrev[i].rng):
                list.add(domainsAtNode[i].name)

    return  SimpleDomainResponse(changedNames: list, vars: domainsAtNode)


proc setToJson(s: Set, nodeId : string, wantCollapsedChildren : bool): JsonNode =

    let json = %*{}

    json["name"] = %s.name
    json["Cardinality"] = %s.getCardinality()
    if (s.inner == nil):
        json["Included"] = %s.included
        json["Excluded"] = %s.excluded
    else:
        if wantCollapsedChildren:
            json["Children"] = getCollapsedSetChildren(s)
        # else:
            # json["Children"] = %setToTreeView(s)


    return json

proc getExpandedSetChild*(nodeId, path : string): Set =

# TODO return nil if cant find child


    var s = cast[Set](prettyLookup[nodeId][path.split(".")[0]])

    for name in path.split(".")[1..^1]:
        for kid in s.children:
            if kid.name == name:
                s = kid
                break;

    return (s)

proc loadSetChild*(nodeId, path : string): JsonNode =
    let s = getExpandedSetChild(nodeId, path)
    let update = setToJson(s, nodeId, true)
    return %*{"structure" : %setToTreeView(s), "update" : update, "path" : path}


proc loadPrettyDomains*(nodeId, paths: string): JsonNode =
    var list : seq[string]
    var id : int
    var domainsAtNode : seq[Variable]
    var changedExpressions : seq[Expression]
    
    discard parseInt(nodeId, id)

    domainsAtNode.deepCopy(getPrettyDomainsOfNode(db, nodeId))

    let jsonList = %[] 


    if paths != "":
        for path in paths.split(":"):
            let child = (getExpandedSetChild(nodeId, path))
            jsonList.add(setToJson(child, nodeId, false))


    for v in domainsAtNode:
        if (v of Set):
            jsonList.add(setToJson(cast[Set](v), nodeId, true))
        else:
            jsonList.add(%v)
    

    if (id != 1):
        var domainsAtPrev = getPrettyDomainsOfNode(db, $(id - 1))

        for i in 0..<domainsAtNode.len():

            # echo domainsAtNode[i]
            # echo domainsAtPrev[i]

            if domainsAtNode[i] of Set:

                let sets = "liItemsDomain Variables"

                let s1 = cast[Set](domainsAtNode[i])
                let s2 = cast[Set](domainsAtPrev[i])

                var different = false

                # for kid in s1.children:
                #     list.add("li" & kid.name)

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

    # echo (%jsonList).pretty()

    return %*{"vars" : jsonList, "changed" : list, "changedExpressions": expressionsToJson(changedExpressions) }


proc getLongestBranchingVarName*() : JsonNode =
    return % db.getRow(sql"select max(length(branchingVariable)) from Node")[0]



# proc loadChildSets*(setName, nodeId : string) : JsonNode = 
#     return getChildSets(setName, nodeId)



# proc loadAllNodes*(): JsonNode = 

#     # var id2Node = initTable[int, Node]()
#     # var id2Parent = initTable[int, Node]()
#     # var id2ChildIds = initTable[int, seq[int]]()
#     var id2Node = %*{}
#     var id2Parent = %*{}
#     var id2ChildIds = %*{}
#     var nodeId : int
#     var parentId : int

#     let query = sql(
#         """with recursive
#         correctPath(n) as (
#         select max(nodeId) from Node
#         union 
#         select parentId  from Node, correctPath
#             where nodeId=correctPath.n  and parentId > 0
#         )
#         select nodeId, parentId, branchingVariable, value, isLeftChild from Node where nodeId in correctPath;""")

#     # for row in db.fastRows(sql"select nodeId, parentId, branchingVariable, value, isLeftChild from Node"):
#     for row in db.fastRows(query):
#         discard parseInt(row[0], nodeId)
#         discard parseInt(row[1], parentId)
#         let node = %*{"id" : nodeId, "name": row[2], "children": []}
#         if nodeId == 1:

#             id2Node["1"] = node
#             id2ChildIds["1"] = %*[]

#         else:
#             id2Node[$parentId]["children"].add(node)
#             id2Node[$nodeId] = node
#             id2Parent[$nodeId] = id2Node[$parentId]
#             id2ChildIds[$parentId].add(node["id"])
#             id2ChildIds[$nodeId] = %*[]
    
#     let query1 = sql(
#         """with recursive
# 	correctPath(n) as (
# 	select max(nodeId) from Node
# 	union 
# 	select parentId  from Node, correctPath
# 		where nodeId=correctPath.n  and parentId > 0
#     )
#     select nodeId, parentId, branchingVariable, value, isLeftChild from Node where parentId in correctPath and nodeId not in correctPath;""")

#     for row in db.fastRows(query1):
#         discard parseInt(row[0], nodeId)
#         discard parseInt(row[1], parentId)
#         let node = %*{"id" : nodeId, "name": row[2], "children": []}
#         id2Node[$parentId]["children"].add(node)
#         id2Node[$nodeId] = node
#         id2Parent[$nodeId] = id2Node[$parentId]
#         id2ChildIds[$parentId].add(node["id"])
#         id2ChildIds[$nodeId] = %*[]

#     # return id2Node["1"]

#     return %*{"id2Node" : id2Node, "id2Parent" : id2Parent,  "id2ChildIds" : id2ChildIds}

        # echo id2Node


    # result = newJObject()
    # for k, v in id2ChildIds.pairs:
    #      result[$k] = %v

    # echo result

    # echo %Blah(b: id2ChildIds)

    # return %*{"id2Node" : id2Node, "id2Parent" : id2Parent, "id2ChildIds": id2ChildIds}


# proc loadCore*