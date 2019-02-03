include types
import re, strutils, os, tables, json, db_sqlite, parseutils 
# import nre except toSeq
export re, strutils, os, tables, json, db_sqlite, parseutils 

let digitRe = re"\_(\d+)"

proc parseAux(minionFilePath: string): Table[string, Expression] =
    var lookup = initTable[string, Expression]()
    let auxDef = re"aux\d* #(.*)"
    let minionFile = readFile(minionFilePath)
    let find = minionFile.findAll(auxDef)

    try:

        for a in find:
            let splitted = a.split("#")
            let name = splitted[0].strip()
            var rhs = splitted[1].replace(re"\(?Active-CSE: \d* occurrences of this expression or equivalent: ","")

            let nestedAux = re"aux\d*"

            while (rhs.findAll(nestedAux).len() > 0):
                for nested in rhs.findAll(nestedAux):
                    if (lookup.hasKey(nested)):
                        rhs = rhs.replace(nested, lookup[nested].name)
                    # echo nested

            # echo rhs


            lookup[name] = newExpression(rhs)
    except:
        raise newException(MinionParseException, "Failed to parse eprime")

    return lookup

proc parseSet(s: JsonNode, name: string): Set =

    # let n = "inner"

    let arr = s.getElems()

    if arr[^1].hasKey("DomainSet"):
        let innerArr = arr[^1]["DomainSet"]
        if (arr[0].hasKey("Set_ExplicitVarSizeWithMarker")):
            return newMarkerSet(name, inner = parseSet(innerArr, name))

        if (arr[0].hasKey("Set_ExplicitVarSizeWithFlags")):
            return newFlagSet(name, inner = parseSet(innerArr, name))

        if (arr[0].hasKey("Set_Explicit")):
            return newExplicitSet(name, inner = parseSet(innerArr, name))
            
        # if innerArr[0].hasKey("Set_Occurrence"):

    if arr[0].hasKey("Set_Explicit"):
        return newExplicitSet(name, arr[1]["SizeAttr_Size"]["Constant"]["ConstantInt"].getInt(-1)) 

    if arr[^1].hasKey("DomainInt"):

        var bounds  : JsonNode

        bounds = arr[^1]["DomainInt"].getElems()[0]

        if (bounds.hasKey("RangeBounded")):
            bounds = arr[^1]["DomainInt"].getElems()[0]["RangeBounded"]
        else:
            bounds = arr[^1]["DomainInt"].getElems()[1][0]["RangeBounded"]
        # echo bounds
        var l = bounds[0]["Constant"]["ConstantInt"].getInt(-1)
        var u = bounds[1]["Constant"]["ConstantInt"].getInt(-1)

        if (l == -1):
            l = bounds[0]["Constant"]["ConstantInt"][1].getInt(-1)
            u = bounds[1]["Constant"]["ConstantInt"][1].getInt(-1)

            if (l == -1):
                echo "ERRORORORRORORORORRO"


        if arr[0].hasKey("Set_ExplicitVarSizeWithDummy"):
            return newDummySet(name, lowerBound = l, upperBound = u, dummyVal = u + 1) 

        elif arr[0].hasKey("Set_Occurrence"):
            return newOccurrenceSet(name, lowerBound = l, upperBound = u) 

        elif arr[0].hasKey("Set_ExplicitVarSizeWithMarker"):
            return newMarkerSet(name, lowerBound = l, upperBound = u) 

        elif arr[0].hasKey("Set_ExplicitVarSizeWithFlags"):
            return newFlagSet(name, lowerBound = l, upperBound = u) 

proc parseEprime(eprimeFilePath: string): Table[string, Variable] =

    var varLookup = initTable[string, Variable]()
    var clean = ""

    for line in readFile(eprimeFilePath).split("Conjure's")[1].split("\n"):
        if len(line) == 0:
            continue
        clean &= line[1..^1]

    try:
    
        for key in parseJson(clean)["representations"].getElems():

            if ( not key[0].haskey("Name")):
                continue

            let name = key[0]["Name"].getStr()

            if key[1].hasKey("DomainInt"):
                varLookup[name] = newVariable(name)

            if key[1].hasKey("DomainSet"):

                varLookup[name] = parseSet(key[1]["DomainSet"], name)
    except:
        raise
        # raise newException(EprimeParseException, "Failed to parse eprime")

    # echo varLookup
    return varLookup


# import "parseEprime.nim", "parseAux.nim"
var eprimeLookup : Table[string, Variable]
var auxLookup : Table[string, Expression]

proc initParser(minionFilePath: string, eprimeFilePath: string) =
    eprimeLookup.clear()
    auxLookup.clear()
    eprimeLookup = parseEprime(eprimeFilePath)
    auxLookup = parseAux(minionFilePath)

proc getSimpleDomainsOfNode(db: DbConn, amount: string, start: string, nodeId: string): seq[Variable] =

    var domains : seq[Variable]

    for domain in db.fastRows(sql"select name, lower, upper from domain where nodeId = ? and domainId >= ? order by name limit ? ",nodeId, start, amount):

        # echo domain[0]
        # echo auxLookup

        if (auxLookup.hasKey(domain[0])):
            let d = auxLookup[domain[0]]
            d.rng = getPrettyRange(domain[1], domain[2])
            domains.add(d)
        else:
            let v = newVariable(name = domain[0])
            v.rng = getPrettyRange(domain[1], domain[2])
            domains.add(v)

    return domains


proc getPrettyExplicitOrOccurrenceOrDummySet(db: DbConn, inner: Set, parent: Set, nodeId : string) =


    echo "@ occurence or dummy"

    var num : int
    var lower : int
    # var markerSet : MarkerSet
    # var temp : Set
    var currentSet : Set
    var prefix = "%_"
    let qLeft = "SELECT name, lower FROM domain WHERE name like '" 
    let qRight = "' and lower = upper and nodeId = ?;"

    if inner of OccurrenceSet:
        currentSet = cast[OccurrenceSet](inner)
        prefix &= "Occurrence%"
    if inner of DummySet:
        currentSet = cast[DummySet](inner)
        prefix &= "ExplicitVarSizeWithDummy%"
    let query = qLeft & parent.name & prefix & qRight

    var setTable = initTable[int, Set]()

    for res in db.rows(sql(query), nodeId):
        discard res[1].parseInt(lower)
        let numbers = res[0].findAll(digitRe)
        var setId : int
        discard numbers[0][1..^1].parseInt(setId)

        if (parent of MarkerSet):
            let mS = cast[MarkerSet](parent)
            # echo "setId " & $setId
            # echo "marker Lower " & $mS.markerLower

            if (setId > mS.markerLower):
                break
        
        if (parent of FlagSet):
            let fS = cast[FlagSet](parent)

            if (setId > fS.maxSetTo1):
                break

            echo fS.maxSetTo1
            # echo fS.maxSetTo0


        if not setTable.hasKey(setId):
            currentSet.deepCopy(inner)
            currentSet.name = $setId
            setTable[setId] = currentSet
        else:
            currentSet = setTable[setId]


        discard numbers[^1][1..^1].parseInt(num)

        if (inner of OccurrenceSet):
            if res[1] == "1":
                currentSet.included.add(num)
            else:
                currentSet.excluded.add(num)

        if (inner of DummySet):
            var dS = cast[DummySet](currentSet)

            if res[1] == $dS.dummyVal:
                dS.excludedCount.inc()
            else:
                dS.included.add(lower)

        if (inner of ExplicitSet):
            var eS = cast[ExplicitSet](currentSet)
            es.included.add(lower)


    for child in setTable.values():
        parent.children.add(child)

proc getPrettyNestedFlagSet(db: DbConn, inner: Set, parent: Set, nodeId : string) =

    var num : int
    var lower : int
    var upper : int
    var currentSet : FlagSet
    var setId : int
    var maxSetTo1Table = initTable[int, int]()
    var maxSetTo0Table = initTable[int, int]()

    let query1 = sql("SELECT name FROM domain WHERE name like '" & parent.name & "%_ExplicitVarSizeWithFlags_Flags_%' and lower = 1 and upper = 1 and nodeId = ? order by name desc;")
    for res in db.fastRows(query1, nodeId):

        let numbers = res[0].findAll(digitRe)
        var setId : int
        discard numbers[0][1..^1].parseInt(setId)
        discard numbers[^1][1..^1].parseInt(num)

        if (maxSetTo1Table.hasKey(setId)):
            continue

        maxSetTo1Table[setId] = num
        

    let query2 = sql("SELECT name FROM domain WHERE name like '" & parent.name & "%_ExplicitVarSizeWithFlags_Flags_%' and lower = 0 and upper = 0 and nodeId = ? order by name desc;")
    for res in db.fastRows(query2, nodeId):

        echo res

        let numbers = res[0].findAll(digitRe)
        var setId : int
        discard numbers[0][1..^1].parseInt(setId)
        discard numbers[^1][1..^1].parseInt(num)

        if (maxSetTo0Table.hasKey(setId)):
            continue

        maxSetTo0Table[setId] = num

    # select * from domain where name like "%s_ExplicitVarSizeWithFlags_Values_%"  and lower = upper order by name asc 
    let query3 = sql("SELECT name, lower FROM domain WHERE name like '" & parent.name & "%_ExplicitVarSizeWithFlags_Values_%' and lower = upper and nodeId = ? order by name asc;")
    # echo markerQuery
    var setTable = initTable[int, FlagSet]()
    for res in db.rows(query3, nodeId):
        # echo res
        discard res[1].parseInt(lower)

        let numbers = res[0].findAll(digitRe)
        discard numbers[0][1..^1].parseInt(setId)
        discard numbers[^1][1..^1].parseInt(num)

        if (parent of MarkerSet):
            let markerParent = cast[MarkerSet](parent)
            if (setId > markerParent.markerLower):
                break

        if not setTable.hasKey(setId):
            let casted = cast[FlagSet](inner)
            currentSet.deepCopy(casted)
            currentSet.name = $setId
            setTable[setId] = currentSet
        else:
            currentSet = setTable[setId]
        
        # echo maxSetTo1Table
        # echo maxSetTo0Table

        if maxSetTo1Table.hasKey(setId):
            if num <= maxSetTo1Table[setId]:
                currentSet.included.add(lower)
                currentSet.maxSetTo1 = maxSetTo1Table[setId]
        
        if maxSetTo0Table.hasKey(setId):
            if num <= maxSetTo0Table[setId] and not lower in currentSet.included:
                currentSet.excluded.add(lower)

    for child in setTable.values():
        parent.children.add(child)

proc getPrettyNestedMarkedSet(db: DbConn, inner: Set, parent: Set, nodeId : string) =

    var num : int
    var lower : int
    var upper : int
    var currentSet : MarkerSet
    var prefix = "%_ExplicitVarSizeWithMarker_Values_%"
    let qLeft = "SELECT name, lower FROM domain WHERE name like '" 
    let qRight = "' and lower = upper and nodeId = ?;"
    var setId : int
    var markerTable = initTable[int, (int, int)]()

    let markerQuery = "SELECT name, lower, upper FROM domain WHERE name like '" & parent.name & "%_ExplicitVarSizeWithMarker_Marker_%' and nodeId = ?;" 
    # echo markerQuery
    for res in db.rows(sql(markerQuery), nodeId):
        # echo res
        discard res[1].parseInt(lower)
        discard res[2].parseInt(upper)

        if lower == 0 and upper == 0:
            continue 

        let numbers = res[0].findAll(digitRe)
        discard numbers[0][1..^1].parseInt(setId)

        if (parent of MarkerSet):
            let markerParent = cast[MarkerSet](parent)
            if (setId > markerParent.markerLower):
                break

        markerTable[setId] = (lower, upper)

    if markerTable.len() == 0:
        return

    echo markerTable

    let query = qLeft & parent.name & prefix & qRight
    var setTable = initTable[int, MarkerSet]()

    for res in db.rows(sql(query), nodeId):

        # echo res

        discard res[1].parseInt(lower)
        let numbers = res[0].findAll(digitRe)
        var setId : int
        discard numbers[0][1..^1].parseInt(setId)

        if not setTable.hasKey(setId):
            let casted = cast[MarkerSet](inner)
            currentSet.deepCopy(casted)
            currentSet.name = $setId
            setTable[setId] = currentSet
        else:
            currentSet = setTable[setId]

        discard numbers[^1][1..^1].parseInt(num)

    
        (currentSet.markerLower, currentSet.markerUpper) = markerTable[setId]
        
        # echo "lowerBound " & $lowerBound
        # echo "upperBound " & $upperBound
        # echo "num " & $num
        # echo "lower " & $lower

        if num <= currentSet.markerLower:
            currentSet.included.add(lower)
        if num > currentSet.markerUpper and not (lower in currentSet.included):
            currentSet.excluded.add(lower)
    

    for child in setTable.values():
        parent.children.add(child)



proc getPrettySetDomain(db: DbConn, variable: Variable, parent: Set, nodeId: string) =
    let s = cast[Set](variable)

    var num : int
    var lower : int
    
    if (s of ExplicitSet):
        let eSet = cast[ExplicitSet](variable)

        if (eSet.inner != nil):
            getPrettySetDomain(db, s.inner, eSet, nodeId)
        else:
            let query0 = sql("SELECT lower FROM domain WHERE name like '" & variable.name & "_%Explicit%' and lower = upper and nodeId = ?;")
            for res in db.fastRows(query0, nodeId):
                discard res[0].parseInt(lower)
                eSet.included.add(lower)

    if (s of OccurrenceSet):
        let oSet = cast[OccurrenceSet](variable)
        if (parent != nil):
            getPrettyExplicitOrOccurrenceOrDummySet(db, oSet, parent, nodeId)
        else:
            let query = sql("SELECT name, lower FROM domain WHERE name like '%" & variable.name & "_Occurrence%' and lower = upper and nodeId = ?;")
            # echo $query
            for res in db.fastRows(query, nodeId):
                let numbers = res[0].findAll(digitRe)
                discard numbers[^1][1..^1].parseInt(num)

                if res[1] == "1":
                    # echo "Including " & $num
                    oSet.included.add(num)
                else:
                    # echo "Excluding " & $num
                    oSet.excluded.add(num)

    if (variable of DummySet):
        # echo "HERE"
        let dSet = cast[DummySet](variable)
        if (parent != nil):
            getPrettyExplicitOrOccurrenceOrDummySet(db, dSet, parent, nodeId)
        else:
            let query = sql("SELECT name, lower FROM domain WHERE name like '%" & variable.name & "_ExplicitVarSizeWithDummy%' and lower = upper and nodeId = ?;")
            for res in db.fastRows(query, nodeId):
                # echo res
                # echo res[0].findAll(digitRe)
                discard res[1].parseInt(lower)
                let numbers = res[0].findAll(digitRe)
                discard numbers[^1][1..^1].parseInt(num)

                if res[1] == $dSet.dummyVal:
                    dSet.excludedCount.inc()
                    # dSet.excluded.add(num)
                else:
                    dSet.included.add(lower)

    if variable of FlagSet:
        # var maxSetTo1 : int
        let fSet = cast[FlagSet](variable)

        let query1 = "SELECT name FROM domain WHERE name like '" & variable.name & "_%ExplicitVarSizeWithFlags%_Flags_%' and lower = 1 and upper = 1 and nodeId = ? order by name desc limit 1;"
        # echo query1
        var res = db.getValue(sql(query1), nodeId)

        if res != "":
            let numbers = res.findAll(digitRe)
            discard numbers[0][1..^1].parseInt(fSet.maxSetTo1)

        let query2 = sql("SELECT name FROM domain WHERE name like '" & variable.name & "_%ExplicitVarSizeWithFlags%_Flags_%' and lower = 0 and upper = 0 and nodeId = ? order by name desc limit 1;")
        res = db.getValue(query2, nodeId)
        if res != "":
            # discard res.findAll(digitRe)[0].parseInt(fSet.maxSetTo0)
            let numbers = res.findAll(digitRe)
            discard numbers[0][1..^1].parseInt(fSet.maxSetTo0)

        if (fSet.inner != nil):
            if( fSet.inner of OccurrenceSet or fSet.inner of DummySet):
                getPrettySetDomain(db, s.inner, fSet, nodeId)
            
            if (fSet.inner of MarkerSet):
                getPrettyNestedMarkedSet(db, s.inner, fSet, nodeId)

            if (fSet.inner of FlagSet):
                getPrettyNestedFlagSet(db, s.inner, fSet, nodeId)

            return

        # echo "here"
        # select * from domain where name like "%s_ExplicitVarSizeWithFlags_Values_%"  and lower = upper order by name asc 
        let query3 = "SELECT name, lower FROM domain WHERE name like '" & variable.name & "_%ExplicitVarSizeWithFlags_Values_%' and lower = upper and nodeId = ? order by name asc;"

        echo query3

        for res in db.rows(sql(query3), nodeId):
            discard res[1].parseInt(lower)
            discard res[0].findAll(digitRe)[0].parseInt(num)

            echo "num " & $num
            echo "maxSetTo0 " & $fSet.maxSetTo0
            echo "maxSetTo1 " & $fSet.maxSetTo1

            if num <= fSet.maxSetTo1:
                fSet.included.add(lower)

            elif num <= fSet.maxSetTo0:
                fSet.excluded.add(lower)
            

    if variable of MarkerSet:
        let mSet = cast[MarkerSet](variable)
        let query0 = "SELECT lower, upper FROM domain WHERE name like '" & variable.name & "%_Marker' and nodeId = ?;"
        # echo query0
        var res = db.getRow(sql(query0), nodeId)
        # echo res
        discard res[0].parseInt(mSet.markerLower)
        discard res[1].parseInt(mSet.markerUpper)

        # echo "HEEEERE"

        if (mSet.inner != nil):
            if( mSet.inner of OccurrenceSet or mSet.inner of DummySet):
                getPrettySetDomain(db, s.inner, mSet, nodeId)
                return
            
            if (mSet.inner of MarkerSet):
                getPrettyNestedMarkedSet(db, s.inner, mSet, nodeId)
                return

            if (mSet.inner of FlagSet):
                getPrettyNestedFlagSet(db, s.inner, mSet, nodeId)
                return
        
        


        let query1 = "SELECT name, lower FROM domain WHERE name like '" & variable.name & "%_ExplicitVarSizeWithMarker_Values%' and lower = upper and nodeId = ? order by name asc;"
        # echo query1
        for res in db.rows(sql(query1), nodeId):
            echo res
            discard res[1].parseInt(lower)
            discard res[0].findAll(digitRe)[0].parseInt(num)

            if num <= mSet.markerLower:
                mSet.included.add(lower)
            if num > mSet.markerUpper:
                mSet.excluded.add(lower)


proc getPrettyDomainsOfNode(db: DbConn, nodeId: string) : seq[Variable] =
    var list : seq[Variable]

    var tableCopy : Table[string, Variable]
    tableCopy.deepCopy(eprimeLookup)

    for variable in tableCopy.values():
        list.add(variable)
        # echo variable

        if (variable of Set):
            getPrettySetDomain(db, variable, nil, nodeId)
            # break

        elif variable of Expression:
            discard
        else:
            # echo variable.name
            let query0 = sql("SELECT lower, upper FROM domain WHERE name = '" & variable.name & "' and nodeId = ?;")
            var res = db.getRow(query0, nodeId)
            variable.rng = getPrettyRange(res[0], res[1])

    return list



proc setToTreeView(s : Set): TreeViewNode =

    var setType : string

    if (s of OccurrenceSet):
        setType = "Occurrence"
    if (s of DummySet):
        setType = "Dummy"
    if (s of MarkerSet):
        setType = "Marker"
    if (s of FlagSet):
        setType = "Flags"
    if (s of ExplicitSet):
        setType = "Explicit"

    let t = TreeViewNode(name: "Type", children: @[TreeViewNode(name: setType)])
    let cardinality = TreeViewNode(name: "Cardinality", children: @[TreeViewNode(name: s.getCardinality())])
    let included = TreeViewNode(name: "Included", children: @[TreeViewNode(name: ($s.included)[2..^2])])
    let excluded = TreeViewNode(name: "Excluded", children: @[TreeViewNode(name: ($s.excluded)[2..^2])])

    if (s.inner == nil):
        return (TreeViewNode(name: s.name, children: @[t, cardinality, included, excluded]))

    return (TreeViewNode(name: s.name, children: @[t, cardinality]))


proc domainsToJson(domains: seq[Variable]): JsonNode =

    let root = TreeViewNode(name: "Items")
    let variables = TreeViewNode(name: "Domain Variables")
    let expressions = TreeViewNode(name: "Expressions")
    let changedExpressions = TreeViewNode(name: "Changed Expressions")
    # let sets = TreeViewNode(name: "Sets")

    root.children = @[variables, expressions, changedExpressions]

    for d in domains:


        if d of Expression:
            expressions.children.add(TreeViewNode(name: d.name, children: @[TreeViewNode(name: d.rng)]))

        elif d of Set:

            let s = cast[Set](d)

            # echo s

            let treeRep = (setToTreeView(s))
            let kids = TreeViewNode(name: "Children", children: @[TreeViewNode()]) 

            if (s.children.len() > 0):

                for kid in s.children:
                    kids.children.add(setToTreeView(kid))

                treeRep.children.add(kids)

            variables.children.add(treeRep)

        else:
            variables.children.add(TreeViewNode(name: d.name, children: @[TreeViewNode(name: d.rng)]))
        
    # let list = @[root]
    # let json = (%list).pretty()
    return %root


proc expressionsToJson(expressions: seq[Expression]): JsonNode =
    var list : seq[TreeViewNode]

    for exp in expressions:
        list.add(TreeViewNode(name: exp.name, children: @[TreeViewNode(name: exp.rng)]))

    return %list



# proc getPrettyDomainsOfNode(db: DbConn, nodeId: string) : seq[Variable] =
#     # echo auxLookup
#     var domains : seq[Variable]

#     var tableCopy : Table[string, Variable]
#     tableCopy.deepCopy(eprimeLookup)

#     for domain in db.fastRows(sql"select name, lower, upper from domain where nodeId = ? order by name", nodeId):
#         # echo domain

#         if (auxLookup.hasKey(domain[0])):
#             let d = auxLookup[domain[0]]
#             d.rng = getPrettyRange(domain[1], domain[2])
#             domains.add(d)
        
#         elif (tableCopy.hasKey(domain[0])):
#             let v = tableCopy[domain[0]]
#             v.rng = getPrettyRange(domain[1], domain[2])
#             domains.add(v)

#         else:
#             let splitted = domain[0].split("_")
#             let setName = splitted[0]
#             let lower = domain[1]
#             let upper = domain[2]

#             if tableCopy.hasKey(setName):

#                 let s = tableCopy[setName]
#                 # echo splitted
#                 try:
#                     let num = parseInt(splitted[^1])

#                     if s of DummySet:

#                         let dummySet = cast[DummySet](s)

#                         if (lower != $dummySet.dummyVal):
#                             dummySet.included.add(num)
#                         else:
#                             dummySet.excluded.add(num)


#                         # if (not (dummySet in domains)):
#                         #     domains.add(dummySet)

#                     elif s of OccurrenceSet:
#                         let oSet = cast[OccurrenceSet](s)

#                         if (lower == upper):
#                             if (lower == "1"):
#                                 oSet.included.add(num)
#                             else:
#                                 oSet.excluded.add(num)
                            
#                             # echo "HERE"

#                     elif s of MarkerSet:
#                         let mSet = cast[MarkerSet](s)

#                         if (lower == upper):
#                             var l : int
#                             discard parseInt(lower, l)

#                             # if mSet.markerLower == mSet.markerUpper:
#                             if num <= mSet.markerLower:
#                                 mSet.included.add(l)
                                
                            
#                             if num > mSet.markerUpper:
#                                 mSet.excluded.add(l)

#                             # echo mSet
#                             # mSet.included.add(l)

#                     elif s of FlagSet:
#                         let fSet = cast[FlagSet](s)


#                         if (domain[0].contains("_ExplicitVarSizeWithFlags_Flags_")):

#                             fSet.flagCount.inc()
#                             # fSet.flags.add(num)
#                             if (lower == "1" and upper == "1"):
#                                 if num > fSet.maxSetTo1:
#                                     fSet.maxSetTo1 = num
#                                     # echo num

#                             if (lower == "0" and upper == "0"):
#                                 if num > fSet.maxSetTo0:
#                                     fSet.maxSetTo0 = num
#                         else:
#                             if (lower == upper):
#                                 var l : int
#                                 discard parseInt(lower, l)

#                                 if (num <= fSet.maxSetTo1):
#                                     fSet.included.add(l)

#                                 if (num > fSet.maxSetTo1 and num <= fSet.maxSetTo0):
#                                     fSet.excluded.add(l)

#                     elif s of ExplicitSet:
#                         let eSet = cast[ExplicitSet](s)
#                         var l : int
#                         discard parseInt(lower, l)
#                         if (lower == upper):
#                             eSet.included.add(l)


#                 except ValueError:
                    
#                     let mSet = cast[MarkerSet](s)
#                     # mSet.cardinality = getPrettyRange(lower, upper)
#                     # if lower == upper:
#                     var l : int
#                     discard parseInt(lower, l)
#                     var u : int
#                     discard parseInt(upper, u)
#                     mSet.markerLower = l
#                     mSet.markerUpper = u
                    

#                 if (not (s in domains)):
#                     domains.add(s)

#     # echo domains
#     return domains