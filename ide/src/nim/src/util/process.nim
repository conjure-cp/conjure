include types
import re, strutils, os, tables, json, db_sqlite, parseutils 
export re, strutils, os, tables, json, db_sqlite, parseutils 

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

            lookup[name] = Expression(name: rhs)
    except:
        raise newException(MinionParseException, "Failed to parse eprime")

    return lookup

proc parseSet(s: JsonNode, n: string): Set =

    # let n = "inner"

    let arr = s.getElems()

    if arr[^1].hasKey("DomainSet"):
        let innerArr = arr[^1]["DomainSet"]
        if (arr[0].hasKey("Set_ExplicitVarSizeWithMarker")):
            return MarkerSet(name: n, inner: parseSet(innerArr, "inner"))
        # if innerArr[0].hasKey("Set_Occurrence"):

    if arr[0].hasKey("Set_Explicit"):
        return ExplicitSet(name: n, cardinality: arr[1]["SizeAttr_Size"]["Constant"]["ConstantInt"].getInt(-1)) 

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
            return DummySet(name: n, lowerBound: l, upperBound: u, dummyVal: u + 1) 

        elif arr[0].hasKey("Set_Occurrence"):
            return OccurrenceSet(name: n, lowerBound: l, upperBound: u) 

        elif arr[0].hasKey("Set_ExplicitVarSizeWithMarker"):
            return MarkerSet(name: n, lowerBound: l, upperBound: u) 

        elif arr[0].hasKey("Set_ExplicitVarSizeWithFlags"):
            return FlagSet(name: n, lowerBound: l, upperBound: u) 

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

            let n = key[0]["Name"].getStr()

            if key[1].hasKey("DomainInt"):
                varLookup[n] = Variable(name: n)

            if key[1].hasKey("DomainSet"):

                varLookup[n] = parseSet(key[1]["DomainSet"], n)
    except:
        raise
        # raise newException(EprimeParseException, "Failed to parse eprime")

    echo varLookup
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
            let v = Variable(name: domain[0])
            v.rng = getPrettyRange(domain[1], domain[2])
            domains.add(v)

    return domains

proc getPrettyDomainsOfNode(db: DbConn, nodeId: string) : seq[Variable] =
    var list : seq[Variable]

    var tableCopy : Table[string, Variable]
    tableCopy.deepCopy(eprimeLookup)

    for variable in tableCopy.values():
        list.add(variable)
        # echo "dasd"
        let digitRe = re"(\d*$)"

        if (variable of Set):

            let s = cast[Set](variable)
            if (s.inner != nil):
                continue

            var num : int
            var lower : int

            if (variable of ExplicitSet):
                let eSet = cast[ExplicitSet](variable)
                let query0 = sql("SELECT lower FROM domain WHERE name like '%" & variable.name & "_Explicit_%' and lower = upper and nodeId = ?;")
                for res in db.fastRows(query0, nodeId):
                    discard res[0].parseInt(lower)
                    eSet.included.add(lower)

            if (variable of OccurrenceSet):
                let oSet = cast[OccurrenceSet](variable)
                let query = sql("SELECT name, lower FROM domain WHERE name like '%" & variable.name & "_Occurrence%' and lower = upper and nodeId = ?;")
                # echo $query
                for res in db.fastRows(query, nodeId):
                    # echo res[0].findAll(digitRe)
                    discard res[0].findAll(digitRe)[0].parseInt(num)
                    if res[1] == "1":
                        echo "Including " & $num
                        oSet.included.add(num)
                    else:
                        echo "Excluding " & $num
                        oSet.excluded.add(num)

            if (variable of DummySet):
                # echo "HERE"
                let dSet = cast[DummySet](variable)
                let query = sql("SELECT name, lower FROM domain WHERE name like '%" & variable.name & "_ExplicitVarSizeWithDummy%' and lower = upper and nodeId = ?;")
                for res in db.fastRows(query, nodeId):
                    # echo res
                    # echo res[0].findAll(digitRe)
                    discard res[1].parseInt(lower)
                    discard res[0].findAll(digitRe)[0].parseInt(num)

                    if res[1] == $dSet.dummyVal:
                        dSet.excludedCount.inc()
                        # dSet.excluded.add(num)
                    else:
                        dSet.included.add(lower)

            if variable of FlagSet:
                # var maxSetTo1 : int
                var maxSetTo0 = -1
                let fSet = cast[FlagSet](variable)

                let query1 = sql("SELECT name FROM domain WHERE name like '%" & variable.name & "_ExplicitVarSizeWithFlags_Flags_%' and lower = 1 and upper = 1 and nodeId = ? order by name desc limit 1;")
                var res = db.getValue(query1, nodeId)
                discard res.findAll(digitRe)[0].parseInt(fSet.maxSetTo1)

                let query2 = sql("SELECT name FROM domain WHERE name like '%" & variable.name & "_ExplicitVarSizeWithFlags_Flags_%' and lower = 0 and upper = 0 and nodeId = ? order by name desc limit 1;")
                res = db.getValue(query2, nodeId)
                if res != "":
                    discard res.findAll(digitRe)[0].parseInt(maxSetTo0)

                # select * from domain where name like "%s_ExplicitVarSizeWithFlags_Values_%"  and lower = upper order by name asc 
                let query3 = sql("SELECT name, lower FROM domain WHERE name like '%" & variable.name & "_ExplicitVarSizeWithFlags_Values_%' and lower = upper and nodeId = ? order by name asc;")
                for res in db.rows(query3, nodeId):
                    discard res[1].parseInt(lower)
                    discard res[0].findAll(digitRe)[0].parseInt(num)

                    # echo "num " & $num
                    # echo "maxSetTo0 " & $maxSetTo0
                    # echo "maxSetTo1 " & $maxSetTo1

                    if maxSetTo0 > -1:
                        if num > maxSetTo0:
                            break
                        else:
                            if num > fSet.maxSetTo1:
                                break

                    elif num > fSet.maxSetTo1 and num <= maxSetTo0:
                        fSet.excluded.add(lower)
                    
                    elif num <= fSet.maxSetTo1:
                        fSet.included.add(lower)

            if variable of MarkerSet:
                let mSet = cast[MarkerSet](variable)
                let query0 = sql("SELECT lower, upper FROM domain WHERE name = '" & variable.name & "_ExplicitVarSizeWithMarker_Marker' and nodeId = ?;")
                var res = db.getRow(query0, nodeId)
                discard res[0].findAll(digitRe)[0].parseInt(mSet.markerLower)
                discard res[1].findAll(digitRe)[0].parseInt(mSet.markerUpper)

                let query1 = sql("SELECT name, lower FROM domain WHERE name like '%" & variable.name & "_ExplicitVarSizeWithMarker_Values_%' and lower = upper and nodeId = ? order by name asc;")
                for res in db.fastRows(query1, nodeId):
                    discard res[1].parseInt(lower)
                    discard res[0].findAll(digitRe)[0].parseInt(num)

                    if num <= mSet.markerLower:
                        mSet.included.add(lower)
                    if num > mSet.markerUpper:
                        mSet.excluded.add(lower)

        elif variable of Expression:
            discard
        else:
            let query0 = sql("SELECT lower, upper FROM domain WHERE name = '" & variable.name & "' and nodeId = ?;")
            var res = db.getRow(query0, nodeId)
            variable.rng = getPrettyRange(res[0], res[1])










    return list

    # for variable in eprimeLookup.values()



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
            variables.children.add(TreeViewNode(name: s.name, children: @[t, cardinality, included, excluded]))

            # if not (s.inner == nil):
            #     let inner = TreeViewNode(name: "Inner", children: @[TreeViewNode(name: ($s.inner.name)[2..^2])])

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

