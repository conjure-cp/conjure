include sets
import re, strutils, os, tables, json, db_sqlite, parseutils 
# import nre except toSeq
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
            return newExplicitSet(name,
            cardinality = arr[1]["SizeAttr_Size"]["Constant"]["ConstantInt"].getInt(-1),
            inner = parseSet(innerArr, name))
            
        # if innerArr[0].hasKey("Set_Occurrence"):


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


        if arr[0].hasKey("Set_Explicit"):
            return newExplicitSet(name, lowerBound = l, upperBound = u,
            cardinality = arr[1]["SizeAttr_Size"]["Constant"]["ConstantInt"].getInt(-1)) 

        elif arr[0].hasKey("Set_ExplicitVarSizeWithDummy"):
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
var prettyLookup = initTable[string, Table[string, Variable]]()
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


proc getPrettyDomainsOfNode(db: DbConn, nodeId: string) : (seq[Variable]) =
    var varList : seq[Variable]
    # var tableCopy = prettyLookup["1"]
    prettyLookup[nodeId] = initTable[string, Variable]()
    prettyLookup[nodeId].deepCopy(eprimeLookup)

    for variable in prettyLookup[nodeId].values():
        varList.add(variable)
        # prettyLookup[Variable.name]
        # echo variable

        if (variable of Set):
            let s = cast[Set](variable)
            decideSet(db, s, nil, s.name, nodeId, @[])
            # break

        elif variable of Expression:
            discard
        else:
            # echo variable.name
            let query0 = sql("SELECT lower, upper FROM domain WHERE name = '" & variable.name & "' and nodeId = ?;")
            var res = db.getRow(query0, nodeId)
            variable.rng = getPrettyRange(res[0], res[1])

    return (varList)



proc setToTreeView(s : Set): TreeViewNode =

    var setType : string

    # echo s

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

    let kids = TreeViewNode(name: "Children", children: @[]) 

    # if (s.children.len() > 0):

    #     for kid in s.children:

    #         kids.children.add(setToTreeView(kid))
    #         discard

    if (s.inner == nil):
        return (TreeViewNode(name: s.name, children: @[t, cardinality, included, excluded]))

    return (TreeViewNode(name: s.name, children: @[t, cardinality, kids]))


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

proc getCollapsedSetChildren(s : Set): JsonNode =
    # let s = cast[Set](eprimeLookup[setName])

    # echo prettyLookup[nodeId]

    echo s.children.len()
    let json = %*{}
    if s.children.len() > 0:
        discard
        json["children"] = %*[]

        for kid in s.children:
            json["children"].add(%*{"name" : kid.name, "_children": []})
    
    return json





    # echo s.children.len()
    # let json = %*{}
    # if s.children.len() > 0:
    #     discard
    #     json["children"] = %*[]

    #     for kid in s.children:
    #         json["children"].add(%*{"name" : kid.name, "children": %setToTreeView(kid)})
    
    # return json