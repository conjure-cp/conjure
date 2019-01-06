import re, strutils, os, tables, json, db_sqlite, parseutils
export re, strutils, os, tables, json, db_sqlite, parseutils
# import "parseEprime.nim", "parseAux.nim"

type Variable* = ref object of RootObj
  name: string
  rng: string

type Expression = ref object of Variable

type Set = ref object of Variable
    lower: int
    upper: int
    included: seq[int]
    excluded: seq[int]

type OccurrenceSet = ref object of Set

type DummySet = ref object of Set
    dummyVal : int
    cardinality : int

proc getPrettyRange(lower: string, upper: string): string =
    if lower == upper:
       return "int(" & $lower & ")" 
    return "int(" & $lower & ".." & $upper & ")"

proc `$`*(v:Variable): string =
    if v of Expression:
        return "<Expr> " & v.name & " " & v.rng
    if v of OccurrenceSet:
        let s = cast[OccurrenceSet](v)
        return "<OSet> " & getPrettyRange($s.lower, $s.upper) & " " & " inc " & $s.included & " exc " & $s.excluded 
    if v of DummySet:
        let s = cast[DummySet](v)
        return "<DSet> " & getPrettyRange($s.lower, $s.upper) & " " & $s.dummyVal & " inc " & $s.included & " exc " & $s.excluded & " card " & $ s.cardinality
    return "<Variable> " & v.name & " " & v.rng 

proc getCardinality(s: Set): string =
    if s of DummySet:
        return getPrettyRange($len(s.included), $cast[DummySet](s).cardinality)
    if s of OccurrenceSet:
        return getPrettyRange($len(s.included), $(s.upper - len(s.excluded))) 
    return "ERROR"

proc parseAux(minionFilePath: string): Table[string, Expression] =
    var lookup = initTable[string, Expression]()
    let auxDef = re"aux\d* #(.*)"
    let minionFile = readFile(minionFilePath)
    let find = minionFile.findAll(auxDef)

    for a in find:
        let splitted = a.split("#")
        let name = splitted[0].strip()
        var rhs = splitted[1].replace(re"\(?Active-CSE: \d* occurrences of this expression or equivalent: ","")

        let nestedAux = re"aux\d*"

        for nested in rhs.findAll(nestedAux):
            if (lookup.hasKey(nested)):
                rhs = rhs.replace(nested, lookup[nested].name)

        lookup[name] = Expression(name: rhs)

    return lookup

proc parseEprime*(eprimeFilePath: string): Table[string, Variable] =

    var varLookup = initTable[string, Variable]()
    var clean = ""

    for line in readFile(eprimeFilePath).split("Conjure's")[1].split("\n"):
        if len(line) == 0:
            continue
        clean &= line[1..^1]
    
    for key in parseJson(clean)["representations"].getElems():
        # echo key
        # try:
        let n = key[0]["Name"].getStr()

        if key[1].hasKey("DomainInt"):
            varLookup[n] = Variable(name: n)

        if key[1].hasKey("DomainSet"):
            
            let array = key[1]["DomainSet"].getElems()
            # echo array[^1].pretty()
            var bounds  : JsonNode

            bounds = array[^1]["DomainInt"].getElems()[0]

            if (bounds.hasKey("RangeBounded")):
                bounds = array[^1]["DomainInt"].getElems()[0]["RangeBounded"]
            else:
                bounds = array[^1]["DomainInt"].getElems()[1][0]["RangeBounded"]
            # echo bounds
            var l = bounds[0]["Constant"]["ConstantInt"].getInt(-1)
            var u = bounds[1]["Constant"]["ConstantInt"].getInt(-1)

            if (l == -1):
                l = bounds[0]["Constant"]["ConstantInt"][1].getInt(-1)
                u = bounds[1]["Constant"]["ConstantInt"][1].getInt(-1)

                if (l == -1):
                    echo "ERRORORORRORORORORRO"


            if array[0].hasKey("Set_ExplicitVarSizeWithDummy"):
                varLookup[n] = DummySet(name: n, lower: l, upper: u, dummyVal: u + 1, cardinality: u + 1) 

            elif array[0].hasKey("Set_Occurrence"):
                varLookup[n] = OccurrenceSet(name: n, lower: l, upper: u) 
        # except:
        #         discard "Failed to parse Eprime"

    echo varLookup
    return varLookup



var eprimeLookup : Table[string, Variable]
var auxLookup : Table[string, Expression]

proc initParser*(minionFilePath: string, eprimeFilePath: string) =
    eprimeLookup = parseEprime(eprimeFilePath)
    auxLookup = parseAux(minionFilePath)

proc getSimpleDomainsOfNode*(db: DbConn, amount: string, start: string, nodeId: string): seq[Variable] =

    var domains : seq[Variable]

    for domain in db.fastRows(sql"select name, lower, upper from domain where nodeId = ? and domainId >= ? limit ?",nodeId, start, amount):

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


# proc getChangedVariables(db: DbConn, nodeId: string)  : seq[string] = 
#     for domain in db.fastRows(sql"select * from (select * from domain where nodeId = ?) where ", nodeId):
    

proc getPrettyDomainsOfNode(db: DbConn, nodeId: string) : seq[Variable] =
    # echo auxLookup
    var domains : seq[Variable]

    var tableCopy : Table[string, Variable]
    tableCopy.deepCopy(eprimeLookup)

    for domain in db.fastRows(sql"select name, lower, upper from domain where nodeId = ?", nodeId):
        # echo domain

        if (auxLookup.hasKey(domain[0])):
            let d = auxLookup[domain[0]]
            d.rng = getPrettyRange(domain[1], domain[2])
            domains.add(d)
        
        elif (tableCopy.hasKey(domain[0])):
            let v = tableCopy[domain[0]]
            v.rng = getPrettyRange(domain[1], domain[2])
            domains.add(v)

        else:
            let splitted = domain[0].split("_")
            let setName = splitted[0]
            let lower = domain[1]
            let upper = domain[2]

            if tableCopy.hasKey(setName):

                let s = tableCopy[setName]
                let num = parseInt(splitted[^1])

                if s of DummySet:

                    let dummySet = cast[DummySet](s)

                    if (lower != $dummySet.dummyVal):
                        dummySet.included.add(num)
                    else:
                        dummySet.cardinality.dec()

                    # if (not (dummySet in domains)):
                    #     domains.add(dummySet)

                elif s of OccurrenceSet:
                    let oSet = cast[OccurrenceSet](s)

                    if (lower == upper):
                        if (lower == "1"):
                            oSet.included.add(num)
                        else:
                            oSet.excluded.add(num)
                        
                        echo "HERE"

                if (not (s in domains)):
                    domains.add(s)

    return domains


type TreeViewNode = ref object of RootObj
  name: string
  children: seq[TreeViewNode]

proc domainsToJson(domains: seq[Variable]): JsonNode =

    let root = TreeViewNode(name: "Items")
    let variables = TreeViewNode(name: "Variables")
    let expressions = TreeViewNode(name: "Expressions")
    let sets = TreeViewNode(name: "Sets")

    root.children = @[variables, sets, expressions]

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

            let t = TreeViewNode(name: "Type", children: @[TreeViewNode(name: setType)])
            let cardinality = TreeViewNode(name: "Cardinality", children: @[TreeViewNode(name: s.getCardinality())])
            let included = TreeViewNode(name: "Included", children: @[TreeViewNode(name: ($s.included)[2..^2])])
            let excluded = TreeViewNode(name: "Excluded", children: @[TreeViewNode(name: ($s.excluded)[2..^2])])
            sets.children.add(TreeViewNode(name: s.name, children: @[t, cardinality, included, excluded]))

        else:
            variables.children.add(TreeViewNode(name: d.name, children: @[TreeViewNode(name: d.rng)]))
        
    # let list = @[root]
    # let json = (%list).pretty()
    return %root