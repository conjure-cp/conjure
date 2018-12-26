import re, strutils, os, tables, json, db_sqlite, parseutils
# import "parseEprime.nim", "parseAux.nim"

type Variable* = ref object of RootObj
  name: string
  range: string

type Expression* = ref object of Variable

type Set* = ref object of Variable
    lower: int
    upper: int
    included: seq[int]
    excluded: seq[int]

type OccurrenceSet* = ref object of Set

type DummySet* = ref object of Set
    dummyVal : int
    cardinality : int

        


proc getPrettyRange(lower: string, upper: string): string =
    if lower == upper:
       return "(" & $lower & ")" 
    return "(" & $lower & ".." & $upper & ")"

proc `$`*(v:Variable): string =
    if v of Expression:
        return "<Expr> " & v.name & " " & v.range
    if v of OccurrenceSet:
        return "<OSet> " 
    if v of DummySet:
        let s = cast[DummySet](v)
        return "<DSet> " & getPrettyRange($s.lower, $s.upper) & " " & $s.dummyVal & " inc " & $s.included & " exc " & $s.excluded & " card " & $ s.cardinality
    return "<Variable> " & v.name & " " & v.range 

proc getCardinality(s: Set): string =
    if s of DummySet:
        return getPrettyRange($len(s.included), $cast[DummySet](s).cardinality)
    if s of OccurrenceSet:
        return getPrettyRange($len(s.included), $(s.upper - len(s.excluded))) 
    return "ERROR"

proc parseAux*(minionFilePath: string): Table[string, Expression] =
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
        # echo name
        # echo rhs
    return lookup

proc parseEprime*(eprimeFilePath: string): Table[string, Variable] =

    var varLookup = initTable[string, Variable]()
    var clean = ""

    for line in readFile(eprimeFilePath).split("Conjure's")[1].split("\n"):
        if len(line) == 0:
            continue
        clean &= line[1..^1]
    
    for key in parseJson(clean)["representations"].getElems():
        let n = key[0]["Name"].getStr()

        if key[1].hasKey("DomainInt"):
            varLookup[n] = Variable(name: n)

        if key[1].hasKey("DomainSet"):
            
            let array = key[1]["DomainSet"].getElems()
            let bounds = array[^1]["DomainInt"].getElems()[0]["RangeBounded"]
            let l = bounds[0]["Constant"]["ConstantInt"].getInt()
            let u = bounds[1]["Constant"]["ConstantInt"].getInt()

            if array[0].hasKey("Set_ExplicitVarSizeWithDummy"):
                varLookup[n] = DummySet(name: n, lower: l, upper: u, dummyVal: u + 1, cardinality: u + 1) 

    # echo varLookup
    return varLookup



var eprimeLookup : Table[string, Variable]
var auxLookup : Table[string, Expression]
var db : DbConn

proc init(dbPath: string, minionFilePath: string, eprimeFilePath: string) =
    eprimeLookup = parseEprime(eprimeFilePath)
    auxLookup = parseAux(minionFilePath)
    db = open(dbPath, "", "", "")

proc getPrettyDomainsOfNode(nodeId: int) : seq[Variable] =
    # echo auxLookup
    var domains : seq[Variable]

    for domain in db.fastRows(sql"select name, lower, upper from domain where nodeId = ?", nodeId):
        
        if (auxLookup.hasKey(domain[0])):
            let d = auxLookup[domain[0]]
            d.range = getPrettyRange(domain[1], domain[2])
            domains.add(d)
        
        elif (eprimeLookup.hasKey(domain[0])):
            let v = eprimeLookup[domain[0]]
            v.range = getPrettyRange(domain[1], domain[2])
            domains.add(v)

        else:
            let splitted = domain[0].split("_")
            let setName = splitted[0]
            let lower = domain[1]
            let upper = domain[2]
            let s = eprimeLookup[setName]

            if s of DummySet:

                let dummySet = cast[DummySet](s)

                if (not (dummySet in domains)):
                    domains.add(dummySet)

                let num = parseInt(splitted[^1])
                if (lower != $dummySet.dummyVal):
                    dummySet.included.add(num)
                else:
                    dummySet.cardinality.dec()

    # for d in domains:
    #     echo d

    return domains


type TreeViewNode* = ref object of RootObj
  text: string
  nodes: seq[TreeViewNode]

proc domainsToJson(domains: seq[Variable]): string =

    let root = TreeViewNode(text: "Items")
    let variables = TreeViewNode(text: "Variables")
    let expressions = TreeViewNode(text: "Expressions")
    let sets = TreeViewNode(text: "Sets")

    root.nodes = @[variables, sets, expressions]

    for d in domains:

        if d of Expression:
            expressions.nodes.add(TreeViewNode(text: d.name, nodes: @[TreeViewNode(text: d.range)]))

        elif d of Set:

            let s = cast[Set](d)
            var setType : string

            if (s of OccurrenceSet):
                setType = "Occurrence"
            if (s of DummySet):
                setType = "Dummy"

            let t = TreeViewNode(text: "Type", nodes: @[TreeViewNode(text: setType)])
            let cardinality = TreeViewNode(text: "Cardinality", nodes: @[TreeViewNode(text: s.getCardinality())])
            let included = TreeViewNode(text: "Included", nodes: @[TreeViewNode(text: $s.included)])
            let excluded = TreeViewNode(text: "Included", nodes: @[TreeViewNode(text: $s.excluded)])
            sets.nodes.add(TreeViewNode(text: s.name, nodes: @[t, cardinality, included, excluded]))

        else:
            variables.nodes.add(TreeViewNode(text: d.name, nodes: @[TreeViewNode(text: d.range)]))
        
    let list = @[root]
    let json = $(%list).pretty()
    return json


let minionFilePath = "../test/testData/conjure-output/model000001.eprime-minion"
let eprimeFilePath = "../test/testData/conjure-output/model000001.eprime"
let dbPath = "../test/testData/test.db"

init(dbPath, minionFilePath, eprimeFilePath)
# discard getPrettyDomainsOfNode(1)
echo domainsToJson(getPrettyDomainsOfNode(3))