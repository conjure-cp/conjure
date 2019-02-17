import types, parser, sets
import re, strutils, os, tables, json, db_sqlite, parseutils 

var prettyLookup = initTable[string, Table[string, Variable]]()
var eprimeLookup : Table[string, Variable]
var auxLookup : Table[string, Expression]

proc initParser(minionFilePath: string, eprimeFilePath: string) =
    eprimeLookup.clear()
    auxLookup.clear()
    eprimeLookup = parseEprime(eprimeFilePath)
    auxLookup = parseAux(minionFilePath)

proc getSimpleDomainsOfNode(db: DbConn,  nodeId: string, wantExpressions: bool = false): seq[Variable] =

    var query = "select name, lower, upper from domain where"
    if (not wantExpressions):
        query &= " name not like 'aux%' and "
    query &= " nodeId = ? order by name limit ?"

    for domain in db.fastRows(sql(query), nodeId, "-1"):

        if (auxLookup.hasKey(domain[0])):
            let d = auxLookup[domain[0]]
            d.rng = getPrettyRange(domain[1], domain[2])
            result.add(d)
        else:
            let v = newVariable(name = domain[0])
            v.rng = getPrettyRange(domain[1], domain[2])
            result.add(v)


proc getPrettyDomainsOfNode*(db: DbConn, nodeId: string) : (seq[Variable]) =

    prettyLookup[nodeId] = initTable[string, Variable]()
    prettyLookup[nodeId].deepCopy(eprimeLookup)

    for variable in prettyLookup[nodeId].values():
        result.add(variable)

        if (variable of Set):
            let s = cast[Set](variable)
            decideSet(db, s, nil, s.name, nodeId, @[])
        else:
            let query0 = sql("SELECT lower, upper FROM domain WHERE name = '" & variable.name & "' and nodeId = ?;")
            var res = db.getRow(query0, nodeId)
            variable.rng = getPrettyRange(res[0], res[1])


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

    let kids = TreeViewNode(name: "Children", children: @[]) 

    if (s.inner == nil):
        return (TreeViewNode(name: s.name, children: @[t, cardinality, included, excluded]))

    return (TreeViewNode(name: s.name, children: @[t, cardinality, kids]))


proc domainsToJson(domains: seq[Variable]): JsonNode =

    let root = TreeViewNode(name: "Items")
    let variables = TreeViewNode(name: "Domain Variables")
    let expressions = TreeViewNode(name: "Expressions")
    let changedExpressions = TreeViewNode(name: "Changed Expressions")

    root.children = @[variables, expressions, changedExpressions]

    for d in domains:
        if d of Expression:
            expressions.children.add(TreeViewNode(name: d.name, children: @[TreeViewNode(name: d.rng)]))

        elif d of Set:
            let s = cast[Set](d)
            let treeRep = (setToTreeView(s))
            variables.children.add(treeRep)

        else:
            variables.children.add(TreeViewNode(name: d.name, children: @[TreeViewNode(name: d.rng)]))
        
    return %root

proc getCollapsedSetChildren(s : Set): JsonNode =
    let json = %*{}
    if s.children.len() > 0:
        discard
        json["children"] = %*[]

        for kid in s.children:
            json["children"].add(%*{"name" : kid.name, "_children": []})
    
    return json

proc expressionsToJson(expressions: seq[Expression]): JsonNode =
    var list : seq[TreeViewNode]
    for exp in expressions:
        list.add(TreeViewNode(name: exp.name, children: @[TreeViewNode(name: exp.rng)]))
    return %list
