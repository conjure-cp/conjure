import types, util, parser, parseSets
import re, strutils, os, tables, json, db_sqlite, parseutils

var prettyLookup* = initTable[string, Table[string, Variable]]()
var eprimeLookup: Table[string, Variable]
var auxLookup: Table[string, Expression]

proc initParser(db: DbConn, minionFilePath: string, eprimeFilePath: string) =
    eprimeLookup.clear()
    auxLookup.clear()
    eprimeLookup = parseEprime(db, eprimeFilePath)
    auxLookup = parseAux(minionFilePath)

proc getSimpleDomainsOfNode(db: DbConn, nodeId: string, wantExpressions: bool = false): seq[Variable] =

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

proc getPrettyDomainsOfNode*(db: DbConn, nodeId: string): (seq[Variable]) =
    # echo eprimeLookup

    prettyLookup[nodeId] = initTable[string, Variable]()
    prettyLookup[nodeId].deepCopy(eprimeLookup)

    # echo prettyLookup[nodeId]

    for variable in prettyLookup[nodeId].values():
        result.add(variable)

        if (variable of Set):
            let s = Set(variable)
            decideSet(db, s, nil, s.name, nodeId, @[])
        else:
            # echo variable
            let query0 = sql("SELECT lower, upper FROM domain WHERE name = '" & variable.name & "' and nodeId = ?;")
            var res = db.getRow(query0, nodeId)
            variable.rng = getPrettyRange(res[0], res[1])

