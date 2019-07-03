import types, util, parser, parseSets
import re, strutils, os, tables, json, db_sqlite, parseutils, sequtils, util

var prettyLookup* = initTable[string, Table[string, Variable]]()
var eprimeLookup  = initTable[string, Variable]()
var auxLookup     = initTable[string, Expression]()

proc getInitialVariables*() : seq[Variable] =
    ## Gets list of pretty variables at initial node
    return toSeq(eprimeLookup.values())

proc initParser*(db: DbConn, minionFilePath: string, eprimeFilePath: string) =
    ## Parses eprime and minion files
    eprimeLookup.clear()
    auxLookup.clear()
    eprimeLookup = parseEprime(db, eprimeFilePath)
    auxLookup = parseAux(minionFilePath)

proc getSimpleDomainsOfNode*(db: DbConn, nodeId: string, wantExpressions: bool = false): seq[Variable] =
    ## Returns a list of simple domains
    var query = "select name, storeDump  from domain where"
    if (not wantExpressions):
        query &= " name not like 'aux%' and "
    query &= " nodeId = ? order by name limit ?"

    for domain in db.fastRows(sql(query), nodeId, "-1"):

        if (auxLookup.hasKey(domain[0])):
            var e = newExpression(auxLookup[domain[0]].name, domain[0])
            e.rng = prettifyMinionStoreDump(domain[1])
            result.add(e)
        else:
            let v = newVariable(name = domain[0])
            v.rng = prettifyMinionStoreDump(domain[1])
            result.add(v)

proc getPrettyDomainsOfNode*(db: DbConn, nodeId: string, wantExpressions: bool = false): (seq[Variable]) =
    # Returns list of pretty domains

    prettyLookup[nodeId] = initTable[string, Variable]()
    prettyLookup[nodeId].deepCopy(eprimeLookup)

    for variable in prettyLookup[nodeId].values():
        result.add(variable)
        if (variable of Set):
            let s = Set(variable)
            decideSet(db, s, nil, s.name, nodeId, @[])
        else:
            let query0 = sql("SELECT lower, upper FROM domain WHERE name = '" & variable.name & "' and nodeId = ?;")
            var res = db.getRow(query0, nodeId)
            variable.rng = getPrettyRange(res[0], res[1])

    if wantExpressions:
        for expression in auxLookup.values():
            result.add(expression)
            let query0 = sql("SELECT lower, upper FROM domain WHERE name = '" & expression.auxName & "' and nodeId = ?;")
            var res = db.getRow(query0, nodeId)
            expression.rng = getPrettyRange(res[0], res[1])