import types, util, parser, parseSets
import re, strutils, os, tables, json, db_sqlite, parseutils, sequtils

var prettyLookup* = initTable[string, Table[string, Variable]]()
var eprimeLookup: Table[string, Variable]
var auxLookup: Table[string, Expression]

proc getInitialVariables*() : seq[Variable] =
    return toSeq(eprimeLookup.values())

proc initParser*(db: DbConn, minionFilePath: string, eprimeFilePath: string) =
    eprimeLookup.clear()
    auxLookup.clear()
    eprimeLookup = parseEprime(db, eprimeFilePath)
    auxLookup = parseAux(minionFilePath)
    # echo auxLookup.len()

proc getSimpleDomainsOfNode*(db: DbConn, nodeId: string, wantExpressions: bool = false): seq[Variable] =

    var query = "select name, lower, upper from domain where"

    if (not wantExpressions):
        query &= " name not like 'aux%' and "

    query &= " nodeId = ? order by name limit ?"

    # echo auxLookup
    # echo wantExpressions

    for domain in db.fastRows(sql(query), nodeId, "-1"):

        if (auxLookup.hasKey(domain[0])):
            var e = newExpression(auxLookup[domain[0]].name)
            e.rng = getPrettyRange(domain[1], domain[2])
            result.add(e)
        else:

            let v = newVariable(name = domain[0])
            # echo v
            v.rng = getPrettyRange(domain[1], domain[2])
            result.add(v)

    # echo result
    # for res in result:
    #     echo res
    #     if res of Expression:
    #         echo Expression(res)

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

