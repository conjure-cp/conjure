import types
import re, strutils, os, tables, json, db_sqlite, parseutils

proc parseSetEprime(db: DbConn, s: JsonNode, name: string, depth: int): Set

proc parseEprime*(db: DbConn, eprimeFilePath: string): Table[string, Variable] =
    ## Initialises a table for all domains in the eprime file
    var varLookup = initTable[string, Variable]()
    var clean = ""

    for line in readFile(eprimeFilePath).split("Conjure's")[1].split("\n"):
        if len(line) == 0:
            continue
        clean &= line[1..^1]

    for key in parseJson(clean)["representations"].getElems():

        if (not key[0].haskey("Name")):
            continue

        let name = key[0]["Name"].getStr()

        if key[1].hasKey("DomainInt"):
            varLookup[name] = newVariable(name)

        # if key[1].hasKey("DomainSet"):
        #     varLookup[name] = parseSetEprime(db, key[1]["DomainSet"], name, 0)

    return varLookup

proc getExplicitCard(db: DBConn, name: string, depth: int): int =
    # Gets the cardinality for explicit sets
    let query1 = "select count(*) from (select count(*) from Domain where nodeId = 0 and name like '" & name & "\\_%Explicit\\_%' escape '\\'" & "group by index" & $depth & ")"
    let query2 = "select count(*) from (select count(*) from Domain where nodeId = 0 and name like '" & name & "\\_%Explicit%' escape '\\'" & "group by index" & $depth & ")"
    discard db.getValue(sql(query1)).parseInt(result)

    if (result == 0):
        let q = "select name from Domain where nodeId = 0 and name like '" & name & "\\_%Explicit%' escape '\\'" & "group by index" & $depth & " limit 1"
        let name = db.getValue(sql(q))
        let splitted = name.split("_")
        let valid = re"Explicit(R\d)*"
        if (splitted[1].match(valid)):
            discard db.getValue(sql(query2)).parseInt(result)




proc parseSetEprime(db: DbConn, s: JsonNode, name: string, depth: int): Set =
    # Parses a set in the eprime file
    let arr = s.getElems()

    if arr[^1].hasKey("DomainSet"):

        let innerArr = arr[^1]["DomainSet"]
        if (arr[0].hasKey("Set_ExplicitVarSizeWithMarker")):
            return newMSet(name, inner = parseSetEprime(db, innerArr, name, depth+1))

        if (arr[0].hasKey("Set_ExplicitVarSizeWithFlags")):
            return newFSet(name, inner = parseSetEprime(db, innerArr, name, depth+1))

        if (arr[0].hasKey("Set_Explicit")):
            return newEset(name, getExplicitCard(db, name, depth), inner = parseSetEprime(db, innerArr, name, depth+1))

    elif arr[^1].hasKey("DomainInt"):

        if arr[0].hasKey("Set_Explicit"):
            return newEset(name, cardinality = getExplicitCard(db, name, depth))

        elif arr[0].hasKey("Set_ExplicitVarSizeWithDummy"):
            let query = "select upper from Domain where nodeId = 0 and name like '" & name & "\\_%Dummy%' escape '\\' order by name desc limit 1;" 
            var dummyVal : int
            discard db.getValue(sql(query)).parseInt(dummyVal)
            return newDset(name, dummyVal)

        elif arr[0].hasKey("Set_Occurrence"):
            return newOset(name)

        elif arr[0].hasKey("Set_ExplicitVarSizeWithMarker"):
            return newMset(name)

        elif arr[0].hasKey("Set_ExplicitVarSizeWithFlags"):
            return newFset(name)

proc parseAux*(minionFilePath: string): Table[string, Expression] =
    ## Parse aux file to expanc nested aux expressions
    var lookup = initTable[string, Expression]()
    let auxDef = re"aux\d* #(.*)"
    let minionFile = readFile(minionFilePath)

    let find = minionFile.findAll(auxDef)

    for a in find:
        let splitted = a.split("#")
        let auxName = splitted[0].strip()
        var rhs = splitted[1].replace(re"\(?Active-CSE: \d* occurrences of this expression or equivalent: ", "")

        let nestedAux = re"aux\d*"

        for nested in rhs.findAll(nestedAux):
            if (lookup.hasKey(nested)):
                rhs = rhs.replace(nested, lookup[nested].name)
        lookup[auxName] = newExpression(rhs, auxName)

    return lookup

