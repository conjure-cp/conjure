import types
import re, strutils, os, tables, json, db_sqlite, parseutils

proc parseSetEprime(db: DbConn, s: JsonNode, name: string): Set

proc parseEprime*(db: DbConn, eprimeFilePath: string): Table[string, Variable] =

    var varLookup = initTable[string, Variable]()
    var clean = ""
    # try:
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

        if key[1].hasKey("DomainSet"):
            varLookup[name] = parseSetEprime(db, key[1]["DomainSet"], name)

    return varLookup

proc parseSetEprime(db: DbConn, s: JsonNode, name: string): Set =

    let arr = s.getElems()

    if arr[^1].hasKey("DomainSet"):
        let innerArr = arr[^1]["DomainSet"]
        if (arr[0].hasKey("Set_ExplicitVarSizeWithMarker")):
            return newMSet(name, inner = parseSetEprime(db, innerArr, name))

        if (arr[0].hasKey("Set_ExplicitVarSizeWithFlags")):
            return newFSet(name, inner = parseSetEprime(db, innerArr, name))

        if (arr[0].hasKey("Set_Explicit")):
            let card = arr[1]["SizeAttr_Size"]["Constant"][
                    "ConstantInt"].getInt(-1)
            return newEset(name, card, inner = parseSetEprime(db, innerArr, name))

    elif arr[^1].hasKey("DomainInt"):

        if arr[0].hasKey("Set_Explicit"):
            return newEset(name, cardinality = arr[1]["SizeAttr_Size"][
                    "Constant"]["ConstantInt"].getInt(-1))

        elif arr[0].hasKey("Set_ExplicitVarSizeWithDummy"):
            let query = "select upper from Domain where nodeId = 0 and name like '" & name & "\\_%Dummy%' escape '\\' limit 1;" 
            # echo query
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

    # try:

    var lookup = initTable[string, Expression]()
    let auxDef = re"aux\d* #(.*)"
    let minionFile = readFile(minionFilePath)

    # echo minionFile[0..100]

    let find = minionFile.findAll(auxDef)

    echo find.len()

    for a in find:
        let splitted = a.split("#")
        let name = splitted[0].strip()
        var rhs = splitted[1].replace(
                re"\(?Active-CSE: \d* occurrences of this expression or equivalent: ", "")

        let nestedAux = re"aux\d*"

        for nested in rhs.findAll(nestedAux):
            if (lookup.hasKey(nested)):
                rhs = rhs.replace(nested, lookup[nested].name)
        lookup[name] = newExpression(rhs)

    # echo lookup
    # for e in lookup.values():
    #     echo e.name

    # echo lookup.len()

    return lookup

