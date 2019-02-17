import types
import re, strutils, os, tables, json, db_sqlite, parseutils 

proc parseSetEprime(s: JsonNode, name: string): Set

proc parseEprime*(eprimeFilePath: string): Table[string, Variable] =

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

                varLookup[name] = parseSetEprime(key[1]["DomainSet"], name)
    except:
        raise
        # raise newException(EprimeParseException, "Failed to parse eprime")

    # echo varLookup
    return varLookup

proc parseSetEprime(s: JsonNode, name: string): Set =

    let arr = s.getElems()

    if arr[^1].hasKey("DomainSet"):
        let innerArr = arr[^1]["DomainSet"]
        if (arr[0].hasKey("Set_ExplicitVarSizeWithMarker")):
            return newMarkerSet(name, inner = parseSetEprime(innerArr, name))

        if (arr[0].hasKey("Set_ExplicitVarSizeWithFlags")):
            return newFlagSet(name, inner = parseSetEprime(innerArr, name))

        if (arr[0].hasKey("Set_Explicit")):
            return newExplicitSet(name,
            cardinality = arr[1]["SizeAttr_Size"]["Constant"]["ConstantInt"].getInt(-1),
            inner = parseSetEprime(innerArr, name))

    if arr[^1].hasKey("DomainInt"):

        var bounds  : JsonNode

        bounds = arr[^1]["DomainInt"].getElems()[0]


        if (bounds.hasKey("RangeBounded")):
            bounds = arr[^1]["DomainInt"].getElems()[0]["RangeBounded"]
        else:
            bounds = arr[^1]["DomainInt"].getElems()[1][0]["RangeBounded"]
        # echo bounds.pretty()
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

        while (rhs.findAll(nestedAux).len() > 0):
            for nested in rhs.findAll(nestedAux):
                if (lookup.hasKey(nested)):
                    rhs = rhs.replace(nested, lookup[nested].name)
        lookup[name] = newExpression(rhs)

    return lookup