import types, setUtils/marker, setUtils/flags, setUtils/common
import re, strutils, os, tables, json, db_sqlite, parseutils 

let digitRe = re"\_(\d+)"

proc nameToNumber(name: string, depth: int): int = 
    var setId : int
    let numbers = name.findAll(digitRe)

    discard numbers[depth][1..^1].parseInt(setId)
    return setId

proc getNestedPrettyExplicitOrOccurrenceOrDummySet*(db: DbConn, inner: Set, parent: Set, outerSetName: string, nodeId : string, depth : int) =

    echo "@ Nested occurence or dummy or explicit"

    var num : int
    var lower : int
    var upper : int
    # var markerSet : MarkerSet
    # var temp : Set
    var currentSet : Set
    var prefix = "%_"
    let qLeft = "SELECT name, lower, upper FROM domain WHERE name like '" 
    let qRight = "' and nodeId = ?;"

    if inner of OccurrenceSet:
        currentSet = cast[OccurrenceSet](inner)
        prefix &= "Occurrence%"
    if inner of DummySet:
        currentSet = cast[DummySet](inner)
        prefix &= "ExplicitVarSizeWithDummy%"
    if inner of ExplicitSet:
        currentSet = cast[ExplicitSet](inner)
        prefix &= "%_Explicit_%"


    let query = qLeft & outerSetName & prefix & qRight


    var setTable = initTable[int, Set]()

    # echo query

    for res in db.rows(sql(query), nodeId):

        # echo res

        # echo "here"
        discard res[1].parseInt(lower)
        discard res[2].parseInt(upper)

        # echo depth

        # echo res

        let setId = nameToNumber(res[0], depth-1)

        if (parent of MarkerSet):
            let mS = cast[MarkerSet](parent)
            # echo "setId " & $setId
            # echo "marker Lower " & $mS.markerLower

            if (setId > mS.markerLower):
                break
        
        if (parent of FlagSet):
            let fS = cast[FlagSet](parent)

            if (setId > fS.markerLower):
                break

            # echo fS.maxSetTo1
            # echo fS.maxSetTo0

        # echo "setid " & $setId

        if not setTable.hasKey(setId):
            currentSet.deepCopy(inner)
            # currentSet.name = $setId
            currentSet.name = getSetName(parent, setId)
            setTable[setId] = currentSet
        else:
            currentSet = setTable[setId]


        # discard numbers[^1][1..^1].parseInt(num)

        num = nameToNumber(res[0], depth)

        if lower == upper:
            if (inner of OccurrenceSet):
                # echo res[1]

                if res[1] == "1":
                    currentSet.included.add(num)
                else:
                    currentSet.excluded.add(num)

            if (inner of DummySet):
                var dS = cast[DummySet](currentSet)

                if res[1] == $dS.dummyVal:
                    dS.excludedCount.inc()
                else:
                    dS.included.add(lower)

            if (inner of ExplicitSet):
                var eS = cast[ExplicitSet](currentSet)
                es.included.add(lower)


    for child in setTable.values():
        parent.children.add(child)

# proc getPrettySetDomain(db: DbConn, variable: Variable, parent: Set, nodeId: string, depth: int) =
#     let s = cast[Set](variable)

#     var num : int
#     var lower : int
    
#     if (s of ExplicitSet):
#         let eSet = cast[ExplicitSet](variable)

#         if (eSet.inner != nil):
#             getNestedPrettyExplicitOrOccurrenceOrDummySet(db, s.inner, eSet, eSet.name, nodeId, depth + 1)
#         else:
#             let query0 = sql("SELECT lower FROM domain WHERE name like '" & variable.name & "_%Explicit%' and lower = upper and nodeId = ?;")
#             for res in db.fastRows(query0, nodeId):
#                 discard res[0].parseInt(lower)
#                 eSet.included.add(lower)

#     if (s of OccurrenceSet):
#         let oSet = cast[OccurrenceSet](variable)
#         let query = sql("SELECT name, lower FROM domain WHERE name like '%" & variable.name & "_Occurrence%' and lower = upper and nodeId = ?;")
#         # echo $query
#         for res in db.fastRows(query, nodeId):
#             # let numbers = res[0].findAll(digitRe)
#             # discard numbers[^1][1..^1].parseInt(num)

#             num = nameToNumber(res[0], 0)

#             if res[1] == "1":
#                 # echo "Including " & $num
#                 oSet.included.add(num)
#             else:
#                 # echo "Excluding " & $num
#                 oSet.excluded.add(num)

#     if (variable of DummySet):
#         # echo "HERE"
#         let query = sql("SELECT name, lower FROM domain WHERE name like '%" & variable.name & "_ExplicitVarSizeWithDummy%' and lower = upper and nodeId = ?;")
#         let dSet = cast[DummySet](variable)
#         for res in db.fastRows(query, nodeId):
#             # echo res
#             # echo res[0].findAll(digitRe)
#             discard res[1].parseInt(lower)
#             # let numbers = res[0].findAll(digitRe)
#             # discard numbers[^1][1..^1].parseInt(num)

#             num = nameToNumber(res[0], 0)
            

#             if res[1] == $dSet.dummyVal:
#                 dSet.excludedCount.inc()
#                 # dSet.excluded.add(num)
#             else:
#                 dSet.included.add(lower)

#     if variable of FlagSet:
#         parseFlags(db, cast[Set](variable), variable.name, nodeId, @[])

#     if variable of MarkerSet:
#         parseMarker(db, cast[Set](variable), variable.name, nodeId, @[])


proc parseFlags(db: DbConn, s, parent: Set, outerSetName, nodeId: string, parents: seq[int])
proc parseMarker(db: DbConn, s, parent: Set, outerSetName, nodeId: string, parents: seq[int])
proc parseOccurrence(db: DbConn, s, parent: Set, outerSetName, nodeId: string, parents: seq[int])

proc decideSet(db: DbConn, s,  parent: Set, outerSetName, nodeId: string, parents: seq[int]) =
    if s of FlagSet:
        parseFlags(db, s, parent, outerSetName, nodeId, parents)

    if s of MarkerSet:
        parseMarker(db, s, parent, outerSetName, nodeId, parents)

    if s of OccurrenceSet:
        parseOccurrence(db, s, parent, outerSetName, nodeId, parents)

proc parseOccurrence(db: DbConn, s, parent: Set, outerSetName, nodeId: string, parents: seq[int]) =
    var index = parents.len()
    var query = "SELECT index" & $index & ", lower FROM domain WHERE name like '%" & outerSetName 
    query &= "_Occurrence%' and lower = upper "

    if parent != nil:
        query &= getParentIdIndexes(parents)

    query &= " and nodeId = ?;"
    
    echo query
    for res in db.fastRows(sql(query), nodeId):
        echo res
        var number : int
        discard res[0].parseInt(number)

        if (res[1] == "1"):
            s.included.add(number)
        else:
            s.excluded.add(number)

proc parseFlags(db: DbConn, s, parent : Set, outerSetName, nodeId: string, parents: seq[int]) =

    # echo "HERE!!"

    var copy = parents
    var flagQuery = getFlagQuery(parents, outerSetName)
    # echo flagQuery
    var highestFlag = db.getValue(sql(flagQuery), nodeId)

    discard highestFlag.parseInt(s.markerLower)

    for setId in countUp(1, s.markerLower):
        # echo "setid " & $setId
        # echo copy

        if (s.inner != nil):
            let childSet = makeChildSet(s, setID)
            var newCopy = copy
            newCopy.add(setId)
            decideSet(db, childSet, s, outerSetName, nodeId, newCopy)
        else:
            let valuesQuery = getFlagValuesQuery(s, copy, outerSetName)
            # echo valuesQuery
            includeValues(db, s, valuesQuery, nodeId)
            break;

proc parseMarker(db: DbConn, s, parent: Set, outerSetName, nodeId: string,  parents: seq[int]) =

    var copy = parents
    # echo "Marker copy " & $parents

    let markerQuery = getMarkerQuery(copy, outerSetName)

    var res = db.getRow(sql(markerQuery), nodeId)
    # echo nodeId
    # echo markerQuery
    # echo res

    discard res[0].parseInt(s.markerLower)
    discard res[1].parseInt(s.markerUpper)
    
    for setId in countUp(1, s.markerLower): 

        if (s.inner != nil):
            # echo "DECIDING"

            let childSet = makeChildSet(s, setId)
            var newCopy = copy
            newCopy.add(setId)
            decideSet(db, childSet, s, outerSetName, nodeId, newCopy)
        else:
            # echo "HEEREERE!!!"
            let valuesQuery = getMarkerValuesQuery(copy, s.markerLower, outerSetName)
            includeValues(db, s, valuesQuery, nodeId)
            break;
