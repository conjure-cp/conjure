import types, setUtils/marker, setUtils/flags
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

            if (setId > fS.maxSetTo1):
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

    # echo setTable
    # echo parent.children

proc getPrettyNestedFlagSet(db: DbConn, inner: Set, parent: Set, outerSetName, nodeId : string, depth: int) =

    var num : int
    var lower : int
    var currentSet : FlagSet
    var setId : int
    var maxSetTo1Table = initTable[int, int]()
    var maxSetTo0Table = initTable[int, int]()

    let query1 = sql("SELECT name FROM domain WHERE name like '" & parent.name & "%_ExplicitVarSizeWithFlags_Flags_%' and lower = 1 and upper = 1 and nodeId = ? order by name desc;")
    for res in db.fastRows(query1, nodeId):

        setId = nameToNumber(res[0], depth - 1)
        num = nameToNumber(res[0], depth)

        if (maxSetTo1Table.hasKey(setId)):
            continue

        maxSetTo1Table[setId] = num
        

    let query2 = sql("SELECT name FROM domain WHERE name like '" & parent.name & "%_ExplicitVarSizeWithFlags_Flags_%' and lower = 0 and upper = 0 and nodeId = ? order by name desc;")
    for res in db.fastRows(query2, nodeId):

        echo res

        setId = nameToNumber(res[0], depth - 1)
        num = nameToNumber(res[0], depth)
        # discard numbers[0][1..^1].parseInt(setId)

        if (maxSetTo0Table.hasKey(setId)):
            continue

        maxSetTo0Table[setId] = num

    # select * from domain where name like "%s_ExplicitVarSizeWithFlags_Values_%"  and lower = upper order by name asc 
    let query3 = sql("SELECT name, lower FROM domain WHERE name like '" & parent.name & "%_ExplicitVarSizeWithFlags_Values_%' and lower = upper and nodeId = ? order by name asc;")
    # echo markerQuery
    var setTable = initTable[int, FlagSet]()
    for res in db.rows(query3, nodeId):
        # echo res
        discard res[1].parseInt(lower)

        setId = nameToNumber(res[0], depth - 1)
        num = nameToNumber(res[0], depth)

        if (parent of MarkerSet):
            let markerParent = cast[MarkerSet](parent)
            if (setId > markerParent.markerLower):
                break

        if not setTable.hasKey(setId):
            let casted = cast[FlagSet](inner)
            currentSet.deepCopy(casted)
            # currentSet.name = $setId
            currentSet.name = getSetName(parent, setId)
            setTable[setId] = currentSet
        else:
            currentSet = setTable[setId]
        
        # echo maxSetTo1Table
        # echo maxSetTo0Table

        if maxSetTo1Table.hasKey(setId):
            if num <= maxSetTo1Table[setId]:
                currentSet.included.add(lower)
                currentSet.maxSetTo1 = maxSetTo1Table[setId]
        
        if maxSetTo0Table.hasKey(setId):
            if num <= maxSetTo0Table[setId] and not lower in currentSet.included:
                currentSet.excluded.add(lower)

    for child in setTable.values():
        parent.children.add(child)


    



                # if (upper == lower):
                #     childSet.included.add()

                    


                # # let numbers = res[0].findAll(digitRe)



# proc getPrettyNestedMarkedSet(db: DbConn, inner: Set, parent: Set, outerSetName: string, nodeId : string, depth: int) =

#     echo $depth & " @ Nested marker "

#     var num : int
#     var lower : int
#     var upper : int
#     var currentSet : MarkerSet
#     var setId : int
#     var markerTable = initTable[int, (int, int)]()
    
#     var qnull : string

#     qnull &= " and index" & $i & " is not null";
#     for i in countUp(0, depth - 1):

#     for i in countUp(depth, maxIndex):
#         qnull &= " and index" & $i & " is null";

#     let markerId = "index" & $(depth - 1)

#     let markerQuery = "SELECT " & markerId & ",lower, upper, name FROM domain WHERE name like '" & outerSetName & "\\_%\\_Marker\\_%' escape '\\'" & qnull & " and nodeId = ?;"
#     echo markerQuery
#     for res in db.rows(sql(markerQuery), nodeId):
#         discard res[0].parseInt(setId)
#         discard res[1].parseInt(lower)
#         discard res[2].parseInt(upper)

#         if lower == 0 and upper == 0:
#             continue 

#         echo res
#         echo setId

#         if (parent of MarkerSet):
#             let markerParent = cast[MarkerSet](parent)
#             if (setId > markerParent.markerLower):
#                 break

#         markerTable[setId] = (lower, upper)

#         # echo res
        
#         if (inner.inner != nil):
#             # currentSet = cast[MarkerSet](inner)
#             let casted = cast[MarkerSet](inner)
#             currentSet.deepCopy(casted)

#             # echo setId

#             # currentSet.name = $depth & ":" & $setId
#             currentSet.name = getSetName(parent, setId)
#             currentSet.markerLower = lower
#             currentSet.markerUpper = upper

#             parent.children.add(currentSet)

#             if( inner.inner of OccurrenceSet or inner.inner of DummySet or inner.inner of ExplicitSet):
#                 getNestedPrettyExplicitOrOccurrenceOrDummySet(db, inner.inner, currentSet, outerSetName, nodeId, depth + 1)
            
#             if (inner.inner of MarkerSet):
#                 getPrettyNestedMarkedSet(db, inner.inner, currentSet, outerSetName, nodeId, depth + 1)

#             if (inner.inner of FlagSet):
#                 getPrettyNestedFlagSet(db, inner.inner, currentSet, outerSetName, nodeId, depth + 1)


#     # echo markerTable


#     if (inner.inner != nil):
#         return

#     if markerTable.len() == 0:
#         return

#     echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
#     var prefix = "%_ExplicitVarSizeWithMarker_Values_%"
#     let qLeft = "SELECT name, lower FROM domain WHERE name like '" 
#     let qRight = "' and lower = upper and nodeId = ?;"

#     let query = qLeft & outerSetName & prefix & qRight

#     echo query

#     var setTable = initTable[int, MarkerSet]()

#     for res in db.rows(sql(query), nodeId):

#         # echo res

#         discard res[1].parseInt(lower)
#         # # let numbers = res[0].findAll(digitRe)
#         # var setId : int
#         # discard numbers[0][1..^1].parseInt(setId)

#         setId = nameToNumber(res[0], depth - 1)
        

#         if not setTable.hasKey(setId):
#             let casted = cast[MarkerSet](inner)
#             currentSet.deepCopy(casted)
#             currentSet.name = $setId
#             setTable[setId] = currentSet
#         else:
#             currentSet = setTable[setId]

#         # discard numbers[^1][1..^1].parseInt(num)

#         num = nameToNumber(res[0], depth)
        
    
#         (currentSet.markerLower, currentSet.markerUpper) = markerTable[setId]
        
#         # echo markerTable

#         # echo "lowerBound " & $lowerBound
#         # echo "upperBound " & $upperBound
#         # echo "num " & $num
#         # echo "lower " & $lower

#         if num <= currentSet.markerLower:
#             currentSet.included.add(lower)
#         if num > currentSet.markerUpper and not (lower in currentSet.included):
#             currentSet.excluded.add(lower)
    

#     for child in setTable.values():
#         parent.children.add(child)



proc getPrettySetDomain(db: DbConn, variable: Variable, parent: Set, nodeId: string, depth: int) =
    let s = cast[Set](variable)

    var num : int
    var lower : int
    
    if (s of ExplicitSet):
        let eSet = cast[ExplicitSet](variable)

        if (eSet.inner != nil):
            getNestedPrettyExplicitOrOccurrenceOrDummySet(db, s.inner, eSet, eSet.name, nodeId, depth + 1)
        else:
            let query0 = sql("SELECT lower FROM domain WHERE name like '" & variable.name & "_%Explicit%' and lower = upper and nodeId = ?;")
            for res in db.fastRows(query0, nodeId):
                discard res[0].parseInt(lower)
                eSet.included.add(lower)

    if (s of OccurrenceSet):
        let oSet = cast[OccurrenceSet](variable)
        let query = sql("SELECT name, lower FROM domain WHERE name like '%" & variable.name & "_Occurrence%' and lower = upper and nodeId = ?;")
        # echo $query
        for res in db.fastRows(query, nodeId):
            # let numbers = res[0].findAll(digitRe)
            # discard numbers[^1][1..^1].parseInt(num)

            num = nameToNumber(res[0], 0)

            if res[1] == "1":
                # echo "Including " & $num
                oSet.included.add(num)
            else:
                # echo "Excluding " & $num
                oSet.excluded.add(num)

    if (variable of DummySet):
        # echo "HERE"
        let query = sql("SELECT name, lower FROM domain WHERE name like '%" & variable.name & "_ExplicitVarSizeWithDummy%' and lower = upper and nodeId = ?;")
        let dSet = cast[DummySet](variable)
        for res in db.fastRows(query, nodeId):
            # echo res
            # echo res[0].findAll(digitRe)
            discard res[1].parseInt(lower)
            # let numbers = res[0].findAll(digitRe)
            # discard numbers[^1][1..^1].parseInt(num)

            num = nameToNumber(res[0], 0)
            

            if res[1] == $dSet.dummyVal:
                dSet.excludedCount.inc()
                # dSet.excluded.add(num)
            else:
                dSet.included.add(lower)

    if variable of FlagSet:
        parseFlags(db, variable, variable.name, nodeId, @[])


    if variable of MarkerSet:
        let mSet = cast[MarkerSet](variable)
        let query0 = "SELECT lower, upper FROM domain WHERE name like '" & variable.name & "%_Marker' and index0 is null and index1 is null and index2 is null and nodeId = ?;"
        # echo query0
        var res = db.getRow(sql(query0), nodeId)
        # echo res
        discard res[0].parseInt(mSet.markerLower)
        discard res[1].parseInt(mSet.markerUpper)



        # echo mSet.getCardinality()
        # echo "HEEEERE"

        if (mSet.inner != nil):
            if( mSet.inner of OccurrenceSet or mSet.inner of DummySet or mSet.inner of ExplicitSet):
                getNestedPrettyExplicitOrOccurrenceOrDummySet(db, s.inner, mSet, mSet.name, nodeId, depth + 1)
            
            if (mSet.inner of MarkerSet):
                # var pp: seq[int]
                getPrettyNestedMarkedSet(db, mSet, mSet.name, nodeId, @[])

            if (mSet.inner of FlagSet):
                getPrettyNestedFlagSet(db, s.inner, mSet, mSet.name, nodeId, depth + 1)
            return
        
        


        let query1 = "SELECT index0, lower FROM domain WHERE name like '" & variable.name & "%_ExplicitVarSizeWithMarker_Values%' and lower = upper and nodeId = ? order by name asc;"
        # echo query1
        for res in db.rows(sql(query1), nodeId):
            # echo res
            discard res[1].parseInt(lower)
            # discard res[0].findAll(digitRe)[0].parseInt(num)
            # num = nameToNumber(res[0], 0)

            discard res[0].parseInt(num)

            if num <= mSet.markerLower:
                mSet.included.add(lower)
            if num > mSet.markerUpper:
                mSet.excluded.add(lower)