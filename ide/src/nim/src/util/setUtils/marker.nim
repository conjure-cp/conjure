import re, strutils, os, tables, json, db_sqlite, parseutils 
import ../types


proc getParentIdIndex(parents: seq[int]): string =
    for i in countUp(0, parents.len() - 1):
        result &= " and index" & $i & " = " & $parents[i] 

proc getMarkerIdIndex(depth, parentLower: int): string =
    return  "and index" & $(depth) & " <= " & $parentLower  

proc getNullIndexes(depth: int): string =
    for i in countUp(depth + 1, maxIndex):
        result &= " and index" & $i & " is null";

proc getMarkerQuery(parents: seq[int], parentLower: int, outerSetName: string): string =
    let parentIdIndexes = getParentIdIndex(parents)
    let markerIdIndex = getMarkerIdIndex(parents.len(), parentLower)
    let nullIndexes = getNullIndexes(parents.len())
    result &= "SELECT lower, upper, name FROM domain WHERE name like '"
    result &= outerSetName & "\\_%\\_Marker\\_%' escape '\\' " & parentIdIndexes 
    result &= " " & markerIdIndex  &  nullIndexes & " and nodeId = ?;"

proc getValuesQuery(parents: seq[int], parentLower: int, outerSetName: string): string =
    let parentIdIndexes = getParentIdIndex(parents)
    let markerIdIndex = getMarkerIdIndex(parents.len(), parentLower)
    let nullIndexes = getNullIndexes(parents.len())
    result &= "SELECT lower, name FROM domain WHERE lower = upper and name like '" 
    result &= outerSetName & "\\_%\\Marker_Values\\_%' escape '\\' " & parentIdIndexes 
    result &= " " & markerIdIndex  &  nullIndexes & " and nodeId = ?;"

proc getPrettyNestedMarkedSet*(db: DbConn, parent: Set, outerSetName, nodeId: string,  parents: seq[int]) =

    var setId = 0
    var copy = parents
    var parentLower : int
    var parentId : int

    if (parent of MarkerSet):
        let markerParent = cast[MarkerSet](parent)
        parentId = markerParent.id
        parentLower = markerParent.markerLower

    if (parentId != -1):
        copy.add(parentId)

    let markerQuery = getMarkerQuery(copy, parentLower, outerSetName)

    # echo markerQuery
    
    for res in db.fastRows(sql(markerQuery), nodeId):
        setId.inc()

        var currentSet : MarkerSet
        let casted = cast[MarkerSet](parent.inner)
        currentSet.deepCopy(casted)
        currentSet.id = setId
        currentSet.name = getSetName(parent, setId)
        parent.children.add(currentSet)

        discard res[0].parseInt(currentSet.markerLower)
        discard res[1].parseInt(currentSet.markerUpper)

        if (currentSet.inner != nil):
            if (currentSet.inner of MarkerSet):
                getPrettyNestedMarkedSet(db, currentSet, outerSetName, nodeId, copy)

        else:
            var newCopy = copy
            newCopy.add(currentSet.id)
            let includedValuesQuery = getValuesQuery(newCopy, currentSet.markerLower, outerSetName)
            
            for res in db.rows(sql(includedValuesQuery), nodeId):
                var lower : int
                discard res[0].parseInt(lower)
                currentSet.included.add(lower)