import re, strutils, os, tables, json, db_sqlite, parseutils 
import flags, ../types


proc getParentIdIndex(parents: seq[int]): string =
    for i in countUp(0, parents.len() - 1):
        result &= " and index" & $i & " = " & $parents[i] 

proc getMarkerIdIndex(depth, parentLower: int): string =
    return  "and index" & $(depth) & " <= " & $parentLower  

proc getNullIndexes(depth: int): string =
    for i in countUp(depth, maxIndex):
        result &= " and index" & $i & " is null";

proc getMarkerQuery(parents: seq[int], outerSetName: string): string =
    let parentIdIndexes = getParentIdIndex(parents)
    var markerIdIndex = ""
    # if (parents.len() > 0):
        # markerIdIndex = getMarkerIdIndex(parents.len())
    let nullIndexes = getNullIndexes(parents.len())
    result &= "SELECT lower, upper, name FROM domain WHERE name like '"
    if parents.len() == 0:
        result &= outerSetName & "\\_%\\_Marker%' escape '\\' " & parentIdIndexes 
    else:
        result &= outerSetName & "\\_%\\_Marker\\_%' escape '\\' " & parentIdIndexes 
    result &= " " & markerIdIndex 
    result &= nullIndexes 
    result &= " and nodeId = ? ;"

proc getValuesQuery(parents: seq[int], parentLower: int, outerSetName: string): string =
    let parentIdIndexes = getParentIdIndex(parents)
    # let markerIdIndex = getMarkerIdIndex(parents.len(), parentLower)
    let nullIndexes = getNullIndexes(parents.len())
    result &= "SELECT lower, name FROM domain WHERE lower = upper and name like '" 
    result &= outerSetName & "\\_%\\Marker_Values\\_%' escape '\\' " & parentIdIndexes 
    result &=  nullIndexes & " and nodeId = ?;"

proc parseMarker*(db: DbConn, v: Variable, outerSetName, nodeId: string,  parents: seq[int]) =

    var copy = parents
    var parent = cast[MarkerSet](v)
    echo "Marker copy " & $parents

    let markerQuery = getMarkerQuery(copy, outerSetName)

    var res = db.getRow(sql(markerQuery), nodeId)
    # echo nodeId
    echo markerQuery
    echo res

    discard res[0].parseInt(parent.markerLower)
    discard res[1].parseInt(parent.markerUpper)
    
    for setId in countUp(1, parent.markerLower): 
        # echo res

        # discard res[0].parseInt(parent.markerLower)
        # discard res[1].parseInt(parent.markerUpper)
        # echo parent

        if (parent.inner != nil):

            # echo parent.name

            var currentSet : MarkerSet
            let casted = cast[MarkerSet](parent.inner)
            currentSet.deepCopy(casted)
            currentSet.id = setId
            currentSet.name = getSetName(parent, setId)
            parent.children.add(currentSet)
            var newCopy = copy
            newCopy.add(setId)
        

            if (parent.inner of MarkerSet):
                parseMarker(db, currentSet, outerSetName, nodeId, newCopy)
            if (parent.inner of FlagSet):
                # let casted = cast[FlagSet](parent.inner)
                # var fSet : FlagSet
                # fSet.deepCopy(casted)
                parseFlags(db, currentSet, outerSetName, nodeId, newCopy)
            # if (currentSet.inner of FlagSet):
            #     discard
                # parseFlags(db, currentSet, outerSetName, nodeId, copy)
            # echo "here" & $parents.len()

        else:
            echo "HEEREERE!!!"
            var newCopy = copy
            newCopy.add(parent.id)
            let valuesQuery = getValuesQuery(newCopy, parent.markerLower, outerSetName)
            # echo valuesQuery
            
            for res in db.rows(sql(valuesQuery), nodeId):
                var lower : int
                discard res[0].parseInt(lower)
                parent.included.add(lower)