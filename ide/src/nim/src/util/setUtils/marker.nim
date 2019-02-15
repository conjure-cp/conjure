import re, strutils, os, tables, json, db_sqlite, parseutils 
import ../types
import flags
import common

proc getMarkerQuery(parents: seq[int], outerSetName: string): string =
    let parentIdIndexes = getParentIdIndexes(parents)
    var markerIdIndex = ""
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
    let parentIdIndexes = getParentIdIndexes(parents)
    let markerIdIndex = getSingleIndex(parents.len(), parentLower)
    let nullIndexes = getNullIndexes(parents.len() + 1)
    result &= "SELECT lower, name FROM domain WHERE lower = upper and name like '" 
    result &= outerSetName & "\\_%\\Marker_Values\\_%' escape '\\' " & parentIdIndexes 
    result &= markerIdIndex & nullIndexes & " and nodeId = ?;"

proc parseMarker*(db: DbConn, s: Set, outerSetName, nodeId: string,  parents: seq[int]) =

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

            let childSet = makeChildSet(s, setId)
            var newCopy = copy
            newCopy.add(setId)

            if (s.inner of MarkerSet):
                parseMarker(db, childSet, outerSetName, nodeId, newCopy)
            if (s.inner of FlagSet):
                parseFlags(db, childSet, outerSetName, nodeId, newCopy)

        else:
            # echo "HEEREERE!!!"
            let valuesQuery = getValuesQuery(copy, s.markerLower, outerSetName)
            includeValues(db, s, valuesQuery, nodeId)
            break;