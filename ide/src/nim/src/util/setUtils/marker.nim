import re, strutils, os, tables, json, db_sqlite, parseutils 
import ../types
import common

proc getMarkerQuery*(parents: seq[int], outerSetName: string): string =
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

proc getMarkerValuesQuery*(parents: seq[int], parentLower: int, outerSetName: string): string =
    let parentIdIndexes = getParentIdIndexes(parents)
    let markerIdIndex = getSingleIndex(parents.len(), parentLower)
    let nullIndexes = getNullIndexes(parents.len() + 1)
    result &= "SELECT lower, name FROM domain WHERE lower = upper and name like '" 
    result &= outerSetName & "\\_%\\Marker_Values\\_%' escape '\\' " & parentIdIndexes 
    result &= markerIdIndex & nullIndexes & " and nodeId = ?;"
