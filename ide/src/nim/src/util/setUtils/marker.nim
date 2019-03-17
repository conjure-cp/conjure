import re, strutils, os, tables, json, db_sqlite, parseutils
import ../types
import common

proc getMarkerQuery*(ancestors: seq[int], outerSetName: string): string =
    ## Returns query to select the marker record from the database
    let parentIdIndexes = getParentIdIndexes(ancestors)
    var markerIdIndex = ""
    let nullIndexes = getNullIndexes(ancestors.len())
    result &= "SELECT lower, upper, name FROM domain WHERE name like '"

    if ancestors.len() == 0:
        result &= outerSetName & "\\_%\\_Marker%' escape '\\' " &
                parentIdIndexes
    else:
        result &= outerSetName & "\\_%\\_Marker\\_%' escape '\\' " &
                parentIdIndexes

    result &= " " & markerIdIndex
    result &= nullIndexes
    result &= " and nodeId = ? ;"

proc getMarkerValuesQuery*(ancestors: seq[int], parentLower: int, outerSetName: string): string =
    ## Returns query to get active values from marker set
    let parentIdIndexes = getParentIdIndexes(ancestors)
    let markerIdIndex = getSingleIndexLE(ancestors.len(), parentLower)
    let nullIndexes = getNullIndexes(ancestors.len() + 1)
    result &= "SELECT lower, name FROM domain WHERE lower = upper and name like '"
    result &= outerSetName & "\\_%Marker_Values\\_%' escape '\\' " &
            parentIdIndexes
    result &= markerIdIndex & nullIndexes & " and nodeId = ?;"


proc getNonExcludedMarkerValuesQuery*(s: Set, ancestors: seq[int], outerSetName: string): string =
    ## Returns query to get non diactivated values from the marker set
    let parentIndexes = getParentIdIndexes(ancestors)
    let index = " index" & $ancestors.len() 
    let bound = " and " & index &  " <= " & $s.markerUpper 

    let nullIndexes = getNullIndexes(ancestors.len() + 1)

    result = "SELECT lower, upper FROM domain WHERE name like '" & outerSetName & "\\_%Marker_Values\\_%' escape '\\' "
    result &= parentIndexes & bound & nullIndexes & " and nodeId = ?;"