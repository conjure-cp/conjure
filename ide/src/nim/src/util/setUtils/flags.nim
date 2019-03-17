import re, strutils, os, tables, json, db_sqlite, parseutils
import ../types
import common

import typetraits

proc getTrueFlagCountQuery*(ancestors: seq[int], outerSetName: string): string =
    ## Returns query to find the number of flags set to true.
    var parentIndexes = ""
    var nullIndexes = getNullIndexes(1)
    var index = 0
    var nonNullIndexes = ""

    if (ancestors.len() > 0):
        parentIndexes = getParentIdIndexes(ancestors)
        nonNullIndexes = getNotNullIndexes(ancestors.len(), ancestors.len()) 
        nullIndexes = getNullIndexes(ancestors.len()+1)
        index = ancestors.len()

    result = "SELECT count(domainId) FROM domain WHERE name like '" &
            outerSetName & "\\_%\\_Flags\\_%' escape '\\' "
    result &= " and lower = 1 and upper = 1 " & parentIndexes & nonNullIndexes & nullIndexes &
            " and nodeId = ?"

proc getFalseFlagCountQuery*(ancestors: seq[int], outerSetName: string): string =
    ## Returns query to find the number of flags set to false.
    var parentIndexes = ""
    var nullIndexes = getNullIndexes(1)
    var index = 0
    var nonNullIndexes = ""

    if (ancestors.len() > 0):
        parentIndexes = getParentIdIndexes(ancestors)
        nonNullIndexes = getNotNullIndexes(ancestors.len(), ancestors.len()) 
        nullIndexes = getNullIndexes(ancestors.len()+1)
        index = ancestors.len()

    result = "SELECT count(domainId) FROM domain WHERE name like '" &
            outerSetName & "\\_%\\_Flags\\_%' escape '\\' "
    result &= " and lower = 0 and upper = 0 " & parentIndexes & nonNullIndexes & nullIndexes &
            " and nodeId = ?"

proc getNonFalseFlagCountQuery*(ancestors: seq[int], outerSetName: string): string =
    ## Returns query to find the number of flags not set to false.
    var parentIndexes = ""
    var nullIndexes = getNullIndexes(1)
    var index = 0
    var nonNullIndexes = ""

    if (ancestors.len() > 0):
        parentIndexes = getParentIdIndexes(ancestors)
        nonNullIndexes = getNotNullIndexes(ancestors.len(), ancestors.len()) 
        nullIndexes = getNullIndexes(ancestors.len()+1)
        index = ancestors.len()

    result = "SELECT count(domainId) FROM domain WHERE name like '" &
            outerSetName & "\\_%\\_Flags\\_%' escape '\\' "
    result &= " and  upper != 0 " & parentIndexes & nonNullIndexes & nullIndexes &
            " and nodeId = ?"


proc getFlagValuesIncludedQuery*(s: Set, ancestors: seq[int], outerSetName: string): string =
    ## Returns query to select values that are active from the database
    let parentIndexes = getParentIdIndexes(ancestors)
    let singleIndex = getSingleIndexLE(ancestors.len(), s.markerLower)
    let nullIndexes = getNullIndexes(ancestors.len() + 1)

    result = "SELECT lower FROM domain WHERE name like '" & outerSetName & "\\_%ExplicitVarSizeWithFlags_Values\\_%' escape '\\' "
    result &= " and lower = upper " & parentIndexes & singleIndex &
            nullIndexes & " and nodeId = ?;"


proc getNonExcludedFlagValuesQuery*(s: Set, ancestors: seq[int], outerSetName: string): string =
    ## returns query to select values that are not deactivated
    let f = s.markerLower + s.markerUpper + 1

    let parentIndexes = getParentIdIndexes(ancestors)
    let index = " index" & $ancestors.len() 
    let bound = " and " & index & " >= " & $f
    let nullIndexes = getNullIndexes(ancestors.len() + 1)

    result = "SELECT lower, upper FROM domain WHERE name like '" & outerSetName & "\\_%ExplicitVarSizeWithFlags_Values\\_%' escape '\\' "
    result &= parentIndexes & bound & nullIndexes & " and nodeId = ?;"