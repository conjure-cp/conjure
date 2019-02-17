import re, strutils, os, tables, json, db_sqlite, parseutils 
import ../types 
import common

import typetraits

proc getFlagIndexes*(ancestors: seq[int]): string =
    for i in countUp(0, ancestors.len() - 1):
        result &= " and index" & $i & " <= " & $ancestors[i]  & " "

proc getFlagQuery*(ancestors : seq[int], outerSetName: string): string =
    var parentIndexes = ""
    var nullIndexes = getNullIndexes(1)
    var index = 0

    if (ancestors.len() > 0):
        parentIndexes = getFlagIndexes(ancestors)
        nullIndexes = getNullIndexes(ancestors.len()+1)
        index = ancestors.len()

    result = "SELECT index" & $index & " FROM domain WHERE name like '" & outerSetName & "\\_%\\_Flags\\_%' escape '\\' "
    result &= " and lower = 1 and upper = 1 " & parentIndexes & nullIndexes & " and nodeId = ? order by name desc limit 1;"


proc getFlagValuesQuery*(s: Set, ancestors: seq[int], outerSetName: string): string =
    let parentIndexes = getParentIdIndexes(ancestors)
    let singleIndex = getSingleIndexLE(ancestors.len(), s.markerLower)
    let nullIndexes = getNullIndexes(ancestors.len() + 1)
    result = "SELECT lower FROM domain WHERE name like '" & outerSetName & "_%ExplicitVarSizeWithFlags_Values_%'"
    result &= " and lower = upper " & parentIndexes & singleIndex & nullIndexes & " and nodeId = ?;" 


            
