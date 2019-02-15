import ../types
import db_sqlite, parseutils

proc getNullIndexes*(depth: int): string =
    for i in countUp(depth, maxIndex):
        result &= " and index" & $i & " is null "; 

proc getSingleIndex*(depth, parentLower: int): string =
    return "and index" & $(depth) & " = " & $parentLower & " " 

proc getSingleIndexLE*(depth, parentLower: int): string =
    return "and index" & $(depth) & " <= " & $parentLower & " " 

proc getParentIdIndexes*(ancestors: seq[int]): string =
    for i in countUp(0, ancestors.len() - 1):
        result &= " and index" & $i & " = " & $ancestors[i]  & " "

proc includeValues*(db: DbConn, s: Set, valuesQuery : string, nodeId: string) = 
    for res in db.rows(sql(valuesQuery), nodeId):
        var lower : int
        discard res[0].parseInt(lower)
        s.included.add(lower)

proc makeChildSet*(s : Set, setId : int): Set =
    var currentSet : Set
    currentSet.deepCopy(s.inner)
    currentSet.name = getSetName(s, setId)
    currentSet.id = setId
    s.children.add(currentSet)
    return currentSet

proc getInnerSetQuery*(s, parent: Set, ancestors: seq[int], outerSetName: string): string =
    var index = ancestors.len()
    result = "SELECT index" & $index & ", lower FROM domain WHERE name like '%" & outerSetName 

    if (s of OccurrenceSet):
        result &= "_Occurrence%' and lower = upper "
    if (s of DummySet):
        result &= "_ExplicitVarSizeWithDummy%' and lower = upper "

    if parent != nil:
        result &= getParentIdIndexes(ancestors)

    result &= " and nodeId = ?;"