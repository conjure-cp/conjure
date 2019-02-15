import ../types
import db_sqlite, parseutils
proc getNullIndexes*(depth: int): string =
    for i in countUp(depth, maxIndex):
        result &= " and index" & $i & " is null "; 

proc getSingleIndex*(depth, parentLower: int): string =
    return "and index" & $(depth) & " <= " & $parentLower & " " 

proc getParentIdIndexes*(parents: seq[int]): string =
    for i in countUp(0, parents.len() - 1):
        result &= " and index" & $i & " = " & $parents[i]  & " "

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