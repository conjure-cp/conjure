import ../types
import db_sqlite, parseutils, intsets

proc getNotNullIndexes*(start, finish: int): string =
    for i in countUp(start, finish):
        result &= " and index" & $i & " is not null ";

proc getNullIndexes*(depth: int): string =
    for i in countUp(depth, maxIndex):
        result &= " and index" & $i & " is null ";

proc getSingleIndex*(depth, parentLower: int): string =
    return "and index" & $(depth) & " = " & $parentLower & " "

proc getSingleIndexLE*(depth, parentLower: int): string =
    return "and index" & $(depth) & " <= " & $parentLower & " "

proc getParentIdIndexes*(ancestors: seq[int]): string =
    for i in countUp(0, ancestors.len() - 1):
        result &= " and index" & $i & " = " & $ancestors[i] & " "

proc includeValues*(db: DbConn, s: Set, valuesQuery, nodeId: string) =
    for res in db.rows(sql(valuesQuery), nodeId):
        var lower: int
        discard res[0].parseInt(lower)
        s.included.incl(lower)

proc dontExcludeValues*(db: DbConn, s: Set, valuesQuery, nodeId: string) =
    var lower: int
    var upper: int
    for res in db.fastRows(sql(valuesQuery), nodeId):
        discard res[0].parseInt(lower)
        discard res[1].parseInt(upper)
        for i in countUp(lower, upper):
            s.notExcluded.incl(i)

proc makeChildSet*(s: Set, setId: int): Set =
    var currentSet: Set
    currentSet.deepCopy(s.inner)
    currentSet.name = getSetName(s, setId)
    currentSet.id = setId
    s.children.add(currentSet)
    return currentSet

proc getInnerSetQuery*(s, parent: Set, ancestors: seq[int],
        outerSetName: string): string =
    var index = ancestors.len()
    result = "SELECT index" & $index & ", lower, upper FROM domain WHERE name like '%" & outerSetName

    if (s of OccurrenceSet):
        result &= "\\_%Occurrence%' "
    if (s of DummySet):
        result &= "\\_%ExplicitVarSizeWithDummy%' "
    if (s of ExplicitSet):
        result &= "\\_%Explicit\\_%' "

    result &= "escape '\\' "

    if parent != nil:
        result &= getParentIdIndexes(ancestors)

    result &= " and nodeId = ?;"
