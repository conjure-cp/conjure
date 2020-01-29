import strutils, system, types, parseutils

proc getPrettyBranchingCondition*(vars: seq[Variable], branchName, isLeft,
        val: string): string

proc getLabel*(vars: seq[Variable], branchName, isLeft, value: string,
        wantPretty: bool = false): string =
    ## Returns the label for a node

    if branchName == "":
        return ""

    if wantPretty:
        var pretty = ""

        # if ("_" in branchName):
        #     pretty = getPrettyBranchingCondition(vars, branchName, isLeft, value)

        if pretty != "":
            return pretty

    result &= branchName

    if (isLeft == "1"):
        result &= " = "
    else:
        result &= " != "

    result &= value

proc innerSet*(s: Set, isLeft, val, name, branchName: string): string =

    let splitted = branchName.split("_")
    # echo "here"
    # echo splitted
    # echo s

    if splitted.len() == 3:

        if s of MarkerSet:
            if isLeft == "1":
                return "Cardinality of " & name & " was set to " & val
            return "Cardinality of " & name & " was not set to " & val

        if s of OccurrenceSet:
            var num: int
            discard splitted[^1].parseInt(num)

            if isLeft == "1":
                if val == "1":
                    return $num & " was included in " & name
                return $num & " was excluded from " & name
            else:
                if val == "1":
                    return $num & " was excluded from " & name
                return $num & " was included in " & name

        if s of ExplicitSet:
            if isLeft == "1":
                return val & " was included in " & name
            return val & " was not included in " & name

        if s of DummySet:

            let d = DummySet(s)
            var num: int
            discard splitted[^1].parseInt(num)

            if (val == $d.dummyVal):
                if isLeft == "1":
                    return "Cardinality of " & name & " is at most " & $num
                return "Cardinality of " & name & " is at least " & $num

            if isLeft == "1":
                return val & " was included in " & name
            return val & " was not included in " & name

    if splitted.len() > 3:

        if s of MarkerSet:
            if (not branchName.contains("Values")):
                if isLeft == "1":
                    return "Cardinality of " & name & " was set to " & val
                return "Cardinality of " & name & " was not set to " & val
            else:
                if isLeft == "1":
                    return val & " was included in " & name
                return val & " was not included in " & name

        if s of FlagSet:
            var num: int
            discard splitted[^1].parseInt(num)
            # echo name
            # echo name.contains("Values")

            if branchName.contains("Values"):
                if isLeft == "1":
                    return val & " was included in " & name
                else:
                    return val & " was not included in " & name
            else:
                if isLeft == "1":
                    if val == "1":
                        return "Cardinality of " & name & " is at least " & $num
                    else:
                        return "Cardinality of " & name & " is less than " & $num
                else:
                    if val == "1":
                        return "Cardinality of " & name & " is less than " & $num
                    else:
                        return "Cardinality of " & name & " is at least " & $num

proc handleExplicit(name, isLeft, val: string): string =
    if isLeft == "1":
        return val & " was included in " & name
    return val & " was not included in " & name

proc handleMarkerValues(name, isLeft, val: string): string =
    if isLeft == "1":
        return val & " was included in " & name
    else:
        return val & " was not included in " & name

proc handleMarkerMarker(name, isLeft, val: string): string =
    if isLeft == "1":
        return "Cardinality of " & name & " was set to " & val
    return "Cardinality of " & name & " was not set to " & val

proc handleFlagFlags(name, isLeft, val, num: string): string =
    if isLeft == "1":
        if val == "1":
            return "Cardinality of " & name & " is at least " & $num
        else:
            return "Cardinality of " & name & " is less than " & $num
    else:
        if val == "1":
            return "Cardinality of " & name & " is less than " & $num
        else:
            return "Cardinality of " & name & " is at least " & $num

proc handleOccurrence(name, isLeft, val, num: string): string =
    if isLeft == "1":
        if val == "1":
            return num & " was included in " & name
        return num & " was excluded from " & name
    else:
        if val == "1":
            return num & " was excluded from " & name
        return num & " was included in " & name

proc handleDummy(name, isLeft, val, num: string, s: Set): string =
    let d = DummySet(s)

    if (val == $d.dummyVal):
        if isLeft == "1":
            return "Cardinality of " & name & " is at most " & num
        return "Cardinality of " & name & " is at least " & num

    if isLeft == "1":
        return val & " was included in " & name
    return val & " was not included in " & name


proc getSetName(parentName: string, indexes: seq[int]): string =
    if indexes.len() > 0:
        return parentName & "-" & indexes.join("-")
    return parentName

proc getSetLabel(s: Set, isLeft, val, branchName: string): string =

    var indexes: seq[int]
    let splitted = branchName.split("_")
    var depth = 0

    for split in splitted:
        try:
            indexes.add(split.parseInt())
            depth.inc
        except ValueError:
            discard

    var current = s
    var deepest = s
    var maxDepth = 0

    while deepest != nil:
        maxDepth.inc()
        deepest = deepest.inner

    var copy = depth

    while copy > 1:
        current = current.inner
        copy.dec()

    # var b = branchName

    # var childName = s.name

    if current of MarkerSet:
        if (splitted[splitted.len() - depth - 1]) == "Values":
            let childName = getSetName(s.name, indexes[0..^2])
            return handleMarkerValues(childName, isLeft, val)
        else:
            let childName = getSetName(s.name, indexes)
            return handleMarkerMarker(childName, isLeft, val)

    if current of FlagSet:
        if (splitted[splitted.len() - depth - 1]) == "Values":
            let childName = getSetName(s.name, indexes[0..^2])
            return handleMarkerValues(childName, isLeft, val)
        else:
            let childName = getSetName(s.name, indexes[0..^2])
            return handleFlagFlags(childName, isLeft, val, $parseInt(splitted[^1]))

    if current of ExplicitSet:
        let childName = getSetName(s.name, indexes[0..^2])
        return handleExplicit(childName, isLeft, val)

    if current of OccurrenceSet:
        let childName = getSetName(s.name, indexes[0..^2])
        return handleOccurrence(childName, isLeft, val, $parseInt(splitted[^1]))

    if current of DummySet:
        let childName = getSetName(s.name, indexes[0..^2])
        return handleDummy(childName, isLeft, val, $parseInt(splitted[^1]), current)







proc getPrettyBranchingCondition*(vars: seq[Variable], branchName, isLeft,
        val: string): string =
    ## Returns a prettified label

    let splitted = branchName.split("_")

    let name = splitted[0]

    var s: Set
    s = nil

    for v in vars:
        if (v.name == name):
            s = Set(v)
            break

    if s == nil:
        return ""

    return getSetLabel(s, isLeft, val, branchName)

    # if s.current == nil:
    #     return innerSet(s, isLeft, val, name, branchName)
    # else:
    #     return getInnerSetName(s, isLeft, val, branchName)


    # return ""

