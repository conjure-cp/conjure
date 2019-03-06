import util, strutils, intsets, sequtils, algorithm, system, types, parseutils


proc getPrettyBranchingCondition*(vars: seq[Variable], branchName: string, isLeft: string, val: string): string 


proc getLabel*(vars: seq[Variable], branchName, isLeft, value: string, wantPretty: bool = false): string =

    if branchName == "":
        return ""

    if wantPretty:
        var pretty = ""

        if ("_" in branchName):
            pretty = getPrettyBranchingCondition(vars, branchName, isLeft, value)

        if  pretty != "":

            # echo branchName, isLeft, value
            # echo pretty

            return pretty

    result &= branchName

    if (isLeft == "1"):
        result &= " = "
    else:
        result &= " != "

    result &= value

proc getPrettyBranchingCondition*(vars: seq[Variable], branchName: string, isLeft: string, val: string): string =

    # echo isLeft

    let splitted = branchName.split("_")

    let name = splitted[0]

    var s : Set
    s = nil

    for v in vars:
        if (v.name == name):
            s = Set(v)
            break

    if s == nil:
        return ""


    if splitted.len() == 3:

        if s of MarkerSet:
            if isLeft == "1":
                return "Cardinality of " & name & " was set to " & val
            return "Cardinality of " & name & " was not set to " & val
        
        if s of OccurrenceSet:
            var num : int
            discard splitted[^1].parseInt(num)

            if isLeft == "1":
                if val == "1":
                    return $num & " was included in " & name
                return $num & " was excluded from " & name
            else:
                if val == "1":
                    return $num & " was not included in " & name
                return $num & " was not excluded from " & name

        if s of ExplicitSet:
            if isLeft == "1":
                return val & " was included in " & name
            return val & " was not included in " & name

        if s of DummySet:

            let d = DummySet(s)

            if (val == $d.dummyVal):
                if isLeft == "1":
                    return "Cardinality of " & name & " was reduced by 1"
                return "Cardinality of " & name & " was not reduced by 1"

            if isLeft == "1":
                return val & " was included in " & name
            return val & " was not included in " & name
        
    if splitted.len() == 4:

        if s of MarkerSet:
            if isLeft == "1":
                return val & " was included in " & name
            return val & " was not included in " & name
    return ""

