import intsets, algorithm, sequtils


proc getPrettyRange*(lowerBound: string, upperBound: string): string =
    if lowerBound == upperBound:
       return "int(" & $lowerBound & ")" 
    return "int(" & $lowerBound & ".." & $upperBound & ")"

proc prettifyIntSet*(i: IntSet): string =

    result = "int("

    var list = toSeq(i.items())

    if list.len() == 0:
        return "int()"

    list.sort(cmp)

    var index = 0
    var prevNum = $list[0]

    result &= $list[0]

    while (index < list.len() - 1):

        while (list[index + 1] - list[index] == 1):
            index.inc()
            if (index == list.len() - 1):
                break
        
        if ($list[index] != prevNum):
            result &= ".." & $list[index]
            prevNum = $list[index]

        index.inc()

        if (index <= list.len()-1):
            result &= ","
            result &= list[index]
            prevNum = $list[index]
        
    
    result &= ")"
