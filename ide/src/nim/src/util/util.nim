
proc getPrettyRange*(lowerBound: string, upperBound: string): string =
    if lowerBound == upperBound:
       return "int(" & $lowerBound & ")" 
    return "int(" & $lowerBound & ".." & $upperBound & ")"