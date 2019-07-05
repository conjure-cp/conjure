import intsets, algorithm, sequtils, json


proc getPrettyRange*(lowerBound: string, upperBound: string): string =
    ## Returns a range in the same format as essence
    if lowerBound == upperBound:
       return "int(" & $lowerBound & ")" 
    return "int(" & $lowerBound & ".." & $upperBound & ")"

# [ [2,2],[4,5],[7,15] ]

proc prettifyMinionStoreDump*(dump: string): string =

    result = "int("

    let list = parseJson(dump)

    for inner in list:
        if inner[0] == inner[1]:
            result &= $inner[0] & ", "
        else:
            result &= $inner[0] & ".." & $inner[1] & ", "

    result = result[0..result.len()-3] & ")"


# proc prettifyIntSet*(i: IntSet): string =
#     ## Returns a set as a range in the same format as essence

#     result = "int("

#     var list = toSeq(i.items())

#     if list.len() == 0:
#         return "int()"

#     list.sort(cmp)

#     var index = 0
#     var prevNum = $list[0]

#     result &= $list[0]

#     while (index < list.len() - 1):

#         while (list[index + 1] - list[index] == 1):
#             index.inc()
#             if (index == list.len() - 1):
#                 break
        
#         if ($list[index] != prevNum):
#             result &= ".." & $list[index]
#             prevNum = $list[index]

#         index.inc()

#         if (index <= list.len()-1):
#             result &= ","
#             result &= list[index]
#             prevNum = $list[index]
    
#     result &= ")"
