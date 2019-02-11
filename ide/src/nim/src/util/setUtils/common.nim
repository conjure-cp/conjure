include ../types

proc getSetName(parent : Set, setId : int): string =
    return parent.name & "-" & $setId