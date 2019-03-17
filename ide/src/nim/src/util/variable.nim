type Variable* = ref object of RootObj
    ## Represents a domain variable
    name*: string
    rng*: string

proc newVariable*(name, rng: string = "UNDEFINED"): Variable =
    ## Constructor
    return Variable(name: name, rng: rng)

proc `$`*(v: Variable): string =
    ## toString method
    result = "<Variable> " & v.name & " " & v.rng
