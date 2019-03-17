import variable

type Expression* = ref object of Variable
    ## Represents an aux variable
    auxName* : string

proc newExpression*(name, auxName: string): Expression =
    return Expression(name: name, auxName: auxName)

proc `$`*(e: Expression): string =
    return "<Expr> " & e.auxName & " " & e.name & " " & e.rng
