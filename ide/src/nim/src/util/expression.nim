import variable

type Expression* = ref object of Variable
proc newExpression*(name, rng: string = "UNDEFINED"): Expression =
    return Expression(name: name, rng: rng)

proc `$`*(v: Expression): string =
    return "<Expr> " & v.name & " " & v.rng
