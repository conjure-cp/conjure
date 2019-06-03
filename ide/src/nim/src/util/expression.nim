import variable

type Expression* = ref object of Variable
proc newExpression*(name: string): Expression =
    return Expression(name: name)

proc `$`*(e: Expression): string =
    return "<Expr> " & e.name & " " & e.rng
