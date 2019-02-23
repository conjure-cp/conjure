type Variable* = ref object of RootObj
  name*: string
  rng*: string

proc newVariable*(name, rng: string = "UNDEFINED"): Variable =
  return Variable(name: name, rng: rng)

proc `$`*(v: Variable): string =
  # if v of Expression:
    #     return "<Expr> " & v.name & " " & v.rng

  result = "<Variable> " & v.name & " " & v.rng
