// field expressions
let x = a.b
let x = a.b.c
let x = H20.Water.water.h
let x = p.Parser.token
let x = p.Lang.Parser.token.pos

// set field
lexbuf.lnum = lexbuf.lnum + 1

// array access sugar
let x = arr[0]
let x = arr[x: int]

// array mutation sugar
arr[0] = a + b

// call expressions
f()
f(x : int)
f(a, b, c)
f(a, b, c,) // trailing comma

// labelled arguments
f(~a, ~b=bArg, ~c?, ~d=?expr,)

// labelled argumetns with constraints
f(~a=x :int, ~b=? y: int)
