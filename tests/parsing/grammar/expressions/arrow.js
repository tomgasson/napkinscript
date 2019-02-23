let f = x => x + 1

let f = _ => Js.log("test")

let f = () => Js.log("unit")

// pattern
let f = (Reducer(inst, comp)) => inst.render(comp)
let f = (Instance) => ()
let f = (a, b) => a + b
let f = (1, 2) => ()
let f = ("stringPattern") => ()
let f = ("stringPattern", "stringPattern") => ()
let f = (()) => ()
let f = ((a: int), (b: int)) => a + b
let f = (_, _) => ()
let f = ([a, b], [c, d]) => a + b  + c + d
let f = ({a}) => a + 1
let f = ({a, b}, {c, d}) => a + b + c + d
let f = (/a/) => a + 1
let f = (/a, b/) => a + b
let f = (/a, b/, /c, d/) => a + b + c + d
let f = (exception Terminate) => ()
let f = (exception Terminate, exception Exit) => ()
let f = (lazy x) => ()
let f = (lazy x, lazy y) => ()
let f = (list()) => ()
let f = (list(x, ...xs)) => x + xs->Belt.List.length

// constrained pattern
let f = (x: int, y: int) => x + y

// ~labelName
let f = (~a, ~b) => a + b

// ~labelName as pattern
let f = (~a as x, ~b as y) => x + y

// ~ labelName as pattern : type
let f = (~a as x : int , ~b as y : int) => x + y

// ~ labelName = expr
let f = (~a=1, ~b=2, c) => a + b + c

// ~ labelName as pattern = expr
let f = (~a as x=1, ~b as y=2, c) => x + y + c

// ~ labelName as pattern : type = expr
let f = (~a as x: int=1, ~b as y: int=2, c) => x + y + c

// ~ labelName = ?
let f = (~a=?, ~b=?, c) => switch /a, b/ {
| /Some(a), Some(b)/ => a + b + c
| _ => 3
}

// ~ labelName as pattern = ?
let f = (~a as x=?, ~b as y=?, c) => switch /x, y/ {
| /Some(a), Some(b)/ => a + b + c
| _ => 3
}

// ~ labelName as pattern : type = ?
let f = (~a as x : option<int>=?, ~b as y : option<int>=?, c) => switch /x, y/ {
| /Some(a), Some(b)/ => a + b + c
| _ => 3
}

// trailing comma
let f = (a, b,) => a + b
