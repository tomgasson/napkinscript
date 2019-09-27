type t = x => unit
type t = (x) => unit
type t = (int, string) => unit

type t = (~a: int, ~b: int) => int
type t = (~a: int=?, ~b: int=?) => int

type t = int => int => int => int

type t = (~a: int) => (~b: int) => (~c: int) => int

let f: x => unit = xf
let f: (x) => unit = xf
let f: (int, string) => unit = xf
let t: (~a: int, ~b: int) => int = xf
let t: (~a: int=?, ~b: int=?) => int = xf
let t: int => int => int => int = xf
let t: (~a: int) => (~b: int) => (~c: int) => int = xf

// single type parameter sugar
type t = ~f:int => string
type t = ~f:int=? => string
// single type parameter sugar
let f: ~f:int => string = fx
let f: ~f:int=? => string = fx

// different cases
type t = (~f: int) => string
type t = ~f: int => string
type t = (~f: int => string) => float
type t = ~f: (int => string) => float
type t = ~f: int => string => float

// if the @attr sits before the labelled arg, it's part of the arrow
// otherwise it's part of the type_expr itself, like on the last float
type t = (@attrBeforeLblA ~a: int, @attrBeforeLblB ~b: int, @attr float) => unit
// above is equivalent too
type t = @attrBeforeLblA ((~a: int) =>  (@attrBeforeLblB ((~b: int) => (@attr float => unit))) )

type t = @attr ~a: int => unit
