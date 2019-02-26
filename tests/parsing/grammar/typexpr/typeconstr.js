type t = string
type t = Parser.t
type t = Lang.Parser.t
type t = option<string>
type t = option<string,>
type t = Option.t<string>
type t = Option.t<string,>
type t = Mod.Sub.t<a, b, c>
type t = Mod.Sub.t<a, b, c,>
type t = list
type t = list<string>

let t: string = x 
let t: Parser.t = x
let t: Lang.Parser.t = x
let t: option<string> = x
let t: option<string,> = x
let t: Option.t<string> = x
let t: Option.t<string,> = x
let t: Mod.Sub.t<a, b, c> = x
let t: Mod.Sub.t<a, b, c,> = x
let t: list = x
let t: list<string> = x

// >= isn't an infix op
let t: list<string>= x
