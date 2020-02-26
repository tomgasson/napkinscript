let f = (a, b) => {a + b}
let f = (a, b) => { a }

let f = (a, b) => {
  a + b
}

let f = (a, b) => {
  a
}

let x = { a }
let x = { a + b }

let x = {
  // here
  a
}


let x = {
  // here
  a + b
}

let _ = { a }

let _ = { "constant" }

let _ = { () => Js.log("test") }
let _ = { switch b { | true => () | false => () } }
let _ = switch { b } { | true => () | false => () }
let _ = { apply(a, b) }
let _ =  {apply}(a, b) 

let _ = { try danger() catch { | Exit => () } }
let _ = try { danger() } catch { | Exit => () }

let _ = ({a}, {b}, {x + y})
let _ = { ({a}, {b}, {x + y})}

let _ = { Rgb(r, g, b) }
let _ = Rgb({r}, {g}, {b})

let _ = { {name:  "steve", age: 30 } }
let _ =  {name:  {"steve"}, age: 30 } 

let _ = { user.name }.toString()
let _ = { { user.name }.toString() }
let _ = { user.name }.first
let _ = { { user.name }.first }

let _ = { [a, b, c] }
let _ =  [{a}, {b}, {c}] 

let _ = {true} ? {true} : {false}
let _ = {{true} ? {true} : {false}}

let _ = { if true { () } else { () } }
let _ = { if { true } { () } else { () } }

let _ = { while true { () } }
let _ = while { true } { () }


let _ = { for _ in { 0 } to { 10 } { () } }

let _ = {(foo: string)}
let f = () => {(foo: string)} // equivalent to  (): string => ...

let _ = assert { true }
let _ = { assert { true } }
let _ = { lazy { true } }

let _ = { %extension }
let _ = { module(ME) }
let _ = { module(ME: MyMod) }
