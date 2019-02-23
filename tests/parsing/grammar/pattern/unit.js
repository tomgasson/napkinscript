let () = ()
let () as x = ()
let (()) = ()
let (() as x) = ()
let (()) as x = ()
let ((): unit) = ()
let ((): unit) as x = ()
let (((): unit) as x)  = ()

switch x {
| () => ()
| () as _u => ()
| (()) => ()
| (()) as _u => ()
| (() as _u) => ()
| ((): unit) => ()
| ((): unit) as _u => ()
| (((): unit) as _u) => ()
}

for () in () to () { () }
for () in () to () { () }
for (() in () to ()) { () }
for ((()) in () to ()) { () }
for (((): unit) in () to ()) { () }

let f = () => ()
let f = (()) => ()
let f = ((), ()) => ()
let f = (() : unit) => ()
let f = ((() : unit)) => ()
