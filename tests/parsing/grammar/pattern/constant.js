let "stringPattern" = ()
let "stringPattern" as s = ()
let ("stringPattern" : string) = ()
let ("stringPattern" : string) as s = ()

switch x {
| "stringPattern" => ()
| "stringPattern" as s => ()
| ("stringPattern" : string) as s => ()
}

for "stringPattern" in 0 to 10 { () }
for "stringPattern" as s in 0 to 10 { () }

for (("stringPattern") in 0 to 10) { () }
for (("stringPattern") as s in 0 to 10) { () }
for (("stringPattern" as s) in 0 to 10) { () }

let f = ("stringPattern") => ()
let f = ("stringPattern" as s) => ()
let f = (("stringPattern" as s)) => ()
let f = (("stringPattern" : string)) => ()
let f = (("stringPattern" : string) as s) => ()
let f = ("stringPattern" : string) => ()

let 1 = ()
let 1 as x = ()
let (1: int) = ()
let (1: int) as x = ()

switch x {
| 1 => ()
| 1 as x => ()
| (1 : int) => ()
| (1 : int) as x => ()
}

let f = (1) => ()
let f = (1 as x) => ()
let f = ((1: int)) => ()
let f = ((1: int) as x) => ()
let f = (1: int) => ()

for i in 0 to 10 { () }
for i as x in 0 to 10 { () }
for ((i) in 0 to 10) { () }
for ((i) as x in 0 to 10) { () }
for ((i as x) in 0 to 10) { () }
