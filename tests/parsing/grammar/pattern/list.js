let list() = ()
let list(x) = ()
let list(x, ...xs) = ()
let list(x, y, ...tail) = ()
let list(x, y) = ()
let list(x, y,) = () // trailing comma
let list(x, ...xs,) = () // trailing comma
let list(x, y, ...tail,) = ()// trailing comma
let list(x, list(y, ...ys), ...xs) = ()
let (list(x, ...xs) : list<int>) = ()

switch x {
| list() => ()
| list(x) => ()
| list(x, ...xs) => ()
| list(x, y, ...tail) => ()
| list(x, y) => ()
| list(x, y,) => () // trailing comma
| list(x, ...xs,) => () // trailing comma
| list(x, y, ...tail,) => ()// trailing comma
| list(x, list(y, ...ys), ...xs) => ()
| (list(x, ...xs) : list<int>) => ()
}

let f = (list()) => ()
let f = (list(x)) => ()
let f = (list(x, ...xs)) => ()
let f = (list(x, y, ...tail)) => ()
let f = (list(x, y)) => ()
let f = (list(x, y,)) => () // trailing comma
let f = (list(x, ...xs,)) => () // trailing comma
let f = (list(x, y, ...tail,)) => ()// trailing comma
let f = (list(x, list(y, ...ys), ...xs)) => ()
let f = (list(x, ...xs) : list<int>) => ()

for list() in x to y {
  ()
}
for (list() in x to y) {
  ()
}
for ((list()) in x to y) {
  ()
}
for list(x) in x to y {
  ()
}
for (list(x) in x to y) {
  ()
}
for ((list(x)) in x to y) {
  ()
}
for list(x, ...xs) in x to y {
  ()
}
for (list(x, ...xs) in x to y) {
  ()
}
for ((list(x, ...xs)) in x to y) {
  ()
}
for ((list(x, ...xs) : list<int>) in x to y) {
  ()
}

