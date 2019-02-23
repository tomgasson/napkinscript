let /1, 2/ = ()
let /1, 2,/ = ()
let /1 : int, 2 : int/ = ()
let (/1 : int, 2 : int/ : /int, int/) = ()

switch x {
| /1, 2/ => () 
| /1, 2,/ => () 
| /1: int, 2 : int/ => () 
| (/1: int, 2 : int/ : /int, int/) => () 
}

let f = (/x/) => ()
let f = (/x, y,/) => x + y
let f = (/x, y/ : /int, int/) => ()
let f = ((/x, y/ : /int, int/)) => ()

for /x, y/ in 0 to 10 {
  ()
}
for (/x, y/ in 0 to 10) {
  ()
}
for ((/x, y/) in 0 to 10) {
  ()
}
for ((/x, y/ : /int, int/) in 0 to 10) {
  ()
}
