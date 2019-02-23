let {a} = x
let {a,} = x // trailing comma
let {a, b} = x
let {a, b,} = x // trailing comma
let {ReasonReact.state} = x
let {ReasonReact.state: state as prevState} = x
let {ReasonReact.state as theState} = x 
let {a: u} = x
let {a: (u: int)} = x
let {a: {x, y}} = x
let {a, _ } = x
let {a, _, } = x
let ({a} : myRecord) = x

switch x {
| {a} => ()
| {a,} => () // trailing comma
| {a, b} => () 
| {a, b,} => () // trailing comma
| {ReasonReact.state} => ()
| {ReasonReact.state: state as prevState} => ()
| {ReasonReact.state as theState} => ()
| {a: u} => ()
| {a: (u: int)} => ()
| {a: {x, y}} => ()
| {a, _ } => ()
| {a, _, } => ()
| ({a} : myRecord) => () 
}

let f = ({a}) => ()
let f = ({a,}) => () // trailing comma
let f = ({a, b}) => () 
let f = ({a, b,}) => () // trailing comma
let f = ({ReasonReact.state}) => () 
let f = ({ReasonReact.state: state as prevState}) => () 
let f = ({ReasonReact.state as theState}) => () 
let f = ({a: u}) => ()
let f = ({a: (u: int)}) => ()
let f = ({a: {x, y}}) => () 
let f = ({a, _ }) => () 
let f = ({a, _,}) => () 
let f = ({a}: myRecord) => () 
let f = (({a}: myRecord)) => () 


for {a} in 0 to 10 {
  ()
}
for (({a}) in 0 to 10) {
  ()
}
for ({a} in 0 to 10) {
  ()
}

for {ReasonReact.state} in 0 to 10 {
  ()
}
for ({ReasonReact.state} in 0 to 10) {
  ()
}
for (({ReasonReact.state}) in 0 to 10) {
  ()
}

for {ReasonReact.state: state as prevState} in 0 to 10 {
  ()
}
for ({ReasonReact.state: state as prevState} in 0 to 10) {
  ()
}
for (({ReasonReact.state: state as prevState}) in 0 to 10) {
  ()
}
for {ReasonReact.state as theState} in 0 to 10 {
  ()
}
for ({ReasonReact.state as theState} in 0 to 10) {
  ()
}
for (({ReasonReact.state as theState}) in 0 to 10) {
  ()
}
for {a: u} in 0 to 10 {
  ()
}
for (({a: u}) in 0 to 10) {
  ()
}
for ({a: u} in 0 to 10) {
  ()
}
for {a: (u: int)} in 0 to 10 {
  ()
}
for (({a: (u: int)}) in 0 to 10) {
  ()
}
for ({a: (u: int)} in 0 to 10) {
  ()
}
for {a: {x, y}} in 0 to 10 {
  ()
}
for (({a: {x, y}}) in 0 to 10) {
  ()
}
for ({a: {x, y}} in 0 to 10) {
  ()
}
for {a, _} in 0 to 10 {
  ()
}
for (({a, _}) in 0 to 10) {
  ()
}
for ({a, _} in 0 to 10) {
  ()
}
for (({a} : myRecord) in 0 to 10) {
  ()
}
