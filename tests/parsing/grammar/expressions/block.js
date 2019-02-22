let b = {
  module Array = Belt.Array
  [1, 2]
  ->Array.map(x => x + 1)
  ->Js.log
}

let b = {
  open Belt.Array
  [1, 2]
  ->map(x => x + 1)
  ->Js.log
}

let b = {
  exception QuitEarly
  raise(QuitEarly)
}

// let-bindings
let b = {
  let a = 1
  let b = 2
  a + b
}

let b = {
  let _ = sideEffect();
}

let b = {
  let _ = sideEffect() // no semi
}

let b = {
  a()
  b()
  c()
}

let b = {
  a()
  b()
  let a = 1
  f(a)
}

let b = {
  let a = 1
  let b = 2
}

// parse semicolons
let b = {
  module Array = Belt.Array;
  open Array;
  exception Terminate(int);
  let a = 1;
  let b = 2;
  sideEffect();
  let x = (1 + 2)->(x => x + 1);
  raise(Terminate(x)); 
}

let b = {
  f()
  g()
  h()
  let arr = [1, 2, 3]
}

let res = {
  let a = "a starts out as"
  {
    print_string(a)
    let a = 20
    print_int(a)
  }
  print_string(a)
}

let res = {
  let a = "first its a string"
  let a = 20
  print_int(a)
  print_int(a)
  print_int(a)
}

let res = {
  let a = "a is always a string"
  print_string(a)
  let b = 30
  print_int(b)
}

let nestedLet = {
  let _ = 1
  ()
}

let nestedLet = {
  let _ = 1
  2
}
