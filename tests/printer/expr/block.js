let x = {
  module Foo = Bar
  exception Exit
  open Belt
  let a = 1
  let b = 2
  sideEffect()
  sideEffect2()
  ()
}

let () = {
  let () = foo() // don't print unit on the next line
}
