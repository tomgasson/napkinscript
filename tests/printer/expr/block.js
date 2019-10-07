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
