let x = -a
let x = -.a
let x = !a
let x = &a

let isMale = !user["female"]
let stuff = &addressRefs[0]

!(!a)
!(!(!a))
!(a |> f(b))

(-1)->add
-1->add
-(1->add)
!foo->print_bool
(!foo)->print_bool
!(foo->parseTruth)


node.left->peekMinNode
// same as the above
(node.left)->peekMinNode

// field access has higher precedence than unary -
let x = -a.bar
// same as above
let x = -(a.bar)

!(lazy x)
lazy !x
!(assert x)
assert !x
!(@attr expr)
!(arg => doStuffWith(arg))
let x = !(truth: bool)
let x = (!truth: bool)

let z = !%extension
&x.left = value
&(x.left = value)
// (!x.left) = value -> this does not parse, lhs of Pexp_setfield can't be a unary expression?
-[a, b, c]
!module(Foo: Bar)
!module(Foo)
let x = -apply(arg)
let x = -apply(. arg)
let x = -Foo(a, b, c)
let x = -{x:1, y: 2}
let x = -list[1, 2, 3]
let x = -(if true { 1 } else { 2 })
let x = -(for i in 0 to 10 { () })
let x = -(switch x {| Foo => 1})
let x = -(while i < 10 { i.contents = 20 })
let x = -(1, 2, 3)
let x = -{
  let a = 1
  let b = 2
  a + b
}
let x = -{
  sideEffect()
  generateNumber()
}

-(true ? 0 : 1)

let () = {
  getResult()
  -10
}
