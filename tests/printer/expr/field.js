let n = user.name

(currentNode.parent->castNotOption).color
(&myRef).color
&(myRef.intValue)
&myRef.intValue

let x = -a.bar


let x = true.x
let x = (12: int).x
let x = list(1, 2, ...x).x
let x = module(Foo: Bar).x
let x = module(Foo).x
let x = Rgb(1, 2, 3).x
let x = [a, b, c].x
let x = {x: 1, y: 3}.x
let x = (/1, 2, 3/).x
let x = (%extension).x
let x = user.name.last
let x = streets[0].house
let x = apply(arg1, arg2).field
let x = apply(. arg1, arg2).field
let x = (-1).x
let x = (!true).x
let x = (x => print(x)).x
let x = (switch x {
  | Blue => ()
  | Yello => ()
}).x

let x = (for i in 0 to 10 {
  print_int(i)
}).x

let x = (if i < 10 {
  print_int(i)
} else {
  print_int(1000)
}).x

let x = (while i < 10 {
  print_int(i)
}).x

let x = (assert false).x
let x = (lazy false).x
let x = (try sideEffect() catch {| Exit => ()}).x

let x = (@attr expr).x

let x = (a + b).x

let x = @attr user.name
let x = (isLeft ? node.left : node.right).color
let x = (@attrOnTernary (isLeft ? node.left : node.right)).color

