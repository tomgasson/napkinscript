let x = a + b
let x = @attr (a + b)
let x = @attr (@attr a + @attr b)
let x = a && b + c
let x = (a && b) + c
let x = a && b || c 

let x = (a && b) + (c && d)

while (continuePrefix.contents && aPrefixLen.contents && bPrefixLen.contents && foobarLen.contents) {
  ()
}

// uncurried attribute shouldn't result in parens
while (
  rbt.compare(. Js.Array2.unsafe_get(old, oldIter.contents), node.value) < 0
) {
  ()
}

// unary expr as child of binary expr doesn't need parens
let x = !filePath.includes(allMlSuffixesCategory) &&
  !filePath.endsWith(allScriptDirectoriesCategory)

let name = names[0] ++ names[1]
let name = user["firstName"] ++ user["lastName"]

let x = foo ++ bar
let x = foo != bar
let x = foo !== bar

let x = foo ++ bar
let x = 1 + 1
let x = (a => a + 1) + (b => b + 2)
let x = -1 + -1
let x = switch z { | Red => 1 } |> switch y { | Blue => 2 }
let x = try z catch { | Exit => 1 } |> try y catch { | Blue => 2 }
let x = if true { 1 } else { 2 } + if false { 2 } else { 3 }
let x = for i in 0 to 10 { () } |> for i in 0 to 10 { () }

let x = (/a, b/) + /b, c/
let x = Vec3(a, b, c) + Vec4(a, b, c, d)
let x = {x: 1, y: 2} + {x: 2, y :3}
let x = user.firstName ++ user.lastName
let x = x.left = value |> x.right = value
let x = (x.left = value) |> x.right = value
let x = (x.left = value) |> (x.right = value)
let () = (x.left = value) |> logMutation
let () = x.left = value |> logMutation
let () = x.left = (value |> process) |> x.right = value |> process
let () = (x: int) |> (print_int: int => unit)

// math
x + y / z
x / y + z
100 * x / total
2 / 3 * 10 / 2 + 2
let rotateX = ((range / rect.height) * refY - range / 2) * getXMultiplication(rect.width)
let rotateY = ((range / rect.width) * refX - range / 2) * getYMultiplication(rect.width)

let x = longIdentifier + longIdentifier + longIdentifier;
let x = longIdentifier + longIdentifier + longIdentifier + longIdentifier - longIdentifier + longIdentifier;
let x = longIdentifier + longIdentifier * longIdentifier + longIdentifier - longIdentifier + longIdentifier;
let x = longIdentifier + longIdentifier * longIdentifier * longIdentifier / longIdentifier + longIdentifier;

let x = longIdentifier && longIdentifier && longIdentifier && longIdentifier && longIdentifier && longIdentifier;
let x = longIdentifier && longIdentifier || longIdentifier && longIdentifier || longIdentifier && longIdentifier;

if (
  successorParent.color === Black &&
    (sibling === None ||
      (siblingNN.color === Black &&
      siblingNN.left === None ||
        (siblingNN.left->castNotOption).color === Black &&
      (siblingNN.right === None ||
        (siblingNN.right->castNotOption).color === Black)))
) {
  if sibling !== None {
    siblingNN.color = Red
  }
  successorRef.contents = successorParent
}

let truth = longEqualityExpression.someRecordField ==
wowThisDoesHaveToBePrettyLong.someRecordField &&
  longEqualityExpression.someRecordField ==
  wowThisDoesHaveToBePrettyLong.someRecordField

while (
  continuePrefix.contents &&
  aPrefixLen.contents <= aLen &&
  bPrefixLen.contents <= bLen
) {
  let nextNonwhiteA = nextNonWhiteChar(aStr, 1, aPrefixLen.contents - 1)
  let nextNonwhiteB = nextNonWhiteChar(bStr, 1, bPrefixLen.contents - 1)
  ()
}
let () = {
  let () = sideEffect()
  if (
    (isLeft(successor) && sibling.right === None) ||
      ((sibling.right->castNotOption).color === Black &&
      sibling.left !== None &&
      (sibling.left->castNotOption).color === Red)
  ) {
    sibling.color = Red
    sibling.left->castNotOption.color = Black
    rotateRight(rbt, sibling)
  }
  if (
    sibling !== None && (sibling->castNotOption).color === Black
  ) {
    let sibling = sibling->castNotOption
    if (
      isLeft(successor) &&
      sibling.right === None ||
        (sibling.right->castNotOption).color === Black &&
      sibling.left !== None &&
      (sibling.left->castNotOption).color === Red
    ) {
      sibling.color = Red
      sibling.left->castNotOption.color = Black
      rotateRight(rbt, sibling)
    } else if (
      !isLeft(successor) &&
      sibling.left === None ||
        (sibling.left->castNotOption).color === Black &&
      sibling.right !== None &&
      (sibling.right->castNotOption).color === Red
    ) {
      sibling.color = Red
      sibling.right->castNotOption.color = Black
      rotateLeft(rbt, sibling)
    }
    break.contents = true
  } else {
    let sibling = siblingOf(successor)
    let sibling = sibling->castNotOption
    sibling.color = successorParent.color
    if isLeft(successor) {
      sibling.right->castNotOption.color = Black
      rotateRight(rbt, successorParent)
    } else {
      sibling.left->castNotOption.color = Black
      rotateLeft(rbt, successorParent)
    }
  }
}

let x = a && (b || c)
let x = a && (b || c) && d
let x = a && b + c
let x = a && b + c && d

let x = a && @attr b && c
let x = @attr a && @attr b && @attr c
let x = a && @attr (b && c)
let x = a && @attr (b && c) && @attr (d && e)

let x = a && @attr (x |> f(g))
let x = a && @attr (x |> f(g)) && @attr (y |> f(h))

let x = a && a.b
let x = a && x.y && g.h
let x = a && true
let x = a && true && true
let x = a && {let a = true; let b = true; a || b}
let x = a && {let a = true; let b = true; a || b} && {let a = true; let b = true; a || b}

let x = a && x => x->Js.log
let x = a && (x => x->Js.log) && x => x->Js.log

let x = a && !b
let x = a && !b && !c

let x = a && f(b)
let x = a && f(b) && f(c)

let x = a && f(. b)
let x = a && f(. b) && f(. c)

let x = a && x |> f(g)
let x = a && x |> f(g) && y |> f(h)

let x = a && switch color {
  | Blue => "blue"
  | Red => "red"
}
let x = a && switch color {
  | Blue => "blue"
  | Red => "red"
} && switch color {
  | Blue => "blue"
  | Red => "red"
}

let x = a && try unsafe() catch {
  | Error => ()
}

let x = a && try unsafe() catch {
  | Error => ()
} && try unsafe() catch {
  | Error => ()
}

let x = a && /a, b/
let x = a && /a, b/ && /c, d/

let x = a && Foo(x, y)
let x = a && Foo(x, y) && Foo(x, y, z)

let x = a && {x: 1, y: 2}
let x = a && {x: 1, y: 2} && {x: 3, y: 3}

let x = a && b.c
let x = a && b.c && d.e

let x = a && row.id = 1
let x = a && (row.id = 1) && row2.id = 3

let x = a && [a, b]
let x = a && [a, b] && [c, d]

let x = a && if true { true } else { false }
let x = a && if true { true } else { false } && if false { false } else { true}

let x = a && {sideEffect(); do()}
let x = a && {sideEffect(); do()} && {sideEffect(); do()}

let x = a && while true { do() }
let x = a && while true { do() } && while true { do() }

let x = a && for i in 0 to 10 { true }
let x = a && for i in 0 to 10 { true } && for i in 0 to 10 { false }

let x = a && (truth: bool)
let x = a && (truth: bool) && (otherTruth: bool)

let x = a && {module L = Log; L.log()}
let x = a && {module L = Log; L.log()} && {module L = Log; L.log()}

let x = a && {exception Exit; raise(Exit)}
let x = a && {exception Exit; raise(Exit)} && {exception Exit; raise(Exit)}

let x = a && assert false
let x = a && assert false && assert true

let x = a && lazy false
let x = a && lazy false && lazy true

let x = a && {open React; killPerform()}
let x = a && {open React; killPerform()} && {open Dom; regainPerform()}

let x = a && %bs.raw("true")
let x = a && %bs.raw("true") && %bs.raw("false")

let x = a && module(Foo)
let x = a && module(Foo) && module(Bar)

let x = a && module(Foo: Bool)
let x = a && module(Foo: Bool) && module(Bar: Bool)

let x = a && truths[0]
let x = a && truths[0] && truths[1]
