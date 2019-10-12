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
let x = x.left = (value |> process) |> x.right = value |> process
