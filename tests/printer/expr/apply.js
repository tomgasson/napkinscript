console.log()
Js.log("arg1", "arg2")
let rbt = make(~compare)
let rbt = make(~compare?)
let rbt = make(~compare=intCompare)
let rbt = make(~compare=?intCompare)
let rbt = make(~compare=?intCompare: (int, int) => int)

let () = applyFunctionToArguments(
  superLongIdentifierWooooooowThisIsSuchLong,
  superLongIdentifierWooooooowThisIsSuchLong,
  superLongIdentifierWooooooowThisIsSuchLong,
  superLongIdentifierWooooooowThisIsSuchLong,
)

let cmp = rbt.compare(. Js.Array2.unsafe_get(old, oldIter.contents), node.value)
let cmp = rbt.compare2(. Js.Array2.unsafe_get(old, oldIter.contents), longerNode.longValue)
let uncurriedUnit = apply(.)

let coordinate = make2dCoordinate({x: 1, y: 2})
let coordinate = make3dCoordinate({...base, field1: thisIsAPrettyLongNameHere, field2: thisIsAPrettyLongNameHere, field3: thisIsAPrettyLongNameHere})

let coordinate = make2dCoordinateArray([x, y])
let coordinate = make3dCoordinateArray([thisIsAPrettyLongNameHere, thisIsAPrettyLongNameHere,thisIsAPrettyLongNameHere])

let coordinate = make2dCoordinateTuple(/x, y/)
let coordinate = make3dCoordinateTuple(/thisIsAPrettyLongNameHere, thisIsAPrettyLongNameHere,thisIsAPrettyLongNameHere/)

let coordinate = make2dCoordinateList(list(x, y))
let coordinate = make3dCoordinateList(list(thisIsAPrettyLongNameHere, thisIsAPrettyLongNameHere,thisIsAPrettyLongNameHere, ...allCoords))

let coordinate = makeJsCoordinate({"x": 1, "y": 1})
let user = makeJsUser({
  "name": "steve",
  "age":  32
})
