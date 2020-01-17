let makeSet = (type s, cmp) => {
  module S = Set.Make({
    type t = s
    let compare = cmp
  })
  module(S : Set.S with type elt = s) // <- here
}

let three = module(Three : X_int)
let numbers = [three, module(Four)]
let numbers = (three, module(Four))
let numbers = list[three, module( Four )]

let numbers = [three, module({let x = 4})]
let numbers = (three, module({let x = 4}))
let numbers = list[three, module({let x = 4})]

let plus = (m1, m2) => {
  module({
    let x = to_int(m1) + to_int(m2)
  }: X_int)
}

let plus = (m1, m2) => module({
  let x = to_int(m1) + to_int(m2)
}: X_int)

let unique_instance = module({
  module Query_handler = Unique
  let this = Unique.create(0)
} : Query_handler_instance)

let build_instance = (
  type a,
  module(Q: Query_handler with type config = a),
  config
) => module({
  module Query_handler = Q
  let this = Q.create(config)
}: Query_handler_instance)
let build_instance = (
  type a,
  module(Q: Query_handler with type config = a),
  config
) => {
  module({
    module Query_handler = Q
    let this = Q.create(config)
  }: Query_handler_instance)
}

let unique_instance = build_instance(module(Unique), 0)

let build_dispatch_table = handlers => {
  let table = Hashtbl.create(module(String))
  List.iter(handlers, ~f=(module(I: Query_handler_instance) as instance) =>
    Hashtbl.set(table, ~key=I.Query_handler.name, ~data=instance), table
  )
}

module(Three)
module(Three: X_int)
