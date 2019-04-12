module type Signature = {
  type t

  @bs.send
  external linkProgram: (t, ~program: webGlProgram) => unit = "linkProgram"

  external add_nat: (nat, int, int) => int = "add_nat_bytecode" "add_nat_native"
}
