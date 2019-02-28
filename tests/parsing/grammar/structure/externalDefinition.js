external clear : (t, int) => unit = ""

// multiple primitives
external add_nat: nat => int = "add_nat_bytecode" "add_nat_native"

@bs.send
external attachShader: (t, ~program: webGlProgram, ~shader: webGlShader) => unit = ""
