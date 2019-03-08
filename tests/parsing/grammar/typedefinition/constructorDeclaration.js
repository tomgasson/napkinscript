type t = Rgb
type t =
  | Rgb

type t =
  | White(grayscale)
  | Black(grayscale,) // trailing comma
  | Rgb(int, int, int) 
  | Rgba(int, int, int, int,) // trailing comma

type t =
  | Rgb({r: int, g: int, b: int})
  // trailing comma
  | Rgba({r: int, g: int, b: int, a: int,},)
  | JsColor({. "gradient": int})
  | JsColor({. "gradient": int},)
  | JsColor({. "gradient": int}, color)
  | JsColor({. "gradient": int}, color,)
  | JsColor({. "gradient": int}, {. "hex": string}, int)
  | JsT({.}) // Note: this compiles to bucklescript
  | JsT({..}) // Note: this compiles to bucklescript

// gadt
type t = Rgb : t
type t =
  | Rgb : t

type t =
  | White(grayscale) : t
  | Black(grayscale,) : t// trailing comma
  | Rgb(int, int, int) : t
  | Rgba(int, int, int, int,) : t // trailing comma

type t =
  | Rgb({r: int, g: int, b: int}) : t
  // trailing comma
  | Rgba({r: int, g: int, b: int, a: int,},) : t
  | JsColor({. "gradient": int}) : t
  | JsColor({. "gradient": int},) : t
  | JsColor({. "gradient": int}, color) : t
  | JsColor({. "gradient": int}, color,) : t
  | JsColor({. "gradient": int}, {. "hex": string}, int) : t
  | JsT({.}) : t // Note: this compiles to bucklescript
  | JsT({..}) : t // Note: this compiles to bucklescript


// attributes
// type t = @attr EmptyColor
type t = 
  | @attr EmptyColor
  | @onConstr White(grayscale) : @onGadt t
