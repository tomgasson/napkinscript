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

// attributes
// type t = @attr EmptyColor
type t = 
  | @attr EmptyColor
  | @onConstr White(grayscale) : @onGadt t
