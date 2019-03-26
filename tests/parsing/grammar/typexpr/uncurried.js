type t = {
  mutable field: (. float, int, bool) => unit
}

type t = (. float, int, bool) => unit

@bs.val
external setTimeout : ((. unit) => unit, int) => timerId = "setTimeout"
// totally different meaning
external setTimeout : (. unit => unit, int) => timerId = "setTimeout"
