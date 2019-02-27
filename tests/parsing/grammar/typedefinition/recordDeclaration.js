type t = {x: int}
type t = {mutable x: int}
// trailing comma
type t = {x: int,}
type t = {mutable x: int,}

type t = {x: int, y: int}
type t = {mutable x: int, mutable y: int}
// trailing comma
type t = {x: int, y: int,}
type t = {mutable x: int, mutable y: int}

// poly-typexpr
type t = {x: 'a 'b. polyType}

// attributes
type t = {@attr x: int, @attr2 y: int}
type t = {@rowAttr x: @onInt int, @rowAttr2 y: @onInt int}


