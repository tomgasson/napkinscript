type color = Red | Blue
type color = Red | Blue | SuperLongColoooooooooourName | AnotherSuuuuuuuuuperLongName

// gadt
type color = | Red: color | Blue: color
type color = Red: color | Blue: color | SuperLongColoooooooooourName: color | AnotherSuuuuuuuuuperLongName: color

type color = Rgb(float, float, float) | Hex(int)
type color = Rgb(superLongTypeName, superLongTypeName, superLongTypeName, superLongTypeName) | Hex(superLongTypeName, superLongTypeName, superLongTypeName, superLongTypeName)

type node <_, 'value> =
  | Root({
      mutable value: 'value,
      mutable updatedTime: float,
    }): node<root, 'value>
  | Derived({
      mutable cachedValue: 'value,
      parent: node<_, 'value>,
      root: node<root, 'value>,
      updateF: 'value => 'value,
      mutable updatedTime: float,
    }): node<derived, 'value>

type t<'a> =
  | AutoDisposing('a => unit, option<exn> => unit, ref<bool>)
  | Delegating(
      'ctx,
      t<'b>,
      ('ctx, t<'b>, 'a) => unit,
      ('ctx, t<'b>, option<exn>) => unit,
      ref<bool>,
    ): t<'a>;

type t<'a> =
  | AutoDisposing('a => unit, option<exn> => unit, ref<bool>)
  | Delegating(
      'ctx,
      t<'b>,
      ('ctx, t<'b>, 'a) => unit,
      ('ctx, t<'b>, option<exn>) => unit,
      ref<bool>,
    ): t<'superLongTypeName, 'superLongTypeName, 'superLongTypeName, 'superLongTypeName>;

type jsx<'nature> =
  | Empty: jsx<empty>
  | Cons(elem</'s1, 'a1/, 'sub1>, jsx<'tl>): jsx</('a1, 's1) => 'sub1, 'tl/>

type jsx<'nature> =
  | Empty: jsx<empty>
  | Cons(elem</'s1, 'a1/, 'sub1>, jsx<'tl>): jsxxxxxxxxxxxxxxxxx</('a1, 's1) => 'sub1, 'tl/>

type color = Red | Blue
  constraint 't = string

type result<'good, 'bad> =
  | Good('good)
  | Bad('bad)
  constraint 'good = boolean
  constraint 'bad = float 
