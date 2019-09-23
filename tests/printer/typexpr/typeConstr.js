type t = Option.t<string,>
type t = Mod.Sub.t<a, b, c>
type t = Mod.Sub.t<soooooLoooooong, soooooLoooooong, soooooLoooooong, soooooLoooooong, soooooLoooooong, soooooLoooooong, b, c>

type t = Mod.Sub.t<Mod.Sub.t<soooooLoooooong, soooooLoooooong, soooooLoooooong>, soooooLoooooong, soooooLoooooong, soooooLoooooong, b, c>
type t = Mod.Sub.t<Mod.Sub.t<soooooLoooooong, soooooLoooooong, soooooLoooooong, soooooLoooooong, soooooLoooooong>, Nested.Module.t<string, float>, soooooLoooooong, soooooLoooooong, b, c>


let t: list<{"age": int}> = x

let t: list<{"age": int, "name": string, "moreFields": veryVeryVeryLongtype, "superLongNameThat": whyDoesATypeWithThisAmountOfCharactersMakeItIntoThisFile, "because": testingLineBreaks}> = x

let t: list<
  {"age": int, @attr "name": string},
  {"name": string, @attr "age": int},
  {"name": string, @attr "age": int},
> = x

type t = Option.t</tupleTyp1, tupleTyp2, tupleTyp3/>
type t = Option.t</tupleTyp1, tupleTyp2, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3/>
let t: Option.t</tupleTyp1, tupleTyp2, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3, tupleTyp3/> = x
