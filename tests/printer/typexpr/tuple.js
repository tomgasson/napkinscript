type t = /string, int, float/

type t = /
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak
/

type t = constr</string, int, float/>

type t = constr</
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak
/>

external foo: /string, int, float/ = "external_binding"

external foo: /
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak
/ = "external_binding"

let x: /int, int/ = /1, 2/

let x: /
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak,
  superLongTypeNameThatWillBreak
/ = /1, 2/
