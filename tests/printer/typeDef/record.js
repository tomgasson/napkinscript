type user = {name: string, age: int}
type result<'success, 'failure> = {ok: 'success, fail: 'failure}
type result<'success1, 'failure1, 'success2, 'failure2, 'success3, 'failure3,> = {ok: 'success1, fail: 'failure2}

type user = {name: string, age: int, a: string, lot: string, more: string, fields: string}
type user<'lotsOfTypeVars, 'lotsOfTypeVars2,'lotsOfTypeVars3, 'lotsOfTypeVars4> = {name: string, age: int, a: string, lot: string, more: string, fields: string}

// auto-breaks over multiple lines, because user gave "hints" to printer
type user = {
  name: string,
  age: int
}

type result<'good, 'bad> = {good: 'good, bad: 'bad}
  constraint 'good = boolean constraint 'bad = float 

type user = {
  name: 't,
  age: int
} constraint 't = string
  
