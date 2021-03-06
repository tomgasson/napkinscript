module Doc = {
  type rec mode = Break | Flat
  
  type rec lineStyle =
    | Classic
    | Soft
    | Hard
    | Literal
  
  type rec t =
    | Nil
    | Text(string)
    | Concat(list<t>)
    | Indent(t)
    | IfBreaks(t, t)
    | LineSuffix(t)
    | LineBreak(lineStyle)
    | Group((bool, t))
    | CustomLayout(list<t>)
    | BreakParent
  
  let nil = Nil
  let line = LineBreak(Classic)
  let hardLine = LineBreak(Hard)
  let softLine = LineBreak(Soft)
  let literalLine = LineBreak(Literal)
  let text = s => Text(s)
  let concat = l => Concat(l)
  let indent = d => Indent(d)
  let ifBreaks = (t, f) => IfBreaks(t, f)
  let lineSuffix = d => LineSuffix(d)
  let group = d => Group(false, d)
  let breakableGroup = (~forceBreak, d) => Group(forceBreak, d)
  let customLayout = gs => CustomLayout(gs)
  let breakParent = BreakParent
  
  let space = Text(" ")
  let comma = Text(",")
  let dot = Text(".")
  let dotdot = Text("..")
  let dotdotdot = Text("...")
  let lessThan = Text("<")
  let greaterThan = Text(">")
  let lbrace = Text("{")
  let rbrace = Text("}")
  let lparen = Text("(")
  let rparen = Text(")")
  let lbracket = Text("[")
  let rbracket = Text("]")
  let trailingComma = IfBreaks(comma, nil)
  
  let propagateForcedBreaks = doc => {
    let rec walk = doc =>
      switch doc {
      | (Text(_) | Nil) | LineSuffix(_) => (false, doc)
      | BreakParent => (true, Nil)
      | LineBreak(Hard | Literal) => (true, doc)
      | LineBreak(Classic | Soft) => (false, doc)
      | Indent(children) =>
        let (childForcesBreak, newChildren) = walk(children)
        (childForcesBreak, Indent(newChildren))
      | IfBreaks(trueDoc, falseDoc) =>
        let (falseForceBreak, falseDoc) = walk(falseDoc)
        if falseForceBreak {
          let (_, trueDoc) = walk(trueDoc)
          (true, trueDoc)
        } else {
          let (forceBreak, trueDoc) = walk(trueDoc)
          (forceBreak, IfBreaks(trueDoc, falseDoc))
        }
      | Group(forceBreak, children) =>
        let (childForcesBreak, newChildren) = walk(children)
        let shouldBreak = forceBreak || childForcesBreak
        (shouldBreak, Group(shouldBreak, newChildren))
      | Concat(children) =>
        let (forceBreak, newChildren) = List.fold_left(
          ((forceBreak, newChildren), child) => {
            let (childForcesBreak, newChild) = walk(child)
            (forceBreak || childForcesBreak, list[newChild, ...newChildren])
          },
          (false, list[]),
          children,
        )
        
        (forceBreak, Concat(List.rev(newChildren)))
      | CustomLayout(children) =>
        let (forceBreak, newChildren) = List.fold_left(
          ((forceBreak, newChildren), child) => {
            let (childForcesBreak, newChild) = walk(child)
            (forceBreak || childForcesBreak, list[newChild, ...newChildren])
          },
          (false, list[]),
          children,
        )
        
        (forceBreak, CustomLayout(List.rev(newChildren)))
      }
    
    let (_, processedDoc) = walk(doc)
    processedDoc
  }
  
  let join = (~sep, docs) => {
    let rec loop = (acc, sep, docs) =>
      switch docs {
      | list[] => List.rev(acc)
      | list[x] => List.rev(list[x, ...acc])
      | list[x, ...xs] => loop(list[sep, x, ...acc], sep, xs)
      }
    
    Concat(loop(list[], sep, docs))
  }
  
  let rec fits = w =>
    x => switch x {
    | _ when w < 0 => false
    | list[] => true
    | list[(_ind, _mode, Text(txt)), ...rest] =>
      fits(w - String.length(txt), rest)
    | list[(ind, mode, Indent(doc)), ...rest] =>
      fits(w, list[(ind + 2, mode, doc), ...rest])
    | list[(_ind, Flat, LineBreak(break)), ...rest] =>
      if break == Hard || break == Literal {
        true
      } else {
        let w = if break == Classic {
          w - 1
        } else {
          w
        }
        fits(w, rest)
      }
    | list[(_ind, _mode, Nil), ...rest] => fits(w, rest)
    | list[(_ind, Break, LineBreak(break)), ...rest] => true
    | list[(ind, mode, Group(forceBreak, doc)), ...rest] =>
      let mode = if forceBreak {
        Break
      } else {
        mode
      }
      fits(w, list[(ind, mode, doc), ...rest])
    | list[(ind, mode, IfBreaks(breakDoc, flatDoc)), ...rest] =>
      if mode == Break {
        fits(w, list[(ind, mode, breakDoc), ...rest])
      } else {
        fits(w, list[(ind, mode, flatDoc), ...rest])
      }
    | list[(ind, mode, Concat(docs)), ...rest] =>
      let ops = List.map(doc => (ind, mode, doc), docs)
      fits(w, List.append(ops, rest))
    
    | list[(_ind, _mode, LineSuffix(_)), ...rest] => fits(w, rest)
    | list[(_ind, _mode, BreakParent), ...rest] => fits(w, rest)
    | list[(_ind, _mode, CustomLayout(_)), ...rest] => fits(w, rest)
    }
  
  let toString = (~width, doc) => {
    let doc = propagateForcedBreaks(doc)
    let buffer = Buffer.create(1000)
    
    let rec process = (~pos, lineSuffices, stack) =>
      switch stack {
      | list[(ind, mode, doc) as cmd, ...rest] =>
        switch doc {
        | Nil | BreakParent => process(~pos, lineSuffices, rest)
        | Text(txt) =>
          Buffer.add_string(buffer, txt)
          process(~pos=String.length(txt) + pos, lineSuffices, rest)
        | LineSuffix(doc) =>
          process(~pos, list[(ind, mode, doc), ...lineSuffices], rest)
        | Concat(docs) =>
          let ops = List.map(doc => (ind, mode, doc), docs)
          process(~pos, lineSuffices, List.append(ops, rest))
        | Indent(doc) =>
          process(~pos, lineSuffices, list[(ind + 2, mode, doc), ...rest])
        | IfBreaks(breakDoc, flatDoc) =>
          if mode == Break {
            process(~pos, lineSuffices, list[(ind, mode, breakDoc), ...rest])
          } else {
            process(~pos, lineSuffices, list[(ind, mode, flatDoc), ...rest])
          }
        | LineBreak(lineStyle) =>
          if mode == Break {
            switch lineSuffices {
            | list[] =>
              Buffer.add_string(buffer, "\n")
              Buffer.add_string(buffer, String.make(ind, ' '))
              process(~pos=ind, list[], rest)
            | docs =>
              process(
                ~pos=ind,
                list[],
                List.concat(list[List.rev(lineSuffices), list[cmd, ...rest]]),
              )
            }
          } else {
            let pos = switch lineStyle {
            | Classic =>
              Buffer.add_string(buffer, " ")
              pos + 1
            | Hard | Literal =>
              Buffer.add_string(buffer, "\n")
              0
            | Soft => pos
            }
            
            process(~pos, lineSuffices, rest)
          }
        | Group(shouldBreak, doc) =>
          if (
            shouldBreak || !fits(width - pos, list[(ind, Flat, doc), ...rest])
          ) {
            process(~pos, lineSuffices, list[(ind, Break, doc), ...rest])
          } else {
            process(~pos, lineSuffices, list[(ind, Flat, doc), ...rest])
          }
        | CustomLayout(docs) =>
          let rec findGroupThatFits = groups =>
            switch groups {
            | list[] => Nil
            | list[lastGroup] => lastGroup
            | list[doc, ...docs] =>
              if fits(width - pos, list[(ind, Flat, doc), ...rest]) {
                doc
              } else {
                findGroupThatFits(docs)
              }
            }
          
          let doc = findGroupThatFits(docs)
          process(~pos, lineSuffices, list[(ind, Flat, doc), ...rest])
        }
      | list[] =>
        switch lineSuffices {
        | list[] => ()
        | suffices => process(~pos=0, list[], List.rev(suffices))
        }
      }
    
    process(~pos=0, list[], list[(0, Flat, doc)])
    
    let len = Buffer.length(buffer)
    if len > 0 && Buffer.nth(buffer, len - 1) !== '\n' {
      Buffer.add_char(buffer, '\n')
    }
    Buffer.add_char(buffer, '\n')
    Buffer.contents(buffer)
  }
  
  let debug = t => {
    let rec toDoc = x => switch x {
    | Nil => text("nil")
    | BreakParent => text("breakparent")
    | Text(txt) => text("text(" ++ txt ++ ")")
    | LineSuffix(doc) =>
      group(
        concat(list[
          text("linesuffix("),
          indent(concat(list[line, toDoc(doc)])),
          line,
          text(")"),
        ]),
      )
    | Concat(docs) =>
      group(
        concat(list[
          text("concat("),
          indent(
            concat(list[
              line,
              join(~sep=concat(list[text(","), line]), List.map(toDoc, docs)),
            ]),
          ),
          line,
          text(")"),
        ]),
      )
    | CustomLayout(docs) =>
      group(
        concat(list[
          text("customLayout("),
          indent(
            concat(list[
              line,
              join(~sep=concat(list[text(","), line]), List.map(toDoc, docs)),
            ]),
          ),
          line,
          text(")"),
        ]),
      )
    | Indent(doc) =>
      concat(list[text("indent("), softLine, toDoc(doc), softLine, text(")")])
    | IfBreaks(trueDoc, falseDoc) =>
      group(
        concat(list[
          text("ifBreaks("),
          indent(
            concat(list[
              line,
              toDoc(trueDoc),
              concat(list[text(","), line]),
              toDoc(falseDoc),
            ]),
          ),
          line,
          text(")"),
        ]),
      )
    | LineBreak(break) =>
      let breakTxt = switch break {
      | Classic => "Classic"
      | Soft => "Soft"
      | Hard => "Hard"
      | Literal => "Literal"
      }
      
      text("LineBreak(" ++ breakTxt ++ ")")
    | Group(shouldBreak, doc) =>
      group(
        concat(list[
          text("Group("),
          indent(
            concat(list[
              line,
              text("shouldbreak: " ++ string_of_bool(shouldBreak)),
              concat(list[text(","), line]),
              toDoc(doc),
            ]),
          ),
          line,
          text(")"),
        ]),
      )
    }
    
    let doc = toDoc(t)
    toString(~width=10, doc) |> print_endline
  }
}

module Time: {
  type rec t
  
  let now: unit => t
  
  let toUint64: t => int64
  
  let nanosecond: t
  let microsecond: t
  let millisecond: t
  let second: t
  let minute: t
  let hour: t
  
  let zero: t
  
  let diff: (t, t) => t
  let add: (t, t) => t
  let print: t => float
} = {
  type rec t = int64
  
  let zero = 0L
  
  let toUint64 = s => s
  
  let nanosecond = 1L
  let microsecond = Int64.mul(1000L, nanosecond)
  let millisecond = Int64.mul(1000L, microsecond)
  let second = Int64.mul(1000L, millisecond)
  let minute = Int64.mul(60L, second)
  let hour = Int64.mul(60L, minute)
  
  external init: unit => unit = "caml_mach_initialize"
  let () = init()
  external now: unit => t = "caml_mach_absolute_time"
  
  let diff = (t1, t2) => Int64.sub(t2, t1)
  let add = (t1, t2) => Int64.add(t1, t2)
  let print = t => Int64.to_float(t) *. 1e-6
}

module Benchmark: {
  type rec t
  
  let make: (~name: string, ~time: Time.t=?, ~f: t => unit, unit) => t
  let launch: t => unit
  let report: t => unit
} = {
  type rec benchmarkResult = {
    n: int,
    t: Time.t,
    bytes: float,
    memAllocs: float,
    memBytes: float,
  }
  
  type rec t = {
    name: string,
    time: Time.t,
    mutable start: Time.t,
    mutable n: int,
    mutable duration: Time.t,
    benchFunc: t => unit,
    mutable timerOn: bool,
    mutable startAllocs: float,
    mutable startBytes: float,
    mutable netAllocs: float,
    mutable netBytes: float,
  }
  
  let report = b => {
    print_endline(Format.sprintf("Benchmark: %s", b.name))
    print_endline(Format.sprintf("Nbr of iterations: %d", b.n))
    print_endline(
      Format.sprintf("Benchmark ran during: %fms", Time.print(b.duration)),
    )
    print_endline(
      Format.sprintf(
        "Avg time/op: %fms",
        Time.print(b.duration) /. float_of_int(b.n),
      ),
    )
    print_endline(
      Format.sprintf(
        "Allocs/op: %d",
        int_of_float(b.netAllocs /. float_of_int(b.n)),
      ),
    )
    print_endline(
      Format.sprintf("B/op: %d", int_of_float(b.netBytes /. float_of_int(b.n))),
    )
    
    print_newline()
    ()
  }
  
  let make = (~name, ~time=Time.second, ~f, ()) => {
    name: name,
    time: time,
    start: Time.zero,
    n: 0,
    benchFunc: f,
    duration: Time.zero,
    timerOn: false,
    startAllocs: 0.,
    startBytes: 0.,
    netAllocs: 0.,
    netBytes: 0.,
  }
  
  let mallocs = () => {
    let stats = Gc.quick_stat()
    stats.minor_words +. stats.major_words -. stats.promoted_words
  }
  
  let startTimer = b =>
    if !b.timerOn {
      let allocatedWords = mallocs()
      b.startAllocs = allocatedWords
      b.startBytes = allocatedWords *. 8.
      b.start = Time.now()
      b.timerOn = true
    }
  
  let stopTimer = b =>
    if b.timerOn {
      let allocatedWords = mallocs()
      let diff = Time.diff(b.start, Time.now())
      b.duration = Time.add(b.duration, diff)
      b.netAllocs = b.netAllocs +. allocatedWords -. b.startAllocs
      b.netBytes = b.netBytes +. allocatedWords *. 8. -. b.startBytes
      b.timerOn = false
    }
  
  let resetTimer = b => {
    if b.timerOn {
      let allocatedWords = mallocs()
      b.startAllocs = allocatedWords
      b.netAllocs = allocatedWords *. 8.
      b.start = Time.now()
    }
    b.netAllocs = 0.
    b.netBytes = 0.
  }
  
  let runIteration = (b, n) => {
    Gc.full_major()
    b.n = n
    resetTimer(b)
    startTimer(b)
    b.benchFunc(b)
    stopTimer(b)
  }
  
  let launch = b => {
    let d = b.time
    let n = ref(0)
    while b.duration < d && &n < 1000000000 {
      n := &n + 1
      runIteration(b, &n)
    }
  }
}

module Profile: {
  let record: (~name: string, unit => 'a) => 'a
  let print: unit => unit
} = {
  let state = Hashtbl.create(2)
  
  let record = (~name, f) => {
    let startTime = Time.now()
    let result = f()
    let endTime = Time.now()
    
    Hashtbl.add(state, name, Time.diff(startTime, endTime))
    result
  }
  
  let print = () => {
    let report = Hashtbl.fold(
      (k, v, acc) => {
        let line = Printf.sprintf("%s: %fms\n", k, Time.print(v))
        acc ++ line
      },
      state,
      "\n\n",
    )
    
    print_endline(report)
  }
}

module IO: {
  let readFile: string => string
  let readStdin: unit => string
  let writeFile: (string, string) => unit
} = {
  let chunkSize = 32768
  
  let readFile = filename => {
    let chan = open_in(filename)
    let buffer = Buffer.create(chunkSize)
    let chunk = Bytes.create(chunkSize)
    let rec loop = () => {
      let len = input(chan, chunk, 0, chunkSize)
      if len === 0 {
        close_in(chan)
        Buffer.contents(buffer)
      } else {
        Buffer.add_subbytes(buffer, chunk, 0, len)
        loop()
      }
    }
    
    loop()
  }
  
  let readStdin = () => {
    let buffer = Buffer.create(chunkSize)
    let chunk = Bytes.create(chunkSize)
    let rec loop = () => {
      let len = input(stdin, chunk, 0, chunkSize)
      if len === 0 {
        close_in(stdin)
        Buffer.contents(buffer)
      } else {
        Buffer.add_subbytes(buffer, chunk, 0, len)
        loop()
      }
    }
    
    loop()
  }
  
  let writeFile = (filename, txt) => {
    let chan = open_out_bin(filename)
    output_string(chan, txt)
    close_out(chan)
  }
}

module CharacterCodes = {
  let eof = -1
  
  let space = 0x0020
  let newline = 0x0A
  let lineFeed = 0x0A
  let carriageReturn = 0x0D
  let lineSeparator = 0x2028
  let paragraphSeparator = 0x2029
  
  let tab = 0x09
  
  let bang = 0x21
  let dot = 0x2E
  let colon = 0x3A
  let comma = 0x2C
  let backtick = 0x60
  let question = 0x3F
  let semicolon = 0x3B
  let underscore = 0x5F
  let singleQuote = 0x27
  let doubleQuote = 0x22
  let equal = 0x3D
  let bar = 0x7C
  let tilde = 0x7E
  let question = 0x3F
  let ampersand = 0x26
  let at = 0x40
  let dollar = 0x24
  let percent = 0x25
  
  let lparen = 0x28
  let rparen = 0x29
  let lbracket = 0x5B
  let rbracket = 0x5D
  let lbrace = 0x7B
  let rbrace = 0x7D
  
  let forwardslash = 0x2F
  let backslash = 0x5C
  
  let greaterThan = 0x3E
  let hash = 0x23
  let lessThan = 0x3C
  
  let minus = 0x2D
  let plus = 0x2B
  let asterisk = 0x2A
  
  let _0 = 0x30
  let _1 = 0x31
  let _2 = 0x32
  let _3 = 0x33
  let _4 = 0x34
  let _5 = 0x35
  let _6 = 0x36
  let _7 = 0x37
  let _8 = 0x38
  let _9 = 0x39
  
  module Lower = {
    let a = 0x61
    let b = 0x62
    let c = 0x63
    let d = 0x64
    let e = 0x65
    let f = 0x66
    let g = 0x67
    let h = 0x68
    let i = 0x69
    let j = 0x6A
    let k = 0x6B
    let l = 0x6C
    let m = 0x6D
    let n = 0x6E
    let o = 0x6F
    let p = 0x70
    let q = 0x71
    let r = 0x72
    let s = 0x73
    let t = 0x74
    let u = 0x75
    let v = 0x76
    let w = 0x77
    let x = 0x78
    let y = 0x79
    let z = 0x7A
  }
  
  module Upper = {
    let a = 0x41
    let b = 0x42
    let c = 0x43
    let d = 0x44
    let e = 0x45
    let f = 0x46
    let g = 0x47
    let h = 0x48
    let i = 0x49
    let j = 0x4A
    let k = 0x4B
    let l = 0x4C
    let m = 0x4D
    let b = 0x4E
    let o = 0x4F
    let p = 0x50
    let q = 0x51
    let r = 0x52
    let s = 0x53
    let t = 0x54
    let u = 0x55
    let v = 0x56
    let w = 0x57
    let x = 0x58
    let y = 0x59
    let z = 0x5a
  }
  
  let lower = ch => lor(32, ch)
  
  let isLetter = ch =>
    (Lower.a <= ch && ch <= Lower.z) || (Upper.a <= ch && ch <= Upper.z)
  
  let isUpperCase = ch => Upper.a <= ch && ch <= Upper.z
  
  let isDigit = ch => _0 <= ch && ch <= _9
  
  let isHex = ch =>
    (_0 <= ch && ch <= _9) || (Lower.a <= lower(ch) && lower(ch) <= Lower.f)
  
  let isLineBreak = ch =>
    ch === lineFeed ||
      ch === carriageReturn ||
      ch === lineSeparator || ch === paragraphSeparator
  
  let digitValue = ch =>
    if _0 <= ch && ch <= _9 {
      ch - 48
    } else if Lower.a <= lower(ch) && lower(ch) <= Lower.f {
      lower(ch) - Lower.a + 10
    } else {
      16
    }
}

module Comment: {
  type rec t
  
  let toString: t => string
  
  let loc: t => Location.t
  let txt: t => string
  let prevTokEndPos: t => Lexing.position
  
  let setPrevTokEndPos: (t, Lexing.position) => unit
  
  let isSingleLineComment: t => bool
  
  let makeSingleLineComment: (~loc: Location.t, string) => t
  let makeMultiLineComment: (~loc: Location.t, string) => t
} = {
  type rec style =
    | SingleLine
    | MultiLine
  
  let styleToString = s =>
    switch s {
    | SingleLine => "SingleLine"
    | MultiLine => "MultiLine"
    }
  
  type rec t = {
    txt: string,
    style: style,
    loc: Location.t,
    mutable prevTokEndPos: Lexing.position,
  }
  
  let loc = t => t.loc
  let txt = t => t.txt
  let prevTokEndPos = t => t.prevTokEndPos
  
  let setPrevTokEndPos = (t, pos) => t.prevTokEndPos = pos
  
  let isSingleLineComment = t =>
    switch t.style {
    | SingleLine => true
    | MultiLine => false
    }
  
  let toString = t =>
    Format.sprintf(
      "(txt: %s\nstyle: %s\nlines: %d-%d)",
      t.txt,
      styleToString(t.style),
      t.loc.loc_start.pos_lnum,
      t.loc.loc_end.pos_lnum,
    )
  
  let makeSingleLineComment = (~loc, txt) => {
    txt: txt,
    loc: loc,
    style: SingleLine,
    prevTokEndPos: Lexing.dummy_pos,
  }
  
  let makeMultiLineComment = (~loc, txt) => {
    txt: txt,
    loc: loc,
    style: MultiLine,
    prevTokEndPos: Lexing.dummy_pos,
  }
}

module Token = {
  type rec t =
    | Open
    | True
    | False
    | Character(char)
    | Int((string, option<char>))
    | Float((string, option<char>))
    | String(string)
    | Lident(string)
    | Uident(string)
    | As
    | Dot
    | DotDot
    | DotDotDot
    | Bang
    | Semicolon
    | Let
    | And
    | Rec
    | Underscore
    | SingleQuote
    | Equal
    | EqualEqual
    | EqualEqualEqual
    | Bar
    | Lparen
    | Rparen
    | Lbracket
    | Rbracket
    | Lbrace
    | Rbrace
    | Colon
    | Comma
    | Eof
    | Exception
    | Backslash
    | Forwardslash
    | ForwardslashDot
    | Asterisk
    | AsteriskDot
    | Exponentiation
    | Minus
    | MinusDot
    | Plus
    | PlusDot
    | PlusPlus
    | PlusEqual
    | GreaterThan
    | LessThan
    | LessThanSlash
    | Hash
    | HashEqual
    | HashHash
    | Assert
    | Lazy
    | Tilde
    | Question
    | If
    | Else
    | For
    | In
    | To
    | Downto
    | While
    | Switch
    | When
    | EqualGreater
    | MinusGreater
    | External
    | Typ
    | Private
    | Mutable
    | Constraint
    | Include
    | Module
    | Of
    | With
    | Land
    | Lor
    | Band
    | BangEqual
    | BangEqualEqual
    | LessEqual
    | GreaterEqual
    | ColonEqual
    | At
    | AtAt
    | Percent
    | PercentPercent
    | Comment(Comment.t)
    | List
    | TemplateTail(string)
    | TemplatePart(string)
    | Backtick
    | BarGreater
    | Try
    | Catch
    | Import
    | Export
  
  let precedence = x => switch x {
  | HashEqual | ColonEqual => 1
  | Lor => 2
  | Land => 3
  | ((((((((Equal | EqualEqual) | EqualEqualEqual) | LessThan) | GreaterThan)
    | BangEqual)
    | BangEqualEqual)
    | LessEqual)
    | GreaterEqual)
    | BarGreater =>
    4
  | (((Plus | PlusDot) | Minus) | MinusDot) | PlusPlus => 5
  | ((Asterisk | AsteriskDot) | Forwardslash) | ForwardslashDot => 6
  | Exponentiation => 7
  | (Hash | HashHash) | MinusGreater => 8
  | Dot => 9
  | _ => 0
  }
  
  let toString = x => switch x {
  | Open => "open"
  | True => "true"
  | False => "false"
  | Character(c) => "'" ++ Char.escaped(c) ++ "'"
  | String(s) => s
  | Lident(str) => str
  | Uident(str) => str
  | Dot => "."
  | DotDot => ".."
  | DotDotDot => "..."
  | Int(i, _) => "int " ++ i
  | Float(f, _) => "Float: " ++ f
  | Bang => "!"
  | Semicolon => ";"
  | Let => "let"
  | And => "and"
  | Rec => "rec"
  | Underscore => "_"
  | SingleQuote => "'"
  | Equal => "="
  | EqualEqual => "=="
  | EqualEqualEqual => "==="
  | Eof => "eof"
  | Bar => "|"
  | As => "as"
  | Lparen => "("
  | Rparen => ")"
  | Lbracket => "["
  | Rbracket => "]"
  | Lbrace => "{"
  | Rbrace => "}"
  | Colon => ":"
  | Comma => ","
  | Minus => "-"
  | MinusDot => "-."
  | Plus => "+"
  | PlusDot => "+."
  | PlusPlus => "++"
  | PlusEqual => "+="
  | Backslash => "\\"
  | Forwardslash => "/"
  | ForwardslashDot => "/."
  | Exception => "exception"
  | Hash => "#"
  | HashHash => "##"
  | HashEqual => "#="
  | GreaterThan => ">"
  | LessThan => "<"
  | LessThanSlash => "</"
  | Asterisk => "*"
  | AsteriskDot => "*."
  | Exponentiation => "**"
  | Assert => "assert"
  | Lazy => "lazy"
  | Tilde => "tilde"
  | Question => "?"
  | If => "if"
  | Else => "else"
  | For => "for"
  | In => "in"
  | To => "to"
  | Downto => "downto"
  | While => "while"
  | Switch => "switch"
  | When => "when"
  | EqualGreater => "=>"
  | MinusGreater => "->"
  | External => "external"
  | Typ => "type"
  | Private => "private"
  | Constraint => "constraint"
  | Mutable => "mutable"
  | Include => "include"
  | Module => "module"
  | Of => "of"
  | With => "with"
  | Lor => "||"
  | Band => "&"
  | Land => "&&"
  | BangEqual => "!="
  | BangEqualEqual => "!=="
  | GreaterEqual => ">="
  | LessEqual => "<="
  | ColonEqual => ":="
  | At => "@"
  | AtAt => "@@"
  | Percent => "%"
  | PercentPercent => "%%"
  | Comment(c) => "Comment(" ++ Comment.toString(c) ++ ")"
  | List => "list"
  | TemplatePart(text) => text ++ "${"
  | TemplateTail(text) => "TemplateTail(" ++ text ++ ")"
  | Backtick => "`"
  | BarGreater => "|>"
  | Try => "try"
  | Catch => "catch"
  | Import => "import"
  | Export => "export"
  }
  
  let keywordTable = {
    let keywords = [
      ("true", True),
      ("false", False),
      ("open", Open),
      ("let", Let),
      ("rec", Rec),
      ("and", And),
      ("as", As),
      ("exception", Exception),
      ("assert", Assert),
      ("lazy", Lazy),
      ("if", If),
      ("else", Else),
      ("for", For),
      ("in", In),
      ("to", To),
      ("downto", Downto),
      ("while", While),
      ("switch", Switch),
      ("when", When),
      ("external", External),
      ("type", Typ),
      ("private", Private),
      ("mutable", Mutable),
      ("constraint", Constraint),
      ("include", Include),
      ("module", Module),
      ("of", Of),
      ("list", List),
      ("with", With),
      ("try", Try),
      ("catch", Catch),
      ("import", Import),
      ("export", Export),
    ]
    let t = Hashtbl.create(50)
    Array.iter(((k, v)) => Hashtbl.add(t, k, v), keywords)
    t
  }
  
  let isKeyword = x => switch x {
  | (((((((((((((((((((((((((((((((((True | False) | Open) | Let) | Rec) | And)
    | As)
    | Exception)
    | Assert)
    | Lazy)
    | If)
    | Else)
    | For)
    | In)
    | To)
    | Downto)
    | While)
    | Switch)
    | When)
    | External)
    | Typ)
    | Private)
    | Mutable)
    | Constraint)
    | Include)
    | Module)
    | Of)
    | Land)
    | Lor)
    | List)
    | With)
    | Try)
    | Catch)
    | Import)
    | Export =>
    true
  | _ => false
  }
  
  let lookupKeyword = str =>
    try Hashtbl.find(keywordTable, str) catch {
    | Not_found =>
      if CharacterCodes.isUpperCase(int_of_char(String.get(str, 0))) {
        Uident(str)
      } else {
        Lident(str)
      }
    }
}

module Grammar = {
  type rec t =
    | OpenDescription
    | ModuleLongIdent
    | Ternary
    | Es6ArrowExpr
    | Jsx
    | JsxAttribute
    | JsxChild
    | ExprOperand
    | ExprUnary
    | ExprSetField
    | ExprBinaryAfterOp(Token.t)
    | ExprBlock
    | ExprCall
    | ExprList
    | ExprArrayAccess
    | ExprArrayMutation
    | ExprIf
    | IfCondition
    | IfBranch
    | ElseBranch
    | TypeExpression
    | External
    | PatternMatching
    | PatternMatchCase
    | LetBinding
    | PatternList
    | PatternOcamlList
    | PatternRecord
    
    | TypeDef
    | TypeConstrName
    | TypeParams
    | TypeParam
    | PackageConstraint
    
    | TypeRepresentation
    
    | RecordDecl
    | ConstructorDeclaration
    | ParameterList
    | StringFieldDeclarations
    | FieldDeclarations
    | TypExprList
    | FunctorArgs
    | ModExprList
    | TypeParameters
    | RecordRows
    | RecordRowsStringKey
    | ArgumentList
    | Signature
    | Structure
    | Attribute
    | TypeConstraint
    | Primitive
    | AtomicTypExpr
    | ListExpr
    | JsFfiImport
  
  let toString = x => switch x {
  | OpenDescription => "an open description"
  | ModuleLongIdent => "a module identifier"
  | Ternary => "a ternary expression"
  | Es6ArrowExpr => "an es6 arrow function"
  | Jsx => "a jsx expression"
  | JsxAttribute => "a jsx attribute"
  | ExprOperand => "a basic expression"
  | ExprUnary => "a unary expression"
  | ExprBinaryAfterOp(op) =>
    "an expression after the operator \"" ++ Token.toString(op) ++ "\""
  | ExprIf => "an if expression"
  | IfCondition => "the condition of an if expression"
  | IfBranch => "the true-branch of an if expression"
  | ElseBranch => "the else-branch of an if expression"
  | TypeExpression => "a type"
  | External => "an external"
  | PatternMatching => "the cases of a pattern match"
  | ExprBlock => "a block with expressions"
  | ExprSetField => "a record field mutation"
  | ExprCall => "a function application"
  | ExprArrayAccess => "an array access expression"
  | ExprArrayMutation => "an array mutation"
  | LetBinding => "a let binding"
  | TypeDef => "a type definition"
  | TypeParams => "type parameters"
  | TypeParam => "a type parameter"
  | TypeConstrName => "a type-constructor name"
  | TypeRepresentation => "a type representation"
  | RecordDecl => "a record declaration"
  | PatternMatchCase => "a pattern match case"
  | ConstructorDeclaration => "a constructor declaration"
  | ExprList => "multiple expressions"
  | PatternList => "multiple patterns"
  | PatternOcamlList => "a list pattern"
  | PatternRecord => "a record pattern"
  | ParameterList => "parameters"
  | StringFieldDeclarations => "string field declarations"
  | FieldDeclarations => "field declarations"
  | TypExprList => "list of types"
  | FunctorArgs => "functor arguments"
  | ModExprList => "list of module expressions"
  | TypeParameters => "list of type parameters"
  | RecordRows => "rows of a record"
  | RecordRowsStringKey => "rows of a record with string keys"
  | ArgumentList => "arguments"
  | Signature => "signature"
  | Structure => "structure"
  | Attribute => "an attribute"
  | TypeConstraint => "constraints on a type"
  | Primitive => "an external primitive"
  | AtomicTypExpr => "a type"
  | ListExpr => "an ocaml list expr"
  | PackageConstraint => "a package constraint"
  | JsFfiImport => "js ffi import"
  | JsxChild => "jsx child"
  }
  
  let isSignatureItemStart = x => switch x {
  | ((((((((Token.At | Let) | Typ) | External) | Exception) | Open) | Include)
    | Module)
    | AtAt)
    | PercentPercent =>
    true
  | _ => false
  }
  
  let isAtomicPatternStart = x => switch x {
  | (((((((((((Token.Int(_) | String(_)) | Character(_)) | Lparen) | Lbracket)
    | Lbrace)
    | Underscore)
    | Lident(_))
    | Uident(_))
    | List)
    | Exception)
    | Lazy)
    | Percent =>
    true
  | _ => false
  }
  
  let isAtomicExprStart = x => switch x {
  | ((((((((((((((Token.True | False) | Int(_)) | String(_)) | Float(_))
    | Character(_))
    | Backtick)
    | Uident(_))
    | Lident(_))
    | Lparen)
    | List)
    | Lbracket)
    | Lbrace)
    | LessThan)
    | Module)
    | Percent =>
    true
  | _ => false
  }
  
  let isAtomicTypExprStart = x => switch x {
  | ((((((Token.SingleQuote | Underscore) | Lparen) | Lbrace) | Uident(_))
    | Lident(_))
    | List)
    | Percent =>
    true
  | _ => false
  }
  
  let isExprStart = x => switch x {
  | ((((((((((((((((((((((((((((Token.True | False) | Int(_)) | String(_))
    | Float(_))
    | Character(_))
    | Backtick)
    | Uident(_))
    | Lident(_))
    | Lparen)
    | List)
    | Module)
    | Lbracket)
    | Lbrace)
    | LessThan)
    | Minus)
    | MinusDot)
    | Plus)
    | PlusDot)
    | Bang)
    | Band)
    | Percent)
    | At)
    | If)
    | Switch)
    | While)
    | For)
    | Assert)
    | Lazy)
    | Try =>
    true
  | _ => false
  }
  
  let isJsxAttributeStart = x => switch x {
  | Token.Lident(_) | Question => true
  | _ => false
  }
  
  let isStructureItemStart = x => switch x {
  | ((((((((((Token.Open | Let) | Typ) | External) | Import) | Export)
    | Exception)
    | Include)
    | Module)
    | AtAt)
    | PercentPercent)
    | At =>
    true
  | t when isExprStart(t) => true
  | _ => false
  }
  
  let isPatternStart = x => switch x {
  | (((((((((((((((Token.Int(_) | String(_)) | Character(_)) | True) | False)
    | Lparen)
    | Lbracket)
    | Lbrace)
    | List)
    | Underscore)
    | Lident(_))
    | Uident(_))
    | Exception)
    | Lazy)
    | Percent)
    | Module)
    | At =>
    true
  | _ => false
  }
  
  let isParameterStart = x => switch x {
  | (Token.Typ | Tilde) | Dot => true
  | token when isPatternStart(token) => true
  | _ => false
  }
  
  let isStringFieldDeclStart = x => switch x {
  | Token.String(_) | At => true
  | _ => false
  }
  
  let isFieldDeclStart = x => switch x {
  | (Token.At | Mutable) | Lident(_) => true
  
  | Uident(_) => true
  | t when Token.isKeyword(t) => true
  | _ => false
  }
  
  let isRecordDeclStart = x => switch x {
  | (Token.At | Mutable) | Lident(_) => true
  | _ => false
  }
  
  let isTypExprStart = x => switch x {
  | ((((((((Token.At | SingleQuote) | Underscore) | Lparen) | Uident(_))
    | Lident(_))
    | List)
    | Module)
    | Percent)
    | Lbrace =>
    true
  | _ => false
  }
  
  let isTypeParameterStart = x => switch x {
  | Token.Tilde | Dot => true
  | token when isTypExprStart(token) => true
  | _ => false
  }
  
  let isTypeParamStart = x => switch x {
  | ((Token.Plus | Minus) | SingleQuote) | Underscore => true
  | _ => false
  }
  
  let isFunctorArgStart = x => switch x {
  | ((((Token.At | Uident(_)) | Underscore) | Percent) | Lbrace) | Lparen =>
    true
  | _ => false
  }
  
  let isModExprStart = x => switch x {
  | (((Token.At | Percent) | Uident(_)) | Lbrace) | Lparen => true
  | _ => false
  }
  
  let isRecordRowStart = x => switch x {
  | Token.DotDotDot => true
  | Token.Uident(_) | Lident(_) => true
  
  | t when Token.isKeyword(t) => true
  | _ => false
  }
  
  let isRecordRowStringKeyStart = x => switch x {
  | Token.String(_) => true
  | _ => false
  }
  
  let isArgumentStart = x => switch x {
  | (Token.Tilde | Dot) | Underscore => true
  | t when isExprStart(t) => true
  | _ => false
  }
  
  let isPatternMatchStart = x => switch x {
  | Token.Bar => true
  | t when isPatternStart(t) => true
  | _ => false
  }
  
  let isPatternOcamlListStart = x => switch x {
  | Token.DotDotDot => true
  | t when isPatternStart(t) => true
  | _ => false
  }
  
  let isPatternRecordItemStart = x => switch x {
  | ((Token.DotDotDot | Uident(_)) | Lident(_)) | Underscore => true
  | _ => false
  }
  
  let isAttributeStart = x => switch x {
  | Token.At => true
  | _ => false
  }
  
  let isJsFfiImportStart = x => switch x {
  | Token.Lident(_) | At => true
  | _ => false
  }
  
  let isJsxChildStart = isAtomicExprStart
  
  let isBlockExprStart = x => switch x {
  | ((((((((((((((((((((((((((((((((Token.At | Percent) | Minus) | MinusDot)
    | Plus)
    | PlusDot)
    | Bang)
    | Band)
    | True)
    | False)
    | Int(_))
    | String(_))
    | Character(_))
    | Lident(_))
    | Uident(_))
    | Lparen)
    | List)
    | Lbracket)
    | Lbrace)
    | Forwardslash)
    | Assert)
    | Lazy)
    | If)
    | For)
    | While)
    | Switch)
    | Open)
    | Module)
    | Exception)
    | Let)
    | LessThan)
    | Backtick)
    | Try)
    | Underscore =>
    true
  | _ => false
  }
  
  let isListElement = (grammar, token) =>
    switch grammar {
    | ExprList => token == Token.DotDotDot || isExprStart(token)
    | ListExpr => token == DotDotDot || isExprStart(token)
    | PatternList => token == DotDotDot || isPatternStart(token)
    | ParameterList => isParameterStart(token)
    | StringFieldDeclarations => isStringFieldDeclStart(token)
    | FieldDeclarations => isFieldDeclStart(token)
    | RecordDecl => isRecordDeclStart(token)
    | TypExprList => isTypExprStart(token) || token == Token.LessThan
    | TypeParams => isTypeParamStart(token)
    | FunctorArgs => isFunctorArgStart(token)
    | ModExprList => isModExprStart(token)
    | TypeParameters => isTypeParameterStart(token)
    | RecordRows => isRecordRowStart(token)
    | RecordRowsStringKey => isRecordRowStringKeyStart(token)
    | ArgumentList => isArgumentStart(token)
    | Signature => isSignatureItemStart(token)
    | Structure => isStructureItemStart(token)
    | PatternMatching => isPatternMatchStart(token)
    | PatternOcamlList => isPatternOcamlListStart(token)
    | PatternRecord => isPatternRecordItemStart(token)
    | Attribute => isAttributeStart(token)
    | TypeConstraint => token == Constraint
    | PackageConstraint => token == And
    | ConstructorDeclaration => token == Bar
    | Primitive =>
      switch token {
      | Token.String(_) => true
      | _ => false
      }
    | JsxAttribute => isJsxAttributeStart(token)
    | JsFfiImport => isJsFfiImportStart(token)
    | _ => false
    }
  
  let isListTerminator = (grammar, token) =>
    token == Token.Eof ||
      switch grammar {
      | ExprList =>
        token == Token.Rparen || token == Forwardslash || token == Rbracket
      | ListExpr => token == Token.Rparen
      | ArgumentList => token == Token.Rparen
      | TypExprList =>
        token == Rparen ||
          token == Forwardslash ||
          token == GreaterThan || token == Equal
      | ModExprList => token == Rparen
      | (PatternList | PatternOcamlList) | PatternRecord =>
        token == Forwardslash ||
          token == Rbracket ||
          token == Rparen ||
            token == EqualGreater ||
            token == In || token == Equal
      | ExprBlock => token == Rbrace
      | Structure => token == Rbrace
      | TypeParams => token == Rparen
      | ParameterList => token == EqualGreater || token == Lbrace
      | Attribute => token != At
      | TypeConstraint => token != Constraint
      | PackageConstraint => token != And
      | ConstructorDeclaration => token != Bar
      | Primitive => isStructureItemStart(token) || token == Semicolon
      | JsxAttribute => token == Forwardslash || token == GreaterThan
      | JsFfiImport => token == Rbrace
      | _ => false
      }
  
  let isPartOfList = (grammar, token) =>
    isListElement(grammar, token) || isListTerminator(grammar, token)
}

module Reporting = {
  module TerminalDoc = {
    type rec break =
      | IfNeed
      | Never
      | Always
    
    type rec document =
      | Nil
      | Group(break, document)
      | Text(string)
      | Indent(int, document)
      | Append(document, document)
    
    let group = (~break=IfNeed, doc) => Group(break, doc)
    let text = txt => Text(txt)
    let indent = (i, d) => Indent(i, d)
    let append = (d1, d2) => Append(d1, d2)
    let nil = Nil
    
    type rec stack =
      | Empty
      | Cons(document, stack)
    
    let push = (stack, doc) => Cons(doc, stack)
    
    type rec mode =
      | Flat
      | Break
    
    let rec fits = (w, stack) =>
      switch stack {
      | _ when w < 0 => false
      | Empty => true
      | Cons(doc, stack) =>
        switch doc {
        | Nil => fits(w, stack)
        | Text(txt) => fits(w - String.length(txt), stack)
        | Append(d1, d2) =>
          let stack = {
            let stack = push(stack, d1)
            push(stack, d2)
          }
          
          fits(w, stack)
        | Group(_, d) => fits(w, push(stack, d))
        | Indent(i, d) => fits(w - i, push(stack, d))
        }
      }
    
    let toString = (~width, doc: document) => {
      let buffer = Buffer.create(100)
      let rec loop = (stack, mode, offset) =>
        switch stack {
        | Empty => ()
        | Cons(doc, rest) =>
          switch doc {
          | Nil => loop(rest, mode, offset)
          | Text(txt) =>
            Buffer.add_string(buffer, txt)
            loop(rest, mode, offset + String.length(txt))
          | Indent(i, doc) =>
            let indentation = String.make(i, ' ')
            Buffer.add_string(buffer, indentation)
            loop(push(rest, doc), mode, offset + i)
          | Append(doc1, doc2) =>
            let rest = push(rest, doc2)
            let rest = push(
              rest,
              switch mode == Flat {
              | true => Nil
              | false => text("\n")
              },
            )
            
            let rest = push(rest, doc1)
            loop(rest, mode, offset)
          | Group(break, doc) =>
            let rest = push(rest, doc)
            switch break {
            | Always => loop(rest, Break, offset)
            | Never => loop(rest, Flat, offset)
            | IfNeed =>
              if fits(width - offset, rest) {
                loop(rest, Flat, offset)
              } else {
                loop(rest, Break, offset)
              }
            }
          }
        }
      
      loop(push(Empty, doc), Flat, 0)
      Buffer.contents(buffer)
    }
  }
  
  type rec color =
    | NoColor
    | Red
  
  type rec style = {
    underline: bool,
    color: color,
  }
  
  let emptyStyle = {
    underline: false,
    color: NoColor,
  }
  
  let highlight = (~from, ~len, txt) =>
    if from < 0 || String.length(txt) === 0 || from >= String.length(txt) {
      txt
    } else {
      let before = String.sub(txt, 0, from)
      let content = "[31m" ++ String.sub(txt, from, len) ++ "[0m"
      
      let after = String.sub(txt, from + len, String.length(txt) - from + len)
      before ++ content ++ after
    }
  
  let underline = (~from, ~len, txt) => {
    open TerminalDoc
    let indent = String.make(from, ' ')
    let underline = String.make(len, '^')
    let line = highlight(~from=0, ~len, underline)
    group(~break=Always, append(text(txt), text(indent ++ line)))
  }
  
  let applyStyle = (~from, ~len, style, txt) => {
    open TerminalDoc
    let colorizedText = if style.color != NoColor {
      highlight(~from, ~len, txt)
    } else {
      txt
    }
    
    underline(~from, ~len, colorizedText)
  }
  
  let parseContext = stack =>
    switch stack {
    | list[(Grammar.ExprOperand, _), ...cs] =>
      switch cs {
      | list[(ExprBinaryAfterOp(_) as c, _), ...cs] => Grammar.toString(c)
      | _ => "a basic expression"
      }
    | list[(c, _), ...cs] => Grammar.toString(c)
    | list[] => "your code"
    }
  
  let rec drop = (n, l) =>
    if n === 1 {
      l
    } else {
      drop(
        n - 1,
        switch l {
        | list[x, ...xs] => xs
        | _ => l
        },
      )
    }
  
  let rec take = (n, l) =>
    switch l {
    | _ when n === 0 => list[]
    | list[] => list[]
    | list[x, ...xs] => list[x, ...take(n - 1, xs)]
    }
  
  let renderCodeContext = (~missing, src: string, startPos, endPos) => {
    open Lexing
    let startCol = startPos.pos_cnum - startPos.pos_bol
    let endCol = endPos.pos_cnum - startPos.pos_cnum + startCol
    let startLine = max(1, startPos.pos_lnum - 2)
    let lines = String.split_on_char('\n', src)
    let endLine = {
      let len = List.length(lines)
      min(len, startPos.pos_lnum + 3)
    }
    
    let lines =
      lines |> drop(startLine) |> take(endLine - startLine) |> Array.of_list
    
    let renderLine = (x, ix) => {
      let x = if ix == startPos.pos_lnum {
        switch missing {
        | Some(len) => x ++ String.make(10, ' ')
        | None => x
        }
      } else {
        x
      }
      
      open TerminalDoc
      let rowNr = {
        let txt = string_of_int(ix)
        let len = String.length(txt)
        if ix == startPos.pos_lnum {
          highlight(~from=0, ~len, txt)
        } else {
          txt
        }
      }
      
      let len = {
        let len = if endCol >= 0 {
          endCol - startCol
        } else {
          1
        }
        
        if startCol + len > String.length(x) {
          String.length(x) - startCol - 1
        } else {
          len
        }
      }
      
      let line = if ix == startPos.pos_lnum {
        switch missing {
        | Some(len) =>
          underline(
            ~from=startCol +
            String.length(
              String.length(string_of_int(ix)) |> string_of_int,
            ) + 5,
            ~len,
            x,
          )
        | None =>
          let len = if startCol + len > String.length(x) {
            String.length(x) - startCol
          } else {
            len
          }
          
          text(highlight(~from=startCol, ~len, x))
        }
      } else {
        text(x)
      }
      
      group(
        ~break=Never,
        append(append(text(rowNr), text(" │")), indent(2, line)),
      )
    }
    
    let reportDoc = ref(TerminalDoc.nil)
    
    let linesLen = Array.length(lines)
    for i in 0 to linesLen - 1 {
      let line = lines[i]
      reportDoc := {
        open TerminalDoc
        let ix = startLine + i
        group(~break=Always, append(&reportDoc, renderLine(line, ix)))
      }
    }
    
    TerminalDoc.toString(~width=80, &reportDoc)
  }
  
  type rec problem =
    | Unexpected(Token.t)
    | Expected((Token.t, Lexing.position, option<Grammar.t>))
    | Message(string)
    | Uident
    | Lident
    | Unbalanced(Token.t)
  
  type rec parseError = (Lexing.position, problem)
}

module Diagnostics: {
  type rec t
  type rec category
  
  let unexpected: (Token.t, list<(Grammar.t, Lexing.position)>) => category
  let expected: (~grammar: Grammar.t=?, Lexing.position, Token.t) => category
  let uident: Token.t => category
  let lident: Token.t => category
  let unclosedString: category
  let unclosedTemplate: category
  let unclosedComment: category
  let unknownUchar: int => category
  let message: string => category
  let unbalanced: Token.t => category
  
  let make: (
    ~filename: string,
    ~startPos: Lexing.position,
    ~endPos: Lexing.position,
    category,
  ) => t
  
  let makeReport: (list<t>, string) => string
} = {
  type rec category =
    | Unexpected((Token.t, list<(Grammar.t, Lexing.position)>))
    | Expected((option<Grammar.t>, Lexing.position, Token.t))
    | Message(string)
    | Uident(Token.t)
    | Lident(Token.t)
    | UnclosedString
    | UnclosedTemplate
    | UnclosedComment
    | UnknownUchar(int)
    | Unbalanced(Token.t)
  
  let stringOfCategory = x => switch x {
  | Unexpected(_) => "unexpected"
  | Expected(_) => "expected"
  | Message(txt) => txt
  | Uident(_) => "uident"
  | Lident(_) => "lident"
  | UnclosedString => "unclosed string"
  | UnclosedTemplate => "unclosed template"
  | UnclosedComment => "unclosed comment"
  | Unbalanced(_) => "unbalanced"
  | UnknownUchar(_) => "unknown rune"
  }
  
  type rec t = {
    filename: string,
    startPos: Lexing.position,
    endPos: Lexing.position,
    category: category,
  }
  
  let defaultUnexpected = token =>
    "I'm not sure what to parse here when looking at \"" ++
    Token.toString(token) ++
    "\"."
  
  let toString = (t, src) => {
    open Lexing
    let startchar = t.startPos.pos_cnum - t.startPos.pos_bol
    let endchar = t.endPos.pos_cnum - t.startPos.pos_cnum + startchar
    let locationInfo = Printf.sprintf(
      "File \"%s\", line %d, characters %d-%d:",
      t.filename,
      t.startPos.pos_lnum,
      startchar,
      endchar,
    )
    
    let code = {
      let missing = switch t.category {
      | Expected(_, _, t) => Some(String.length(Token.toString(t)))
      | _ => None
      }
      
      Reporting.renderCodeContext(~missing, src, t.startPos, t.endPos)
    }
    
    let explanation = switch t.category {
    | Uident(currentToken) =>
      switch currentToken {
      | Lident(lident) =>
        let guess = String.capitalize_ascii(lident)
        "Did you mean `" ++ guess ++ "` instead of `" ++ lident ++ "`?"
      | t when Token.isKeyword(t) =>
        let token = Token.toString(t)
        "`" ++
        token ++
        "` is a reserved keyword. Try `" ++
        token ++
        "_` or `_" ++ token ++ "` instead"
      | _ =>
        "At this point, I'm looking for an uppercased identifier like `Belt` or `Array`"
      }
    | Lident(currentToken) =>
      switch currentToken {
      | Uident(uident) =>
        let guess = String.uncapitalize_ascii(uident)
        "Did you mean `" ++ guess ++ "` instead of `" ++ uident ++ "`?"
      | t when Token.isKeyword(t) =>
        let token = Token.toString(t)
        "`" ++
        token ++
        "` is a reserved keyword. Try `" ++
        token ++
        "_` or `_" ++ token ++ "` instead"
      | _ => "I'm expecting an lowercased identifier like `name` or `age`"
      }
    | Message(txt) => txt
    | UnclosedString => "This string is missing a double quote at the end"
    | UnclosedTemplate =>
      "Did you forget to close this template expression with a backtick?"
    | UnclosedComment => "This comment seems to be missing a closing `*/`"
    | UnknownUchar(uchar) =>
      switch uchar {
      | 94 =>
        "Hmm, not sure what I should do here with this character.\nIf you're trying to deref an expression, use `foo.contents` instead."
      | _ => "Hmm, I have no idea what this character means…"
      }
    | Unbalanced(t) =>
      "Closing \"" ++ Token.toString(t) ++ "\" seems to be missing."
    | Expected(context, _, t) =>
      let hint = switch context {
      | Some(grammar) => "It signals the start of " ++ Grammar.toString(grammar)
      | None => ""
      }
      
      "Did you forget a `" ++ Token.toString(t) ++ "` here? " ++ hint
    | Unexpected(t, breadcrumbs) =>
      let name = Token.toString(t)
      switch breadcrumbs {
      | list[(AtomicTypExpr, _), ...breadcrumbs] =>
        switch (breadcrumbs, t) {
        | (
            list[(StringFieldDeclarations, _), ..._],
            (((String(_) | At) | Rbrace) | Comma) | Eof,
          ) =>
          "I'm missing a type here"
        | (_, t) when Grammar.isStructureItemStart(t) || t == Eof =>
          "Missing a type here"
        | _ => defaultUnexpected(t)
        }
      | list[(ExprOperand, _), ...breadcrumbs] =>
        switch (breadcrumbs, t) {
        | (list[(ExprBlock, _), ..._], Rbrace) =>
          "It seems that this expression block is empty"
        | (list[(ExprBlock, _), ..._], Bar) =>
          "Looks like there might be an expression missing here"
        | (list[(ExprSetField, _), ..._], _) =>
          "It seems that this record field mutation misses an expression"
        | (list[(ExprArrayMutation, _), ..._], _) =>
          "Seems that an expression is missing, with what do I mutate the array?"
        | (list[(ExprBinaryAfterOp(_) | ExprUnary, _), ..._], _) =>
          "Did you forget to write an expression here?"
        | (list[(Grammar.LetBinding, _), ..._], _) =>
          "This let-binding misses an expression"
        | (list[_, ..._], Rbracket) => "Missing expression"
        | _ =>
          "I'm not sure what to parse here when looking at \"" ++ name ++ "\"."
        }
      | list[(TypeParam, _), ..._] =>
        switch t {
        | Lident(ident) =>
          "Did you mean '" ++ ident ++ "? A Type parameter starts with a quote."
        | _ =>
          "I'm not sure what to parse here when looking at \"" ++ name ++ "\"."
        }
      | _ =>
        if Token.isKeyword(t) {
          name ++ " is a reserved keyword, Try `" ++ name ++ "_` instead"
        } else {
          "I'm not sure what to parse here when looking at \"" ++ name ++ "\"."
        }
      }
    }
    
    Printf.sprintf("%s\n\n%s\n\n%s\n\n", locationInfo, code, explanation)
  }
  
  let make = (~filename, ~startPos, ~endPos, category) => {
    filename: filename,
    startPos: startPos,
    endPos: endPos,
    category: category,
  }
  
  let makeReport = (diagnostics, src) =>
    List.fold_left(
      (report, diagnostic) => report ++ toString(diagnostic, src) ++ "\n",
      "\n",
      List.rev(diagnostics),
    )
  
  let print = ({category}) => prerr_endline(stringOfCategory(category))
  
  let unexpected = (token, context) => Unexpected(token, context)
  
  let expected = (~grammar=?, pos, token) => Expected(grammar, pos, token)
  
  let uident = currentToken => Uident(currentToken)
  let lident = currentToken => Lident(currentToken)
  let unclosedString = UnclosedString
  let unclosedComment = UnclosedComment
  let unclosedTemplate = UnclosedTemplate
  let unknownUchar = code => UnknownUchar(code)
  let message = txt => Message(txt)
  let unbalanced = token => Unbalanced(token)
}

module Scanner = {
  type rec mode = Template | Tuple | Jsx | Diamond
  
  let string_of_mode = x => switch x {
  | Template => "template"
  | Tuple => "tuple"
  | Jsx => "jsx"
  | Diamond => "diamond"
  }
  
  type rec t = {
    filename: string,
    src: bytes,
    mutable err: (
      ~startPos: Lexing.position,
      ~endPos: Lexing.position,
      Diagnostics.category,
    ) => unit,
    mutable ch: int,
    mutable offset: int,
    mutable rdOffset: int,
    mutable lineOffset: int,
    mutable lnum: int,
    mutable mode: list<mode>,
  }
  
  let setDiamondMode = scanner => scanner.mode = list[Diamond, ...scanner.mode]
  
  let setTemplateMode = scanner =>
    scanner.mode = list[Template, ...scanner.mode]
  
  let setTupleMode = scanner => scanner.mode = list[Tuple, ...scanner.mode]
  
  let setJsxMode = scanner => scanner.mode = list[Jsx, ...scanner.mode]
  
  let popMode = (scanner, mode) =>
    switch scanner.mode {
    | list[m, ...ms] when m == mode => scanner.mode = ms
    | _ => ()
    }
  
  let inTupleMode = scanner =>
    switch scanner.mode {
    | list[Tuple, ..._] => true
    | _ => false
    }
  
  let inDiamondMode = scanner =>
    switch scanner.mode {
    | list[Diamond, ..._] => true
    | _ => false
    }
  
  let inJsxMode = scanner =>
    switch scanner.mode {
    | list[Jsx, ..._] => true
    | _ => false
    }
  
  let inTemplateMode = scanner =>
    switch scanner.mode {
    | list[Template, ..._] => true
    | _ => false
    }
  
  let position = scanner => {
    open Lexing
    {
      pos_fname: scanner.filename,
      pos_lnum: scanner.lnum,
      pos_bol: scanner.lineOffset,
      pos_cnum: scanner.offset,
    }
  }
  
  let printPos = p => {
    print_endline("cnum: " ++ string_of_int(p.Lexing.pos_cnum))
    print_endline("lnum: " ++ string_of_int(p.Lexing.pos_lnum))
    print_endline("bol: " ++ string_of_int(p.Lexing.pos_bol))
    print_endline("beginning of line: " ++ string_of_int(p.Lexing.pos_bol))
    print_endline("-------------------")
  }
  
  let next = scanner =>
    if scanner.rdOffset < Bytes.length(scanner.src) {
      scanner.offset = scanner.rdOffset
      let ch = Bytes.get(scanner.src, scanner.rdOffset)
      scanner.rdOffset = scanner.rdOffset + 1
      scanner.ch = int_of_char(ch)
    } else {
      scanner.offset = Bytes.length(scanner.src)
      scanner.ch = -1
    }
  
  let peek = scanner =>
    if scanner.rdOffset < Bytes.length(scanner.src) {
      int_of_char(Bytes.unsafe_get(scanner.src, scanner.rdOffset))
    } else {
      -1
    }
  
  let make = (b, filename) => {
    let scanner = {
      filename: filename,
      src: b,
      err: (~startPos as _, ~endPos as _, _) => (),
      ch: CharacterCodes.space,
      offset: 0,
      rdOffset: 0,
      lineOffset: 0,
      lnum: 1,
      mode: list[],
    }
    next(scanner)
    scanner
  }
  
  let lookahead = (scanner, callback) => {
    let err = scanner.err
    let ch = scanner.ch
    let offset = scanner.offset
    let rdOffset = scanner.rdOffset
    let lineOffset = scanner.lineOffset
    let lnum = scanner.lnum
    let mode = scanner.mode
    let res = callback(scanner)
    scanner.err = err
    scanner.ch = ch
    scanner.offset = offset
    scanner.rdOffset = rdOffset
    scanner.lineOffset = lineOffset
    scanner.lnum = lnum
    scanner.mode = mode
    res
  }
  
  let skipWhitespace = scanner => {
    let rec scan = () =>
      if (
        scanner.ch === CharacterCodes.space || scanner.ch === CharacterCodes.tab
      ) {
        next(scanner)
        scan()
      } else if CharacterCodes.isLineBreak(scanner.ch) {
        scanner.lineOffset = scanner.offset + 1
        scanner.lnum = scanner.lnum + 1
        next(scanner)
        scan()
      } else {
        ()
      }
    
    scan()
  }
  
  let scanIdentifier = scanner => {
    let startOff = scanner.offset
    while (
      CharacterCodes.isLetter(scanner.ch) ||
      CharacterCodes.isDigit(scanner.ch) ||
      CharacterCodes.underscore === scanner.ch
    ) {
      next(scanner)
    }
    let str = Bytes.sub_string(scanner.src, startOff, scanner.offset - startOff)
    Token.lookupKeyword(str)
  }
  
  let scanDigits = (scanner, ~base) =>
    if base <= 10 {
      while (
        CharacterCodes.isDigit(scanner.ch) ||
        scanner.ch === CharacterCodes.underscore
      ) {
        next(scanner)
      }
    } else {
      while (
        CharacterCodes.isHex(scanner.ch) ||
        scanner.ch === CharacterCodes.underscore
      ) {
        next(scanner)
      }
    }
  
  let scanNumber = scanner => {
    let startOff = scanner.offset
    
    let (base, prefix) = if scanner.ch !== CharacterCodes.dot {
      if scanner.ch === CharacterCodes._0 {
        next(scanner)
        let ch = CharacterCodes.lower(scanner.ch)
        if ch === CharacterCodes.Lower.x {
          next(scanner)
          (16, 'x')
        } else if ch === CharacterCodes.Lower.o {
          next(scanner)
          (8, 'o')
        } else if ch === CharacterCodes.Lower.b {
          next(scanner)
          (2, 'b')
        } else {
          (8, '0')
        }
      } else {
        (10, ' ')
      }
    } else {
      (10, ' ')
    }
    
    scanDigits(scanner, ~base)
    
    let isFloat = if CharacterCodes.dot === scanner.ch {
      next(scanner)
      scanDigits(scanner, ~base)
      true
    } else {
      false
    }
    
    let isFloat = if {
      let exp = CharacterCodes.lower(scanner.ch)
      exp === CharacterCodes.Lower.e || exp === CharacterCodes.Lower.p
    } {
      next(scanner)
      if (
        scanner.ch === CharacterCodes.plus ||
          scanner.ch === CharacterCodes.minus
      ) {
        next(scanner)
      }
      scanDigits(scanner, ~base)
      true
    } else {
      isFloat
    }
    
    let literal = Bytes.sub_string(
      scanner.src,
      startOff,
      scanner.offset - startOff,
    )
    
    let suffix = if (
      (scanner.ch >= CharacterCodes.Lower.g &&
        scanner.ch <= CharacterCodes.Lower.z) ||
        (scanner.ch >= CharacterCodes.Upper.g &&
          scanner.ch <= CharacterCodes.Upper.z)
    ) {
      let ch = scanner.ch
      next(scanner)
      Some(Char.unsafe_chr(ch))
    } else {
      None
    }
    
    if isFloat {
      Token.Float(literal, suffix)
    } else {
      Token.Int(literal, suffix)
    }
  }
  
  let scanString = scanner => {
    let buffer = Buffer.create(256)
    
    let startPos = position(scanner)
    let rec scan = () =>
      if scanner.ch === CharacterCodes.eof {
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.unclosedString)
      } else if scanner.ch === CharacterCodes.doubleQuote {
        next(scanner)
      } else if scanner.ch === CharacterCodes.backslash {
        next(scanner)
        let char_for_backslash = x => switch x {
        | 98 => '\b'
        | 110 => '\n'
        | 114 => '\r'
        | 116 => '\t'
        | c => Char.chr(c)
        }
        
        Buffer.add_char(buffer, char_for_backslash(scanner.ch))
        next(scanner)
        scan()
      } else if CharacterCodes.isLineBreak(scanner.ch) {
        scanner.lineOffset = scanner.offset + 1
        scanner.lnum = scanner.lnum + 1
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.unclosedString)
        next(scanner)
      } else {
        Buffer.add_char(buffer, Char.chr(scanner.ch))
        next(scanner)
        scan()
      }
    
    scan()
    Token.String(Buffer.contents(buffer))
  }
  
  let convertNumber = (scanner, ~n, ~base, ~max) => {
    let x = ref(0)
    for _ in n downto 1 {
      let d = CharacterCodes.digitValue(scanner.ch)
      x := &x * base + d
      next(scanner)
    }
    &x
  }
  
  let scanEscape = scanner => {
    let c = switch scanner.ch {
    | 98 =>
      next(scanner)
      '\b'
    | 110 =>
      next(scanner)
      '\n'
    | 114 =>
      next(scanner)
      '\r'
    | 116 =>
      next(scanner)
      '\t'
    | ch when CharacterCodes.isDigit(ch) =>
      let x = convertNumber(scanner, ~n=3, ~base=10, ~max=255)
      Char.chr(x)
    | ch when ch === CharacterCodes.Lower.x =>
      next(scanner)
      let x = convertNumber(scanner, ~n=2, ~base=16, ~max=255)
      Char.chr(x)
    | ch when ch === CharacterCodes.Lower.o =>
      next(scanner)
      let x = convertNumber(scanner, ~n=3, ~base=8, ~max=255)
      Char.chr(x)
    | ch =>
      next(scanner)
      Char.chr(ch)
    }
    
    next(scanner)
    Token.Character(c)
  }
  
  let scanSingleLineComment = scanner => {
    let startOff = scanner.offset
    let startPos = position(scanner)
    while (
      !CharacterCodes.isLineBreak(scanner.ch) &&
      scanner.rdOffset < Bytes.length(scanner.src)
    ) {
      next(scanner)
    }
    let endPos = position(scanner)
    Token.Comment(
      Comment.makeSingleLineComment(
        ~loc={
          open Location
          {loc_start: startPos, loc_end: endPos, loc_ghost: false}
        },
        Bytes.sub_string(scanner.src, startOff, scanner.offset - startOff),
      ),
    )
  }
  
  let scanMultiLineComment = scanner => {
    let startOff = scanner.offset
    let startPos = position(scanner)
    let rec scan = () =>
      if (
        scanner.ch === CharacterCodes.asterisk &&
          peek(scanner) === CharacterCodes.forwardslash
      ) {
        next(scanner)
        next(scanner)
      } else if scanner.ch === CharacterCodes.eof {
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.unclosedComment)
      } else {
        if CharacterCodes.isLineBreak(scanner.ch) {
          scanner.lineOffset = scanner.offset + 1
          scanner.lnum = scanner.lnum + 1
        }
        next(scanner)
        scan()
      }
    
    scan()
    Token.Comment(
      Comment.makeMultiLineComment(
        ~loc={
          open Location
          {loc_start: startPos, loc_end: position(scanner), loc_ghost: false}
        },
        Bytes.sub_string(scanner.src, startOff, scanner.offset - 2 - startOff),
      ),
    )
  }
  
  let scanTemplate = scanner => {
    let startOff = scanner.offset
    let startPos = position(scanner)
    
    let rec scan = () =>
      if scanner.ch === CharacterCodes.eof {
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.unclosedTemplate)
        popMode(scanner, Template)
        Token.TemplateTail(
          Bytes.sub_string(
            scanner.src,
            startOff,
            scanner.offset - 2 - startOff,
          ),
        )
      } else if scanner.ch === CharacterCodes.backtick {
        next(scanner)
        let contents = Bytes.sub_string(
          scanner.src,
          startOff,
          scanner.offset - 1 - startOff,
        )
        
        popMode(scanner, Template)
        Token.TemplateTail(contents)
      } else if (
        scanner.ch === CharacterCodes.dollar &&
          peek(scanner) === CharacterCodes.lbrace
      ) {
        next(scanner)
        next(scanner)
        let contents = Bytes.sub_string(
          scanner.src,
          startOff,
          scanner.offset - 2 - startOff,
        )
        
        popMode(scanner, Template)
        Token.TemplatePart(contents)
      } else {
        if CharacterCodes.isLineBreak(scanner.ch) {
          scanner.lineOffset = scanner.offset + 1
          scanner.lnum = scanner.lnum + 1
        }
        next(scanner)
        scan()
      }
    
    scan()
  }
  
  let rec scan = scanner => {
    if !inTemplateMode(scanner) {
      skipWhitespace(scanner)
    }
    let startPos = position(scanner)
    let ch = scanner.ch
    let token = if inTemplateMode(scanner) {
      scanTemplate(scanner)
    } else if ch === CharacterCodes.underscore {
      let nextCh = peek(scanner)
      if (
        nextCh === CharacterCodes.underscore ||
          CharacterCodes.isDigit(nextCh) ||
          CharacterCodes.isLetter(nextCh)
      ) {
        scanIdentifier(scanner)
      } else {
        next(scanner)
        Token.Underscore
      }
    } else if CharacterCodes.isLetter(ch) {
      scanIdentifier(scanner)
    } else if CharacterCodes.isDigit(ch) {
      scanNumber(scanner)
    } else {
      next(scanner)
      if ch === CharacterCodes.dot {
        if scanner.ch === CharacterCodes.dot {
          next(scanner)
          if scanner.ch === CharacterCodes.dot {
            next(scanner)
            Token.DotDotDot
          } else {
            Token.DotDot
          }
        } else {
          Token.Dot
        }
      } else if ch === CharacterCodes.doubleQuote {
        scanString(scanner)
      } else if ch === CharacterCodes.singleQuote {
        if scanner.ch === CharacterCodes.backslash {
          next(scanner)
          scanEscape(scanner)
        } else if peek(scanner) === CharacterCodes.singleQuote {
          let ch = scanner.ch
          next(scanner)
          next(scanner)
          Token.Character(Char.chr(ch))
        } else {
          SingleQuote
        }
      } else if ch === CharacterCodes.bang {
        if scanner.ch === CharacterCodes.equal {
          next(scanner)
          if scanner.ch === CharacterCodes.equal {
            next(scanner)
            Token.BangEqualEqual
          } else {
            Token.BangEqual
          }
        } else {
          Token.Bang
        }
      } else if ch === CharacterCodes.semicolon {
        Token.Semicolon
      } else if ch === CharacterCodes.equal {
        if scanner.ch === CharacterCodes.greaterThan {
          next(scanner)
          Token.EqualGreater
        } else if scanner.ch === CharacterCodes.equal {
          next(scanner)
          if scanner.ch === CharacterCodes.equal {
            next(scanner)
            Token.EqualEqualEqual
          } else {
            Token.EqualEqual
          }
        } else {
          Token.Equal
        }
      } else if ch === CharacterCodes.bar {
        if scanner.ch === CharacterCodes.bar {
          next(scanner)
          Token.Lor
        } else if scanner.ch === CharacterCodes.greaterThan {
          next(scanner)
          Token.BarGreater
        } else {
          Token.Bar
        }
      } else if ch === CharacterCodes.ampersand {
        if scanner.ch === CharacterCodes.ampersand {
          next(scanner)
          Token.Land
        } else {
          Token.Band
        }
      } else if ch === CharacterCodes.lparen {
        Token.Lparen
      } else if ch === CharacterCodes.rparen {
        Token.Rparen
      } else if ch === CharacterCodes.lbracket {
        Token.Lbracket
      } else if ch === CharacterCodes.rbracket {
        Token.Rbracket
      } else if ch === CharacterCodes.lbrace {
        Token.Lbrace
      } else if ch === CharacterCodes.rbrace {
        Token.Rbrace
      } else if ch === CharacterCodes.comma {
        Token.Comma
      } else if ch === CharacterCodes.colon {
        if scanner.ch === CharacterCodes.equal {
          next(scanner)
          Token.ColonEqual
        } else {
          Token.Colon
        }
      } else if ch === CharacterCodes.backslash {
        Token.Backslash
      } else if ch === CharacterCodes.forwardslash {
        if scanner.ch === CharacterCodes.forwardslash {
          next(scanner)
          scanSingleLineComment(scanner)
        } else if scanner.ch === CharacterCodes.asterisk {
          next(scanner)
          scanMultiLineComment(scanner)
        } else if scanner.ch === CharacterCodes.dot {
          next(scanner)
          Token.ForwardslashDot
        } else {
          Token.Forwardslash
        }
      } else if ch === CharacterCodes.minus {
        if scanner.ch === CharacterCodes.dot {
          next(scanner)
          Token.MinusDot
        } else if scanner.ch === CharacterCodes.greaterThan {
          next(scanner)
          Token.MinusGreater
        } else {
          Token.Minus
        }
      } else if ch === CharacterCodes.plus {
        if scanner.ch === CharacterCodes.dot {
          next(scanner)
          Token.PlusDot
        } else if scanner.ch === CharacterCodes.plus {
          next(scanner)
          Token.PlusPlus
        } else if scanner.ch === CharacterCodes.equal {
          next(scanner)
          Token.PlusEqual
        } else {
          Token.Plus
        }
      } else if ch === CharacterCodes.greaterThan {
        if scanner.ch === CharacterCodes.equal && !inDiamondMode(scanner) {
          next(scanner)
          Token.GreaterEqual
        } else {
          Token.GreaterThan
        }
      } else if ch === CharacterCodes.lessThan {
        if inJsxMode(scanner) {
          skipWhitespace(scanner)
          if scanner.ch === CharacterCodes.forwardslash {
            let () = next(scanner)
            Token.LessThanSlash
          } else {
            Token.LessThan
          }
        } else if scanner.ch === CharacterCodes.equal {
          next(scanner)
          Token.LessEqual
        } else {
          Token.LessThan
        }
      } else if ch === CharacterCodes.hash {
        if scanner.ch === CharacterCodes.hash {
          next(scanner)
          Token.HashHash
        } else if scanner.ch === CharacterCodes.equal {
          next(scanner)
          Token.HashEqual
        } else {
          Token.Hash
        }
      } else if ch === CharacterCodes.asterisk {
        if scanner.ch === CharacterCodes.asterisk {
          next(scanner)
          Token.Exponentiation
        } else if scanner.ch === CharacterCodes.dot {
          next(scanner)
          Token.AsteriskDot
        } else {
          Token.Asterisk
        }
      } else if ch === CharacterCodes.tilde {
        Token.Tilde
      } else if ch === CharacterCodes.question {
        Token.Question
      } else if ch === CharacterCodes.at {
        if scanner.ch === CharacterCodes.at {
          next(scanner)
          Token.AtAt
        } else {
          Token.At
        }
      } else if ch === CharacterCodes.percent {
        if scanner.ch === CharacterCodes.percent {
          next(scanner)
          Token.PercentPercent
        } else {
          Token.Percent
        }
      } else if ch === CharacterCodes.backtick {
        Token.Backtick
      } else if ch === -1 {
        Token.Eof
      } else {
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.unknownUchar(ch))
        let (_, _, token) = scan(scanner)
        token
      }
    }
    let endPos = position(scanner)
    (startPos, endPos, token)
  }
  
  let reconsiderLessThan = scanner => {
    skipWhitespace(scanner)
    if scanner.ch === CharacterCodes.forwardslash {
      let () = next(scanner)
      Token.LessThanSlash
    } else {
      Token.LessThan
    }
  }
  
  let isBinaryOp = (src, startCnum, endCnum) =>
    if startCnum === 0 {
      false
    } else {
      let leftOk = {
        let c = startCnum - 1 |> Bytes.get(src) |> Char.code
        
        c === CharacterCodes.space ||
          c === CharacterCodes.tab ||
          CharacterCodes.isLineBreak(c)
      }
      
      let rightOk = {
        let c = if endCnum === Bytes.length(src) {
          -1
        } else {
          endCnum |> Bytes.get(src) |> Char.code
        }
        
        c === CharacterCodes.space ||
          c === CharacterCodes.tab ||
          CharacterCodes.isLineBreak(c) || c === CharacterCodes.eof
      }
      
      leftOk && rightOk
    }
}

module JsFfi = {
  type rec scope =
    | Global
    | Module(string)
    | Scope(Longident.t)
  
  type rec label_declaration = {
    jld_attributes: Parsetree.attributes,
    jld_name: string,
    jld_alias: string,
    jld_type: Parsetree.core_type,
    jld_loc: Location.t,
  }
  
  type rec importSpec =
    | Default(label_declaration)
    | Spec(list<label_declaration>)
  
  type rec import_description = {
    jid_loc: Location.t,
    jid_spec: importSpec,
    jid_scope: scope,
    jid_attributes: Parsetree.attributes,
  }
  
  let decl = (~attrs, ~loc, ~name, ~alias, ~typ) => {
    jld_loc: loc,
    jld_attributes: attrs,
    jld_name: name,
    jld_alias: alias,
    jld_type: typ,
  }
  
  let importDescr = (~attrs, ~scope, ~importSpec, ~loc) => {
    jid_loc: loc,
    jid_spec: importSpec,
    jid_scope: scope,
    jid_attributes: attrs,
  }
  
  let toParsetree = importDescr => {
    let bsVal = (Location.mknoloc("bs.val"), Parsetree.PStr(list[]))
    let attrs = switch importDescr.jid_scope {
    | Global => list[bsVal]
    
    | Module(s) =>
      let structure = list[
        Parsetree.Pconst_string(s, None)
        |> Ast_helper.Exp.constant
        |> Ast_helper.Str.eval,
      ]
      let genType = (
        Location.mknoloc("genType.import"),
        Parsetree.PStr(structure),
      )
      list[genType]
    | Scope(longident) =>
      let structureItem = {
        let expr = switch Longident.flatten(longident) |> List.map(s =>
          Ast_helper.Exp.constant(Parsetree.Pconst_string(s, None))
        ) {
        | list[expr] => expr
        | list[] as exprs | _ as exprs => exprs |> Ast_helper.Exp.tuple
        }
        
        Ast_helper.Str.eval(expr)
      }
      
      let bsScope = (
        Location.mknoloc("bs.scope"),
        Parsetree.PStr(list[structureItem]),
      )
      list[bsVal, bsScope]
    }
    
    let valueDescrs = switch importDescr.jid_spec {
    | Default(decl) =>
      let prim = list[decl.jld_name]
      let allAttrs =
        List.concat(list[attrs, importDescr.jid_attributes]) |> List.map(attr =>
          switch attr {
          | (
              {Location.txt: "genType.import"} as id,
              Parsetree.PStr(
                list[{pstr_desc: Parsetree.Pstr_eval(moduleName, _)}],
              ),
            ) =>
            let default =
              Parsetree.Pconst_string(
                "default",
                None,
              ) |> Ast_helper.Exp.constant
            
            let structureItem =
              list[moduleName, default]
              |> Ast_helper.Exp.tuple
              |> Ast_helper.Str.eval
            
            (id, Parsetree.PStr(list[structureItem]))
          | attr => attr
          }
        )
      
      list[
        Ast_helper.Val.mk(
          ~loc=importDescr.jid_loc,
          ~prim,
          ~attrs=allAttrs,
          Location.mknoloc(decl.jld_alias),
          decl.jld_type,
        ) |> Ast_helper.Str.primitive,
      ]
    | Spec(decls) =>
      List.map(
        decl => {
          let prim = list[decl.jld_name]
          let allAttrs = List.concat(list[attrs, importDescr.jid_attributes])
          Ast_helper.Val.mk(
            ~loc=importDescr.jid_loc,
            ~prim,
            ~attrs=allAttrs,
            Location.mknoloc(decl.jld_alias),
            decl.jld_type,
          ) |> Ast_helper.Str.primitive(~loc=decl.jld_loc)
        },
        decls,
      )
    }
    
    Ast_helper.Mod.structure(~loc=importDescr.jid_loc, valueDescrs)
    |> Ast_helper.Incl.mk(~loc=importDescr.jid_loc)
    |> Ast_helper.Str.include_(~loc=importDescr.jid_loc)
  }
}

module Parser = {
  type rec t = {
    mutable scanner: Scanner.t,
    mutable token: Token.t,
    mutable startPos: Lexing.position,
    mutable endPos: Lexing.position,
    mutable prevEndPos: Lexing.position,
    mutable breadcrumbs: list<(Grammar.t, Lexing.position)>,
    mutable errors: list<Reporting.parseError>,
    mutable diagnostics: list<Diagnostics.t>,
    mutable comments: list<Comment.t>,
  }
  
  let err = (~startPos=?, ~endPos=?, p, error) => {
    let d = Diagnostics.make(
      ~filename=p.scanner.filename,
      ~startPos=switch startPos {
      | Some(pos) => pos
      | None => p.startPos
      },
      ~endPos=switch endPos {
      | Some(pos) => pos
      | None => p.endPos
      },
      error,
    )
    
    p.diagnostics = list[d, ...p.diagnostics]
  }
  
  let debugBreadcrumbs = bcs => {
    print_endline("current breadcrumbs:")
    List.iter(((grammar, _)) => print_endline(Grammar.toString(grammar)), bcs)
    print_endline("=================")
  }
  
  let dropLastDiagnostic = p =>
    switch p.diagnostics {
    | list[_, ...ds] => p.diagnostics = ds
    | list[] => ()
    }
  
  let rec next = (~prevEndPos=?, p) => {
    let prevEndPos = switch prevEndPos {
    | Some(pos) => pos
    | None => p.endPos
    }
    let (startPos, endPos, token) = Scanner.scan(p.scanner)
    switch token {
    | Comment(c) =>
      Comment.setPrevTokEndPos(c, p.endPos)
      p.comments = list[c, ...p.comments]
      p.prevEndPos = p.endPos
      p.endPos = endPos
      next(~prevEndPos, p)
    | _ =>
      p.token = token
      
      p.prevEndPos = prevEndPos
      p.startPos = startPos
      p.endPos = endPos
    }
  }
  
  let make = (src, filename) => {
    let scanner = Scanner.make(Bytes.of_string(src), filename)
    let parserState = {
      scanner: scanner,
      token: Token.Eof,
      startPos: Lexing.dummy_pos,
      prevEndPos: Lexing.dummy_pos,
      endPos: Lexing.dummy_pos,
      breadcrumbs: list[],
      errors: list[],
      diagnostics: list[],
      comments: list[],
    }
    parserState.scanner.err = (~startPos, ~endPos, error) => {
      let diagnostic = Diagnostics.make(~filename, ~startPos, ~endPos, error)
      
      parserState.diagnostics = list[diagnostic, ...parserState.diagnostics]
    }
    next(parserState)
    parserState
  }
  
  let leaveBreadcrumb = (p, circumstance) => {
    let crumb = (circumstance, p.startPos)
    p.breadcrumbs = list[crumb, ...p.breadcrumbs]
  }
  
  let eatBreadcrumb = p =>
    switch p.breadcrumbs {
    | list[] => ()
    | list[_, ...crumbs] => p.breadcrumbs = crumbs
    }
  
  let optional = (p, token) =>
    if p.token == token {
      let () = next(p)
      true
    } else {
      false
    }
  
  exception Exit
  
  let expect = (~grammar=?, token, p) =>
    if p.token == token {
      next(p)
    } else {
      let error = Diagnostics.expected(~grammar?, p.prevEndPos, token)
      err(~startPos=p.prevEndPos, p, error)
    }
  
  let lookahead = (p, callback) => {
    let err = p.scanner.err
    let ch = p.scanner.ch
    let offset = p.scanner.offset
    let rdOffset = p.scanner.rdOffset
    let lineOffset = p.scanner.lineOffset
    let lnum = p.scanner.lnum
    let mode = p.scanner.mode
    let token = p.token
    let startPos = p.startPos
    let endPos = p.endPos
    let prevEndPos = p.prevEndPos
    let breadcrumbs = p.breadcrumbs
    let errors = p.errors
    let diagnostics = p.diagnostics
    let comments = p.comments
    
    let res = callback(p)
    
    p.scanner.err = err
    p.scanner.ch = ch
    p.scanner.offset = offset
    p.scanner.rdOffset = rdOffset
    p.scanner.lineOffset = lineOffset
    p.scanner.lnum = lnum
    p.scanner.mode = mode
    p.token = token
    p.startPos = startPos
    p.endPos = endPos
    p.prevEndPos = prevEndPos
    p.breadcrumbs = breadcrumbs
    p.errors = errors
    p.diagnostics = diagnostics
    p.comments = comments
    
    res
  }
}

module NapkinScript = {
  let mkLoc = (startLoc, endLoc) => {
    open Location
    {
      loc_start: startLoc,
      loc_end: endLoc,
      loc_ghost: false,
    }
  }
  
  module Recover = {
    type rec action =
      | Retry
      | Abort
    
    let fakeStructureItem = {
      let id = Location.mknoloc("napkinscript.fakeStructureItem")
      Ast_helper.Str.extension((id, PStr(list[])))
    }
    
    let fakeSignatureItem = {
      let id = Location.mknoloc("napkinscript.fakeSignatureItem")
      Ast_helper.Sig.extension((id, PStr(list[])))
    }
    
    let defaultStructureItem = () => {
      let id = Location.mknoloc("napkinscript.strItemHole")
      Ast_helper.Str.extension((id, PStr(list[])))
    }
    
    let defaultSignatureItem = () => {
      let id = Location.mknoloc("napkinscript.SigItemHole")
      Ast_helper.Sig.extension((id, PStr(list[])))
    }
    
    let defaultExpr = () => {
      let id = Location.mknoloc("napkinscript.exprhole")
      Ast_helper.Exp.mk(Pexp_extension(id, PStr(list[])))
    }
    
    let defaultType = () => {
      let id = Location.mknoloc("napkinscript.typehole")
      Ast_helper.Typ.extension((id, PStr(list[])))
    }
    
    let defaultPattern = () => {
      let id = Location.mknoloc("napkinscript.patternhole")
      Ast_helper.Pat.extension((id, PStr(list[])))
    }
    
    let defaultModuleExpr = () => Ast_helper.Mod.structure(list[])
    let defaultModuleType = () => Ast_helper.Mty.signature(list[])
    
    let recoverEqualGreater = p => {
      Parser.expect(EqualGreater, p)
      switch p.Parser.token {
      | MinusGreater => Parser.next(p)
      | _ => ()
      }
    }
    
    let shouldAbortListParse = p => {
      let rec check = breadcrumbs =>
        switch breadcrumbs {
        | list[] => false
        | list[(grammar, _), ...rest] =>
          if Grammar.isPartOfList(grammar, p.Parser.token) {
            true
          } else {
            check(rest)
          }
        }
      
      check(p.breadcrumbs)
    }
    
    let recoverLident = p =>
      if (
        Token.isKeyword(p.Parser.token) &&
        p.Parser.prevEndPos.pos_lnum === p.startPos.pos_lnum
      ) {
        Parser.err(p, Diagnostics.lident(p.Parser.token))
        Parser.next(p)
        Abort
      } else {
        while !shouldAbortListParse(p) {
          Parser.next(p)
        }
        switch p.Parser.token {
        | Lident(_) => Retry
        | _ => Abort
        }
      }
    
    let skipTokensAndMaybeRetry = (p, ~isStartOfGrammar) =>
      if (
        Token.isKeyword(p.Parser.token) &&
        p.Parser.prevEndPos.pos_lnum === p.startPos.pos_lnum
      ) {
        Parser.next(p)
        Abort
      } else {
        while !shouldAbortListParse(p) {
          Parser.next(p)
        }
        if isStartOfGrammar(p.Parser.token) {
          Retry
        } else {
          Abort
        }
      }
  }
  
  module ErrorMessages = {
    let listPatternSpread = "List pattern matches only supports one `...` spread, at the end.\nExplanation: a list spread at the tail is efficient, but a spread in the middle would create new list[s]; out of performance concern, our pattern matching currently guarantees to never create new intermediate data."
    
    let recordPatternSpread = "Record's `...` spread is not supported in pattern matches.\nExplanation: you can't collect a subset of a record's field into its own record, since a record needs an explicit declaration and that subset wouldn't have one.\nSolution: you need to pull out each field you want explicitly."
    
    let recordPatternUnderscore = "Record patterns only support one `_`, at the end."
    
    let arrayPatternSpread = "Array's `...` spread is not supported in pattern matches.\nExplanation: such spread would create a subarray; out of performance concern, our pattern matching currently guarantees to never create new intermediate data.\nSolution: if it's to validate the first few elements, use a `when` clause + Array size check + `get` checks on the current pattern. If it's to obtain a subarray, use `Array.sub` or `Belt.Array.slice`."
    
    let arrayExprSpread = "Arrays can't use the `...` spread currently. Please use `concat` or other Array helpers."
    
    let recordExprSpread = "Records can only have one `...` spread, at the beginning.\nExplanation: since records have a known, fixed shape, a spread like `{a, ...b}` wouldn't make sense, as `b` would override every field of `a` anyway."
    
    let listExprSpread = "Lists can only have one `...` spread, and at the end.\nExplanation: lists are singly-linked list, where a node contains a value and points to the next node. `list[a, ...bc]` efficiently creates a new item and links `bc` as its next nodes. `[...bc, a]` would be expensive, as it'd need to traverse `bc` and prepend each item to `a` one by one. We therefore disallow such syntax sugar.\nSolution: directly use `concat`."
  }
  
  let jsxAttr = (Location.mknoloc("JSX"), Parsetree.PStr(list[]))
  let uncurryAttr = (Location.mknoloc("bs"), Parsetree.PStr(list[]))
  let ternaryAttr = (Location.mknoloc("ns.ternary"), Parsetree.PStr(list[]))
  
  type rec typDefOrExt =
    | TypeDef((Asttypes.rec_flag, list<Parsetree.type_declaration>))
    | TypeExt(Parsetree.type_extension)
  
  type rec labelledParameter =
    | TermParameter(
        (
          bool,
          Parsetree.attributes,
          Asttypes.arg_label,
          option<Parsetree.expression>,
          Parsetree.pattern,
          Lexing.position,
        ),
      )
    | TypeParameter(
        (
          bool,
          Parsetree.attributes,
          list<Location.loc<string>>,
          Lexing.position,
        ),
      )
  
  type rec recordPatternItem =
    | PatUnderscore
    | PatField((Ast_helper.lid, Parsetree.pattern))
  
  type rec context =
    | OrdinaryExpr
    | TernaryTrueBranchExpr
    | WhenExpr
  
  let getClosingToken = x => switch x {
  | Token.Lparen => Token.Rparen
  | Lbrace => Rbrace
  | Lbracket => Rbracket
  | _ => assert false
  }
  
  let rec goToClosing = (closingToken, state) =>
    switch (state.Parser.token, closingToken) {
    | ((Rparen, Token.Rparen) | (Rbrace, Rbrace)) | (Rbracket, Rbracket) =>
      Parser.next(state)
      ()
    | (((Token.Lbracket | Lparen) | Lbrace) as t, _) =>
      Parser.next(state)
      goToClosing(getClosingToken(t), state)
      goToClosing(closingToken, state)
    | (((Rparen | Token.Rbrace) | Rbracket) | Eof, _) => ()
    | _ =>
      Parser.next(state)
      goToClosing(closingToken, state)
    }
  
  let isEs6ArrowExpression = (~inTernary, p) =>
    Parser.lookahead(p, state =>
      switch state.Parser.token {
      | Lident(_) | Underscore =>
        Parser.next(state)
        switch state.Parser.token {
        | EqualGreater => true
        | _ => false
        }
      | Lparen =>
        let prevEndPos = state.prevEndPos
        Parser.next(state)
        switch state.token {
        | Rparen =>
          Parser.next(state)
          switch state.Parser.token {
          | Colon when !inTernary => true
          | EqualGreater => true
          | _ => false
          }
        | Dot => true
        | Tilde => true
        | Backtick => false
        | _ =>
          goToClosing(Rparen, state)
          switch state.Parser.token {
          | EqualGreater | Lbrace => true
          | Colon when !inTernary => true
          | _ =>
            Parser.next(state)
            
            switch state.Parser.token {
            | EqualGreater
              when state.startPos.pos_lnum === prevEndPos.pos_lnum =>
              true
            | _ => false
            }
          }
        }
      | _ => false
      }
    )
  
  let isEs6ArrowFunctor = p =>
    Parser.lookahead(p, state =>
      switch state.Parser.token {
      | Lparen =>
        Parser.next(state)
        switch state.token {
        | Rparen =>
          Parser.next(state)
          switch state.token {
          | Colon | EqualGreater => true
          | _ => false
          }
        | _ =>
          goToClosing(Rparen, state)
          switch state.Parser.token {
          | EqualGreater | Lbrace => true
          | Colon => true
          | _ => false
          }
        }
      | _ => false
      }
    )
  
  let isEs6ArrowType = p =>
    Parser.lookahead(p, state =>
      switch state.Parser.token {
      | Lparen =>
        Parser.next(state)
        switch state.Parser.token {
        | Rparen =>
          Parser.next(state)
          switch state.Parser.token {
          | EqualGreater => true
          | _ => false
          }
        | Tilde | Dot => true
        | _ =>
          goToClosing(Rparen, state)
          switch state.Parser.token {
          | EqualGreater => true
          | _ => false
          }
        }
      | Tilde => true
      | _ => false
      }
    )
  
  let buildLongident = words =>
    switch List.rev(words) {
    | list[] => assert false
    | list[hd, ...tl] =>
      List.fold_left((p, s) => Longident.Ldot(p, s), Lident(hd), tl)
    }
  
  let makeInfixOperator = (p, token, startPos, endPos) => {
    let stringifiedToken = if token == Token.MinusGreater {
      "|."
    } else if token == Token.PlusPlus {
      "^"
    } else if token == Token.BangEqual {
      "<>"
    } else if token == Token.BangEqualEqual {
      "!="
    } else if token == Token.Equal {
      Parser.err(
        ~startPos,
        ~endPos,
        p,
        Diagnostics.message("Did you mean `==` here?"),
      )
      "="
    } else if token == Token.EqualEqual {
      "="
    } else if token == Token.EqualEqualEqual {
      "=="
    } else {
      Token.toString(token)
    }
    
    let loc = mkLoc(startPos, endPos)
    let operator = Location.mkloc(Longident.Lident(stringifiedToken), loc)
    
    Ast_helper.Exp.ident(~loc, operator)
  }
  
  let negateString = s =>
    if String.length(s) > 0 && String.get(s, 0) == '-' {
      String.sub(s, 1, String.length(s) - 1)
    } else {
      "-" ++ s
    }
  
  let makeUnaryExpr = (startPos, tokenEnd, token, operand) =>
    switch (token, operand.Parsetree.pexp_desc) {
    | (
        Token.Plus | PlusDot,
        Pexp_constant(Pconst_integer(_) | Pconst_float(_)),
      ) =>
      operand
    | (Minus, Pexp_constant(Pconst_integer(n, m))) =>
      {...operand, pexp_desc: Pexp_constant(Pconst_integer(negateString(n), m))}
    | (Minus | MinusDot, Pexp_constant(Pconst_float(n, m))) =>
      {...operand, pexp_desc: Pexp_constant(Pconst_float(negateString(n), m))}
    | (((Token.Plus | PlusDot) | Minus) | MinusDot, _) =>
      let tokenLoc = mkLoc(startPos, tokenEnd)
      let operator = "~" ++ Token.toString(token)
      Ast_helper.Exp.apply(
        ~loc=mkLoc(startPos, operand.Parsetree.pexp_loc.loc_end),
        Ast_helper.Exp.ident(
          ~loc=tokenLoc,
          Location.mkloc(Longident.Lident(operator), tokenLoc),
        ),
        list[(Nolabel, operand)],
      )
    | (Token.Bang, _) =>
      let tokenLoc = mkLoc(startPos, tokenEnd)
      Ast_helper.Exp.apply(
        ~loc=mkLoc(startPos, operand.Parsetree.pexp_loc.loc_end),
        Ast_helper.Exp.ident(
          ~loc=tokenLoc,
          Location.mkloc(Longident.Lident("not"), tokenLoc),
        ),
        list[(Nolabel, operand)],
      )
    | (Token.Band, _) =>
      let tokenLoc = mkLoc(startPos, tokenEnd)
      let operator = Ast_helper.Exp.ident(
        ~loc=tokenLoc,
        Location.mkloc(Longident.Lident("!"), tokenLoc),
      )
      
      Ast_helper.Exp.apply(
        ~loc=mkLoc(startPos, operand.Parsetree.pexp_loc.loc_end),
        operator,
        list[(Nolabel, operand)],
      )
    | _ => operand
    }
  
  let makeListExpression = (loc, seq, extOpt) => {
    let rec handleSeq = x => switch x {
    | list[] =>
      switch extOpt {
      | Some(ext) => ext
      | None =>
        let loc = {...loc, Location.loc_ghost: true}
        let nil = Location.mkloc(Longident.Lident("[]"), loc)
        Ast_helper.Exp.construct(~loc, nil, None)
      }
    | list[e1, ...el] =>
      let exp_el = handleSeq(el)
      let loc = mkLoc(
        e1.Parsetree.pexp_loc.Location.loc_start,
        exp_el.pexp_loc.loc_end,
      )
      
      let arg = Ast_helper.Exp.tuple(~loc, list[e1, exp_el])
      Ast_helper.Exp.construct(
        ~loc,
        Location.mkloc(Longident.Lident("::"), loc),
        Some(arg),
      )
    }
    
    let expr = handleSeq(seq)
    {...expr, pexp_loc: loc}
  }
  
  let makeListPattern = (loc, seq, ext_opt) => {
    let rec handle_seq = x => switch x {
    | list[] =>
      let base_case = switch ext_opt {
      | Some(ext) => ext
      | None =>
        let loc = {...loc, Location.loc_ghost: true}
        let nil = {Location.txt: Longident.Lident("[]"), loc: loc}
        Ast_helper.Pat.construct(~loc, nil, None)
      }
      
      base_case
    | list[p1, ...pl] =>
      let pat_pl = handle_seq(pl)
      let loc = mkLoc(p1.Parsetree.ppat_loc.loc_start, pat_pl.ppat_loc.loc_end)
      let arg = Ast_helper.Pat.mk(~loc, Ppat_tuple(list[p1, pat_pl]))
      Ast_helper.Pat.mk(
        ~loc,
        Ppat_construct(Location.mkloc(Longident.Lident("::"), loc), Some(arg)),
      )
    }
    
    handle_seq(seq)
  }
  
  let makeBsObjType = (~attrs, ~loc, ~closed, rows) => {
    let obj = Ast_helper.Typ.object_(~loc, rows, closed)
    let jsDotTCtor = Location.mkloc(
      Longident.Ldot(Longident.Lident("Js"), "t"),
      loc,
    )
    
    Ast_helper.Typ.constr(~loc, ~attrs, jsDotTCtor, list[obj])
  }
  
  let lidentOfPath = longident =>
    switch Longident.flatten(longident) |> List.rev {
    | list[] => ""
    | list[ident, ..._] => ident
    }
  
  let makeNewtypes = (~attrs, ~loc, newtypes, exp) => {
    let expr = List.fold_right(
      (newtype, exp) => Ast_helper.Exp.mk(~loc, Pexp_newtype(newtype, exp)),
      newtypes,
      exp,
    )
    {...expr, pexp_attributes: attrs}
  }
  
  let wrap_type_annotation = (~loc, newtypes, core_type, body) => {
    let exp = makeNewtypes(
      ~attrs=list[],
      ~loc,
      newtypes,
      Ast_helper.Exp.constraint_(~loc, body, core_type),
    )
    
    let typ = Ast_helper.Typ.poly(
      ~loc,
      newtypes,
      Ast_helper.Typ.varify_constructors(newtypes, core_type),
    )
    
    (exp, typ)
  }
  
  @ocaml.doc(
    "\n    * process the occurrence of _ in the arguments of a function application\n    * replace _ with a new variable, currently __x, in the arguments\n    * return a wrapping function that wraps ((__x) => ...) around an expression\n    * e.g. foo(_, 3) becomes (__x) => foo(__x, 3)\n    "
  )
  let processUnderscoreApplication = args => {
    open Parsetree
    let exp_question = ref(None)
    let hidden_var = "__x"
    let check_arg = ((lab, exp) as arg) =>
      switch exp.pexp_desc {
      | Pexp_ident({txt: Lident("_")} as id) =>
        let new_id = Location.mkloc(Longident.Lident(hidden_var), id.loc)
        let new_exp = Ast_helper.Exp.mk(Pexp_ident(new_id), ~loc=exp.pexp_loc)
        exp_question := Some(new_exp)
        (lab, new_exp)
      | _ => arg
      }
    
    let args = List.map(check_arg, args)
    let wrap = exp_apply =>
      switch &exp_question {
      | Some({pexp_loc: loc}) =>
        let pattern = Ast_helper.Pat.mk(
          Ppat_var(Location.mkloc(hidden_var, loc)),
          ~loc,
        )
        Ast_helper.Exp.mk(Pexp_fun(Nolabel, None, pattern, exp_apply), ~loc)
      | None => exp_apply
      }
    
    (args, wrap)
  }
  
  let rec parseLident = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Lident(ident) =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      (ident, loc)
    | _ =>
      switch Recover.recoverLident(p) {
      | Retry => parseLident(p)
      | Abort => ("_", mkLoc(startPos, p.prevEndPos))
      }
    }
  }
  
  let parseValuePath = p => {
    let startPos = p.Parser.startPos
    let rec aux = (p, path) =>
      switch p.Parser.token {
      | List => Longident.Ldot(path, "list")
      | Lident(ident) => Longident.Ldot(path, ident)
      | Uident(uident) =>
        Parser.next(p)
        Parser.expect(Dot, p)
        aux(p, Ldot(path, uident))
      | token =>
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        Longident.Lident("_")
      }
    
    let ident = switch p.Parser.token {
    | List => Longident.Lident("list")
    | Lident(ident) => Longident.Lident(ident)
    | Uident(ident) =>
      Parser.next(p)
      Parser.expect(Dot, p)
      aux(p, Lident(ident))
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Longident.Lident("_")
    }
    
    Parser.next(p)
    Location.mkloc(ident, mkLoc(startPos, p.prevEndPos))
  }
  
  let parseValuePathTail = (p, startPos, ident) => {
    let rec loop = (p, path) =>
      switch p.Parser.token {
      | Lident(ident) =>
        Parser.next(p)
        Location.mkloc(
          Longident.Ldot(path, ident),
          mkLoc(startPos, p.prevEndPos),
        )
      | Uident(ident) =>
        Parser.next(p)
        Parser.expect(Dot, p)
        loop(p, Longident.Ldot(path, ident))
      | token =>
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        Location.mknoloc(path)
      }
    
    loop(p, ident)
  }
  
  let parseModuleLongIdentTail = (p, startPos, ident) => {
    let rec loop = (p, acc) =>
      switch p.Parser.token {
      | Uident(ident) =>
        Parser.next(p)
        let endPos = p.prevEndPos
        let lident = Longident.Ldot(acc, ident)
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          loop(p, lident)
        | _ => Location.mkloc(lident, mkLoc(startPos, endPos))
        }
      | t =>
        Parser.err(p, Diagnostics.uident(t))
        Location.mkloc(acc, mkLoc(startPos, p.prevEndPos))
      }
    
    loop(p, ident)
  }
  
  let parseModuleLongIdent = p => {
    let startPos = p.Parser.startPos
    let moduleIdent = switch p.Parser.token {
    | Uident(ident) =>
      let lident = Longident.Lident(ident)
      let endPos = p.endPos
      Parser.next(p)
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        parseModuleLongIdentTail(p, startPos, lident)
      | _ => Location.mkloc(lident, mkLoc(startPos, endPos))
      }
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mkloc(Longident.Lident("_"), mkLoc(startPos, p.prevEndPos))
    }
    
    moduleIdent
  }
  
  let parseIdentPath = p => {
    let rec loop = (p, acc) =>
      switch p.Parser.token {
      | Uident(ident) | Lident(ident) =>
        Parser.next(p)
        let lident = Longident.Ldot(acc, ident)
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          loop(p, lident)
        | _ => lident
        }
      | t => acc
      }
    
    switch p.Parser.token {
    | Lident(ident) | Uident(ident) =>
      Parser.next(p)
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        loop(p, Longident.Lident(ident))
      | _ => Longident.Lident(ident)
      }
    | _ => Longident.Lident("_")
    }
  }
  
  let verifyJsxOpeningClosingName = (p, nameExpr) => {
    let closing = switch p.Parser.token {
    | Lident(lident) =>
      Parser.next(p)
      Longident.Lident(lident)
    | Uident(_) => parseModuleLongIdent(p).txt
    | _ => Longident.Lident("")
    }
    
    switch nameExpr.Parsetree.pexp_desc {
    | Pexp_ident(openingIdent) =>
      let opening = {
        let withoutCreateElement =
          Longident.flatten(openingIdent.txt) |> List.filter(s =>
            s != "createElement"
          )
        
        switch Longident.unflatten(withoutCreateElement) {
        | Some(li) => li
        | None => Longident.Lident("")
        }
      }
      
      opening == closing
    | _ => assert false
    }
  }
  
  let string_of_pexp_ident = nameExpr =>
    switch nameExpr.Parsetree.pexp_desc {
    | Pexp_ident(openingIdent) =>
      Longident.flatten(openingIdent.txt)
      |> List.filter(s => s != "createElement")
      |> String.concat(".")
    | _ => ""
    }
  
  let parseOpenDescription = (~attrs, p) => {
    Parser.leaveBreadcrumb(p, Grammar.OpenDescription)
    let startPos = p.Parser.startPos
    Parser.expect(Open, p)
    let override = if Parser.optional(p, Token.Bang) {
      Asttypes.Override
    } else {
      Asttypes.Fresh
    }
    
    let modident = parseModuleLongIdent(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Parser.eatBreadcrumb(p)
    Ast_helper.Opn.mk(~loc, ~attrs, ~override, modident)
  }
  
  let parseConstant = p => {
    let constant = switch p.Parser.token {
    | Int(i, suffix) => Parsetree.Pconst_integer(i, suffix)
    | Float(i, suffix) => Parsetree.Pconst_float(i, suffix)
    | String(s) => Pconst_string(s, None)
    | Character(c) => Pconst_char(c)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Pconst_string("", None)
    }
    
    Parser.next(p)
    constant
  }
  
  let parseCommaDelimitedRegion = (p, ~grammar, ~closing, ~f) => {
    Parser.leaveBreadcrumb(p, grammar)
    let rec loop = nodes =>
      switch f(p) {
      | Some(node) =>
        switch p.Parser.token {
        | Comma =>
          Parser.next(p)
          loop(list[node, ...nodes])
        | token when token == closing || token == Eof =>
          List.rev(list[node, ...nodes])
        | _ =>
          if (
            !(
              p.token == Eof ||
                p.token == closing ||
                Recover.shouldAbortListParse(p)
            )
          ) {
            Parser.expect(Comma, p)
          }
          if p.token == Semicolon {
            Parser.next(p)
          }
          loop(list[node, ...nodes])
        }
      | None =>
        if (
          p.token == Eof ||
            p.token == closing ||
            Recover.shouldAbortListParse(p)
        ) {
          List.rev(nodes)
        } else {
          Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
          Parser.next(p)
          loop(nodes)
        }
      }
    
    let nodes = loop(list[])
    Parser.eatBreadcrumb(p)
    nodes
  }
  
  let parseCommaDelimitedReversedList = (p, ~grammar, ~closing, ~f) => {
    Parser.leaveBreadcrumb(p, grammar)
    let rec loop = nodes =>
      switch f(p) {
      | Some(node) =>
        switch p.Parser.token {
        | Comma =>
          Parser.next(p)
          loop(list[node, ...nodes])
        | token when token == closing || token == Eof => list[node, ...nodes]
        | _ =>
          if (
            !(
              p.token == Eof ||
                p.token == closing ||
                Recover.shouldAbortListParse(p)
            )
          ) {
            Parser.expect(Comma, p)
          }
          if p.token == Semicolon {
            Parser.next(p)
          }
          loop(list[node, ...nodes])
        }
      | None =>
        if (
          p.token == Eof ||
            p.token == closing ||
            Recover.shouldAbortListParse(p)
        ) {
          nodes
        } else {
          Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
          Parser.next(p)
          loop(nodes)
        }
      }
    
    let nodes = loop(list[])
    Parser.eatBreadcrumb(p)
    nodes
  }
  
  let parseDelimitedRegion = (p, ~grammar, ~closing, ~f) => {
    Parser.leaveBreadcrumb(p, grammar)
    let rec loop = nodes =>
      switch f(p) {
      | Some(node) => loop(list[node, ...nodes])
      | None =>
        if (
          p.Parser.token == Token.Eof ||
            p.token == closing ||
            Recover.shouldAbortListParse(p)
        ) {
          List.rev(nodes)
        } else {
          Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
          Parser.next(p)
          loop(nodes)
        }
      }
    
    let nodes = loop(list[])
    Parser.eatBreadcrumb(p)
    nodes
  }
  
  let parseRegion = (p, ~grammar, ~f) => {
    Parser.leaveBreadcrumb(p, grammar)
    let rec loop = nodes =>
      switch f(p) {
      | Some(node) => loop(list[node, ...nodes])
      | None =>
        if p.Parser.token == Token.Eof || Recover.shouldAbortListParse(p) {
          List.rev(nodes)
        } else {
          Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
          Parser.next(p)
          loop(nodes)
        }
      }
    
    let nodes = loop(list[])
    Parser.eatBreadcrumb(p)
    nodes
  }
  
  let rec parsePattern = (~alias=true, ~or_=true, p) => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    let pat = switch p.Parser.token {
    | (True | False) as token =>
      let endPos = p.endPos
      Parser.next(p)
      let loc = mkLoc(startPos, endPos)
      Ast_helper.Pat.construct(
        ~loc,
        Location.mkloc(Longident.Lident(Token.toString(token)), loc),
        None,
      )
    | ((Int(_) | String(_)) | Float(_)) | Character(_) =>
      let endPos = p.endPos
      let c = parseConstant(p)
      let loc = mkLoc(startPos, endPos)
      Ast_helper.Pat.constant(~loc, c)
    | Lparen =>
      Parser.next(p)
      switch p.token {
      | Rparen =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let lid = Location.mkloc(Longident.Lident("()"), loc)
        Ast_helper.Pat.construct(~loc, lid, None)
      | _ =>
        let pat = parseConstrainedPattern(p)
        switch p.token {
        | Comma =>
          Parser.next(p)
          parseTuplePattern(~attrs, ~first=pat, ~startPos, p)
        | _ =>
          Parser.expect(Rparen, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          {...pat, ppat_loc: loc}
        }
      }
    | Lbracket => parseArrayPattern(~attrs, p)
    | Lbrace => parseRecordPattern(~attrs, p)
    | Underscore =>
      let endPos = p.endPos
      let loc = mkLoc(startPos, endPos)
      Parser.next(p)
      Ast_helper.Pat.any(~loc, ~attrs, ())
    | Lident(ident) =>
      let endPos = p.endPos
      let loc = mkLoc(startPos, endPos)
      Parser.next(p)
      Ast_helper.Pat.var(~loc, ~attrs, Location.mkloc(ident, loc))
    | Uident(_) =>
      let constr = parseModuleLongIdent(p)
      switch p.Parser.token {
      | Lparen => parseConstructorPatternArgs(p, constr, startPos, attrs)
      | _ => Ast_helper.Pat.construct(~loc=constr.loc, ~attrs, constr, None)
      }
    | Exception =>
      Parser.next(p)
      let pat = parsePattern(~alias=false, ~or_=false, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Pat.exception_(~loc, ~attrs, pat)
    | Lazy =>
      Parser.next(p)
      let pat = parsePattern(~alias=false, ~or_=false, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Pat.lazy_(~loc, ~attrs, pat)
    | List => parseListPattern(~attrs, p)
    | Module => parseModulePattern(~attrs, p)
    | Percent =>
      let extension = parseExtension(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Pat.extension(~loc, ~attrs, extension)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      switch Recover.skipTokensAndMaybeRetry(
        p,
        ~isStartOfGrammar=Grammar.isAtomicPatternStart,
      ) {
      | Abort => Recover.defaultPattern()
      | Retry => parsePattern(p)
      }
    }
    
    let pat = if alias {
      parseAliasPattern(~attrs, pat, p)
    } else {
      pat
    }
    if or_ {
      parseOrPattern(pat, p)
    } else {
      pat
    }
  }
  
  and parseAliasPattern = (~attrs, pattern, p) =>
    switch p.Parser.token {
    | As =>
      Parser.next(p)
      let (name, loc) = parseLident(p)
      let name = Location.mkloc(name, loc)
      Ast_helper.Pat.alias(
        ~loc={...pattern.ppat_loc, loc_end: p.prevEndPos},
        ~attrs,
        pattern,
        name,
      )
    | _ => pattern
    }
  
  and parseOrPattern = (pattern1, p) =>
    switch p.Parser.token {
    | Bar =>
      Parser.next(p)
      let pattern2 = parsePattern(p)
      let loc = {
        ...pattern1.Parsetree.ppat_loc,
        loc_end: pattern2.ppat_loc.loc_end,
      }
      Ast_helper.Pat.or_(~loc, pattern1, pattern2)
    | _ => pattern1
    }
  
  and parseNonSpreadPattern = (~msg, p) => {
    let () = switch p.Parser.token {
    | DotDotDot =>
      Parser.err(p, Diagnostics.message(msg))
      Parser.next(p)
    | _ => ()
    }
    
    switch p.Parser.token {
    | token when Grammar.isPatternStart(token) =>
      let pat = parsePattern(p)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(pat.ppat_loc.loc_start, typ.Parsetree.ptyp_loc.loc_end)
        Some(Ast_helper.Pat.constraint_(~loc, pat, typ))
      | _ => Some(pat)
      }
    | _ => None
    }
  }
  
  and parseConstrainedPattern = p => {
    let pat = parsePattern(p)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let typ = parseTypExpr(p)
      let loc = mkLoc(pat.ppat_loc.loc_start, typ.Parsetree.ptyp_loc.loc_end)
      Ast_helper.Pat.constraint_(~loc, pat, typ)
    | _ => pat
    }
  }
  
  and parseConstrainedPatternRegion = p =>
    switch p.Parser.token {
    | token when Grammar.isPatternStart(token) =>
      Some(parseConstrainedPattern(p))
    | _ => None
    }
  
  and parseRecordPatternField = p => {
    let startPos = p.Parser.startPos
    let label = parseValuePath(p)
    let pattern = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      parsePattern(p)
    | _ =>
      Ast_helper.Pat.var(
        ~loc=label.loc,
        Location.mkloc(Longident.last(label.txt), label.loc),
      )
    }
    
    switch p.token {
    | As =>
      Parser.next(p)
      let (name, loc) = parseLident(p)
      let name = Location.mkloc(name, loc)
      let aliasPattern = Ast_helper.Pat.alias(
        ~loc=mkLoc(startPos, p.prevEndPos),
        pattern,
        name,
      )
      
      (
        Location.mkloc(
          label.txt,
          mkLoc(startPos, aliasPattern.ppat_loc.loc_end),
        ),
        aliasPattern,
      )
    | _ => (label, pattern)
    }
  }
  
  and parseRecordPatternItem = p =>
    switch p.Parser.token {
    | DotDotDot =>
      Parser.next(p)
      Some(true, PatField(parseRecordPatternField(p)))
    | Uident(_) | Lident(_) => Some(false, PatField(parseRecordPatternField(p)))
    | Underscore =>
      Parser.next(p)
      Some(false, PatUnderscore)
    | _ => None
    }
  
  and parseRecordPattern = (~attrs, p) => {
    let startPos = p.startPos
    Parser.expect(Lbrace, p)
    let rawFields = parseCommaDelimitedReversedList(
      p,
      ~grammar=PatternRecord,
      ~closing=Rbrace,
      ~f=parseRecordPatternItem,
    )
    
    Parser.expect(Rbrace, p)
    let (fields, closedFlag) = {
      let (rawFields, flag) = switch rawFields {
      | list[(_hasSpread, PatUnderscore), ...rest] => (rest, Asttypes.Open)
      | rawFields => (rawFields, Asttypes.Closed)
      }
      
      List.fold_left(
        ((fields, flag), curr) => {
          let (hasSpread, field) = curr
          switch field {
          | PatField(field) =>
            if hasSpread {
              let (_, pattern) = field
              Parser.err(
                ~startPos=pattern.Parsetree.ppat_loc.loc_start,
                p,
                Diagnostics.message(ErrorMessages.recordPatternSpread),
              )
            }
            (list[field, ...fields], flag)
          | PatUnderscore => (fields, flag)
          }
        },
        (list[], flag),
        rawFields,
      )
    }
    
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Pat.record(~loc, ~attrs, fields, closedFlag)
  }
  
  and parseTuplePattern = (~attrs, ~first, ~startPos, p) => {
    let patterns = parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.PatternList,
      ~closing=Rparen,
      ~f=parseConstrainedPatternRegion,
    )
    
    Parser.expect(Rparen, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Pat.tuple(~loc, ~attrs, list[first, ...patterns])
  }
  
  and parsePatternRegion = p =>
    switch p.Parser.token {
    | DotDotDot =>
      Parser.next(p)
      Some(true, parseConstrainedPattern(p))
    | token when Grammar.isPatternStart(token) =>
      Some(false, parseConstrainedPattern(p))
    | _ => None
    }
  
  and parseModulePattern = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Module, p)
    Parser.expect(Lparen, p)
    let uident = switch p.token {
    | Uident(uident) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(uident, loc)
    | _ => Location.mknoloc("_")
    }
    
    switch p.token {
    | Colon =>
      let colonStart = p.Parser.startPos
      Parser.next(p)
      let packageTypAttrs = parseAttributes(p)
      let packageType = parsePackageType(
        ~startPos=colonStart,
        ~attrs=packageTypAttrs,
        p,
      )
      Parser.expect(Rparen, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let unpack = Ast_helper.Pat.unpack(~loc=uident.loc, uident)
      Ast_helper.Pat.constraint_(~loc, ~attrs, unpack, packageType)
    | _ =>
      Parser.expect(Rparen, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Pat.unpack(~loc, ~attrs, uident)
    }
  }
  
  and parseListPattern = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(List, p)
    Parser.expect(Lbracket, p)
    let listPatterns = parseCommaDelimitedReversedList(
      p,
      ~grammar=Grammar.PatternOcamlList,
      ~closing=Rbracket,
      ~f=parsePatternRegion,
    )
    
    Parser.expect(Rbracket, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let filterSpread = ((hasSpread, pattern)) =>
      if hasSpread {
        Parser.err(
          ~startPos=pattern.Parsetree.ppat_loc.loc_start,
          p,
          Diagnostics.message(ErrorMessages.listPatternSpread),
        )
        pattern
      } else {
        pattern
      }
    
    switch listPatterns {
    | list[(true, pattern), ...patterns] =>
      let patterns = patterns |> List.map(filterSpread) |> List.rev
      let pat = makeListPattern(loc, patterns, Some(pattern))
      {...pat, ppat_loc: loc}
    | patterns =>
      let patterns = patterns |> List.map(filterSpread) |> List.rev
      let pat = makeListPattern(loc, patterns, None)
      {...pat, ppat_loc: loc}
    }
  }
  
  and parseArrayPattern = (~attrs, p) => {
    let startPos = p.startPos
    Parser.expect(Lbracket, p)
    let patterns = parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.PatternList,
      ~closing=Rbracket,
      ~f=parseNonSpreadPattern(~msg=ErrorMessages.arrayPatternSpread),
    )
    
    Parser.expect(Rbracket, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Pat.array(~loc, ~attrs, patterns)
  }
  
  and parseConstructorPatternArgs = (p, constr, startPos, attrs) => {
    let lparen = p.startPos
    Parser.expect(Lparen, p)
    let args = switch parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.PatternList,
      ~closing=Rparen,
      ~f=parseConstrainedPatternRegion,
    ) {
    | list[pattern] => Some(pattern)
    | patterns =>
      Some(Ast_helper.Pat.tuple(~loc=mkLoc(lparen, p.endPos), patterns))
    }
    
    Parser.expect(Rparen, p)
    Ast_helper.Pat.construct(
      ~loc=mkLoc(startPos, p.prevEndPos),
      ~attrs,
      constr,
      args,
    )
  }
  
  and parseExpr = (~context=OrdinaryExpr, p) => {
    let expr = parseOperandExpr(~context, p)
    let expr = parseBinaryExpr(~context, ~a=expr, p, 1)
    parseTernaryExpr(expr, p)
  }
  
  and parseTernaryExpr = (leftOperand, p) =>
    switch p.Parser.token {
    | Question =>
      Parser.leaveBreadcrumb(p, Grammar.Ternary)
      Parser.next(p)
      let trueBranch = parseExpr(~context=TernaryTrueBranchExpr, p)
      Parser.expect(Colon, p)
      let falseBranch = parseExpr(p)
      Parser.eatBreadcrumb(p)
      let loc = {
        ...leftOperand.Parsetree.pexp_loc,
        loc_start: leftOperand.pexp_loc.loc_start,
        loc_end: falseBranch.Parsetree.pexp_loc.loc_end,
      }
      Ast_helper.Exp.ifthenelse(
        ~attrs=list[ternaryAttr],
        ~loc,
        leftOperand,
        trueBranch,
        Some(falseBranch),
      )
    | _ => leftOperand
    }
  
  and parseEs6ArrowExpression = (~parameters=?, p) => {
    let startPos = p.Parser.startPos
    Parser.leaveBreadcrumb(p, Grammar.Es6ArrowExpr)
    let parameters = switch parameters {
    | Some(params) => params
    | None => parseParameters(p)
    }
    
    let returnType = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      Some(parseTypExpr(~es6Arrow=false, p))
    | _ => None
    }
    
    Parser.expect(EqualGreater, p)
    let body = {
      let expr = parseExpr(p)
      switch returnType {
      | Some(typ) =>
        Ast_helper.Exp.constraint_(
          ~loc=mkLoc(expr.pexp_loc.loc_start, typ.Parsetree.ptyp_loc.loc_end),
          expr,
          typ,
        )
      | None => expr
      }
    }
    
    Parser.eatBreadcrumb(p)
    let endPos = p.prevEndPos
    let arrowExpr = List.fold_right(
      (parameter, expr) =>
        switch parameter {
        | TermParameter(uncurried, attrs, lbl, defaultExpr, pat, startPos) =>
          let attrs = if uncurried {
            list[uncurryAttr, ...attrs]
          } else {
            attrs
          }
          Ast_helper.Exp.fun_(
            ~loc=mkLoc(startPos, endPos),
            ~attrs,
            lbl,
            defaultExpr,
            pat,
            expr,
          )
        | TypeParameter(uncurried, attrs, newtypes, startPos) =>
          let attrs = if uncurried {
            list[uncurryAttr, ...attrs]
          } else {
            attrs
          }
          makeNewtypes(~attrs, ~loc=mkLoc(startPos, endPos), newtypes, expr)
        },
      parameters,
      body,
    )
    
    {...arrowExpr, pexp_loc: {...arrowExpr.pexp_loc, loc_start: startPos}}
  }
  
  and parseParameter = p =>
    if (
      p.Parser.token == Token.Typ ||
        p.token == Tilde ||
        p.token == Dot || Grammar.isPatternStart(p.token)
    ) {
      let startPos = p.Parser.startPos
      let uncurried = Parser.optional(p, Token.Dot)
      
      let attrs = parseAttributes(p)
      if p.Parser.token == Typ {
        Parser.next(p)
        let lidents = parseLidentList(p)
        Some(TypeParameter(uncurried, attrs, lidents, startPos))
      } else {
        let (attrs, lbl, pat) = switch p.Parser.token {
        | Tilde =>
          Parser.next(p)
          let (lblName, _loc) = parseLident(p)
          switch p.Parser.token {
          | (Comma | Equal) | Rparen =>
            let loc = mkLoc(startPos, p.prevEndPos)
            (
              attrs,
              Asttypes.Labelled(lblName),
              Ast_helper.Pat.var(~loc, Location.mkloc(lblName, loc)),
            )
          | Colon =>
            let lblEnd = p.prevEndPos
            Parser.next(p)
            let typ = parseTypExpr(p)
            let loc = mkLoc(startPos, lblEnd)
            let pat = {
              let pat = Ast_helper.Pat.var(~loc, Location.mkloc(lblName, loc))
              let loc = mkLoc(startPos, p.prevEndPos)
              Ast_helper.Pat.constraint_(~loc, pat, typ)
            }
            (attrs, Asttypes.Labelled(lblName), pat)
          | As =>
            Parser.next(p)
            let pat = parseConstrainedPattern(p)
            (attrs, Asttypes.Labelled(lblName), pat)
          | t =>
            Parser.err(p, Diagnostics.unexpected(t, p.breadcrumbs))
            let loc = mkLoc(startPos, p.prevEndPos)
            (
              attrs,
              Asttypes.Labelled(lblName),
              Ast_helper.Pat.var(~loc, Location.mkloc(lblName, loc)),
            )
          }
        | _ =>
          let pattern = parseConstrainedPattern(p)
          let attrs = List.concat(list[attrs, pattern.ppat_attributes])
          (list[], Asttypes.Nolabel, {...pattern, ppat_attributes: attrs})
        }
        
        let parameter = switch p.Parser.token {
        | Equal =>
          Parser.next(p)
          let lbl = switch lbl {
          | Asttypes.Labelled(lblName) => Asttypes.Optional(lblName)
          | Asttypes.Optional(_) as lbl => lbl
          | Asttypes.Nolabel => Asttypes.Nolabel
          }
          
          switch p.Parser.token {
          | Question =>
            Parser.next(p)
            (uncurried, attrs, lbl, None, pat, startPos)
          | _ =>
            let expr = parseExpr(p)
            (uncurried, attrs, lbl, Some(expr), pat, startPos)
          }
        | _ => (uncurried, attrs, lbl, None, pat, startPos)
        }
        
        Some(TermParameter(parameter))
      }
    } else {
      None
    }
  
  and parseParameterList = p => {
    let parameters = parseCommaDelimitedRegion(
      ~grammar=Grammar.ParameterList,
      ~f=parseParameter,
      ~closing=Rparen,
      p,
    )
    
    Parser.expect(Rparen, p)
    parameters
  }
  
  and parseParameters = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Lident(ident) =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.Parser.prevEndPos)
      list[
        TermParameter(
          false,
          list[],
          Asttypes.Nolabel,
          None,
          Ast_helper.Pat.var(~loc, Location.mkloc(ident, loc)),
          startPos,
        ),
      ]
    | Underscore =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.Parser.prevEndPos)
      list[
        TermParameter(
          false,
          list[],
          Asttypes.Nolabel,
          None,
          Ast_helper.Pat.any(~loc, ()),
          startPos,
        ),
      ]
    | Lparen =>
      Parser.next(p)
      switch p.Parser.token {
      | Rparen =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.Parser.prevEndPos)
        let unitPattern = Ast_helper.Pat.construct(
          ~loc,
          Location.mkloc(Longident.Lident("()"), loc),
          None,
        )
        
        list[
          TermParameter(
            false,
            list[],
            Asttypes.Nolabel,
            None,
            unitPattern,
            startPos,
          ),
        ]
      | Dot =>
        Parser.next(p)
        switch p.token {
        | Rparen =>
          Parser.next(p)
          let loc = mkLoc(startPos, p.Parser.prevEndPos)
          let unitPattern = Ast_helper.Pat.construct(
            ~loc,
            Location.mkloc(Longident.Lident("()"), loc),
            None,
          )
          
          list[
            TermParameter(
              true,
              list[],
              Asttypes.Nolabel,
              None,
              unitPattern,
              startPos,
            ),
          ]
        | _ =>
          switch parseParameterList(p) {
          | list[
              TermParameter(_, attrs, lbl, defaultExpr, pattern, startPos),
              ...rest,
            ] =>
            list[
              TermParameter(true, attrs, lbl, defaultExpr, pattern, startPos),
              ...rest,
            ]
          | parameters => parameters
          }
        }
      | _ => parseParameterList(p)
      }
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      list[]
    }
  }
  
  and parseConstrainedExpr = p => {
    let expr = parseExpr(p)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let typ = parseTypExpr(p)
      let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
      Ast_helper.Exp.constraint_(~loc, expr, typ)
    | _ => expr
    }
  }
  
  and parseConstrainedExprRegion = p =>
    switch p.Parser.token {
    | token when Grammar.isExprStart(token) =>
      let expr = parseExpr(p)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
        Some(Ast_helper.Exp.constraint_(~loc, expr, typ))
      | _ => Some(expr)
      }
    | _ => None
    }
  
  and parseAtomicExpr = p => {
    Parser.leaveBreadcrumb(p, Grammar.ExprOperand)
    let startPos = p.Parser.startPos
    let expr = switch p.Parser.token {
    | (True | False) as token =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.construct(
        ~loc,
        Location.mkloc(Longident.Lident(Token.toString(token)), loc),
        None,
      )
    | ((Int(_) | String(_)) | Float(_)) | Character(_) =>
      let c = parseConstant(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.constant(~loc, c)
    | Backtick => parseTemplateExpr(p)
    | Uident(_) | Lident(_) => parseValueOrConstructor(p)
    | Lparen =>
      Parser.next(p)
      switch p.Parser.token {
      | Rparen =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Exp.construct(
          ~loc,
          Location.mkloc(Longident.Lident("()"), loc),
          None,
        )
      | t =>
        let expr = parseConstrainedExpr(p)
        switch p.token {
        | Comma =>
          Parser.next(p)
          parseTupleExpr(~startPos, ~first=expr, p)
        | _ =>
          Parser.expect(Rparen, p)
          {...expr, pexp_loc: mkLoc(startPos, p.startPos)}
        }
      }
    | List => parseListExpr(p)
    | Module =>
      Parser.next(p)
      parseFirstClassModuleExpr(~startPos, p)
    | Lbracket => parseArrayExp(p)
    | Lbrace => parseBracedOrRecordExpr(p)
    | LessThan => parseJsx(p)
    | Percent =>
      let extension = parseExtension(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.extension(~loc, extension)
    | token =>
      let errPos = p.prevEndPos
      switch Recover.skipTokensAndMaybeRetry(
        p,
        ~isStartOfGrammar=Grammar.isAtomicExprStart,
      ) {
      | Abort =>
        Parser.err(
          ~startPos=errPos,
          p,
          Diagnostics.unexpected(token, p.breadcrumbs),
        )
        Recover.defaultExpr()
      | Retry => parseAtomicExpr(p)
      }
    }
    
    Parser.eatBreadcrumb(p)
    expr
  }
  
  and parseFirstClassModuleExpr = (~startPos, p) => {
    Parser.expect(Lparen, p)
    
    let modExpr = parseModuleExpr(p)
    let modEndLoc = p.prevEndPos
    switch p.Parser.token {
    | Colon =>
      let colonStart = p.Parser.startPos
      Parser.next(p)
      let attrs = parseAttributes(p)
      let packageType = parsePackageType(~startPos=colonStart, ~attrs, p)
      Parser.expect(Rparen, p)
      let loc = mkLoc(startPos, modEndLoc)
      let firstClassModule = Ast_helper.Exp.pack(~loc, modExpr)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.constraint_(~loc, firstClassModule, packageType)
    | _ =>
      Parser.expect(Rparen, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.pack(~loc, modExpr)
    }
  }
  
  and parseBracketAccess = (p, expr, startPos) => {
    Parser.leaveBreadcrumb(p, Grammar.ExprArrayAccess)
    let lbracket = p.startPos
    Parser.next(p)
    let stringStart = p.startPos
    switch p.Parser.token {
    | String(s) =>
      Parser.next(p)
      let stringEnd = p.prevEndPos
      Parser.expect(Rbracket, p)
      let rbracket = p.prevEndPos
      let e = {
        let identLoc = mkLoc(stringStart, stringEnd)
        let loc = mkLoc(lbracket, rbracket)
        Ast_helper.Exp.apply(
          ~loc,
          Ast_helper.Exp.ident(
            ~loc,
            Location.mkloc(Longident.Lident("##"), loc),
          ),
          list[
            (Nolabel, expr),
            (
              Nolabel,
              Ast_helper.Exp.ident(
                ~loc=identLoc,
                Location.mkloc(Longident.Lident(s), identLoc),
              ),
            ),
          ],
        )
      }
      
      let e = parsePrimaryExpr(~operand=e, p)
      let equalStart = p.startPos
      switch p.token {
      | Equal =>
        Parser.next(p)
        let equalEnd = p.prevEndPos
        let rhsExpr = parseExpr(p)
        let loc = mkLoc(startPos, rhsExpr.pexp_loc.loc_end)
        let operatorLoc = mkLoc(equalStart, equalEnd)
        Ast_helper.Exp.apply(
          ~loc,
          Ast_helper.Exp.ident(
            ~loc=operatorLoc,
            Location.mkloc(Longident.Lident("#="), operatorLoc),
          ),
          list[(Nolabel, e), (Nolabel, rhsExpr)],
        )
      | _ => e
      }
    | _ =>
      let accessExpr = parseConstrainedExpr(p)
      Parser.expect(Rbracket, p)
      let rbracket = p.prevEndPos
      let arrayLoc = mkLoc(lbracket, rbracket)
      switch p.token {
      | Equal =>
        Parser.leaveBreadcrumb(p, ExprArrayMutation)
        Parser.next(p)
        let rhsExpr = parseExpr(p)
        let arraySet = Location.mkloc(
          Longident.Ldot(Lident("Array"), "set"),
          arrayLoc,
        )
        
        let endPos = p.prevEndPos
        let arraySet = Ast_helper.Exp.apply(
          ~loc=mkLoc(startPos, endPos),
          Ast_helper.Exp.ident(~loc=arrayLoc, arraySet),
          list[(Nolabel, expr), (Nolabel, accessExpr), (Nolabel, rhsExpr)],
        )
        
        Parser.eatBreadcrumb(p)
        arraySet
      | _ =>
        let endPos = p.prevEndPos
        let e = Ast_helper.Exp.apply(
          ~loc=mkLoc(startPos, endPos),
          Ast_helper.Exp.ident(
            ~loc=arrayLoc,
            Location.mkloc(Longident.Ldot(Lident("Array"), "get"), arrayLoc),
          ),
          list[(Nolabel, expr), (Nolabel, accessExpr)],
        )
        
        Parser.eatBreadcrumb(p)
        parsePrimaryExpr(~operand=e, p)
      }
    }
  }
  
  and parsePrimaryExpr = (~operand, ~noCall=false, p) => {
    let startPos = operand.pexp_loc.loc_start
    let rec loop = (p, expr) =>
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        let lident = parseValuePath(p)
        switch p.Parser.token {
        | Equal when noCall == false =>
          Parser.leaveBreadcrumb(p, Grammar.ExprSetField)
          Parser.next(p)
          let endPos = p.prevEndPos
          let loc = mkLoc(startPos, endPos)
          let setfield = Ast_helper.Exp.setfield(
            ~loc,
            expr,
            lident,
            parseExpr(p),
          )
          Parser.eatBreadcrumb(p)
          setfield
        | _ =>
          let endPos = p.prevEndPos
          let loc = mkLoc(startPos, endPos)
          loop(p, Ast_helper.Exp.field(~loc, expr, lident))
        }
      | Lbracket
        when noCall == false && p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
        parseBracketAccess(p, expr, startPos)
      | Lparen
        when noCall == false && p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
        loop(p, parseCallExpr(p, expr))
      | _ => expr
      }
    
    loop(p, operand)
  }
  
  and parseUnaryExpr = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | (((((Minus | MinusDot) | Plus) | PlusDot) | Bang) | Band) as token =>
      Parser.leaveBreadcrumb(p, Grammar.ExprUnary)
      let tokenEnd = p.endPos
      Parser.next(p)
      let operand = parseUnaryExpr(p)
      let unaryExpr = makeUnaryExpr(startPos, tokenEnd, token, operand)
      Parser.eatBreadcrumb(p)
      unaryExpr
    | _ => parsePrimaryExpr(~operand=parseAtomicExpr(p), p)
    }
  }
  
  and parseOperandExpr = (~context=OrdinaryExpr, p) => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    let expr = switch p.Parser.token {
    | Assert =>
      Parser.next(p)
      let expr = parseUnaryExpr(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.assert_(~loc, expr)
    | Lazy =>
      Parser.next(p)
      let expr = parseUnaryExpr(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.lazy_(~loc, expr)
    | Try => parseTryExpression(p)
    | If => parseIfExpression(p)
    | For => parseForExpression(p)
    | While => parseWhileExpression(p)
    | Switch => parseSwitchExpression(p)
    | _ =>
      if (
        context !== WhenExpr &&
          isEs6ArrowExpression(~inTernary=context == TernaryTrueBranchExpr, p)
      ) {
        parseEs6ArrowExpression(p)
      } else {
        parseUnaryExpr(p)
      }
    }
    
    {
      ...expr,
      pexp_attributes: List.concat(list[expr.Parsetree.pexp_attributes, attrs]),
    }
  }
  
  and parseBinaryExpr = (~context=OrdinaryExpr, ~a=?, p, prec) => {
    let a = switch a {
    | Some(e) => e
    | None => parseOperandExpr(~context, p)
    }
    
    let rec loop = a => {
      let token = p.Parser.token
      let tokenPrec = switch token {
      | Minus | MinusDot
        when !Scanner.isBinaryOp(
          p.scanner.src,
          p.startPos.pos_cnum,
          p.endPos.pos_cnum,
        ) &&
        p.startPos.pos_lnum > p.prevEndPos.pos_lnum =>
        -1
      | token => Token.precedence(token)
      }
      
      if tokenPrec < prec {
        a
      } else {
        Parser.leaveBreadcrumb(p, Grammar.ExprBinaryAfterOp(token))
        let startPos = p.startPos
        Parser.next(p)
        let endPos = p.prevEndPos
        let b = parseBinaryExpr(~context, p, tokenPrec + 1)
        let loc = mkLoc(a.Parsetree.pexp_loc.loc_start, b.pexp_loc.loc_end)
        let expr = Ast_helper.Exp.apply(
          ~loc,
          makeInfixOperator(p, token, startPos, endPos),
          list[(Nolabel, a), (Nolabel, b)],
        )
        
        loop(expr)
      }
    }
    
    loop(a)
  }
  
  and parseTemplateExpr = p => {
    let hiddenOperator = {
      let op = Location.mknoloc(Longident.Lident("^"))
      Ast_helper.Exp.ident(op)
    }
    
    let rec loop = (acc, p) =>
      switch p.Parser.token {
      | TemplateTail(txt) =>
        Parser.next(p)
        if String.length(txt) > 0 {
          let str = Ast_helper.Exp.constant(Pconst_string(txt, Some("j")))
          Ast_helper.Exp.apply(
            hiddenOperator,
            list[(Nolabel, acc), (Nolabel, str)],
          )
        } else {
          acc
        }
      | TemplatePart(txt) =>
        Parser.next(p)
        let expr = parseExprBlock(p)
        Scanner.setTemplateMode(p.scanner)
        Parser.expect(Rbrace, p)
        let str = Ast_helper.Exp.constant(Pconst_string(txt, Some("j")))
        let next = {
          let a = if String.length(txt) > 0 {
            Ast_helper.Exp.apply(
              hiddenOperator,
              list[(Nolabel, acc), (Nolabel, str)],
            )
          } else {
            acc
          }
          
          Ast_helper.Exp.apply(
            hiddenOperator,
            list[(Nolabel, a), (Nolabel, expr)],
          )
        }
        
        loop(next, p)
      | token =>
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        acc
      }
    
    Scanner.setTemplateMode(p.scanner)
    Parser.expect(Backtick, p)
    switch p.Parser.token {
    | TemplateTail(txt) =>
      Parser.next(p)
      Ast_helper.Exp.constant(Pconst_string(txt, Some("j")))
    | TemplatePart(txt) =>
      Parser.next(p)
      let expr = parseExprBlock(p)
      Scanner.setTemplateMode(p.scanner)
      Parser.expect(Rbrace, p)
      let str = Ast_helper.Exp.constant(Pconst_string(txt, Some("j")))
      let next = if String.length(txt) > 0 {
        Ast_helper.Exp.apply(
          hiddenOperator,
          list[(Nolabel, str), (Nolabel, expr)],
        )
      } else {
        expr
      }
      
      loop(next, p)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Ast_helper.Exp.constant(Pconst_string("", None))
    }
  }
  
  and parseLetBindingBody = (~startPos, ~attrs, p) => {
    Parser.leaveBreadcrumb(p, Grammar.LetBinding)
    let (pat, exp) = {
      let pat = parsePattern(p)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        switch p.token {
        | Typ =>
          Parser.next(p)
          let newtypes = parseLidentList(p)
          Parser.expect(Dot, p)
          let typ = parseTypExpr(p)
          Parser.expect(Equal, p)
          let expr = parseExpr(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let (exp, poly) = wrap_type_annotation(~loc, newtypes, typ, expr)
          let pat = Ast_helper.Pat.constraint_(~loc, pat, poly)
          (pat, exp)
        | _ =>
          let polyType = parsePolyTypeExpr(p)
          let loc = {
            ...pat.ppat_loc,
            loc_end: polyType.Parsetree.ptyp_loc.loc_end,
          }
          let pat = Ast_helper.Pat.constraint_(~loc, pat, polyType)
          Parser.expect(Token.Equal, p)
          let exp = parseExpr(p)
          (pat, exp)
        }
      | _ =>
        Parser.expect(Token.Equal, p)
        let exp = parseExpr(p)
        (pat, exp)
      }
    }
    
    let loc = mkLoc(startPos, p.prevEndPos)
    let vb = Ast_helper.Vb.mk(~loc, ~attrs, pat, exp)
    Parser.eatBreadcrumb(p)
    vb
  }
  
  and parseAttributesAndBinding = (p: Parser.t) => {
    let err = p.scanner.err
    let ch = p.scanner.ch
    let offset = p.scanner.offset
    let rdOffset = p.scanner.rdOffset
    let lineOffset = p.scanner.lineOffset
    let lnum = p.scanner.lnum
    let mode = p.scanner.mode
    let token = p.token
    let startPos = p.startPos
    let endPos = p.endPos
    let prevEndPos = p.prevEndPos
    let breadcrumbs = p.breadcrumbs
    let errors = p.errors
    let diagnostics = p.diagnostics
    let comments = p.comments
    
    switch p.Parser.token {
    | At =>
      let attrs = parseAttributes(p)
      switch p.Parser.token {
      | And => attrs
      | _ =>
        p.scanner.err = err
        p.scanner.ch = ch
        p.scanner.offset = offset
        p.scanner.rdOffset = rdOffset
        p.scanner.lineOffset = lineOffset
        p.scanner.lnum = lnum
        p.scanner.mode = mode
        p.token = token
        p.startPos = startPos
        p.endPos = endPos
        p.prevEndPos = prevEndPos
        p.breadcrumbs = breadcrumbs
        p.errors = errors
        p.diagnostics = diagnostics
        p.comments = comments
        list[]
      }
    | _ => list[]
    }
  }
  
  and parseLetBindings = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.optional(p, Let) |> ignore
    let recFlag = if Parser.optional(p, Token.Rec) {
      Asttypes.Recursive
    } else {
      Asttypes.Nonrecursive
    }
    
    let first = parseLetBindingBody(~startPos, ~attrs, p)
    
    let rec loop = (p, bindings) => {
      let startPos = p.Parser.startPos
      let attrs = parseAttributesAndBinding(p)
      switch p.Parser.token {
      | And =>
        Parser.next(p)
        let attrs = switch p.token {
        | Export =>
          let exportLoc = mkLoc(p.startPos, p.endPos)
          Parser.next(p)
          let genTypeAttr = (
            Location.mkloc("genType", exportLoc),
            Parsetree.PStr(list[]),
          )
          list[genTypeAttr, ...attrs]
        | _ => attrs
        }
        
        ignore(Parser.optional(p, Let))
        let letBinding = parseLetBindingBody(~startPos, ~attrs, p)
        loop(p, list[letBinding, ...bindings])
      | _ => List.rev(bindings)
      }
    }
    
    (recFlag, loop(p, list[first]))
  }
  
  and parseJsxName = p => {
    let longident = switch p.Parser.token {
    | Lident(ident) =>
      let identStart = p.startPos
      let identEnd = p.endPos
      Parser.next(p)
      let loc = mkLoc(identStart, identEnd)
      Location.mkloc(Longident.Lident(ident), loc)
    | Uident(_) =>
      let longident = parseModuleLongIdent(p)
      Location.mkloc(
        Longident.Ldot(longident.txt, "createElement"),
        longident.loc,
      )
    | _ =>
      let msg = "A jsx name should start with a lowercase or uppercase identifier, like: div in <div /> or Navbar in <Navbar />"
      
      Parser.err(p, Diagnostics.message(msg))
      Location.mknoloc(Longident.Lident("_"))
    }
    
    Ast_helper.Exp.ident(~loc=longident.loc, longident)
  }
  
  and parseJsxOpeningOrSelfClosingElement = (~startPos, p) => {
    let jsxStartPos = p.Parser.startPos
    let name = parseJsxName(p)
    let jsxProps = parseJsxProps(p)
    let children = switch p.Parser.token {
    | Forwardslash =>
      let childrenStartPos = p.Parser.startPos
      Parser.next(p)
      let childrenEndPos = p.Parser.startPos
      Parser.expect(GreaterThan, p)
      let loc = mkLoc(childrenStartPos, childrenEndPos)
      makeListExpression(loc, list[], None)
    | GreaterThan =>
      let childrenStartPos = p.Parser.startPos
      Scanner.setJsxMode(p.scanner)
      Parser.next(p)
      let (spread, children) = parseJsxChildren(p)
      let childrenEndPos = p.Parser.startPos
      let () = switch p.token {
      | LessThanSlash => Parser.next(p)
      | LessThan =>
        Parser.next(p)
        Parser.expect(Forwardslash, p)
      | token when Grammar.isStructureItemStart(token) => ()
      | _ => Parser.expect(LessThanSlash, p)
      }
      
      switch p.Parser.token {
      | Lident(_) | Uident(_) when verifyJsxOpeningClosingName(p, name) =>
        Parser.expect(GreaterThan, p)
        let loc = mkLoc(childrenStartPos, childrenEndPos)
        if spread {
          List.hd(children)
        } else {
          makeListExpression(loc, children, None)
        }
      | token =>
        let () = if Grammar.isStructureItemStart(token) {
          let closing = "</" ++ string_of_pexp_ident(name) ++ ">"
          let msg = Diagnostics.message("Missing " ++ closing)
          Parser.err(~startPos, ~endPos=p.prevEndPos, p, msg)
        } else {
          let opening = "</" ++ string_of_pexp_ident(name) ++ ">"
          let msg =
            "Closing jsx name should be the same as the opening name. Did you mean " ++
            opening ++
            " ?"
          Parser.err(
            ~startPos,
            ~endPos=p.prevEndPos,
            p,
            Diagnostics.message(msg),
          )
          Parser.expect(GreaterThan, p)
        }
        
        let loc = mkLoc(childrenStartPos, childrenEndPos)
        if spread {
          List.hd(children)
        } else {
          makeListExpression(loc, children, None)
        }
      }
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      makeListExpression(Location.none, list[], None)
    }
    
    let jsxEndPos = p.prevEndPos
    let loc = mkLoc(jsxStartPos, jsxEndPos)
    Ast_helper.Exp.apply(
      ~loc,
      name,
      List.concat(list[
        jsxProps,
        list[
          (Asttypes.Labelled("children"), children),
          (
            Asttypes.Nolabel,
            Ast_helper.Exp.construct(
              Location.mknoloc(Longident.Lident("()")),
              None,
            ),
          ),
        ],
      ]),
    )
  }
  
  and parseJsx = p => {
    Parser.leaveBreadcrumb(p, Grammar.Jsx)
    let startPos = p.Parser.startPos
    Parser.expect(LessThan, p)
    let jsxExpr = switch p.Parser.token {
    | Lident(_) | Uident(_) => parseJsxOpeningOrSelfClosingElement(~startPos, p)
    | GreaterThan => parseJsxFragment(p)
    | _ => parseJsxName(p)
    }
    
    {...jsxExpr, pexp_attributes: list[jsxAttr]}
  }
  
  and parseJsxFragment = p => {
    let childrenStartPos = p.Parser.startPos
    Scanner.setJsxMode(p.scanner)
    Parser.expect(GreaterThan, p)
    let (_spread, children) = parseJsxChildren(p)
    let childrenEndPos = p.Parser.startPos
    Parser.expect(LessThanSlash, p)
    Parser.expect(GreaterThan, p)
    let loc = mkLoc(childrenStartPos, childrenEndPos)
    makeListExpression(loc, children, None)
  }
  
  and parseJsxProp = p => {
    Parser.leaveBreadcrumb(p, Grammar.JsxAttribute)
    switch p.Parser.token {
    | Question | Lident(_) =>
      let optional = Parser.optional(p, Question)
      let (name, loc) = parseLident(p)
      let propLocAttr = (
        Location.mkloc("ns.jsxPropLoc", loc),
        Parsetree.PStr(list[]),
      )
      
      if optional {
        Some(
          Asttypes.Optional(name),
          Ast_helper.Exp.ident(
            ~attrs=list[propLocAttr],
            ~loc,
            Location.mkloc(Longident.Lident(name), loc),
          ),
        )
      } else {
        switch p.Parser.token {
        | Equal =>
          Parser.next(p)
          
          let optional = Parser.optional(p, Question)
          let attrExpr = {
            let e = parsePrimaryExpr(~operand=parseAtomicExpr(p), p)
            {...e, pexp_attributes: list[propLocAttr, ...e.pexp_attributes]}
          }
          
          let label = if optional {
            Asttypes.Optional(name)
          } else {
            Asttypes.Labelled(name)
          }
          
          Some(label, attrExpr)
        | _ =>
          let attrExpr = Ast_helper.Exp.ident(
            ~loc,
            ~attrs=list[propLocAttr],
            Location.mknoloc(Longident.Lident(name)),
          )
          let label = if optional {
            Asttypes.Optional(name)
          } else {
            Asttypes.Labelled(name)
          }
          
          Some(label, attrExpr)
        }
      }
    | _ => None
    }
  }
  
  and parseJsxProps = p =>
    parseRegion(~grammar=Grammar.JsxAttribute, ~f=parseJsxProp, p)
  
  and parseJsxChildren = p => {
    let rec loop = (p, children) =>
      switch p.Parser.token {
      | Token.Eof | LessThanSlash =>
        Scanner.popMode(p.scanner, Jsx)
        List.rev(children)
      | LessThan =>
        let token = Scanner.reconsiderLessThan(p.scanner)
        if token == LessThan {
          let child = parsePrimaryExpr(
            ~operand=parseAtomicExpr(p),
            ~noCall=true,
            p,
          )
          loop(p, list[child, ...children])
        } else {
          let () = p.token = token
          let () = Scanner.popMode(p.scanner, Jsx)
          List.rev(children)
        }
      | token when Grammar.isJsxChildStart(token) =>
        let child = parsePrimaryExpr(
          ~operand=parseAtomicExpr(p),
          ~noCall=true,
          p,
        )
        loop(p, list[child, ...children])
      | _ =>
        Scanner.popMode(p.scanner, Jsx)
        List.rev(children)
      }
    
    switch p.Parser.token {
    | DotDotDot =>
      Parser.next(p)
      (
        true,
        list[parsePrimaryExpr(~operand=parseAtomicExpr(p), ~noCall=true, p)],
      )
    | _ => (false, loop(p, list[]))
    }
  }
  
  and parseBracedOrRecordExpr = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lbrace, p)
    let expr = switch p.Parser.token {
    | Rbrace =>
      Parser.err(p, Diagnostics.unexpected(Rbrace, p.breadcrumbs))
      let loc = mkLoc(p.prevEndPos, p.endPos)
      Ast_helper.Exp.construct(
        ~loc,
        Location.mkloc(Longident.Lident("()"), loc),
        None,
      )
    | DotDotDot =>
      Parser.next(p)
      let spreadExpr = parseConstrainedExpr(p)
      Parser.expect(Comma, p)
      parseRecordExpr(~startPos, ~spread=Some(spreadExpr), list[], p)
    | String(s) =>
      let field = {
        let loc = mkLoc(p.startPos, p.endPos)
        Parser.next(p)
        Location.mkloc(Longident.Lident(s), loc)
      }
      
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let fieldExpr = parseExpr(p)
        Parser.optional(p, Comma) |> ignore
        parseRecordExprWithStringKeys(~startPos, (field, fieldExpr), p)
      | _ =>
        let constant = Ast_helper.Exp.constant(
          ~loc=field.loc,
          Parsetree.Pconst_string(s, None),
        )
        let a = parsePrimaryExpr(~operand=constant, p)
        let e = parseBinaryExpr(~a, p, 1)
        let e = parseTernaryExpr(e, p)
        switch p.Parser.token {
        | Semicolon =>
          Parser.next(p)
          parseExprBlock(~first=e, p)
        | Rbrace => e
        | _ => parseExprBlock(~first=e, p)
        }
      }
    | Uident(_) | Lident(_) =>
      let valueOrConstructor = parseValueOrConstructor(p)
      switch valueOrConstructor.pexp_desc {
      | Pexp_ident(pathIdent) =>
        let identEndPos = p.prevEndPos
        switch p.Parser.token {
        | Comma =>
          Parser.next(p)
          parseRecordExpr(~startPos, list[(pathIdent, valueOrConstructor)], p)
        | Colon =>
          Parser.next(p)
          let fieldExpr = parseExpr(p)
          switch p.token {
          | Rbrace =>
            let loc = mkLoc(startPos, p.endPos)
            Ast_helper.Exp.record(~loc, list[(pathIdent, fieldExpr)], None)
          | _ =>
            Parser.expect(Comma, p)
            parseRecordExpr(~startPos, list[(pathIdent, fieldExpr)], p)
          }
        
        | Lident(_) =>
          if p.prevEndPos.pos_lnum < p.startPos.pos_lnum {
            Parser.expect(Comma, p)
            parseRecordExpr(~startPos, list[(pathIdent, valueOrConstructor)], p)
          } else {
            Parser.expect(Colon, p)
            parseRecordExpr(~startPos, list[(pathIdent, valueOrConstructor)], p)
          }
        | Semicolon =>
          Parser.next(p)
          parseExprBlock(~first=Ast_helper.Exp.ident(pathIdent), p)
        | Rbrace => Ast_helper.Exp.ident(~loc=pathIdent.loc, pathIdent)
        | EqualGreater =>
          let loc = mkLoc(startPos, identEndPos)
          let ident = Location.mkloc(Longident.last(pathIdent.txt), loc)
          let a = parseEs6ArrowExpression(
            ~parameters=list[
              TermParameter(
                false,
                list[],
                Asttypes.Nolabel,
                None,
                Ast_helper.Pat.var(ident),
                startPos,
              ),
            ],
            p,
          )
          
          let e = parseBinaryExpr(~a, p, 1)
          let e = parseTernaryExpr(e, p)
          switch p.Parser.token {
          | Semicolon =>
            Parser.next(p)
            parseExprBlock(~first=e, p)
          | Rbrace => e
          | _ => parseExprBlock(~first=e, p)
          }
        | _ =>
          let a = parsePrimaryExpr(
            ~operand=Ast_helper.Exp.ident(~loc=pathIdent.loc, pathIdent),
            p,
          )
          let e = parseBinaryExpr(~a, p, 1)
          let e = parseTernaryExpr(e, p)
          switch p.Parser.token {
          | Semicolon =>
            Parser.next(p)
            parseExprBlock(~first=e, p)
          | Rbrace => e
          | _ => parseExprBlock(~first=e, p)
          }
        }
      | _ =>
        let a = parsePrimaryExpr(~operand=valueOrConstructor, p)
        let e = parseBinaryExpr(~a, p, 1)
        let e = parseTernaryExpr(e, p)
        switch p.Parser.token {
        | Semicolon =>
          Parser.next(p)
          parseExprBlock(~first=e, p)
        | Rbrace => e
        | _ => parseExprBlock(~first=e, p)
        }
      }
    | _ => parseExprBlock(p)
    }
    
    Parser.expect(Rbrace, p)
    expr
  }
  
  and parseRecordRowWithStringKey = p =>
    switch p.Parser.token {
    | String(s) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      let field = Location.mkloc(Longident.Lident(s), loc)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let fieldExpr = parseExpr(p)
        Some(field, fieldExpr)
      | _ => Some(field, Ast_helper.Exp.ident(~loc=field.loc, field))
      }
    | _ => None
    }
  
  and parseRecordRow = p => {
    let () = switch p.Parser.token {
    | Token.DotDotDot =>
      Parser.err(p, Diagnostics.message(ErrorMessages.recordExprSpread))
      Parser.next(p)
    | _ => ()
    }
    
    switch p.Parser.token {
    | (Lident(_) | Uident(_)) | List =>
      let field = parseValuePath(p)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let fieldExpr = parseExpr(p)
        Some(field, fieldExpr)
      | _ => Some(field, Ast_helper.Exp.ident(~loc=field.loc, field))
      }
    | _ => None
    }
  }
  
  and parseRecordExprWithStringKeys = (~startPos, firstRow, p) => {
    let rows = list[
      firstRow,
      ...parseCommaDelimitedRegion(
        ~grammar=Grammar.RecordRowsStringKey,
        ~closing=Rbrace,
        ~f=parseRecordRowWithStringKey,
        p,
      ),
    ]
    let loc = mkLoc(startPos, p.endPos)
    let recordStrExpr = Ast_helper.Str.eval(
      ~loc,
      Ast_helper.Exp.record(~loc, rows, None),
    )
    Ast_helper.Exp.extension(
      ~loc,
      (Location.mkloc("bs.obj", loc), Parsetree.PStr(list[recordStrExpr])),
    )
  }
  
  and parseRecordExpr = (~startPos, ~spread=None, rows, p) => {
    let exprs = parseCommaDelimitedRegion(
      ~grammar=Grammar.RecordRows,
      ~closing=Rbrace,
      ~f=parseRecordRow,
      p,
    )
    
    let rows = List.concat(list[rows, exprs])
    let () = switch rows {
    | list[] =>
      let msg = "Record spread needs at least one field that's updated"
      Parser.err(p, Diagnostics.message(msg))
    | rows => ()
    }
    
    let loc = mkLoc(startPos, p.endPos)
    Ast_helper.Exp.record(~loc, rows, spread)
  }
  
  and parseExprBlockItem = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Module =>
      Parser.next(p)
      switch p.token {
      | Lparen => parseFirstClassModuleExpr(~startPos, p)
      | _ =>
        let name = switch p.Parser.token {
        | Uident(ident) =>
          let loc = mkLoc(p.startPos, p.endPos)
          Parser.next(p)
          Location.mkloc(ident, loc)
        | t =>
          Parser.err(p, Diagnostics.uident(t))
          Location.mknoloc("_")
        }
        
        let body = parseModuleBindingBody(p)
        Parser.optional(p, Semicolon) |> ignore
        let expr = parseExprBlock(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Exp.letmodule(~loc, name, body, expr)
      }
    | Exception =>
      let extensionConstructor = parseExceptionDef(~attrs=list[], p)
      Parser.optional(p, Semicolon) |> ignore
      let blockExpr = parseExprBlock(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.letexception(~loc, extensionConstructor, blockExpr)
    | Open =>
      let od = parseOpenDescription(~attrs=list[], p)
      Parser.optional(p, Semicolon) |> ignore
      let blockExpr = parseExprBlock(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.open_(~loc, od.popen_override, od.popen_lid, blockExpr)
    | Let =>
      let (recFlag, letBindings) = parseLetBindings(~attrs=list[], p)
      let next = switch p.Parser.token {
      | Semicolon =>
        Parser.next(p)
        if Grammar.isBlockExprStart(p.Parser.token) {
          parseExprBlock(p)
        } else {
          let loc = mkLoc(p.startPos, p.endPos)
          Ast_helper.Exp.construct(
            ~loc,
            Location.mkloc(Longident.Lident("()"), loc),
            None,
          )
        }
      | token when Grammar.isBlockExprStart(token) => parseExprBlock(p)
      | _ =>
        let loc = mkLoc(p.startPos, p.endPos)
        Ast_helper.Exp.construct(
          ~loc,
          Location.mkloc(Longident.Lident("()"), loc),
          None,
        )
      }
      
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.let_(~loc, recFlag, letBindings, next)
    | _ =>
      let e1 = parseExpr(p)
      ignore(Parser.optional(p, Semicolon))
      if Grammar.isBlockExprStart(p.Parser.token) {
        let fakeUnitPat = {
          let unitLid = Location.mknoloc(Longident.Lident("()"))
          Ast_helper.Pat.construct(unitLid, None)
        }
        
        let e2 = parseExprBlock(p)
        let vb = Ast_helper.Vb.mk(~loc=e1.pexp_loc, fakeUnitPat, e1)
        let loc = {...e1.pexp_loc, loc_end: e2.pexp_loc.loc_end}
        Ast_helper.Exp.let_(~loc, Asttypes.Nonrecursive, list[vb], e2)
      } else {
        e1
      }
    }
  }
  
  and parseExprBlock = (~first=?, p) => {
    Parser.leaveBreadcrumb(p, Grammar.ExprBlock)
    let item = switch first {
    | Some(e) => e
    | None => parseExprBlockItem(p)
    }
    
    let blockExpr = switch p.Parser.token {
    | Semicolon =>
      Parser.next(p)
      if Grammar.isBlockExprStart(p.Parser.token) {
        let next = parseExprBlockItem(p)
        ignore(Parser.optional(p, Semicolon))
        let fakeUnitPat = {
          let unitLid = Location.mknoloc(Longident.Lident("()"))
          Ast_helper.Pat.construct(unitLid, None)
        }
        
        let vb = Ast_helper.Vb.mk(~loc=item.pexp_loc, fakeUnitPat, item)
        let loc = {...vb.pvb_loc, loc_end: next.pexp_loc.loc_end}
        Ast_helper.Exp.let_(~loc, Asttypes.Nonrecursive, list[vb], next)
      } else {
        item
      }
    | token when Grammar.isBlockExprStart(token) =>
      let next = parseExprBlockItem(p)
      ignore(Parser.optional(p, Semicolon))
      let fakeUnitPat = {
        let unitLid = Location.mknoloc(Longident.Lident("()"))
        Ast_helper.Pat.construct(unitLid, None)
      }
      
      let vb = Ast_helper.Vb.mk(~loc=item.pexp_loc, fakeUnitPat, item)
      let loc = {...vb.pvb_loc, loc_end: next.pexp_loc.loc_end}
      Ast_helper.Exp.let_(~loc, Asttypes.Nonrecursive, list[vb], next)
    | _ => item
    }
    
    Parser.eatBreadcrumb(p)
    blockExpr
  }
  
  and parseTryExpression = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Try, p)
    let expr = parseExpr(~context=WhenExpr, p)
    Parser.expect(Catch, p)
    Parser.expect(Lbrace, p)
    let cases = parsePatternMatching(p)
    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.try_(~loc, expr, cases)
  }
  
  and parseIfExpression = p => {
    Parser.leaveBreadcrumb(p, Grammar.ExprIf)
    let startPos = p.Parser.startPos
    Parser.expect(If, p)
    Parser.leaveBreadcrumb(p, Grammar.IfCondition)
    
    let conditionExpr = parseExpr(~context=WhenExpr, p)
    Parser.eatBreadcrumb(p)
    Parser.leaveBreadcrumb(p, IfBranch)
    Parser.expect(Lbrace, p)
    let thenExpr = parseExprBlock(p)
    Parser.expect(Rbrace, p)
    Parser.eatBreadcrumb(p)
    let elseExpr = switch p.Parser.token {
    | Else =>
      Parser.leaveBreadcrumb(p, Grammar.ElseBranch)
      Parser.next(p)
      let elseExpr = switch p.token {
      | If => parseIfExpression(p)
      | _ =>
        Parser.expect(Lbrace, p)
        let blockExpr = parseExprBlock(p)
        Parser.expect(Rbrace, p)
        blockExpr
      }
      
      Parser.eatBreadcrumb(p)
      Some(elseExpr)
    | _ => None
    }
    
    let loc = mkLoc(startPos, p.prevEndPos)
    Parser.eatBreadcrumb(p)
    Ast_helper.Exp.ifthenelse(~loc, conditionExpr, thenExpr, elseExpr)
  }
  
  and parseForRest = (hasOpeningParen, pattern, startPos, p) => {
    Parser.expect(In, p)
    let e1 = parseExpr(p)
    let direction = switch p.Parser.token {
    | To => Asttypes.Upto
    | Downto => Asttypes.Downto
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Asttypes.Upto
    }
    
    Parser.next(p)
    let e2 = parseExpr(~context=WhenExpr, p)
    if hasOpeningParen {
      Parser.expect(Rparen, p)
    }
    Parser.expect(Lbrace, p)
    let bodyExpr = parseExprBlock(p)
    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.for_(~loc, pattern, e1, e2, direction, bodyExpr)
  }
  
  and parseForExpression = p => {
    let startPos = p.Parser.startPos
    Parser.expect(For, p)
    switch p.token {
    | Lparen =>
      let lparen = p.startPos
      Parser.next(p)
      switch p.token {
      | Rparen =>
        Parser.next(p)
        let unitPattern = {
          let loc = mkLoc(lparen, p.prevEndPos)
          let lid = Location.mkloc(Longident.Lident("()"), loc)
          Ast_helper.Pat.construct(lid, None)
        }
        
        parseForRest(
          false,
          parseAliasPattern(~attrs=list[], unitPattern, p),
          startPos,
          p,
        )
      | _ =>
        let pat = parsePattern(p)
        switch p.token {
        | Comma =>
          Parser.next(p)
          let tuplePattern = parseTuplePattern(
            ~attrs=list[],
            ~startPos=lparen,
            ~first=pat,
            p,
          )
          
          let pattern = parseAliasPattern(~attrs=list[], tuplePattern, p)
          parseForRest(false, pattern, startPos, p)
        | _ => parseForRest(true, pat, startPos, p)
        }
      }
    | _ => parseForRest(false, parsePattern(p), startPos, p)
    }
  }
  
  and parseWhileExpression = p => {
    let startPos = p.Parser.startPos
    Parser.expect(While, p)
    let expr1 = parseExpr(~context=WhenExpr, p)
    Parser.expect(Lbrace, p)
    let expr2 = parseExprBlock(p)
    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.while_(~loc, expr1, expr2)
  }
  
  and parsePatternMatchCase = p => {
    Parser.leaveBreadcrumb(p, Grammar.PatternMatchCase)
    switch p.Parser.token {
    | Token.Bar =>
      Parser.next(p)
      let lhs = parsePattern(p)
      let guard = switch p.Parser.token {
      | When =>
        Parser.next(p)
        Some(parseExpr(~context=WhenExpr, p))
      | _ => None
      }
      
      let () = switch p.token {
      | EqualGreater => Parser.next(p)
      | _ => Recover.recoverEqualGreater(p)
      }
      
      let rhs = parseExprBlock(p)
      Parser.eatBreadcrumb(p)
      Some(Ast_helper.Exp.case(lhs, ~guard?, rhs))
    | _ => None
    }
  }
  
  and parsePatternMatching = p => {
    Parser.leaveBreadcrumb(p, Grammar.PatternMatching)
    let cases = parseDelimitedRegion(
      ~grammar=Grammar.PatternMatching,
      ~closing=Rbrace,
      ~f=parsePatternMatchCase,
      p,
    )
    
    let () = switch cases {
    | list[] =>
      Parser.err(
        ~startPos=p.prevEndPos,
        p,
        Diagnostics.message("Pattern matching needs at least one case"),
      )
    | _ => ()
    }
    
    cases
  }
  
  and parseSwitchExpression = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Switch, p)
    let switchExpr = parseExpr(~context=WhenExpr, p)
    Parser.expect(Lbrace, p)
    let cases = parsePatternMatching(p)
    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.match_(~loc, switchExpr, cases)
  }
  
  and parseArgument = p =>
    if (
      p.Parser.token == Token.Tilde ||
        p.token == Dot ||
        p.token == Underscore || Grammar.isExprStart(p.token)
    ) {
      switch p.Parser.token {
      | Dot =>
        let uncurried = true
        let startPos = p.Parser.startPos
        Parser.next(p)
        switch p.token {
        | Rparen =>
          let loc = mkLoc(startPos, p.prevEndPos)
          let unitExpr = Ast_helper.Exp.construct(
            ~loc,
            Location.mkloc(Longident.Lident("()"), loc),
            None,
          )
          
          Some(uncurried, Asttypes.Nolabel, unitExpr)
        | _ => parseArgument2(p, ~uncurried)
        }
      | _ => parseArgument2(p, ~uncurried=false)
      }
    } else {
      None
    }
  
  and parseArgument2 = (p, ~uncurried) =>
    switch p.Parser.token {
    | Underscore when !isEs6ArrowExpression(~inTernary=false, p) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      let exp = Ast_helper.Exp.ident(
        ~loc,
        Location.mkloc(Longident.Lident("_"), loc),
      )
      Some(uncurried, Asttypes.Nolabel, exp)
    | Tilde =>
      Parser.next(p)
      
      switch p.Parser.token {
      | Lident(ident) =>
        let startPos = p.startPos
        Parser.next(p)
        let endPos = p.prevEndPos
        let loc = mkLoc(startPos, endPos)
        let identExpr = Ast_helper.Exp.ident(
          ~loc,
          Location.mkloc(Longident.Lident(ident), loc),
        )
        switch p.Parser.token {
        | Question =>
          Parser.next(p)
          Some(uncurried, Asttypes.Optional(ident), identExpr)
        | Equal =>
          Parser.next(p)
          let label = switch p.Parser.token {
          | Question =>
            Parser.next(p)
            Asttypes.Optional(ident)
          | _ => Labelled(ident)
          }
          
          let expr = switch p.Parser.token {
          | Underscore =>
            let loc = mkLoc(p.startPos, p.endPos)
            Parser.next(p)
            Ast_helper.Exp.ident(
              ~loc,
              Location.mkloc(Longident.Lident("_"), loc),
            )
          | _ => parseConstrainedExpr(p)
          }
          
          Some(uncurried, label, expr)
        | _ => Some(uncurried, Labelled(ident), identExpr)
        }
      | t =>
        Parser.err(p, Diagnostics.lident(t))
        Some(uncurried, Nolabel, Recover.defaultExpr())
      }
    | _ => Some(uncurried, Nolabel, parseConstrainedExpr(p))
    }
  
  and parseCallExpr = (p, funExpr) => {
    Parser.expect(Lparen, p)
    let startPos = p.Parser.startPos
    Parser.leaveBreadcrumb(p, Grammar.ExprCall)
    let args = parseCommaDelimitedRegion(
      ~grammar=Grammar.ArgumentList,
      ~closing=Rparen,
      ~f=parseArgument,
      p,
    )
    
    Parser.expect(Rparen, p)
    let args = switch args {
    | list[] =>
      let loc = mkLoc(startPos, p.prevEndPos)
      
      list[
        (
          false,
          Asttypes.Nolabel,
          Ast_helper.Exp.construct(
            ~loc,
            Location.mkloc(Longident.Lident("()"), loc),
            None,
          ),
        ),
      ]
    | args => args
    }
    
    let loc = {...funExpr.pexp_loc, loc_end: p.prevEndPos}
    
    let rec group = (grp, acc) =>
      x => switch x {
      | list[(uncurried, lbl, expr), ...xs] =>
        let (_u, grp) = grp
        if uncurried === true {
          group(
            (true, list[(lbl, expr)]),
            list[(_u, List.rev(grp)), ...acc],
            xs,
          )
        } else {
          group((_u, list[(lbl, expr), ...grp]), acc, xs)
        }
      | list[] =>
        let (_u, grp) = grp
        List.rev(list[(_u, List.rev(grp)), ...acc])
      }
    
    let args = switch args {
    | list[(u, lbl, expr), ...args] =>
      group((u, list[(lbl, expr)]), list[], args)
    | list[] => list[]
    }
    
    let apply = List.fold_left(
      (callBody, group) => {
        let (uncurried, args) = group
        let (args, wrap) = processUnderscoreApplication(args)
        let exp = if uncurried {
          let attrs = list[uncurryAttr]
          Ast_helper.Exp.apply(~loc, ~attrs, callBody, args)
        } else {
          Ast_helper.Exp.apply(~loc, callBody, args)
        }
        
        wrap(exp)
      },
      funExpr,
      args,
    )
    
    Parser.eatBreadcrumb(p)
    apply
  }
  
  and parseValueOrConstructor = p => {
    let startPos = p.Parser.startPos
    let rec aux = (p, acc) =>
      switch p.Parser.token {
      | Uident(ident) =>
        let endPosLident = p.endPos
        Parser.next(p)
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          aux(p, list[ident, ...acc])
        | Lparen when p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
          let lparen = p.startPos
          let args = parseConstructorArgs(p)
          let rparen = p.prevEndPos
          let lident = buildLongident(list[ident, ...acc])
          let tail = switch args {
          | list[] => None
          | list[arg] => Some(arg)
          | args =>
            let loc = mkLoc(lparen, rparen)
            Some(Ast_helper.Exp.tuple(~loc, args))
          }
          
          let loc = mkLoc(startPos, p.prevEndPos)
          let identLoc = mkLoc(startPos, endPosLident)
          Ast_helper.Exp.construct(~loc, Location.mkloc(lident, identLoc), tail)
        | _ =>
          let loc = mkLoc(startPos, p.prevEndPos)
          let lident = buildLongident(list[ident, ...acc])
          Ast_helper.Exp.construct(~loc, Location.mkloc(lident, loc), None)
        }
      | Lident(ident) =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let lident = buildLongident(list[ident, ...acc])
        Ast_helper.Exp.ident(~loc, Location.mkloc(lident, loc))
      | token =>
        Parser.next(p)
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        Recover.defaultExpr()
      }
    
    aux(p, list[])
  }
  
  and parseConstructorArgs = p => {
    let lparen = p.Parser.startPos
    Parser.expect(Lparen, p)
    let args = parseCommaDelimitedRegion(
      ~grammar=Grammar.ExprList,
      ~f=parseConstrainedExprRegion,
      ~closing=Rparen,
      p,
    )
    
    Parser.expect(Rparen, p)
    switch args {
    | list[] =>
      let loc = mkLoc(lparen, p.prevEndPos)
      list[
        Ast_helper.Exp.construct(
          ~loc,
          Location.mkloc(Longident.Lident("()"), loc),
          None,
        ),
      ]
    | args => args
    }
  }
  
  and parseTupleExpr = (~first, ~startPos, p) => {
    let exprs = parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.ExprList,
      ~closing=Rparen,
      ~f=parseConstrainedExprRegion,
    )
    
    Parser.expect(Rparen, p)
    Ast_helper.Exp.tuple(
      ~loc=mkLoc(startPos, p.prevEndPos),
      list[first, ...exprs],
    )
  }
  
  and parseSpreadExprRegion = p =>
    switch p.Parser.token {
    | DotDotDot =>
      Parser.next(p)
      let expr = parseConstrainedExpr(p)
      Some(true, expr)
    | token when Grammar.isExprStart(token) =>
      Some(false, parseConstrainedExpr(p))
    | _ => None
    }
  
  and parseListExpr = p => {
    let startPos = p.Parser.startPos
    Parser.expect(List, p)
    Parser.expect(Lbracket, p)
    let listExprs = parseCommaDelimitedReversedList(
      p,
      ~grammar=Grammar.ListExpr,
      ~closing=Rbracket,
      ~f=parseSpreadExprRegion,
    )
    
    Parser.expect(Rbracket, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    switch listExprs {
    | list[(true, expr), ...exprs] =>
      let exprs = exprs |> List.map(snd) |> List.rev
      makeListExpression(loc, exprs, Some(expr))
    | exprs =>
      let exprs =
        exprs
        |> List.map(((spread, expr)) => {
          if spread {
            Parser.err(p, Diagnostics.message(ErrorMessages.listExprSpread))
          }
          expr
        })
        |> List.rev
      
      makeListExpression(loc, exprs, None)
    }
  }
  
  and parseNonSpreadExp = (~msg, p) => {
    let () = switch p.Parser.token {
    | DotDotDot =>
      Parser.err(p, Diagnostics.message(msg))
      Parser.next(p)
    | _ => ()
    }
    
    switch p.Parser.token {
    | token when Grammar.isExprStart(token) =>
      let expr = parseExpr(p)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
        Some(Ast_helper.Exp.constraint_(~loc, expr, typ))
      | _ => Some(expr)
      }
    | _ => None
    }
  }
  
  and parseArrayExp = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lbracket, p)
    let exprs = parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.ExprList,
      ~closing=Rbracket,
      ~f=parseNonSpreadExp(~msg=ErrorMessages.arrayExprSpread),
    )
    
    Parser.expect(Rbracket, p)
    Ast_helper.Exp.array(~loc=mkLoc(startPos, p.prevEndPos), exprs)
  }
  
  and parsePolyTypeExpr = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | SingleQuote =>
      let vars = parseTypeVarList(p)
      switch vars {
      | list[_v1, _v2, ..._] =>
        Parser.expect(Dot, p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Typ.poly(~loc, vars, typ)
      | list[var] =>
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          let typ = parseTypExpr(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          Ast_helper.Typ.poly(~loc, vars, typ)
        | EqualGreater =>
          Parser.next(p)
          let typ = Ast_helper.Typ.var(~loc=var.loc, var.txt)
          let returnType = parseTypExpr(~alias=false, p)
          let loc = mkLoc(typ.Parsetree.ptyp_loc.loc_start, p.prevEndPos)
          Ast_helper.Typ.arrow(~loc, Asttypes.Nolabel, typ, returnType)
        | _ => Ast_helper.Typ.var(~loc=var.loc, var.txt)
        }
      | _ => assert false
      }
    | _ => parseTypExpr(p)
    }
  }
  
  and parseTypeVarList = p => {
    let rec loop = (p, vars) =>
      switch p.Parser.token {
      | SingleQuote =>
        Parser.next(p)
        let (lident, loc) = parseLident(p)
        let var = Location.mkloc(lident, loc)
        loop(p, list[var, ...vars])
      | _ => List.rev(vars)
      }
    
    loop(p, list[])
  }
  
  and parseLidentList = p => {
    let rec loop = (p, ls) =>
      switch p.Parser.token {
      | Lident(lident) =>
        let loc = mkLoc(p.startPos, p.endPos)
        Parser.next(p)
        loop(p, list[Location.mkloc(lident, loc), ...ls])
      | _ => List.rev(ls)
      }
    
    loop(p, list[])
  }
  
  and parseAtomicTypExpr = (~attrs, p) => {
    Parser.leaveBreadcrumb(p, Grammar.AtomicTypExpr)
    let startPos = p.Parser.startPos
    let typ = switch p.Parser.token {
    | SingleQuote =>
      Parser.next(p)
      let (ident, loc) = parseLident(p)
      Ast_helper.Typ.var(~loc, ~attrs, ident)
    | Underscore =>
      let endPos = p.endPos
      Parser.next(p)
      Ast_helper.Typ.any(~loc=mkLoc(startPos, endPos), ~attrs, ())
    | Lparen =>
      Parser.next(p)
      switch p.Parser.token {
      | Rparen =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let unitConstr = Location.mkloc(Longident.Lident("unit"), loc)
        Ast_helper.Typ.constr(~attrs, unitConstr, list[])
      | _ =>
        let t = parseTypExpr(p)
        switch p.token {
        | Comma =>
          Parser.next(p)
          parseTupleType(~attrs, ~first=t, ~startPos, p)
        | _ =>
          Parser.expect(Rparen, p)
          {
            ...t,
            ptyp_loc: mkLoc(startPos, p.prevEndPos),
            ptyp_attributes: List.concat(list[attrs, t.ptyp_attributes]),
          }
        }
      }
    | (Uident(_) | Lident(_)) | List =>
      let constr = parseValuePath(p)
      let args = parseTypeConstructorArgs(p)
      Ast_helper.Typ.constr(
        ~loc=mkLoc(startPos, p.prevEndPos),
        ~attrs,
        constr,
        args,
      )
    | Module =>
      Parser.next(p)
      Parser.expect(Lparen, p)
      let packageType = parsePackageType(~startPos, ~attrs, p)
      Parser.expect(Rparen, p)
      {...packageType, ptyp_loc: mkLoc(startPos, p.prevEndPos)}
    | Percent =>
      let extension = parseExtension(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.extension(~attrs, ~loc, extension)
    | Lbrace => parseBsObjectType(~attrs, p)
    | token =>
      switch Recover.skipTokensAndMaybeRetry(
        p,
        ~isStartOfGrammar=Grammar.isAtomicTypExprStart,
      ) {
      | Retry => parseAtomicTypExpr(~attrs, p)
      | Abort =>
        Parser.err(
          ~startPos=p.prevEndPos,
          p,
          Diagnostics.unexpected(token, p.breadcrumbs),
        )
        Recover.defaultType()
      }
    }
    
    Parser.eatBreadcrumb(p)
    typ
  }
  
  and parsePackageType = (~startPos, ~attrs, p) => {
    let modTypePath = parseModuleLongIdent(p)
    switch p.Parser.token {
    | With =>
      Parser.next(p)
      let constraints = parsePackageConstraints(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.package(~loc, ~attrs, modTypePath, constraints)
    | _ =>
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.package(~loc, ~attrs, modTypePath, list[])
    }
  }
  
  and parsePackageConstraints = p => {
    let first = {
      Parser.expect(Typ, p)
      let typeConstr = parseValuePath(p)
      Parser.expect(Equal, p)
      let typ = parseTypExpr(p)
      (typeConstr, typ)
    }
    
    let rest = parseRegion(
      ~grammar=Grammar.PackageConstraint,
      ~f=parsePackageConstraint,
      p,
    )
    
    list[first, ...rest]
  }
  
  and parsePackageConstraint = p =>
    switch p.Parser.token {
    | And =>
      Parser.next(p)
      Parser.expect(Typ, p)
      let typeConstr = parseValuePath(p)
      Parser.expect(Equal, p)
      let typ = parseTypExpr(p)
      Some(typeConstr, typ)
    | _ => None
    }
  
  and parseBsObjectType = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Lbrace, p)
    let objectType = switch p.Parser.token {
    | DotDot =>
      Parser.next(p)
      let closedFlag = Asttypes.Open
      let fields = parseCommaDelimitedRegion(
        ~grammar=Grammar.StringFieldDeclarations,
        ~closing=Rbrace,
        ~f=parseStringFieldDeclaration,
        p,
      )
      
      Parser.expect(Rbrace, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      makeBsObjType(~attrs, ~loc, ~closed=closedFlag, fields)
    | _ =>
      let closedFlag = Asttypes.Closed
      let fields = parseCommaDelimitedRegion(
        ~grammar=Grammar.StringFieldDeclarations,
        ~closing=Rbrace,
        ~f=parseStringFieldDeclaration,
        p,
      )
      
      Parser.expect(Rbrace, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      makeBsObjType(~attrs, ~loc, ~closed=closedFlag, fields)
    }
    
    objectType
  }
  
  and parseTypeAlias = (p, typ) =>
    switch p.Parser.token {
    | As =>
      Parser.next(p)
      Parser.expect(SingleQuote, p)
      let (ident, _loc) = parseLident(p)
      
      Ast_helper.Typ.alias(
        ~loc=mkLoc(typ.Parsetree.ptyp_loc.loc_start, p.prevEndPos),
        typ,
        ident,
      )
    | _ => typ
    }
  
  and parseTypeParameter = p =>
    if (
      p.Parser.token == Token.Tilde ||
        p.token == Dot ||
        Grammar.isTypExprStart(p.token)
    ) {
      let startPos = p.Parser.startPos
      let uncurried = Parser.optional(p, Dot)
      let attrs = parseAttributes(p)
      switch p.Parser.token {
      | Tilde =>
        Parser.next(p)
        let (name, _loc) = parseLident(p)
        Parser.expect(Colon, p)
        let typ = parseTypExpr(p)
        switch p.Parser.token {
        | Equal =>
          Parser.next(p)
          Parser.expect(Question, p)
          Some(uncurried, attrs, Asttypes.Optional(name), typ, startPos)
        | _ => Some(uncurried, attrs, Asttypes.Labelled(name), typ, startPos)
        }
      | _ =>
        let typ = parseTypExpr(p)
        let typWithAttributes = {
          ...typ,
          ptyp_attributes: List.concat(list[attrs, typ.ptyp_attributes]),
        }
        Some(uncurried, list[], Asttypes.Nolabel, typWithAttributes, startPos)
      }
    } else {
      None
    }
  
  and parseTypeParameters = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lparen, p)
    switch p.Parser.token {
    | Rparen =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let unitConstr = Location.mkloc(Longident.Lident("unit"), loc)
      let typ = Ast_helper.Typ.constr(unitConstr, list[])
      list[(false, list[], Asttypes.Nolabel, typ, startPos)]
    | _ =>
      let params = parseCommaDelimitedRegion(
        ~grammar=Grammar.TypeParameters,
        ~closing=Rparen,
        ~f=parseTypeParameter,
        p,
      )
      
      Parser.expect(Rparen, p)
      params
    }
  }
  
  and parseEs6ArrowType = (~attrs, p) => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Tilde =>
      Parser.next(p)
      let (name, _loc) = parseLident(p)
      Parser.expect(Colon, p)
      let typ = parseTypExpr(~alias=false, ~es6Arrow=false, p)
      let arg = switch p.Parser.token {
      | Equal =>
        Parser.next(p)
        Parser.expect(Question, p)
        Asttypes.Optional(name)
      | _ => Asttypes.Labelled(name)
      }
      
      Parser.expect(EqualGreater, p)
      let returnType = parseTypExpr(~alias=false, p)
      Ast_helper.Typ.arrow(~attrs, arg, typ, returnType)
    | _ =>
      let parameters = parseTypeParameters(p)
      Parser.expect(EqualGreater, p)
      let returnType = parseTypExpr(~alias=false, p)
      let endPos = p.prevEndPos
      let typ = List.fold_right(
        ((uncurried, attrs, argLbl, typ, startPos), t) => {
          let attrs = if uncurried {
            list[uncurryAttr, ...attrs]
          } else {
            attrs
          }
          Ast_helper.Typ.arrow(
            ~loc=mkLoc(startPos, endPos),
            ~attrs,
            argLbl,
            typ,
            t,
          )
        },
        parameters,
        returnType,
      )
      
      {
        ...typ,
        ptyp_attributes: List.concat(list[typ.ptyp_attributes, attrs]),
        ptyp_loc: mkLoc(startPos, p.prevEndPos),
      }
    }
  }
  
  and parseTypExpr = (~es6Arrow=true, ~alias=true, p) => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    let typ = if es6Arrow && isEs6ArrowType(p) {
      parseEs6ArrowType(~attrs, p)
    } else {
      let typ = parseAtomicTypExpr(~attrs, p)
      switch p.Parser.token {
      | (EqualGreater | MinusGreater) as token when es6Arrow === true =>
        if token == MinusGreater {
          Parser.expect(EqualGreater, p)
        }
        Parser.next(p)
        let returnType = parseTypExpr(~alias=false, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Typ.arrow(~loc, Asttypes.Nolabel, typ, returnType)
      | _ => typ
      }
    }
    
    let typ = if alias {
      parseTypeAlias(p, typ)
    } else {
      typ
    }
    
    typ
  }
  
  and parseTypExprRegion = p =>
    if Grammar.isTypExprStart(p.Parser.token) {
      Some(parseTypExpr(p))
    } else {
      None
    }
  
  and parseTupleType = (~attrs, ~first, ~startPos, p) => {
    let typexprs = parseCommaDelimitedRegion(
      ~grammar=Grammar.TypExprList,
      ~closing=Rparen,
      ~f=parseTypExprRegion,
      p,
    )
    
    Parser.expect(Rparen, p)
    let tupleLoc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Typ.tuple(~attrs, ~loc=tupleLoc, list[first, ...typexprs])
  }
  
  and parseTypeConstructorArg = p => {
    if p.Parser.token == Token.LessThan {
      Parser.next(p)
    }
    let typ = parseTypExpr(p)
    typ
  }
  
  and parseTypeConstructorArgRegion = p =>
    if Grammar.isTypExprStart(p.Parser.token) {
      Some(parseTypExpr(p))
    } else if p.token == LessThan {
      Parser.next(p)
      parseTypeConstructorArgRegion(p)
    } else {
      None
    }
  
  and parseTypeConstructorArgs = p => {
    let opening = p.Parser.token
    switch opening {
    | LessThan | Lparen =>
      if p.token == Lparen {
        let msg = "Type parameters need to be wrapped in angle brackets, not parentheses, like so: \"Belt.Map.String.t<int>\""
        Parser.err(p, Diagnostics.message(msg))
      }
      Scanner.setDiamondMode(p.scanner)
      Parser.next(p)
      let typeArgs = parseCommaDelimitedRegion(
        ~grammar=Grammar.TypExprList,
        ~closing=GreaterThan,
        ~f=parseTypeConstructorArgRegion,
        p,
      )
      
      let () = switch p.token {
      | Rparen when opening == Token.Lparen => Parser.next(p)
      | _ => Parser.expect(GreaterThan, p)
      }
      
      Scanner.popMode(p.scanner, Diamond)
      typeArgs
    | _ => list[]
    }
  }
  
  and parseConstructorTypeArgs = p => {
    Scanner.setDiamondMode(p.Parser.scanner)
    Parser.expect(LessThan, p)
    let typeArgs = parseCommaDelimitedRegion(
      ~grammar=Grammar.TypExprList,
      ~closing=GreaterThan,
      ~f=parseTypExprRegion,
      p,
    )
    
    Parser.expect(GreaterThan, p)
    Scanner.popMode(p.scanner, Diamond)
    typeArgs
  }
  
  and parseStringFieldDeclaration = p => {
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | String(name) =>
      let nameStartPos = p.startPos
      let nameEndPos = p.endPos
      Parser.next(p)
      let fieldName = Location.mkloc(name, mkLoc(nameStartPos, nameEndPos))
      Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
      let typ = parsePolyTypeExpr(p)
      Some(Parsetree.Otag(fieldName, attrs, typ))
    | token => None
    }
  }
  
  and parseFieldDeclaration = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    let mut = if Parser.optional(p, Token.Mutable) {
      Asttypes.Mutable
    } else {
      Asttypes.Immutable
    }
    
    let (lident, loc) = parseLident(p)
    let name = Location.mkloc(lident, loc)
    let typ = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      parsePolyTypeExpr(p)
    | _ =>
      Ast_helper.Typ.constr(
        ~loc=name.loc,
        {...name, txt: Lident(name.txt)},
        list[],
      )
    }
    
    let loc = mkLoc(startPos, typ.ptyp_loc.loc_end)
    Ast_helper.Type.field(~attrs, ~loc, ~mut, name, typ)
  }
  
  and parseFieldDeclarationRegion = p => {
    let attrs = parseAttributes(p)
    let startPos = p.Parser.startPos
    let mut = if Parser.optional(p, Token.Mutable) {
      Asttypes.Mutable
    } else {
      Asttypes.Immutable
    }
    
    switch p.token {
    | Lident(_) =>
      let (lident, loc) = parseLident(p)
      let name = Location.mkloc(lident, loc)
      let typ = switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        parsePolyTypeExpr(p)
      | _ =>
        Ast_helper.Typ.constr(
          ~loc=name.loc,
          {...name, txt: Lident(name.txt)},
          list[],
        )
      }
      
      let loc = mkLoc(startPos, typ.ptyp_loc.loc_end)
      Some(Ast_helper.Type.field(~attrs, ~loc, ~mut, name, typ))
    | _ => None
    }
  }
  
  and parseRecordDeclaration = p => {
    Parser.leaveBreadcrumb(p, Grammar.RecordDecl)
    Parser.expect(Lbrace, p)
    let rows = parseCommaDelimitedRegion(
      ~grammar=Grammar.RecordDecl,
      ~closing=Rbrace,
      ~f=parseFieldDeclarationRegion,
      p,
    )
    
    Parser.expect(Rbrace, p)
    Parser.eatBreadcrumb(p)
    rows
  }
  
  and parseConstrDeclArgs = p => {
    let constrArgs = switch p.Parser.token {
    | Lparen =>
      Parser.next(p)
      
      switch p.Parser.token {
      | Lbrace =>
        Parser.next(p)
        let startPos = p.Parser.startPos
        switch p.Parser.token {
        | DotDot =>
          Parser.next(p)
          let closedFlag = Asttypes.Open
          let fields = parseCommaDelimitedRegion(
            ~grammar=Grammar.StringFieldDeclarations,
            ~closing=Rbrace,
            ~f=parseStringFieldDeclaration,
            p,
          )
          
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let typ = makeBsObjType(
            ~attrs=list[],
            ~loc,
            ~closed=closedFlag,
            fields,
          )
          Parser.optional(p, Comma) |> ignore
          let moreArgs = parseCommaDelimitedRegion(
            ~grammar=Grammar.TypExprList,
            ~closing=Rparen,
            ~f=parseTypExprRegion,
            p,
          )
          
          Parser.expect(Rparen, p)
          Parsetree.Pcstr_tuple(list[typ, ...moreArgs])
        | _ =>
          let attrs = parseAttributes(p)
          switch p.Parser.token {
          | String(_) =>
            let closedFlag = Asttypes.Closed
            let fields = switch attrs {
            | list[] =>
              parseCommaDelimitedRegion(
                ~grammar=Grammar.StringFieldDeclarations,
                ~closing=Rbrace,
                ~f=parseStringFieldDeclaration,
                p,
              )
            | attrs =>
              let first = {
                Parser.leaveBreadcrumb(p, Grammar.StringFieldDeclarations)
                let field = switch parseStringFieldDeclaration(p) {
                | Some(field) => field
                | None => assert false
                }
                
                let () = switch p.Parser.token {
                | Rbrace | Eof => ()
                | Comma => Parser.next(p)
                | _ => Parser.expect(Comma, p)
                }
                
                Parser.eatBreadcrumb(p)
                switch field {
                | Parsetree.Otag(label, _, ct) =>
                  Parsetree.Otag(label, attrs, ct)
                | Oinherit(ct) => Oinherit(ct)
                }
              }
              
              list[
                first,
                ...parseCommaDelimitedRegion(
                  ~grammar=Grammar.StringFieldDeclarations,
                  ~closing=Rbrace,
                  ~f=parseStringFieldDeclaration,
                  p,
                ),
              ]
            }
            Parser.expect(Rbrace, p)
            let loc = mkLoc(startPos, p.prevEndPos)
            let typ = makeBsObjType(
              ~attrs=list[],
              ~loc,
              ~closed=closedFlag,
              fields,
            )
            Parser.optional(p, Comma) |> ignore
            let moreArgs = parseCommaDelimitedRegion(
              ~grammar=Grammar.TypExprList,
              ~closing=Rparen,
              ~f=parseTypExprRegion,
              p,
            )
            
            Parser.expect(Rparen, p)
            Parsetree.Pcstr_tuple(list[typ, ...moreArgs])
          | _ =>
            let fields = switch attrs {
            | list[] =>
              parseCommaDelimitedRegion(
                ~grammar=Grammar.FieldDeclarations,
                ~closing=Rbrace,
                ~f=parseFieldDeclarationRegion,
                p,
              )
            | attrs =>
              let first = {
                let field = parseFieldDeclaration(p)
                Parser.expect(Comma, p)
                {...field, Parsetree.pld_attributes: attrs}
              }
              
              list[
                first,
                ...parseCommaDelimitedRegion(
                  ~grammar=Grammar.FieldDeclarations,
                  ~closing=Rbrace,
                  ~f=parseFieldDeclarationRegion,
                  p,
                ),
              ]
            }
            
            Parser.expect(Rbrace, p)
            Parser.optional(p, Comma) |> ignore
            Parser.expect(Rparen, p)
            Parsetree.Pcstr_record(fields)
          }
        }
      | _ =>
        let args = parseCommaDelimitedRegion(
          ~grammar=Grammar.TypExprList,
          ~closing=Rparen,
          ~f=parseTypExprRegion,
          p,
        )
        
        Parser.expect(Rparen, p)
        Parsetree.Pcstr_tuple(args)
      }
    | _ => Pcstr_tuple(list[])
    }
    
    let res = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      Some(parseTypExpr(p))
    | _ => None
    }
    
    (constrArgs, res)
  }
  
  and parseTypeConstructorDeclarationWithBar = p =>
    switch p.Parser.token {
    | Bar =>
      let startPos = p.Parser.startPos
      Parser.next(p)
      Some(parseTypeConstructorDeclaration(~startPos, p))
    | _ => None
    }
  
  and parseTypeConstructorDeclaration = (~startPos, p) => {
    Parser.leaveBreadcrumb(p, Grammar.ConstructorDeclaration)
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Uident(uident) =>
      let uidentLoc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      let (args, res) = parseConstrDeclArgs(p)
      Parser.eatBreadcrumb(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Type.constructor(
        ~loc,
        ~attrs,
        ~res?,
        ~args,
        Location.mkloc(uident, uidentLoc),
      )
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Ast_helper.Type.constructor(Location.mknoloc("_"))
    }
  }
  
  and parseTypeConstructorDeclarations = (~first=?, p) => {
    let firstConstrDecl = switch first {
    | None =>
      let startPos = p.Parser.startPos
      ignore(Parser.optional(p, Token.Bar))
      parseTypeConstructorDeclaration(~startPos, p)
    | Some(firstConstrDecl) => firstConstrDecl
    }
    
    list[
      firstConstrDecl,
      ...parseRegion(
        ~grammar=Grammar.ConstructorDeclaration,
        ~f=parseTypeConstructorDeclarationWithBar,
        p,
      ),
    ]
  }
  
  and parseTypeRepresentation = p => {
    Parser.leaveBreadcrumb(p, Grammar.TypeRepresentation)
    
    let privateFlag = if Parser.optional(p, Token.Private) {
      Asttypes.Private
    } else {
      Asttypes.Public
    }
    
    let kind = switch p.Parser.token {
    | Bar | Uident(_) =>
      Parsetree.Ptype_variant(parseTypeConstructorDeclarations(p))
    | Lbrace => Parsetree.Ptype_record(parseRecordDeclaration(p))
    | DotDot =>
      Parser.next(p)
      Ptype_open
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      
      Parsetree.Ptype_variant(list[])
    }
    
    Parser.eatBreadcrumb(p)
    (privateFlag, kind)
  }
  
  and parseTypeParam = p => {
    let variance = switch p.Parser.token {
    | Plus =>
      Parser.next(p)
      Asttypes.Covariant
    | Minus =>
      Parser.next(p)
      Contravariant
    | _ => Invariant
    }
    
    switch p.Parser.token {
    | SingleQuote =>
      Parser.next(p)
      let (ident, loc) = parseLident(p)
      Some(Ast_helper.Typ.var(~loc, ident), variance)
    | Underscore =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Some(Ast_helper.Typ.any(~loc, ()), variance)
    
    | token => None
    }
  }
  
  and parseTypeParams = p => {
    let opening = p.Parser.token
    switch opening {
    | LessThan | Lparen when p.startPos.pos_lnum === p.prevEndPos.pos_lnum =>
      Parser.leaveBreadcrumb(p, Grammar.TypeParams)
      if p.token == Lparen {
        let msg = "Type params require diamonds, example: type node<'a>"
        Parser.err(p, Diagnostics.message(msg))
      }
      Parser.next(p)
      let params = parseCommaDelimitedRegion(
        ~grammar=Grammar.TypeParams,
        ~closing=GreaterThan,
        ~f=parseTypeParam,
        p,
      )
      
      let () = switch p.token {
      | Rparen when opening == Token.Lparen => Parser.next(p)
      | _ => Parser.expect(GreaterThan, p)
      }
      
      Parser.eatBreadcrumb(p)
      params
    | _ => list[]
    }
  }
  
  and parseTypeConstraint = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Token.Constraint =>
      Parser.next(p)
      Parser.expect(SingleQuote, p)
      switch p.Parser.token {
      | Lident(ident) =>
        let identLoc = mkLoc(startPos, p.endPos)
        Parser.next(p)
        Parser.expect(Equal, p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Typ.var(~loc=identLoc, ident), typ, loc)
      | t =>
        Parser.err(p, Diagnostics.lident(t))
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Typ.any(), parseTypExpr(p), loc)
      }
    | _ => None
    }
  }
  
  and parseTypeConstraints = p =>
    parseRegion(~grammar=Grammar.TypeConstraint, ~f=parseTypeConstraint, p)
  
  and parseTypeEquationOrConstrDecl = p => {
    let uidentStartPos = p.Parser.startPos
    switch p.Parser.token {
    | Uident(uident) =>
      Parser.next(p)
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        let typeConstr = parseValuePathTail(
          p,
          uidentStartPos,
          Longident.Lident(uident),
        )
        
        let loc = mkLoc(uidentStartPos, p.prevEndPos)
        let typ = parseTypeAlias(
          p,
          Ast_helper.Typ.constr(~loc, typeConstr, parseTypeConstructorArgs(p)),
        )
        switch p.token {
        | Equal =>
          Parser.next(p)
          let (priv, kind) = parseTypeRepresentation(p)
          (Some(typ), priv, kind)
        | EqualGreater =>
          Parser.next(p)
          let returnType = parseTypExpr(~alias=false, p)
          let loc = mkLoc(uidentStartPos, p.prevEndPos)
          let arrowType = Ast_helper.Typ.arrow(
            ~loc,
            Asttypes.Nolabel,
            typ,
            returnType,
          )
          let typ = parseTypeAlias(p, arrowType)
          (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
        | _ => (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
        }
      | _ =>
        let uidentEndPos = p.endPos
        let (args, res) = parseConstrDeclArgs(p)
        let first = Some(
          {
            let uidentLoc = mkLoc(uidentStartPos, uidentEndPos)
            Ast_helper.Type.constructor(
              ~loc=mkLoc(uidentStartPos, p.prevEndPos),
              ~res?,
              ~args,
              Location.mkloc(uident, uidentLoc),
            )
          },
        )
        (
          None,
          Asttypes.Public,
          Parsetree.Ptype_variant(parseTypeConstructorDeclarations(p, ~first?)),
        )
      }
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      
      (None, Asttypes.Public, Parsetree.Ptype_abstract)
    }
  }
  
  and parseRecordOrBsObjectDecl = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lbrace, p)
    switch p.Parser.token {
    | DotDot =>
      Parser.next(p)
      let closedFlag = Asttypes.Open
      let fields = parseCommaDelimitedRegion(
        ~grammar=Grammar.StringFieldDeclarations,
        ~closing=Rbrace,
        ~f=parseStringFieldDeclaration,
        p,
      )
      
      Parser.expect(Rbrace, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let typ =
        makeBsObjType(
          ~attrs=list[],
          ~loc,
          ~closed=closedFlag,
          fields,
        ) |> parseTypeAlias(p)
      
      (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
    | _ =>
      let attrs = parseAttributes(p)
      switch p.Parser.token {
      | String(_) =>
        let closedFlag = Asttypes.Closed
        let fields = switch attrs {
        | list[] =>
          parseCommaDelimitedRegion(
            ~grammar=Grammar.StringFieldDeclarations,
            ~closing=Rbrace,
            ~f=parseStringFieldDeclaration,
            p,
          )
        | attrs =>
          let first = {
            Parser.leaveBreadcrumb(p, Grammar.StringFieldDeclarations)
            let field = switch parseStringFieldDeclaration(p) {
            | Some(field) => field
            | None => assert false
            }
            
            let () = switch p.Parser.token {
            | Rbrace | Eof => ()
            | Comma => Parser.next(p)
            | _ => Parser.expect(Comma, p)
            }
            
            Parser.eatBreadcrumb(p)
            switch field {
            | Parsetree.Otag(label, _, ct) => Parsetree.Otag(label, attrs, ct)
            | Oinherit(ct) => Oinherit(ct)
            }
          }
          
          list[
            first,
            ...parseCommaDelimitedRegion(
              ~grammar=Grammar.StringFieldDeclarations,
              ~closing=Rbrace,
              ~f=parseStringFieldDeclaration,
              p,
            ),
          ]
        }
        
        Parser.expect(Rbrace, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let typ =
          makeBsObjType(
            ~attrs=list[],
            ~loc,
            ~closed=closedFlag,
            fields,
          ) |> parseTypeAlias(p)
        
        (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
      | _ =>
        Parser.leaveBreadcrumb(p, Grammar.RecordDecl)
        let fields = switch attrs {
        | list[] =>
          parseCommaDelimitedRegion(
            ~grammar=Grammar.FieldDeclarations,
            ~closing=Rbrace,
            ~f=parseFieldDeclarationRegion,
            p,
          )
        | attrs =>
          let first = {
            let field = parseFieldDeclaration(p)
            Parser.optional(p, Comma) |> ignore
            {...field, Parsetree.pld_attributes: attrs}
          }
          
          list[
            first,
            ...parseCommaDelimitedRegion(
              ~grammar=Grammar.FieldDeclarations,
              ~closing=Rbrace,
              ~f=parseFieldDeclarationRegion,
              p,
            ),
          ]
        }
        
        let () = switch fields {
        | list[] =>
          Parser.err(
            ~startPos,
            p,
            Diagnostics.message("A record needs at least one field"),
          )
        | _ => ()
        }
        
        Parser.expect(Rbrace, p)
        Parser.eatBreadcrumb(p)
        (None, Asttypes.Public, Parsetree.Ptype_record(fields))
      }
    }
  }
  
  and parsePrivateEqOrRepr = p => {
    Parser.expect(Private, p)
    switch p.Parser.token {
    | Lbrace =>
      let (manifest, _, kind) = parseRecordOrBsObjectDecl(p)
      (manifest, Asttypes.Private, kind)
    | Uident(_) =>
      let (manifest, _, kind) = parseTypeEquationOrConstrDecl(p)
      (manifest, Asttypes.Private, kind)
    | Bar | DotDot =>
      let (_, kind) = parseTypeRepresentation(p)
      (None, Asttypes.Private, kind)
    | t when Grammar.isTypExprStart(t) =>
      (Some(parseTypExpr(p)), Asttypes.Private, Parsetree.Ptype_abstract)
    | _ =>
      let (_, kind) = parseTypeRepresentation(p)
      (None, Asttypes.Private, kind)
    }
  }
  
  and parseTypeEquationAndRepresentation = p =>
    switch p.Parser.token {
    | (Equal | Bar) as token =>
      if token == Bar {
        Parser.expect(Equal, p)
      }
      Parser.next(p)
      switch p.Parser.token {
      | Uident(_) => parseTypeEquationOrConstrDecl(p)
      | Lbrace => parseRecordOrBsObjectDecl(p)
      | Private => parsePrivateEqOrRepr(p)
      | Bar | DotDot =>
        let (priv, kind) = parseTypeRepresentation(p)
        (None, priv, kind)
      | _ =>
        let manifest = Some(parseTypExpr(p))
        switch p.Parser.token {
        | Equal =>
          Parser.next(p)
          let (priv, kind) = parseTypeRepresentation(p)
          (manifest, priv, kind)
        | _ => (manifest, Public, Parsetree.Ptype_abstract)
        }
      }
    | _ => (None, Public, Parsetree.Ptype_abstract)
    }
  
  and parseTypeDef = (~attrs=?, ~startPos, p) => {
    Parser.leaveBreadcrumb(p, Grammar.TypeDef)
    let attrs = switch attrs {
    | Some(attrs) => attrs
    | None => parseAttributes(p)
    }
    Parser.leaveBreadcrumb(p, Grammar.TypeConstrName)
    let (name, loc) = parseLident(p)
    let typeConstrName = Location.mkloc(name, loc)
    Parser.eatBreadcrumb(p)
    let params = parseTypeParams(p)
    let typeDef = {
      let (manifest, priv, kind) = parseTypeEquationAndRepresentation(p)
      let cstrs = parseTypeConstraints(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Type.mk(
        ~loc,
        ~attrs,
        ~priv,
        ~kind,
        ~params,
        ~cstrs,
        ~manifest?,
        typeConstrName,
      )
    }
    
    Parser.eatBreadcrumb(p)
    typeDef
  }
  
  and parseTypeExtension = (~params, ~attrs, ~name, p) => {
    Parser.expect(PlusEqual, p)
    let priv = if Parser.optional(p, Token.Private) {
      Asttypes.Private
    } else {
      Asttypes.Public
    }
    
    let constrStart = p.Parser.startPos
    Parser.optional(p, Bar) |> ignore
    let first = {
      let (attrs, name, kind) = switch p.Parser.token {
      | Bar =>
        Parser.next(p)
        parseConstrDef(~parseAttrs=true, p)
      | _ => parseConstrDef(~parseAttrs=true, p)
      }
      
      let loc = mkLoc(constrStart, p.prevEndPos)
      Ast_helper.Te.constructor(~loc, ~attrs, name, kind)
    }
    
    let rec loop = (p, cs) =>
      switch p.Parser.token {
      | Bar =>
        let startPos = p.Parser.startPos
        Parser.next(p)
        let (attrs, name, kind) = parseConstrDef(~parseAttrs=true, p)
        let extConstr = Ast_helper.Te.constructor(
          ~attrs,
          ~loc=mkLoc(startPos, p.prevEndPos),
          name,
          kind,
        )
        
        loop(p, list[extConstr, ...cs])
      | _ => List.rev(cs)
      }
    
    let constructors = loop(p, list[first])
    Ast_helper.Te.mk(~attrs, ~params, ~priv, name, constructors)
  }
  
  and parseTypeDefinitions = (~attrs, ~name, ~params, ~startPos, p) => {
    let typeDef = {
      let (manifest, priv, kind) = parseTypeEquationAndRepresentation(p)
      let cstrs = parseTypeConstraints(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Type.mk(
        ~loc,
        ~attrs,
        ~priv,
        ~kind,
        ~params,
        ~cstrs,
        ~manifest?,
        {...name, txt: lidentOfPath(name.Location.txt)},
      )
    }
    
    let rec loop = (p, defs) => {
      let startPos = p.Parser.startPos
      let attrs = parseAttributesAndBinding(p)
      switch p.Parser.token {
      | And =>
        Parser.next(p)
        let attrs = switch p.token {
        | Export =>
          let exportLoc = mkLoc(p.startPos, p.endPos)
          Parser.next(p)
          let genTypeAttr = (
            Location.mkloc("genType", exportLoc),
            Parsetree.PStr(list[]),
          )
          list[genTypeAttr, ...attrs]
        | _ => attrs
        }
        
        let typeDef = parseTypeDef(~attrs, ~startPos, p)
        loop(p, list[typeDef, ...defs])
      | _ => List.rev(defs)
      }
    }
    
    loop(p, list[typeDef])
  }
  
  and parseTypeDefinitionOrExtension = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Token.Typ, p)
    let recFlag = switch p.token {
    | Rec =>
      Parser.next(p)
      Asttypes.Recursive
    | Lident("nonrec") =>
      Parser.next(p)
      Asttypes.Nonrecursive
    | _ => Asttypes.Nonrecursive
    }
    
    let name = parseValuePath(p)
    let params = parseTypeParams(p)
    switch p.Parser.token {
    | PlusEqual => TypeExt(parseTypeExtension(~params, ~attrs, ~name, p))
    | _ =>
      let typeDefs = parseTypeDefinitions(~attrs, ~name, ~params, ~startPos, p)
      TypeDef(recFlag, typeDefs)
    }
  }
  
  and parsePrimitive = p =>
    switch p.Parser.token {
    | String(s) =>
      Parser.next(p)
      Some(s)
    | _ => None
    }
  
  and parsePrimitives = p =>
    switch parseRegion(~grammar=Grammar.Primitive, ~f=parsePrimitive, p) {
    | list[] =>
      let msg = "An external definition should have at least one primitive. Example: \"setTimeout\""
      Parser.err(p, Diagnostics.message(msg))
      list[]
    | primitives => primitives
    }
  
  and parseExternalDef = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.leaveBreadcrumb(p, Grammar.External)
    Parser.expect(Token.External, p)
    let (name, loc) = parseLident(p)
    let name = Location.mkloc(name, loc)
    Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
    let typExpr = parseTypExpr(p)
    Parser.expect(Equal, p)
    let prim = parsePrimitives(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let vb = Ast_helper.Val.mk(~loc, ~attrs, ~prim, name, typExpr)
    Parser.eatBreadcrumb(p)
    vb
  }
  
  and parseConstrDef = (~parseAttrs, p) => {
    let attrs = if parseAttrs {
      parseAttributes(p)
    } else {
      list[]
    }
    let name = switch p.Parser.token {
    | Uident(name) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(name, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }
    
    let kind = switch p.Parser.token {
    | Lparen =>
      let (args, res) = parseConstrDeclArgs(p)
      Parsetree.Pext_decl(args, res)
    | Equal =>
      Parser.next(p)
      let longident = parseModuleLongIdent(p)
      Parsetree.Pext_rebind(longident)
    | _ => Parsetree.Pext_decl(Pcstr_tuple(list[]), None)
    }
    
    (attrs, name, kind)
  }
  
  and parseExceptionDef = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Token.Exception, p)
    let (_, name, kind) = parseConstrDef(~parseAttrs=false, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Te.constructor(~loc, ~attrs, name, kind)
  }
  
  @progress(
    (
      Parser.next,
      Parser.expect,
      Recover.recoverLident,
      Recover.skipTokensAndMaybeRetry,
    )
  )
  and parseStructure = (p): Parsetree.structure =>
    parseRegion(p, ~grammar=Grammar.Structure, ~f=parseStructureItemRegion)
  
  and parseStructureItemRegion = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Open =>
      let openDescription = parseOpenDescription(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.open_(~loc, openDescription))
    | Let =>
      let (recFlag, letBindings) = parseLetBindings(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.value(~loc, recFlag, letBindings))
    | Typ =>
      switch parseTypeDefinitionOrExtension(~attrs, p) {
      | TypeDef(recFlag, types) =>
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Str.type_(~loc, recFlag, types))
      | TypeExt(ext) =>
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Str.type_extension(~loc, ext))
      }
    | External =>
      let externalDef = parseExternalDef(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.primitive(~loc, externalDef))
    | Import =>
      let importDescr = parseJsImport(~startPos, ~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      let structureItem = JsFfi.toParsetree(importDescr)
      Some({...structureItem, pstr_loc: loc})
    | Exception =>
      let exceptionDef = parseExceptionDef(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.exception_(~loc, exceptionDef))
    | Include =>
      let includeStatement = parseIncludeStatement(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.include_(~loc, includeStatement))
    | Export =>
      let structureItem = parseJsExport(~startPos, ~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some({...structureItem, pstr_loc: loc})
    | Module =>
      let structureItem = parseModuleOrModuleTypeImplOrPackExpr(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some({...structureItem, pstr_loc: loc})
    | AtAt =>
      let attr = parseStandaloneAttribute(p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.attribute(~loc, attr))
    | PercentPercent =>
      let extension = parseExtension(~moduleLanguage=true, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.extension(~attrs, ~loc, extension))
    | token when Grammar.isExprStart(token) =>
      let exp = parseExpr(p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      switch exp.pexp_desc {
      | Pexp_apply(_) =>
        let fakeUnitPat = {
          let unitLid = Location.mknoloc(Longident.Lident("()"))
          Ast_helper.Pat.construct(unitLid, None)
        }
        
        let vb = Ast_helper.Vb.mk(~attrs, fakeUnitPat, exp)
        Some(Ast_helper.Str.value(~loc, Asttypes.Nonrecursive, list[vb]))
      | _ => Some(Ast_helper.Str.eval(~loc, ~attrs, exp))
      }
    | _ => None
    }
  }
  
  and parseJsImport = (~startPos, ~attrs, p) => {
    Parser.expect(Token.Import, p)
    let importSpec = switch p.Parser.token {
    | Token.Lident(_) | Token.At =>
      let decl = switch parseJsFfiDeclaration(p) {
      | Some(decl) => decl
      | None => assert false
      }
      
      JsFfi.Default(decl)
    | _ => JsFfi.Spec(parseJsFfiDeclarations(p))
    }
    
    let scope = parseJsFfiScope(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    JsFfi.importDescr(~attrs, ~importSpec, ~scope, ~loc)
  }
  
  and parseJsExport = (~startPos, ~attrs, p) => {
    let exportStart = p.Parser.startPos
    Parser.expect(Token.Export, p)
    let exportLoc = mkLoc(exportStart, p.prevEndPos)
    let genTypeAttr = (
      Location.mkloc("genType", exportLoc),
      Parsetree.PStr(list[]),
    )
    let attrs = list[genTypeAttr, ...attrs]
    switch p.Parser.token {
    | Typ =>
      switch parseTypeDefinitionOrExtension(~attrs, p) {
      | TypeDef(recFlag, types) => Ast_helper.Str.type_(recFlag, types)
      | TypeExt(ext) => Ast_helper.Str.type_extension(ext)
      }
    | _ =>
      let (recFlag, letBindings) = parseLetBindings(~attrs, p)
      Ast_helper.Str.value(recFlag, letBindings)
    }
  }
  
  and parseJsFfiScope = p =>
    switch p.Parser.token {
    | Token.Lident("from") =>
      Parser.next(p)
      switch p.token {
      | String(s) =>
        Parser.next(p)
        JsFfi.Module(s)
      | Uident(_) | Lident(_) =>
        let value = parseIdentPath(p)
        JsFfi.Scope(value)
      | _ => JsFfi.Global
      }
    | _ => JsFfi.Global
    }
  
  and parseJsFfiDeclarations = p => {
    Parser.expect(Token.Lbrace, p)
    let decls = parseCommaDelimitedRegion(
      ~grammar=Grammar.JsFfiImport,
      ~closing=Rbrace,
      ~f=parseJsFfiDeclaration,
      p,
    )
    
    Parser.expect(Rbrace, p)
    decls
  }
  
  and parseJsFfiDeclaration = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Lident(_) =>
      let (ident, _) = parseLident(p)
      let alias = switch p.token {
      | As =>
        Parser.next(p)
        let (ident, _) = parseLident(p)
        ident
      | _ => ident
      }
      
      Parser.expect(Token.Colon, p)
      let typ = parseTypExpr(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(JsFfi.decl(~loc, ~alias, ~attrs, ~name=ident, ~typ))
    | _ => None
    }
  }
  
  and parseIncludeStatement = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Token.Include, p)
    let modExpr = parseModuleExpr(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Incl.mk(~loc, ~attrs, modExpr)
  }
  
  and parseAtomicModuleExpr = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Uident(ident) =>
      let longident = parseModuleLongIdent(p)
      Ast_helper.Mod.ident(~loc=longident.loc, longident)
    | Lbrace =>
      Parser.next(p)
      let structure = Ast_helper.Mod.structure(
        parseDelimitedRegion(
          ~grammar=Grammar.Structure,
          ~closing=Rbrace,
          ~f=parseStructureItemRegion,
          p,
        ),
      )
      Parser.expect(Rbrace, p)
      let endPos = p.prevEndPos
      {...structure, pmod_loc: mkLoc(startPos, endPos)}
    | Lparen =>
      Parser.next(p)
      let modExpr = parseConstrainedModExpr(p)
      Parser.expect(Rparen, p)
      modExpr
    | Lident("unpack") =>
      Parser.next(p)
      Parser.expect(Lparen, p)
      let expr = parseExpr(p)
      switch p.Parser.token {
      | Colon =>
        let colonStart = p.Parser.startPos
        Parser.next(p)
        let attrs = parseAttributes(p)
        let packageType = parsePackageType(~startPos=colonStart, ~attrs, p)
        Parser.expect(Rparen, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let constraintExpr = Ast_helper.Exp.constraint_(~loc, expr, packageType)
        
        Ast_helper.Mod.unpack(~loc, constraintExpr)
      | _ =>
        Parser.expect(Rparen, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Mod.unpack(~loc, expr)
      }
    | Percent =>
      let extension = parseExtension(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Mod.extension(~loc, extension)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Recover.defaultModuleExpr()
    }
  }
  
  and parsePrimaryModExpr = p => {
    let startPos = p.Parser.startPos
    let modExpr = parseAtomicModuleExpr(p)
    let rec loop = (p, modExpr) =>
      switch p.Parser.token {
      | Lparen => loop(p, parseModuleApplication(p, modExpr))
      | _ => modExpr
      }
    
    let modExpr = loop(p, modExpr)
    {...modExpr, pmod_loc: mkLoc(startPos, p.prevEndPos)}
  }
  
  and parseFunctorArgName = p => {
    let startPos = p.Parser.startPos
    let ident = switch p.Parser.token {
    | Uident(ident) =>
      Parser.next(p)
      ident
    | Underscore =>
      Parser.next(p)
      "_"
    | _ =>
      let msg = "a functor arg name should be module name or _"
      Parser.err(p, Diagnostics.message(msg))
      "_"
    }
    
    Location.mkloc(ident, mkLoc(startPos, p.prevEndPos))
  }
  
  and parseFunctorArg = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Uident(ident) =>
      Parser.next(p)
      let uidentEndPos = p.prevEndPos
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let moduleType = parseModuleType(p)
        let loc = mkLoc(startPos, uidentEndPos)
        let argName = Location.mkloc(ident, loc)
        Some(attrs, argName, Some(moduleType), startPos)
      | Dot =>
        Parser.next(p)
        let moduleType = {
          let moduleLongIdent = parseModuleLongIdentTail(
            p,
            startPos,
            Longident.Lident(ident),
          )
          Ast_helper.Mty.ident(~loc=moduleLongIdent.loc, moduleLongIdent)
        }
        
        let argName = Location.mknoloc("_")
        Some(attrs, argName, Some(moduleType), startPos)
      | _ =>
        let loc = mkLoc(startPos, uidentEndPos)
        let modIdent = Location.mkloc(Longident.Lident(ident), loc)
        let moduleType = Ast_helper.Mty.ident(~loc, modIdent)
        let argName = Location.mknoloc("_")
        Some(attrs, argName, Some(moduleType), startPos)
      }
    | Underscore =>
      Parser.next(p)
      let argName = Location.mkloc("_", mkLoc(startPos, p.prevEndPos))
      Parser.expect(Colon, p)
      let moduleType = parseModuleType(p)
      Some(attrs, argName, Some(moduleType), startPos)
    | _ => None
    }
  }
  
  and parseFunctorArgs = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lparen, p)
    let args = parseCommaDelimitedRegion(
      ~grammar=Grammar.FunctorArgs,
      ~closing=Rparen,
      ~f=parseFunctorArg,
      p,
    )
    
    Parser.expect(Rparen, p)
    switch args {
    | list[] =>
      list[
        (
          list[],
          Location.mkloc("*", mkLoc(startPos, p.prevEndPos)),
          None,
          startPos,
        ),
      ]
    | args => args
    }
  }
  
  and parseFunctorModuleExpr = p => {
    let startPos = p.Parser.startPos
    let args = parseFunctorArgs(p)
    let returnType = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      Some(parseModuleType(~es6Arrow=false, p))
    | _ => None
    }
    
    Parser.expect(EqualGreater, p)
    let rhsModuleExpr = {
      let modExpr = parseModuleExpr(p)
      switch returnType {
      | Some(modType) =>
        Ast_helper.Mod.constraint_(
          ~loc=mkLoc(
            modExpr.pmod_loc.loc_start,
            modType.Parsetree.pmty_loc.loc_end,
          ),
          modExpr,
          modType,
        )
      | None => modExpr
      }
    }
    
    let endPos = p.prevEndPos
    let modExpr = List.fold_right(
      ((attrs, name, moduleType, startPos), acc) =>
        Ast_helper.Mod.functor_(
          ~loc=mkLoc(startPos, endPos),
          ~attrs,
          name,
          moduleType,
          acc,
        ),
      args,
      rhsModuleExpr,
    )
    
    {...modExpr, pmod_loc: mkLoc(startPos, endPos)}
  }
  
  and parseModuleExpr = (~attrs=list[], p) => {
    let attrs = parseAttributes(p)
    let modExpr = if isEs6ArrowFunctor(p) {
      parseFunctorModuleExpr(p)
    } else {
      parsePrimaryModExpr(p)
    }
    
    {
      ...modExpr,
      pmod_attributes: List.concat(list[modExpr.pmod_attributes, attrs]),
    }
  }
  
  and parseConstrainedModExpr = p => {
    let modExpr = parseModuleExpr(p)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let modType = parseModuleType(p)
      let loc = mkLoc(modExpr.pmod_loc.loc_start, modType.pmty_loc.loc_end)
      Ast_helper.Mod.constraint_(~loc, modExpr, modType)
    | _ => modExpr
    }
  }
  
  and parseConstrainedModExprRegion = p =>
    if Grammar.isModExprStart(p.Parser.token) {
      Some(parseConstrainedModExpr(p))
    } else {
      None
    }
  
  and parseModuleApplication = (p, modExpr) => {
    let startPos = p.Parser.startPos
    Parser.expect(Lparen, p)
    let args = parseCommaDelimitedRegion(
      ~grammar=Grammar.ModExprList,
      ~closing=Rparen,
      ~f=parseConstrainedModExprRegion,
      p,
    )
    
    Parser.expect(Rparen, p)
    let args = switch args {
    | list[] =>
      let loc = mkLoc(startPos, p.prevEndPos)
      list[Ast_helper.Mod.structure(~loc, list[])]
    | args => args
    }
    
    List.fold_left(
      (modExpr, arg) =>
        Ast_helper.Mod.apply(
          ~loc=mkLoc(
            modExpr.Parsetree.pmod_loc.loc_start,
            arg.Parsetree.pmod_loc.loc_end,
          ),
          modExpr,
          arg,
        ),
      modExpr,
      args,
    )
  }
  
  and parseModuleOrModuleTypeImplOrPackExpr = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Module, p)
    switch p.Parser.token {
    | Typ => parseModuleTypeImpl(~attrs, startPos, p)
    | Lparen =>
      let expr = parseFirstClassModuleExpr(~startPos, p)
      Ast_helper.Str.eval(~attrs, expr)
    | _ => parseMaybeRecModuleBinding(~attrs, ~startPos, p)
    }
  }
  
  and parseModuleTypeImpl = (~attrs, startPos, p) => {
    Parser.expect(Typ, p)
    let nameStart = p.Parser.startPos
    let name = switch p.Parser.token {
    | Uident(ident) =>
      Parser.next(p)
      let loc = mkLoc(nameStart, p.prevEndPos)
      Location.mkloc(ident, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }
    
    Parser.expect(Equal, p)
    let moduleType = parseModuleType(p)
    let moduleTypeDeclaration = Ast_helper.Mtd.mk(
      ~attrs,
      ~loc=mkLoc(nameStart, p.prevEndPos),
      ~typ=moduleType,
      name,
    )
    
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Str.modtype(~loc, moduleTypeDeclaration)
  }
  
  and parseMaybeRecModuleBinding = (~attrs, ~startPos, p) =>
    switch p.Parser.token {
    | Token.Rec =>
      Parser.next(p)
      Ast_helper.Str.rec_module(parseModuleBindings(~startPos, ~attrs, p))
    | _ =>
      Ast_helper.Str.module_(
        parseModuleBinding(~attrs, ~startPos=p.Parser.startPos, p),
      )
    }
  
  and parseModuleBinding = (~attrs, ~startPos, p) => {
    let name = switch p.Parser.token {
    | Uident(ident) =>
      let startPos = p.Parser.startPos
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Location.mkloc(ident, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }
    
    let body = parseModuleBindingBody(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Mb.mk(~attrs, ~loc, name, body)
  }
  
  and parseModuleBindingBody = p => {
    let returnModType = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      Some(parseModuleType(p))
    | _ => None
    }
    
    Parser.expect(Equal, p)
    let modExpr = parseModuleExpr(p)
    switch returnModType {
    | Some(modType) =>
      Ast_helper.Mod.constraint_(
        ~loc=mkLoc(
          modExpr.pmod_loc.loc_start,
          modType.Parsetree.pmty_loc.loc_end,
        ),
        modExpr,
        modType,
      )
    | None => modExpr
    }
  }
  
  and parseModuleBindings = (~attrs, ~startPos, p) => {
    let rec loop = (p, acc) => {
      let startPos = p.Parser.startPos
      let attrs = parseAttributesAndBinding(p)
      switch p.Parser.token {
      | And =>
        Parser.next(p)
        ignore(Parser.optional(p, Module))
        let modBinding = parseModuleBinding(~attrs, ~startPos, p)
        loop(p, list[modBinding, ...acc])
      | _ => List.rev(acc)
      }
    }
    
    let first = parseModuleBinding(~attrs, ~startPos, p)
    loop(p, list[first])
  }
  
  and parseAtomicModuleType = p => {
    let startPos = p.Parser.startPos
    let moduleType = switch p.Parser.token {
    | Uident(_) =>
      let moduleLongIdent = parseModuleLongIdent(p)
      Ast_helper.Mty.ident(~loc=moduleLongIdent.loc, moduleLongIdent)
    | Lparen =>
      Parser.next(p)
      let mty = parseModuleType(p)
      Parser.expect(Rparen, p)
      {...mty, pmty_loc: mkLoc(startPos, p.prevEndPos)}
    | Lbrace => parseSpecification(p)
    | Module => parseModuleTypeOf(p)
    | Percent =>
      let extension = parseExtension(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Mty.extension(~loc, extension)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Recover.defaultModuleType()
    }
    
    let moduleTypeLoc = mkLoc(startPos, p.prevEndPos)
    {...moduleType, pmty_loc: moduleTypeLoc}
  }
  
  and parseFunctorModuleType = p => {
    let startPos = p.Parser.startPos
    let args = parseFunctorArgs(p)
    Parser.expect(EqualGreater, p)
    let rhs = parseModuleType(p)
    let endPos = p.prevEndPos
    let modType = List.fold_right(
      ((attrs, name, moduleType, startPos), acc) =>
        Ast_helper.Mty.functor_(
          ~loc=mkLoc(startPos, endPos),
          ~attrs,
          name,
          moduleType,
          acc,
        ),
      args,
      rhs,
    )
    
    {...modType, pmty_loc: mkLoc(startPos, endPos)}
  }
  
  and parseModuleType = (~es6Arrow=true, ~with_=true, p) => {
    let attrs = parseAttributes(p)
    let modty = if es6Arrow && isEs6ArrowFunctor(p) {
      parseFunctorModuleType(p)
    } else {
      let modty = parseAtomicModuleType(p)
      switch p.Parser.token {
      | EqualGreater when es6Arrow === true =>
        Parser.next(p)
        let rhs = parseModuleType(~with_=false, p)
        let str = Location.mknoloc("_")
        let loc = mkLoc(modty.pmty_loc.loc_start, p.prevEndPos)
        Ast_helper.Mty.functor_(~loc, str, Some(modty), rhs)
      | _ => modty
      }
    }
    
    let moduleType = {
      ...modty,
      pmty_attributes: List.concat(list[modty.pmty_attributes, attrs]),
    }
    if with_ {
      parseWithConstraints(moduleType, p)
    } else {
      moduleType
    }
  }
  
  and parseWithConstraints = (moduleType, p) =>
    switch p.Parser.token {
    | With =>
      Parser.next(p)
      let first = parseWithConstraint(p)
      let rec loop = (p, acc) =>
        switch p.Parser.token {
        | And =>
          Parser.next(p)
          loop(p, list[parseWithConstraint(p), ...acc])
        | _ => List.rev(acc)
        }
      
      let constraints = loop(p, list[first])
      let loc = mkLoc(moduleType.pmty_loc.loc_start, p.prevEndPos)
      Ast_helper.Mty.with_(~loc, moduleType, constraints)
    | _ => moduleType
    }
  
  and parseWithConstraint = p =>
    switch p.Parser.token {
    | Module =>
      Parser.next(p)
      let modulePath = parseModuleLongIdent(p)
      switch p.Parser.token {
      | ColonEqual =>
        Parser.next(p)
        let lident = parseModuleLongIdent(p)
        Parsetree.Pwith_modsubst(modulePath, lident)
      | Equal =>
        Parser.next(p)
        let lident = parseModuleLongIdent(p)
        Parsetree.Pwith_module(modulePath, lident)
      | token =>
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        let lident = parseModuleLongIdent(p)
        Parsetree.Pwith_modsubst(modulePath, lident)
      }
    | Typ =>
      Parser.next(p)
      let typeConstr = parseValuePath(p)
      let params = parseTypeParams(p)
      switch p.Parser.token {
      | ColonEqual =>
        Parser.next(p)
        let typExpr = parseTypExpr(p)
        Parsetree.Pwith_typesubst(
          typeConstr,
          Ast_helper.Type.mk(
            ~loc=typeConstr.loc,
            ~params,
            ~manifest=typExpr,
            Location.mkloc(Longident.last(typeConstr.txt), typeConstr.loc),
          ),
        )
      | Equal =>
        Parser.next(p)
        let typExpr = parseTypExpr(p)
        let typeConstraints = parseTypeConstraints(p)
        Parsetree.Pwith_type(
          typeConstr,
          Ast_helper.Type.mk(
            ~loc=typeConstr.loc,
            ~params,
            ~manifest=typExpr,
            ~cstrs=typeConstraints,
            Location.mkloc(Longident.last(typeConstr.txt), typeConstr.loc),
          ),
        )
      | token =>
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        let typExpr = parseTypExpr(p)
        let typeConstraints = parseTypeConstraints(p)
        Parsetree.Pwith_type(
          typeConstr,
          Ast_helper.Type.mk(
            ~loc=typeConstr.loc,
            ~params,
            ~manifest=typExpr,
            ~cstrs=typeConstraints,
            Location.mkloc(Longident.last(typeConstr.txt), typeConstr.loc),
          ),
        )
      }
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      raise(Exit)
    }
  
  and parseModuleTypeOf = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Module, p)
    Parser.expect(Typ, p)
    Parser.expect(Of, p)
    let moduleExpr = parseModuleExpr(p)
    Ast_helper.Mty.typeof_(~loc=mkLoc(startPos, p.prevEndPos), moduleExpr)
  }
  
  and parseSpecification = p => {
    Parser.expect(Lbrace, p)
    let spec = parseDelimitedRegion(
      ~grammar=Grammar.Signature,
      ~closing=Rbrace,
      ~f=parseSignatureItemRegion,
      p,
    )
    
    Parser.expect(Rbrace, p)
    Ast_helper.Mty.signature(spec)
  }
  
  @progress(
    (
      Parser.next,
      Parser.expect,
      Recover.recoverLident,
      Recover.skipTokensAndMaybeRetry,
    )
  )
  and parseSignature = p =>
    parseRegion(~grammar=Grammar.Signature, ~f=parseSignatureItemRegion, p)
  
  and parseSignatureItemRegion = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Let =>
      let valueDesc = parseSignLetDesc(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.value(~loc, valueDesc))
    | Typ =>
      switch parseTypeDefinitionOrExtension(~attrs, p) {
      | TypeDef(recFlag, types) =>
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Sig.type_(~loc, recFlag, types))
      | TypeExt(ext) =>
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Sig.type_extension(~loc, ext))
      }
    | External =>
      let externalDef = parseExternalDef(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.value(~loc, externalDef))
    | Exception =>
      let exceptionDef = parseExceptionDef(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.exception_(~loc, exceptionDef))
    | Open =>
      let openDescription = parseOpenDescription(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.open_(~loc, openDescription))
    | Include =>
      Parser.next(p)
      let moduleType = parseModuleType(p)
      let includeDescription = Ast_helper.Incl.mk(
        ~loc=mkLoc(startPos, p.prevEndPos),
        ~attrs,
        moduleType,
      )
      
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.include_(~loc, includeDescription))
    | Module =>
      Parser.next(p)
      switch p.Parser.token {
      | Uident(_) =>
        let modDecl = parseModuleDeclarationOrAlias(~attrs, p)
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Sig.module_(~loc, modDecl))
      | Rec =>
        let recModule = parseRecModuleSpec(~attrs, ~startPos, p)
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Sig.rec_module(~loc, recModule))
      | Typ => Some(parseModuleTypeDeclaration(~attrs, ~startPos, p))
      | t =>
        let modDecl = parseModuleDeclarationOrAlias(~attrs, p)
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Sig.module_(~loc, modDecl))
      }
    | AtAt =>
      let attr = parseStandaloneAttribute(p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.attribute(~loc, attr))
    | PercentPercent =>
      let extension = parseExtension(~moduleLanguage=true, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.extension(~attrs, ~loc, extension))
    | Import =>
      Parser.next(p)
      parseSignatureItemRegion(p)
    | _ => None
    }
  }
  
  and parseRecModuleSpec = (~attrs, ~startPos, p) => {
    Parser.expect(Rec, p)
    let rec loop = (p, spec) => {
      let startPos = p.Parser.startPos
      let attrs = parseAttributesAndBinding(p)
      switch p.Parser.token {
      | And =>
        Parser.expect(And, p)
        let decl = parseRecModuleDeclaration(~attrs, ~startPos, p)
        loop(p, list[decl, ...spec])
      | _ => List.rev(spec)
      }
    }
    
    let first = parseRecModuleDeclaration(~attrs, ~startPos, p)
    loop(p, list[first])
  }
  
  and parseRecModuleDeclaration = (~attrs, ~startPos, p) => {
    let name = switch p.Parser.token {
    | Uident(modName) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(modName, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }
    
    Parser.expect(Colon, p)
    let modType = parseModuleType(p)
    Ast_helper.Md.mk(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, name, modType)
  }
  
  and parseModuleDeclarationOrAlias = (~attrs, p) => {
    let startPos = p.Parser.startPos
    let moduleName = switch p.Parser.token {
    | Uident(ident) =>
      let loc = mkLoc(p.Parser.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(ident, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }
    
    let body = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      parseModuleType(p)
    | Equal =>
      Parser.next(p)
      let lident = parseModuleLongIdent(p)
      Ast_helper.Mty.alias(lident)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Recover.defaultModuleType()
    }
    
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Md.mk(~loc, ~attrs, moduleName, body)
  }
  
  and parseModuleTypeDeclaration = (~attrs, ~startPos, p) => {
    Parser.expect(Typ, p)
    
    let moduleName = switch p.Parser.token {
    | Uident(ident) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(ident, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }
    
    let typ = switch p.Parser.token {
    | Equal =>
      Parser.next(p)
      Some(parseModuleType(p))
    | _ => None
    }
    
    let moduleDecl = Ast_helper.Mtd.mk(~attrs, ~typ?, moduleName)
    Ast_helper.Sig.modtype(~loc=mkLoc(startPos, p.prevEndPos), moduleDecl)
  }
  
  and parseSignLetDesc = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Let, p)
    let (name, loc) = parseLident(p)
    let name = Location.mkloc(name, loc)
    Parser.expect(Colon, p)
    let typExpr = parsePolyTypeExpr(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Val.mk(~loc, ~attrs, name, typExpr)
  }
  
  and parseAttributeId = p => {
    let startPos = p.Parser.startPos
    let rec loop = (p, acc) =>
      switch p.Parser.token {
      | Lident(ident) | Uident(ident) =>
        Parser.next(p)
        let id = acc ++ ident
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          loop(p, id ++ ".")
        | _ => id
        }
      | token when Token.isKeyword(token) =>
        Parser.next(p)
        let id = acc ++ Token.toString(token)
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          loop(p, id ++ ".")
        | _ => id
        }
      | token =>
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        acc
      }
    
    let id = loop(p, "")
    let endPos = p.prevEndPos
    Location.mkloc(id, mkLoc(startPos, endPos))
  }
  
  and parsePayload = p => {
    let structure = switch p.Parser.token {
    | Lparen when p.startPos.pos_cnum == p.prevEndPos.pos_cnum =>
      Parser.next(p)
      let items = parseDelimitedRegion(
        ~grammar=Grammar.Structure,
        ~closing=Rparen,
        ~f=parseStructureItemRegion,
        p,
      )
      
      Parser.expect(Rparen, p)
      items
    | _ => list[]
    }
    
    Parsetree.PStr(structure)
  }
  
  and parseAttribute = p =>
    switch p.Parser.token {
    | At =>
      Parser.next(p)
      let attrId = parseAttributeId(p)
      let payload = parsePayload(p)
      Some(attrId, payload)
    | _ => None
    }
  
  and parseAttributes = p =>
    parseRegion(p, ~grammar=Grammar.Attribute, ~f=parseAttribute)
  
  and parseStandaloneAttribute = p => {
    Parser.expect(AtAt, p)
    let attrId = parseAttributeId(p)
    let payload = parsePayload(p)
    (attrId, payload)
  }
  
  and parseExtension = (~moduleLanguage=false, p) => {
    if moduleLanguage {
      Parser.expect(PercentPercent, p)
    } else {
      Parser.expect(Percent, p)
    }
    let attrId = parseAttributeId(p)
    let payload = parsePayload(p)
    (attrId, payload)
  }
}

module ParsetreeViewer: {
  let arrowType: Parsetree.core_type => (
    Parsetree.attributes,
    list<(Parsetree.attributes, Asttypes.arg_label, Parsetree.core_type)>,
    Parsetree.core_type,
  )
  
  let functorType: Parsetree.module_type => (
    list<(
      Parsetree.attributes,
      Asttypes.loc<string>,
      option<Parsetree.module_type>,
    )>,
    Parsetree.module_type,
  )
  
  let processUncurriedAttribute: Parsetree.attributes => (
    bool,
    Parsetree.attributes,
  )
  
  let collectIfExpressions: Parsetree.expression => (
    list<(Parsetree.expression, Parsetree.expression)>,
    option<Parsetree.expression>,
  )
  
  let collectListExpressions: Parsetree.expression => (
    list<Parsetree.expression>,
    option<Parsetree.expression>,
  )
  
  let funExpr: Parsetree.expression => (
    Parsetree.attributes,
    list<(
      Parsetree.attributes,
      Asttypes.arg_label,
      option<Parsetree.expression>,
      Parsetree.pattern,
    )>,
    Parsetree.expression,
  )
  
  let isHuggableExpression: Parsetree.expression => bool
  
  let isHuggablePattern: Parsetree.pattern => bool
  
  let isGhostUnitBinding: (int, Parsetree.value_binding) => bool
  
  let operatorPrecedence: string => int
  
  let isUnaryExpression: Parsetree.expression => bool
  let isBinaryOperator: string => bool
  let isBinaryExpression: Parsetree.expression => bool
  
  let isMultiplicativeOperator: string => bool
  let isEqualityOperator: string => bool
  let flattenableOperators: (string, string) => bool
  
  let hasAttributes: Parsetree.attributes => bool
  
  let isArrayAccess: Parsetree.expression => bool
  let isTernaryExpr: Parsetree.expression => bool
  
  let collectTernaryParts: Parsetree.expression => (
    list<(Parsetree.expression, Parsetree.expression)>,
    Parsetree.expression,
  )
  
  let parametersShouldHug: list<(
    Parsetree.attributes,
    Asttypes.arg_label,
    option<Parsetree.expression>,
    Parsetree.pattern,
  )> => bool
  
  let filterTernaryAttributes: Parsetree.attributes => Parsetree.attributes
  
  let isJsxExpression: Parsetree.expression => bool
  let hasJsxAttribute: Parsetree.attributes => bool
  
  let shouldIndentBinaryExpr: Parsetree.expression => bool
  let shouldInlineRhsBinaryExpr: Parsetree.expression => bool
  let filterPrinteableAttributes: Parsetree.attributes => Parsetree.attributes
  let partitionPrinteableAttributes: Parsetree.attributes => (
    Parsetree.attributes,
    Parsetree.attributes,
  )
  
  let requiresSpecialCallbackPrinting: list<(
    Asttypes.arg_label,
    Parsetree.expression,
  )> => bool
  
  let modExprApply: Parsetree.module_expr => (
    list<Parsetree.module_expr>,
    Parsetree.module_expr,
  )
  
  let modExprFunctor: Parsetree.module_expr => (
    list<(
      Parsetree.attributes,
      Asttypes.loc<string>,
      option<Parsetree.module_type>,
    )>,
    Parsetree.module_expr,
  )
  
  let splitGenTypeAttr: Parsetree.attributes => (bool, Parsetree.attributes)
  
  let collectPatternsFromListConstruct: (
    list<Parsetree.pattern>,
    Parsetree.pattern,
  ) => (list<Parsetree.pattern>, Parsetree.pattern)
  
  let isBlockExpr: Parsetree.expression => bool
} = {
  open Parsetree
  
  let arrowType = ct => {
    let rec process = (attrsBefore, acc, typ) =>
      switch typ {
      | {
          ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2),
          ptyp_attributes: list[],
        } =>
        let arg = (list[], lbl, typ1)
        process(attrsBefore, list[arg, ...acc], typ2)
      | {
          ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2),
          ptyp_attributes: list[({txt: "bs"}, _)] as attrs,
        } =>
        let arg = (attrs, lbl, typ1)
        process(attrsBefore, list[arg, ...acc], typ2)
      | {
          ptyp_desc: Ptyp_arrow(Nolabel, typ1, typ2),
          ptyp_attributes: attrs,
        } as returnType =>
        let args = List.rev(acc)
        (attrsBefore, args, returnType)
      | {
          ptyp_desc: Ptyp_arrow((Labelled(_) | Optional(_)) as lbl, typ1, typ2),
          ptyp_attributes: attrs,
        } =>
        let arg = (attrs, lbl, typ1)
        process(attrsBefore, list[arg, ...acc], typ2)
      | typ => (attrsBefore, List.rev(acc), typ)
      }
    
    switch ct {
    | {
        ptyp_desc: Ptyp_arrow(Nolabel, _typ1, _typ2),
        ptyp_attributes: attrs,
      } as typ =>
      process(attrs, list[], {...typ, ptyp_attributes: list[]})
    | typ => process(list[], list[], typ)
    }
  }
  
  let functorType = modtype => {
    let rec process = (acc, modtype) =>
      switch modtype {
      | {
          pmty_desc: Pmty_functor(lbl, argType, returnType),
          pmty_attributes: attrs,
        } =>
        let arg = (attrs, lbl, argType)
        process(list[arg, ...acc], returnType)
      | modType => (List.rev(acc), modType)
      }
    
    process(list[], modtype)
  }
  
  let processUncurriedAttribute = attrs => {
    let rec process = (uncurriedSpotted, acc, attrs) =>
      switch attrs {
      | list[] => (uncurriedSpotted, List.rev(acc))
      | list[({Location.txt: "bs"}, _), ...rest] => process(true, acc, rest)
      | list[attr, ...rest] =>
        process(uncurriedSpotted, list[attr, ...acc], rest)
      }
    
    process(false, list[], attrs)
  }
  
  let collectIfExpressions = expr => {
    let rec collect = (acc, expr) =>
      switch expr.pexp_desc {
      | Pexp_ifthenelse(ifExpr, thenExpr, Some(elseExpr)) =>
        collect(list[(ifExpr, thenExpr), ...acc], elseExpr)
      | Pexp_ifthenelse(ifExpr, thenExpr, None as elseExpr) =>
        let ifs = List.rev(list[(ifExpr, thenExpr), ...acc])
        (ifs, elseExpr)
      | _ => (List.rev(acc), Some(expr))
      }
    
    collect(list[], expr)
  }
  
  let collectListExpressions = expr => {
    let rec collect = (acc, expr) =>
      switch expr.pexp_desc {
      | Pexp_construct({txt: Longident.Lident("[]")}, _) =>
        (List.rev(acc), None)
      | Pexp_construct(
          {txt: Longident.Lident("::")},
          Some({pexp_desc: Pexp_tuple(list[hd, tail])}),
        ) =>
        collect(list[hd, ...acc], tail)
      | _ => (List.rev(acc), Some(expr))
      }
    
    collect(list[], expr)
  }
  
  let funExpr = expr => {
    let rec collectNewTypes = (acc, returnExpr) =>
      switch returnExpr {
      | {
          pexp_desc: Pexp_newtype(stringLoc, returnExpr),
          pexp_attributes: list[],
        } =>
        collectNewTypes(list[stringLoc, ...acc], returnExpr)
      | returnExpr =>
        let loc = switch (acc, List.rev(acc)) {
        | (list[startLoc, ..._], list[endLoc, ..._]) =>
          {...startLoc.loc, loc_end: endLoc.loc.loc_end}
        | _ => Location.none
        }
        
        let txt = List.fold_right(
          (curr, acc) => acc ++ " " ++ curr.Location.txt,
          acc,
          "type",
        )
        (Location.mkloc(txt, loc), returnExpr)
      }
    
    let rec collect = (attrsBefore, acc, expr) =>
      switch expr {
      | {
          pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr),
          pexp_attributes: list[],
        } =>
        let parameter = (list[], lbl, defaultExpr, pattern)
        collect(attrsBefore, list[parameter, ...acc], returnExpr)
      | {pexp_desc: Pexp_newtype(stringLoc, rest), pexp_attributes: attrs} =>
        let (var, returnExpr) = collectNewTypes(list[stringLoc], rest)
        let parameter = (
          attrs,
          Asttypes.Nolabel,
          None,
          Ast_helper.Pat.var(~loc=stringLoc.loc, var),
        )
        collect(attrsBefore, list[parameter, ...acc], returnExpr)
      | {
          pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr),
          pexp_attributes: list[({txt: "bs"}, _)] as attrs,
        } =>
        let parameter = (attrs, lbl, defaultExpr, pattern)
        collect(attrsBefore, list[parameter, ...acc], returnExpr)
      | {
          pexp_desc: 
            Pexp_fun(
              (Labelled(_) | Optional(_)) as lbl,
              defaultExpr,
              pattern,
              returnExpr,
            ),
          pexp_attributes: attrs,
        } =>
        let parameter = (attrs, lbl, defaultExpr, pattern)
        collect(attrsBefore, list[parameter, ...acc], returnExpr)
      | expr => (attrsBefore, List.rev(acc), expr)
      }
    
    switch expr {
    | {
        pexp_desc: Pexp_fun(Nolabel, defaultExpr, pattern, returnExpr),
        pexp_attributes: attrs,
      } as expr =>
      collect(attrs, list[], {...expr, pexp_attributes: list[]})
    | expr => collect(list[], list[], expr)
    }
  }
  
  let isHuggableExpression = expr =>
    switch expr.pexp_desc {
    | ((((Pexp_array(_) | Pexp_tuple(_))
      | Pexp_construct({txt: Longident.Lident("::")}, _))
      | Pexp_construct({txt: Longident.Lident("[]")}, _))
      | Pexp_extension({txt: "bs.obj"}, _))
      | Pexp_record(_) =>
      true
    | _ => false
    }
  
  let isHuggablePattern = pattern =>
    switch pattern.ppat_desc {
    | ((Ppat_array(_) | Ppat_tuple(_)) | Ppat_record(_)) | Ppat_construct(_) =>
      true
    | _ => false
    }
  
  let isGhostUnitBinding = (i, vb) =>
    switch vb.pvb_pat {
    | {
        ppat_loc: loc,
        ppat_desc: Ppat_construct({txt: Longident.Lident("()")}, None),
      } when loc.loc_ghost && i === 0 =>
      true
    | _ => false
    }
  
  let operatorPrecedence = operator =>
    switch operator {
    | ":=" => 1
    | "||" => 2
    | "&&" => 3
    | ((((((("=" | "==") | "<") | ">") | "!=") | "!==") | "<=") | ">=")
      | "|>" =>
      4
    | ((("+" | "+.") | "-") | "-.") | "++" => 5
    | (("*" | "*.") | "/") | "/." => 6
    | "**" => 7
    | ("#" | "##") | "|." => 8
    | _ => 0
    }
  
  let isUnaryOperator = operator =>
    switch operator {
    | (((("~+" | "~+.") | "~-") | "~-.") | "not") | "!" => true
    | _ => false
    }
  
  let isUnaryExpression = expr =>
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list[(Nolabel, _arg)],
      ) when isUnaryOperator(operator) =>
      true
    | _ => false
    }
  
  let isBinaryOperator = operator =>
    switch operator {
    | (((((((((((((((((((((((":=" | "||") | "&&") | "=") | "==") | "<") | ">")
      | "!=")
      | "!==")
      | "<=")
      | ">=")
      | "|>")
      | "+")
      | "+.")
      | "-")
      | "-.")
      | "++")
      | "^")
      | "*")
      | "*.")
      | "/")
      | "/.")
      | "**")
      | "|.")
      | "<>" =>
      true
    | _ => false
    }
  
  let isBinaryExpression = expr =>
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list[(Nolabel, _operand1), (Nolabel, _operand2)],
      ) when isBinaryOperator(operator) =>
      true
    | _ => false
    }
  
  let isMultiplicativeOperator = operator =>
    switch operator {
    | (("*" | "*.") | "/") | "/." => true
    | _ => false
    }
  
  let isEqualityOperator = operator =>
    switch operator {
    | (("=" | "==") | "<>") | "!=" => true
    | _ => false
    }
  
  let flattenableOperators = (parentOperator, childOperator) => {
    let precParent = operatorPrecedence(parentOperator)
    let precChild = operatorPrecedence(childOperator)
    if precParent === precChild {
      !(
        isMultiplicativeOperator(parentOperator) &&
        isMultiplicativeOperator(childOperator) &&
        (parentOperator != childOperator)
      ) &&
      !(isEqualityOperator(parentOperator) && isEqualityOperator(childOperator))
    } else {
      false
    }
  }
  
  let hasAttributes = attrs => {
    let attrs = List.filter(
      attr =>
        switch attr {
        | ({Location.txt: "bs" | "ns.ternary"}, _) => false
        | _ => true
        },
      attrs,
    )
    switch attrs {
    | list[] => false
    | _ => true
    }
  }
  
  let isArrayAccess = expr =>
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Ldot(Lident("Array"), "get")})},
        list[(Nolabel, parentExpr), (Nolabel, memberExpr)],
      ) =>
      true
    | _ => false
    }
  
  let isTernaryExpr = expr =>
    switch expr {
    | {
        pexp_attributes: list[({txt: "ns.ternary"}, _), ..._],
        pexp_desc: Pexp_ifthenelse(_),
      } =>
      true
    | _ => false
    }
  
  let collectTernaryParts = expr => {
    let rec collect = (acc, expr) =>
      switch expr {
      | {
          pexp_attributes: list[({txt: "ns.ternary"}, _), ..._],
          pexp_desc: Pexp_ifthenelse(condition, consequent, Some(alternate)),
        } =>
        collect(list[(condition, consequent), ...acc], alternate)
      | alternate => (List.rev(acc), alternate)
      }
    
    collect(list[], expr)
  }
  
  let parametersShouldHug = parameters =>
    switch parameters {
    | list[(list[], Asttypes.Nolabel, None, pat)] when isHuggablePattern(pat) =>
      true
    | _ => false
    }
  
  let filterTernaryAttributes = attrs =>
    List.filter(
      attr =>
        switch attr {
        | ({Location.txt: "ns.ternary"}, _) => false
        | _ => true
        },
      attrs,
    )
  
  let isJsxExpression = expr =>
    switch expr {
    | {pexp_attributes: list[({txt: "JSX"}, _)], pexp_desc: Pexp_apply(_)} =>
      true
    | _ => false
    }
  
  let hasJsxAttribute = attributes =>
    switch attributes {
    | list[({Location.txt: "JSX"}, _), ..._] => true
    | _ => false
    }
  
  let shouldIndentBinaryExpr = expr => {
    let samePrecedenceSubExpression = (operator, subExpression) =>
      switch subExpression {
      | {
          pexp_desc: 
            Pexp_apply(
              {pexp_desc: Pexp_ident({txt: Longident.Lident(subOperator)})},
              list[(Nolabel, lhs), (Nolabel, rhs)],
            ),
        } when isBinaryOperator(subOperator) =>
        flattenableOperators(operator, subOperator)
      | _ => true
      }
    
    switch expr {
    | {
        pexp_desc: 
          Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
            list[(Nolabel, lhs), (Nolabel, rhs)],
          ),
      } when isBinaryOperator(operator) =>
      isEqualityOperator(operator) ||
      !samePrecedenceSubExpression(operator, lhs)
    | _ => false
    }
  }
  
  let shouldInlineRhsBinaryExpr = rhs =>
    switch rhs.pexp_desc {
    | ((((((((((Parsetree.Pexp_constant(_) | Pexp_let(_)) | Pexp_letmodule(_))
      | Pexp_letexception(_))
      | Pexp_sequence(_))
      | Pexp_open(_))
      | Pexp_ifthenelse(_))
      | Pexp_for(_))
      | Pexp_while(_))
      | Pexp_try(_))
      | Pexp_array(_))
      | Pexp_record(_) =>
      true
    | _ => false
    }
  
  let filterPrinteableAttributes = attrs =>
    List.filter(
      attr =>
        switch attr {
        | ({Location.txt: "bs" | "ns.ternary"}, _) => false
        | _ => true
        },
      attrs,
    )
  
  let partitionPrinteableAttributes = attrs =>
    List.partition(
      attr =>
        switch attr {
        | ({Location.txt: "bs" | "ns.ternary"}, _) => false
        | _ => true
        },
      attrs,
    )
  
  let requiresSpecialCallbackPrinting = args => {
    let rec loop = args =>
      switch args {
      | list[] => false
      | list[(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)})] => true
      | list[(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)}), ..._] => false
      | list[_, ...rest] => loop(rest)
      }
    
    loop(args)
  }
  
  let modExprApply = modExpr => {
    let rec loop = (acc, modExpr) =>
      switch modExpr {
      | {pmod_desc: Pmod_apply(next, arg)} => loop(list[arg, ...acc], next)
      | _ => (acc, modExpr)
      }
    
    loop(list[], modExpr)
  }
  
  let modExprFunctor = modExpr => {
    let rec loop = (acc, modExpr) =>
      switch modExpr {
      | {
          pmod_desc: Pmod_functor(lbl, modType, returnModExpr),
          pmod_attributes: attrs,
        } =>
        let param = (attrs, lbl, modType)
        loop(list[param, ...acc], returnModExpr)
      | returnModExpr => (List.rev(acc), returnModExpr)
      }
    
    loop(list[], modExpr)
  }
  
  let splitGenTypeAttr = attrs =>
    switch attrs {
    | list[({Location.txt: "genType"}, _), ...attrs] => (true, attrs)
    | attrs => (false, attrs)
    }
  
  let rec collectPatternsFromListConstruct = (acc, pattern) => {
    open Parsetree
    switch pattern.ppat_desc {
    | Ppat_construct(
        {txt: Longident.Lident("::")},
        Some({ppat_desc: Ppat_tuple(list[pat, rest])}),
      ) =>
      collectPatternsFromListConstruct(list[pat, ...acc], rest)
    | _ => (List.rev(acc), pattern)
    }
  }
  
  let isBlockExpr = expr =>
    switch expr.pexp_desc {
    | (((Pexp_letmodule(_) | Pexp_letexception(_)) | Pexp_let(_))
      | Pexp_open(_))
      | Pexp_sequence(_) =>
      true
    | _ => false
    }
}

module Parens: {
  let unaryExprOperand: Parsetree.expression => bool
  
  let binaryExprOperand: (~isLhs: bool, Parsetree.expression, string) => bool
  let subBinaryExprOperand: (string, string) => bool
  let flattenOperandRhs: (string, Parsetree.expression) => bool
  
  let lazyOrAssertExprRhs: Parsetree.expression => bool
  
  let fieldExpr: Parsetree.expression => bool
  
  let blockExpr: Parsetree.expression => bool
  
  let setFieldExprRhs: Parsetree.expression => bool
  
  let ternaryOperand: Parsetree.expression => bool
  
  let jsxPropExpr: Parsetree.expression => bool
  let jsxChildExpr: Parsetree.expression => bool
  
  let binaryExpr: Parsetree.expression => bool
  let modTypeFunctorReturn: Parsetree.module_type => bool
  let modTypeWithOperand: Parsetree.module_type => bool
  let modExprFunctorConstraint: Parsetree.module_type => bool
} = {
  let unaryExprOperand = expr =>
    switch expr {
    | {Parsetree.pexp_attributes: attrs}
      when {
        let (uncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(
          attrs,
        )
        
        switch attrs {
        | list[_, ..._] => true
        | list[] => false
        }
      } =>
      true
    | expr
      when ParsetreeViewer.isUnaryExpression(expr) ||
      ParsetreeViewer.isBinaryExpression(expr) =>
      true
    | {
        pexp_desc: 
          Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
      } =>
      false
    | {
        pexp_desc: 
          (((((((((((Pexp_lazy(_) | Pexp_assert(_)) | Pexp_fun(_))
          | Pexp_newtype(_))
          | Pexp_function(_))
          | Pexp_constraint(_))
          | Pexp_setfield(_))
          | Pexp_extension(_))
          | Pexp_match(_))
          | Pexp_try(_))
          | Pexp_while(_))
          | Pexp_for(_))
          | Pexp_ifthenelse(_),
      } =>
      true
    | _ => false
    }
  
  let binaryExprOperand = (~isLhs, expr, parentOperator) =>
    switch expr {
    | {
        Parsetree.pexp_desc: 
          Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
      } =>
      false
    | {
        pexp_desc: 
          ((Pexp_constraint(_) | Pexp_fun(_)) | Pexp_function(_))
          | Pexp_newtype(_),
      } =>
      true
    | expr when ParsetreeViewer.isBinaryExpression(expr) => true
    | expr when ParsetreeViewer.isTernaryExpr(expr) => true
    | {pexp_desc: Pexp_lazy(_) | Pexp_assert(_)} when isLhs => true
    | _ => false
    }
  
  let subBinaryExprOperand = (parentOperator, childOperator) => {
    let precParent = ParsetreeViewer.operatorPrecedence(parentOperator)
    let precChild = ParsetreeViewer.operatorPrecedence(childOperator)
    precParent > precChild ||
      (precParent === precChild &&
        !ParsetreeViewer.flattenableOperators(parentOperator, childOperator)) ||
      parentOperator == "||" && childOperator == "&&"
  }
  
  let flattenOperandRhs = (parentOperator, rhs) =>
    switch rhs.Parsetree.pexp_desc {
    | Parsetree.Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list[(_, left), (_, right)],
      ) when ParsetreeViewer.isBinaryOperator(operator) =>
      let precParent = ParsetreeViewer.operatorPrecedence(parentOperator)
      let precChild = ParsetreeViewer.operatorPrecedence(operator)
      precParent > precChild || (rhs.pexp_attributes != list[])
    | Pexp_constraint(
        {pexp_desc: Pexp_pack(_)},
        {ptyp_desc: Ptyp_package(_)},
      ) =>
      false
    | ((Pexp_fun(_) | Pexp_newtype(_)) | Pexp_setfield(_))
      | Pexp_constraint(_) =>
      true
    | _ when ParsetreeViewer.isTernaryExpr(rhs) => true
    | _ => false
    }
  
  let lazyOrAssertExprRhs = expr =>
    switch expr {
    | {Parsetree.pexp_attributes: attrs}
      when {
        let (uncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(
          attrs,
        )
        
        switch attrs {
        | list[_, ..._] => true
        | list[] => false
        }
      } =>
      true
    | expr when ParsetreeViewer.isBinaryExpression(expr) => true
    | {
        pexp_desc: 
          Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
      } =>
      false
    | {
        pexp_desc: 
          ((((((((((Pexp_lazy(_) | Pexp_assert(_)) | Pexp_fun(_))
          | Pexp_newtype(_))
          | Pexp_function(_))
          | Pexp_constraint(_))
          | Pexp_setfield(_))
          | Pexp_match(_))
          | Pexp_try(_))
          | Pexp_while(_))
          | Pexp_for(_))
          | Pexp_ifthenelse(_),
      } =>
      true
    | _ => false
    }
  
  let fieldExpr = expr =>
    switch expr {
    | {Parsetree.pexp_attributes: attrs}
      when {
        let (uncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(
          attrs,
        )
        
        switch attrs {
        | list[_, ..._] => true
        | list[] => false
        }
      } =>
      true
    | expr
      when ParsetreeViewer.isBinaryExpression(expr) ||
      ParsetreeViewer.isUnaryExpression(expr) =>
      true
    | {
        pexp_desc: 
          Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
      } =>
      false
    | {
        pexp_desc: 
          ((((((((((Pexp_lazy(_) | Pexp_assert(_)) | Pexp_fun(_))
          | Pexp_newtype(_))
          | Pexp_function(_))
          | Pexp_constraint(_))
          | Pexp_setfield(_))
          | Pexp_match(_))
          | Pexp_try(_))
          | Pexp_while(_))
          | Pexp_for(_))
          | Pexp_ifthenelse(_),
      } =>
      true
    | _ => false
    }
  
  let blockExpr = expr =>
    switch expr {
    | {
        Parsetree.pexp_desc: 
          Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
      } =>
      false
    | {pexp_desc: Pexp_constraint(_)} => true
    | _ => false
    }
  
  let setFieldExprRhs = expr =>
    switch expr {
    | {
        Parsetree.pexp_desc: 
          Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
      } =>
      false
    | {pexp_desc: Pexp_constraint(_)} => true
    | _ => false
    }
  
  let ternaryOperand = expr =>
    switch expr {
    | {
        Parsetree.pexp_desc: 
          Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
      } =>
      false
    | {pexp_desc: Pexp_constraint(_)} => true
    | {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)} =>
      let (_attrsOnArrow, _parameters, returnExpr) = ParsetreeViewer.funExpr(
        expr,
      )
      switch returnExpr.pexp_desc {
      | Pexp_constraint(_) => true
      | _ => false
      }
    | _ => false
    }
  
  let startsWithMinus = txt => {
    let len = String.length(txt)
    if len === 0 {
      false
    } else {
      let s = String.get(txt, 0)
      s == '-'
    }
  }
  
  let jsxPropExpr = expr =>
    switch expr {
    | {
        Parsetree.pexp_desc: 
          Pexp_constant(Pconst_integer(x, _) | Pconst_float(x, _)),
        pexp_attributes: list[],
      } when startsWithMinus(x) =>
      true
    | {
        Parsetree.pexp_desc: 
          ((((((((((((Pexp_ident(_) | Pexp_constant(_)) | Pexp_field(_))
          | Pexp_construct(_))
          | Pexp_array(_))
          | Pexp_pack(_))
          | Pexp_record(_))
          | Pexp_extension(_))
          | Pexp_letmodule(_))
          | Pexp_letexception(_))
          | Pexp_open(_))
          | Pexp_sequence(_))
          | Pexp_let(_))
          | Pexp_tuple(_),
        pexp_attributes: list[],
      } =>
      false
    | {
        Parsetree.pexp_desc: 
          Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
        pexp_attributes: list[],
      } =>
      false
    | _ => true
    }
  
  let jsxChildExpr = expr =>
    switch expr {
    | {
        Parsetree.pexp_desc: 
          Pexp_constant(Pconst_integer(x, _) | Pconst_float(x, _)),
        pexp_attributes: list[],
      } when startsWithMinus(x) =>
      true
    | {
        Parsetree.pexp_desc: 
          (((((((((((Pexp_ident(_) | Pexp_constant(_)) | Pexp_field(_))
          | Pexp_construct(_))
          | Pexp_array(_))
          | Pexp_pack(_))
          | Pexp_record(_))
          | Pexp_extension(_))
          | Pexp_letmodule(_))
          | Pexp_letexception(_))
          | Pexp_open(_))
          | Pexp_sequence(_))
          | Pexp_let(_),
        pexp_attributes: list[],
      } =>
      false
    | {
        Parsetree.pexp_desc: 
          Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
        pexp_attributes: list[],
      } =>
      false
    | expr when ParsetreeViewer.isJsxExpression(expr) => false
    | _ => true
    }
  
  let binaryExpr = expr =>
    switch expr {
    | {Parsetree.pexp_attributes: list[_, ..._]} as expr
      when ParsetreeViewer.isBinaryExpression(expr) =>
      true
    | _ => false
    }
  
  let modTypeFunctorReturn = modType =>
    switch modType {
    | {Parsetree.pmty_desc: Pmty_with(_)} => true
    | _ => false
    }
  
  let modTypeWithOperand = modType =>
    switch modType {
    | {Parsetree.pmty_desc: Pmty_functor(_)} => true
    | _ => false
    }
  
  let modExprFunctorConstraint = modType =>
    switch modType {
    | {Parsetree.pmty_desc: Pmty_functor(_)} => true
    | _ => false
    }
}

module CommentTable = {
  type rec t = {
    leading: Hashtbl.t<Location.t, list<Comment.t>>,
    inside: Hashtbl.t<Location.t, list<Comment.t>>,
    trailing: Hashtbl.t<Location.t, list<Comment.t>>,
    printedTrailing: Hashtbl.t<Location.t, list<Comment.t>>,
  }
  
  let make = () => {
    leading: Hashtbl.create(100),
    inside: Hashtbl.create(100),
    trailing: Hashtbl.create(100),
    printedTrailing: Hashtbl.create(100),
  }
  
  let empty = make()
  
  let log = t => {
    open Location
    let leadingStuff = Hashtbl.fold(
      (k: Location.t, v: list<Comment.t>, acc) => {
        let loc = Doc.concat(list[
          Doc.lbracket,
          Doc.text(string_of_int(k.loc_start.pos_lnum)),
          Doc.text(":"),
          Doc.text(string_of_int(k.loc_start.pos_cnum - k.loc_start.pos_bol)),
          Doc.text("-"),
          Doc.text(string_of_int(k.loc_end.pos_lnum)),
          Doc.text(":"),
          Doc.text(string_of_int(k.loc_end.pos_cnum - k.loc_end.pos_bol)),
          Doc.rbracket,
        ])
        let doc = Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat(list[
            loc,
            Doc.indent(
              Doc.concat(list[
                Doc.line,
                Doc.join(
                  ~sep=Doc.comma,
                  List.map(c => Doc.text(Comment.txt(c)), v),
                ),
              ]),
            ),
            Doc.line,
          ]),
        )
        list[doc, ...acc]
      },
      t.leading,
      list[],
    )
    
    let trailingStuff = Hashtbl.fold(
      (k: Location.t, v: list<Comment.t>, acc) => {
        let loc = Doc.concat(list[
          Doc.lbracket,
          Doc.text(string_of_int(k.loc_start.pos_lnum)),
          Doc.text(":"),
          Doc.text(string_of_int(k.loc_start.pos_cnum - k.loc_start.pos_bol)),
          Doc.text("-"),
          Doc.text(string_of_int(k.loc_end.pos_lnum)),
          Doc.text(":"),
          Doc.text(string_of_int(k.loc_end.pos_cnum - k.loc_end.pos_bol)),
          Doc.rbracket,
        ])
        let doc = Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat(list[
            loc,
            Doc.indent(
              Doc.concat(list[
                Doc.line,
                Doc.join(
                  ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                  List.map(c => Doc.text(Comment.txt(c)), v),
                ),
              ]),
            ),
            Doc.line,
          ]),
        )
        list[doc, ...acc]
      },
      t.trailing,
      list[],
    )
    
    Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat(list[
        Doc.text("leading comments:"),
        Doc.line,
        Doc.indent(Doc.concat(leadingStuff)),
        Doc.line,
        Doc.line,
        Doc.text("trailing comments:"),
        Doc.indent(Doc.concat(trailingStuff)),
        Doc.line,
        Doc.line,
      ]),
    )
    |> Doc.toString(~width=80)
    |> print_endline
  }
  
  let attach = (tbl, loc, comments) =>
    switch comments {
    | list[] => ()
    | comments => Hashtbl.replace(tbl, loc, comments)
    }
  
  let partitionByLoc = (comments, loc) => {
    let rec loop = ((leading, inside, trailing), comments) => {
      open Location
      switch comments {
      | list[comment, ...rest] =>
        let cmtLoc = Comment.loc(comment)
        if cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum {
          loop((list[comment, ...leading], inside, trailing), rest)
        } else if cmtLoc.loc_start.pos_cnum >= loc.loc_end.pos_cnum {
          loop((leading, inside, list[comment, ...trailing]), rest)
        } else {
          loop((leading, list[comment, ...inside], trailing), rest)
        }
      | list[] => (List.rev(leading), List.rev(inside), List.rev(trailing))
      }
    }
    
    loop((list[], list[], list[]), comments)
  }
  
  let partitionLeadingTrailing = (comments, loc) => {
    let rec loop = ((leading, trailing), comments) => {
      open Location
      switch comments {
      | list[comment, ...rest] =>
        let cmtLoc = Comment.loc(comment)
        if cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum {
          loop((list[comment, ...leading], trailing), rest)
        } else {
          loop((leading, list[comment, ...trailing]), rest)
        }
      | list[] => (List.rev(leading), List.rev(trailing))
      }
    }
    
    loop((list[], list[]), comments)
  }
  
  let partitionBeforeAfterPos = (comments, pos) => {
    let rec loop = ((before, after), comments) => {
      open Location
      open Lexing
      switch comments {
      | list[comment, ...rest] =>
        let cmtLoc = Comment.loc(comment)
        if cmtLoc.loc_end.pos_cnum <= pos.pos_cnum {
          loop((list[comment, ...before], after), rest)
        } else {
          loop((before, list[comment, ...after]), rest)
        }
      | list[] => (List.rev(before), List.rev(after))
      }
    }
    
    loop((list[], list[]), comments)
  }
  
  let partitionByOnSameLine = (loc, comments) => {
    let rec loop = ((onSameLine, onOtherLine), comments) => {
      open Location
      switch comments {
      | list[] => (List.rev(onSameLine), List.rev(onOtherLine))
      | list[comment, ...rest] =>
        let cmtLoc = Comment.loc(comment)
        if cmtLoc.loc_start.pos_lnum === loc.loc_end.pos_lnum {
          loop((list[comment, ...onSameLine], onOtherLine), rest)
        } else {
          loop((onSameLine, list[comment, ...onOtherLine]), rest)
        }
      }
    }
    
    loop((list[], list[]), comments)
  }
  
  let partitionAdjacentTrailing = (loc1, comments) => {
    open Location
    open Lexing
    let rec loop = (~prevEndPos, afterLoc1, comments) =>
      switch comments {
      | list[] => (List.rev(afterLoc1), list[])
      | list[comment, ...rest] as comments =>
        let cmtPrevEndPos = Comment.prevTokEndPos(comment)
        if prevEndPos.Lexing.pos_cnum === cmtPrevEndPos.pos_cnum {
          let commentEnd = Comment.loc(comment).loc_end
          loop(~prevEndPos=commentEnd, list[comment, ...afterLoc1], rest)
        } else {
          (List.rev(afterLoc1), comments)
        }
      }
    
    loop(~prevEndPos=loc1.loc_end, list[], comments)
  }
  
  let rec collectListPatterns = (acc, pattern) => {
    open Parsetree
    switch pattern.ppat_desc {
    | Ppat_construct(
        {txt: Longident.Lident("::")},
        Some({ppat_desc: Ppat_tuple(list[pat, rest])}),
      ) =>
      collectListPatterns(list[pat, ...acc], rest)
    | Ppat_construct({txt: Longident.Lident("[]")}, None) => List.rev(acc)
    | _ => List.rev(list[pattern, ...acc])
    }
  }
  
  let rec collectListExprs = (acc, expr) => {
    open Parsetree
    switch expr.pexp_desc {
    | Pexp_construct(
        {txt: Longident.Lident("::")},
        Some({pexp_desc: Pexp_tuple(list[expr, rest])}),
      ) =>
      collectListExprs(list[expr, ...acc], rest)
    | Pexp_construct({txt: Longident.Lident("[]")}, _) => List.rev(acc)
    | _ => List.rev(list[expr, ...acc])
    }
  }
  
  let arrowType = ct => {
    open Parsetree
    let rec process = (attrsBefore, acc, typ) =>
      switch typ {
      | {
          ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2),
          ptyp_attributes: list[],
        } =>
        let arg = (list[], lbl, typ1)
        process(attrsBefore, list[arg, ...acc], typ2)
      | {
          ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2),
          ptyp_attributes: list[({txt: "bs"}, _)] as attrs,
        } =>
        let arg = (attrs, lbl, typ1)
        process(attrsBefore, list[arg, ...acc], typ2)
      | {
          ptyp_desc: Ptyp_arrow(Nolabel, typ1, typ2),
          ptyp_attributes: attrs,
        } as returnType =>
        let args = List.rev(acc)
        (attrsBefore, args, returnType)
      | {
          ptyp_desc: Ptyp_arrow((Labelled(_) | Optional(_)) as lbl, typ1, typ2),
          ptyp_attributes: attrs,
        } =>
        let arg = (attrs, lbl, typ1)
        process(attrsBefore, list[arg, ...acc], typ2)
      | typ => (attrsBefore, List.rev(acc), typ)
      }
    
    switch ct {
    | {
        ptyp_desc: Ptyp_arrow(Nolabel, _typ1, _typ2),
        ptyp_attributes: attrs,
      } as typ =>
      process(attrs, list[], {...typ, ptyp_attributes: list[]})
    | typ => process(list[], list[], typ)
    }
  }
  
  let modExprApply = modExpr => {
    let rec loop = (acc, modExpr) =>
      switch modExpr {
      | {Parsetree.pmod_desc: Pmod_apply(next, arg)} =>
        loop(list[arg, ...acc], next)
      | _ => list[modExpr, ...acc]
      }
    
    loop(list[], modExpr)
  }
  
  let modExprFunctor = modExpr => {
    let rec loop = (acc, modExpr) =>
      switch modExpr {
      | {
          Parsetree.pmod_desc: Pmod_functor(lbl, modType, returnModExpr),
          pmod_attributes: attrs,
        } =>
        let param = (attrs, lbl, modType)
        loop(list[param, ...acc], returnModExpr)
      | returnModExpr => (List.rev(acc), returnModExpr)
      }
    
    loop(list[], modExpr)
  }
  
  let functorType = modtype => {
    let rec process = (acc, modtype) =>
      switch modtype {
      | {
          Parsetree.pmty_desc: Pmty_functor(lbl, argType, returnType),
          pmty_attributes: attrs,
        } =>
        let arg = (attrs, lbl, argType)
        process(list[arg, ...acc], returnType)
      | modType => (List.rev(acc), modType)
      }
    
    process(list[], modtype)
  }
  
  let funExpr = expr => {
    open Parsetree
    
    let rec collectNewTypes = (acc, returnExpr) =>
      switch returnExpr {
      | {
          pexp_desc: Pexp_newtype(stringLoc, returnExpr),
          pexp_attributes: list[],
        } =>
        collectNewTypes(list[stringLoc, ...acc], returnExpr)
      | returnExpr =>
        let loc = switch (acc, List.rev(acc)) {
        | (list[startLoc, ..._], list[endLoc, ..._]) =>
          {...endLoc.loc, loc_end: endLoc.loc.loc_end}
        | _ => Location.none
        }
        
        let txt = List.fold_right(
          (curr, acc) => acc ++ " " ++ curr.Location.txt,
          acc,
          "type",
        )
        (Location.mkloc(txt, loc), returnExpr)
      }
    
    let rec collect = (attrsBefore, acc, expr) =>
      switch expr {
      | {
          pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr),
          pexp_attributes: list[],
        } =>
        let parameter = (list[], lbl, defaultExpr, pattern)
        collect(attrsBefore, list[parameter, ...acc], returnExpr)
      | {pexp_desc: Pexp_newtype(stringLoc, rest), pexp_attributes: attrs} =>
        let (var, returnExpr) = collectNewTypes(list[stringLoc], rest)
        let parameter = (
          attrs,
          Asttypes.Nolabel,
          None,
          Ast_helper.Pat.var(~loc=stringLoc.loc, var),
        )
        Format.sprintf(
          "[%d:%d - %d:%d]",
          stringLoc.loc.loc_start.pos_lnum,
          stringLoc.loc.loc_start.pos_cnum - stringLoc.loc.loc_start.pos_bol,
          stringLoc.loc.loc_end.pos_lnum,
          stringLoc.loc.loc_end.pos_cnum - stringLoc.loc.loc_end.pos_bol,
        ) |> print_endline
        collect(attrsBefore, list[parameter, ...acc], returnExpr)
      | {
          pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr),
          pexp_attributes: list[({txt: "bs"}, _)] as attrs,
        } =>
        let parameter = (attrs, lbl, defaultExpr, pattern)
        collect(attrsBefore, list[parameter, ...acc], returnExpr)
      | {
          pexp_desc: 
            Pexp_fun(
              (Labelled(_) | Optional(_)) as lbl,
              defaultExpr,
              pattern,
              returnExpr,
            ),
          pexp_attributes: attrs,
        } =>
        let parameter = (attrs, lbl, defaultExpr, pattern)
        collect(attrsBefore, list[parameter, ...acc], returnExpr)
      | expr => (attrsBefore, List.rev(acc), expr)
      }
    
    switch expr {
    | {
        pexp_desc: Pexp_fun(Nolabel, defaultExpr, pattern, returnExpr),
        pexp_attributes: attrs,
      } as expr =>
      collect(attrs, list[], {...expr, pexp_attributes: list[]})
    | expr => collect(list[], list[], expr)
    }
  }
  
  let rec isBlockExpr = expr => {
    open Parsetree
    switch expr.pexp_desc {
    | (((Pexp_letmodule(_) | Pexp_letexception(_)) | Pexp_let(_))
      | Pexp_open(_))
      | Pexp_sequence(_) =>
      true
    | Pexp_apply(callExpr, _) when isBlockExpr(callExpr) => true
    | Pexp_constraint(expr, _) when isBlockExpr(expr) => true
    | Pexp_field(expr, _) when isBlockExpr(expr) => true
    | Pexp_setfield(expr, _, _) when isBlockExpr(expr) => true
    | _ => false
    }
  }
  
  let rec walkStructure = (s, t, comments) =>
    switch s {
    | _ when comments == list[] => ()
    | list[] => attach(t.inside, Location.none, comments)
    | s =>
      walkList(
        ~getLoc=n => n.Parsetree.pstr_loc,
        ~walkNode=walkStructureItem,
        s,
        t,
        comments,
      )
    }
  
  and walkStructureItem = (si, t, comments) =>
    switch si.Parsetree.pstr_desc {
    | _ when comments == list[] => ()
    | Pstr_primitive(valueDescription) =>
      walkValueDescription(valueDescription, t, comments)
    | Pstr_open(openDescription) =>
      walkOpenDescription(openDescription, t, comments)
    | Pstr_value(_, valueBindings) =>
      walkValueBindings(valueBindings, t, comments)
    | Pstr_type(_, typeDeclarations) =>
      walkTypeDeclarations(typeDeclarations, t, comments)
    | Pstr_eval(expr, _) => walkExpr(expr, t, comments)
    | Pstr_module(moduleBinding) =>
      walkModuleBinding(moduleBinding, t, comments)
    | Pstr_recmodule(moduleBindings) =>
      walkList(
        ~getLoc=mb => mb.Parsetree.pmb_loc,
        ~walkNode=walkModuleBinding,
        moduleBindings,
        t,
        comments,
      )
    | Pstr_modtype(modTypDecl) =>
      walkModuleTypeDeclaration(modTypDecl, t, comments)
    | Pstr_attribute(attribute) => walkAttribute(attribute, t, comments)
    | Pstr_extension(extension, _) => walkExtension(extension, t, comments)
    | Pstr_include(includeDeclaration) =>
      walkIncludeDeclaration(includeDeclaration, t, comments)
    | Pstr_exception(extensionConstructor) =>
      walkExtConstr(extensionConstructor, t, comments)
    | Pstr_typext(typeExtension) =>
      walkTypeExtension(typeExtension, t, comments)
    | Pstr_class_type(_) | Pstr_class(_) => ()
    }
  
  and walkValueDescription = (vd, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(
      comments,
      vd.pval_name.loc,
    )
    attach(t.leading, vd.pval_name.loc, leading)
    let (afterName, rest) = partitionAdjacentTrailing(
      vd.pval_name.loc,
      trailing,
    )
    attach(t.trailing, vd.pval_name.loc, afterName)
    let (before, inside, after) = partitionByLoc(rest, vd.pval_type.ptyp_loc)
    
    attach(t.leading, vd.pval_type.ptyp_loc, before)
    walkTypExpr(vd.pval_type, t, inside)
    attach(t.trailing, vd.pval_type.ptyp_loc, after)
  }
  
  and walkTypeExtension = (te, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(
      comments,
      te.ptyext_path.loc,
    )
    attach(t.leading, te.ptyext_path.loc, leading)
    let (afterPath, rest) = partitionAdjacentTrailing(
      te.ptyext_path.loc,
      trailing,
    )
    attach(t.trailing, te.ptyext_path.loc, afterPath)
    
    let rest = switch te.ptyext_params {
    | list[] => rest
    | typeParams =>
      visitListButContinueWithRemainingComments(
        ~getLoc=((typexpr, _variance)) => typexpr.Parsetree.ptyp_loc,
        ~walkNode=walkTypeParam,
        ~newlineDelimited=false,
        typeParams,
        t,
        rest,
      )
    }
    
    walkList(
      ~getLoc=n => n.Parsetree.pext_loc,
      ~walkNode=walkExtConstr,
      te.ptyext_constructors,
      t,
      rest,
    )
  }
  
  and walkIncludeDeclaration = (inclDecl, t, comments) => {
    let (before, inside, after) = partitionByLoc(
      comments,
      inclDecl.pincl_mod.pmod_loc,
    )
    attach(t.leading, inclDecl.pincl_mod.pmod_loc, before)
    walkModExpr(inclDecl.pincl_mod, t, inside)
    attach(t.trailing, inclDecl.pincl_mod.pmod_loc, after)
  }
  
  and walkModuleTypeDeclaration = (mtd, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(
      comments,
      mtd.pmtd_name.loc,
    )
    attach(t.leading, mtd.pmtd_name.loc, leading)
    switch mtd.pmtd_type {
    | None => attach(t.trailing, mtd.pmtd_name.loc, trailing)
    | Some(modType) =>
      let (afterName, rest) = partitionAdjacentTrailing(
        mtd.pmtd_name.loc,
        trailing,
      )
      attach(t.trailing, mtd.pmtd_name.loc, afterName)
      let (before, inside, after) = partitionByLoc(rest, modType.pmty_loc)
      attach(t.leading, modType.pmty_loc, before)
      walkModType(modType, t, inside)
      attach(t.trailing, modType.pmty_loc, after)
    }
  }
  
  and walkModuleBinding = (mb, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(
      comments,
      mb.pmb_name.loc,
    )
    attach(t.leading, mb.pmb_name.loc, leading)
    let (afterName, rest) = partitionAdjacentTrailing(mb.pmb_name.loc, trailing)
    attach(t.trailing, mb.pmb_name.loc, afterName)
    let (leading, inside, trailing) = partitionByLoc(rest, mb.pmb_expr.pmod_loc)
    attach(t.leading, mb.pmb_expr.pmod_loc, leading)
    walkModExpr(mb.pmb_expr, t, inside)
    attach(t.trailing, mb.pmb_expr.pmod_loc, trailing)
  }
  
  and walkSignature = (signature, t, comments) =>
    switch signature {
    | _ when comments == list[] => ()
    | list[] => attach(t.inside, Location.none, comments)
    | s =>
      walkList(
        ~getLoc=n => n.Parsetree.psig_loc,
        ~walkNode=walkSignatureItem,
        signature,
        t,
        comments,
      )
    }
  
  and walkSignatureItem = (si, t, comments) =>
    switch si.psig_desc {
    | _ when comments == list[] => ()
    | Psig_value(valueDescription) =>
      walkValueDescription(valueDescription, t, comments)
    | Psig_type(_, typeDeclarations) =>
      walkTypeDeclarations(typeDeclarations, t, comments)
    | Psig_typext(typeExtension) =>
      walkTypeExtension(typeExtension, t, comments)
    | Psig_exception(extensionConstructor) =>
      walkExtConstr(extensionConstructor, t, comments)
    | Psig_module(moduleDeclaration) =>
      walkModuleDeclaration(moduleDeclaration, t, comments)
    | Psig_recmodule(moduleDeclarations) =>
      walkList(
        ~getLoc=n => n.Parsetree.pmd_loc,
        ~walkNode=walkModuleDeclaration,
        moduleDeclarations,
        t,
        comments,
      )
    | Psig_modtype(moduleTypeDeclaration) =>
      walkModuleTypeDeclaration(moduleTypeDeclaration, t, comments)
    | Psig_open(openDescription) =>
      walkOpenDescription(openDescription, t, comments)
    | Psig_include(includeDescription) =>
      walkIncludeDescription(includeDescription, t, comments)
    | Psig_attribute(attribute) => walkAttribute(attribute, t, comments)
    | Psig_extension(extension, _) => walkExtension(extension, t, comments)
    | Psig_class(_) | Psig_class_type(_) => ()
    }
  
  and walkIncludeDescription = (id, t, comments) => {
    let (before, inside, after) = partitionByLoc(
      comments,
      id.pincl_mod.pmty_loc,
    )
    attach(t.leading, id.pincl_mod.pmty_loc, before)
    walkModType(id.pincl_mod, t, inside)
    attach(t.trailing, id.pincl_mod.pmty_loc, after)
  }
  
  and walkModuleDeclaration = (md, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(
      comments,
      md.pmd_name.loc,
    )
    attach(t.leading, md.pmd_name.loc, leading)
    let (afterName, rest) = partitionAdjacentTrailing(md.pmd_name.loc, trailing)
    attach(t.trailing, md.pmd_name.loc, afterName)
    let (leading, inside, trailing) = partitionByLoc(rest, md.pmd_type.pmty_loc)
    attach(t.leading, md.pmd_type.pmty_loc, leading)
    walkModType(md.pmd_type, t, inside)
    attach(t.trailing, md.pmd_type.pmty_loc, trailing)
  }
  
  and walkList: 'node. (
    ~prevLoc: Location.t=?,
    ~getLoc: 'node => Location.t,
    ~walkNode: ('node, t, list<Comment.t>) => unit,
    list<'node>,
    t,
    list<Comment.t>,
  ) => unit = (~prevLoc=?, ~getLoc, ~walkNode, l, t, comments) => {
    open Location
    switch l {
    | _ when comments == list[] => ()
    | list[] =>
      switch prevLoc {
      | Some(loc) => attach(t.trailing, loc, comments)
      | None => ()
      }
    | list[node, ...rest] =>
      let currLoc = getLoc(node)
      let (leading, inside, trailing) = partitionByLoc(comments, currLoc)
      switch prevLoc {
      | None => attach(t.leading, currLoc, leading)
      | Some(prevLoc) =>
        if prevLoc.loc_end.pos_lnum === currLoc.loc_start.pos_lnum {
          let (afterPrev, beforeCurr) = partitionAdjacentTrailing(
            prevLoc,
            leading,
          )
          let () = attach(t.trailing, prevLoc, afterPrev)
          attach(t.leading, currLoc, beforeCurr)
        } else {
          let (onSameLineAsPrev, afterPrev) = partitionByOnSameLine(
            prevLoc,
            leading,
          )
          let () = attach(t.trailing, prevLoc, onSameLineAsPrev)
          let (leading, _inside, _trailing) = partitionByLoc(afterPrev, currLoc)
          attach(t.leading, currLoc, leading)
        }
      }
      walkNode(node, t, inside)
      walkList(~prevLoc=currLoc, ~getLoc, ~walkNode, rest, t, trailing)
    }
  }
  
  and visitListButContinueWithRemainingComments: 'node. (
    ~prevLoc: Location.t=?,
    ~newlineDelimited: bool,
    ~getLoc: 'node => Location.t,
    ~walkNode: ('node, t, list<Comment.t>) => unit,
    list<'node>,
    t,
    list<Comment.t>,
  ) => list<Comment.t> = (
    ~prevLoc=?,
    ~newlineDelimited,
    ~getLoc,
    ~walkNode,
    l,
    t,
    comments,
  ) => {
    open Location
    switch l {
    | _ when comments == list[] => list[]
    | list[] =>
      switch prevLoc {
      | Some(loc) =>
        let (afterPrev, rest) = if newlineDelimited {
          partitionByOnSameLine(loc, comments)
        } else {
          partitionAdjacentTrailing(loc, comments)
        }
        
        attach(t.trailing, loc, afterPrev)
        rest
      | None => comments
      }
    | list[node, ...rest] =>
      let currLoc = getLoc(node)
      let (leading, inside, trailing) = partitionByLoc(comments, currLoc)
      let () = switch prevLoc {
      | None =>
        attach(t.leading, currLoc, leading)
        ()
      | Some(prevLoc) =>
        if prevLoc.loc_end.pos_lnum === currLoc.loc_start.pos_lnum {
          let (afterPrev, beforeCurr) = partitionAdjacentTrailing(
            prevLoc,
            leading,
          )
          let () = attach(t.trailing, prevLoc, afterPrev)
          let () = attach(t.leading, currLoc, beforeCurr)
          ()
        } else {
          let (onSameLineAsPrev, afterPrev) = partitionByOnSameLine(
            prevLoc,
            leading,
          )
          let () = attach(t.trailing, prevLoc, onSameLineAsPrev)
          let (leading, _inside, trailing) = partitionByLoc(afterPrev, currLoc)
          let () = attach(t.leading, currLoc, leading)
          ()
        }
      }
      
      walkNode(node, t, inside)
      visitListButContinueWithRemainingComments(
        ~prevLoc=currLoc,
        ~getLoc,
        ~walkNode,
        ~newlineDelimited,
        rest,
        t,
        trailing,
      )
    }
  }
  
  and walkValueBindings = (vbs, t, comments) =>
    walkList(
      ~getLoc=n =>
        if n.Parsetree.pvb_pat.ppat_loc.loc_ghost {
          n.pvb_expr.pexp_loc
        } else {
          n.Parsetree.pvb_loc
        },
      ~walkNode=walkValueBinding,
      vbs,
      t,
      comments,
    )
  
  and walkOpenDescription = (openDescription, t, comments) => {
    let loc = openDescription.popen_lid.loc
    let (leading, trailing) = partitionLeadingTrailing(comments, loc)
    attach(t.leading, loc, leading)
    attach(t.trailing, loc, trailing)
  }
  
  and walkTypeDeclarations = (typeDeclarations, t, comments) =>
    walkList(
      ~getLoc=n => n.Parsetree.ptype_loc,
      ~walkNode=walkTypeDeclaration,
      typeDeclarations,
      t,
      comments,
    )
  
  and walkTypeParam = ((typexpr, _variance), t, comments) =>
    walkTypExpr(typexpr, t, comments)
  
  and walkTypeDeclaration = (td, t, comments) => {
    let (beforeName, rest) = partitionLeadingTrailing(
      comments,
      td.ptype_name.loc,
    )
    attach(t.leading, td.ptype_name.loc, beforeName)
    
    let (afterName, rest) = partitionAdjacentTrailing(td.ptype_name.loc, rest)
    attach(t.trailing, td.ptype_name.loc, afterName)
    
    let rest = switch td.ptype_params {
    | list[] => rest
    | typeParams =>
      visitListButContinueWithRemainingComments(
        ~getLoc=((typexpr, _variance)) => typexpr.Parsetree.ptyp_loc,
        ~walkNode=walkTypeParam,
        ~newlineDelimited=false,
        typeParams,
        t,
        rest,
      )
    }
    
    let rest = switch td.ptype_manifest {
    | Some(typexpr) =>
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(
        rest,
        typexpr.ptyp_loc,
      )
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      let (afterTyp, rest) = partitionAdjacentTrailing(
        typexpr.Parsetree.ptyp_loc,
        afterTyp,
      )
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
      rest
    | None => rest
    }
    
    let rest = switch td.ptype_kind {
    | Ptype_abstract | Ptype_open => rest
    | Ptype_record(labelDeclarations) =>
      walkLabelDeclarations(labelDeclarations, t, rest)
    | Ptype_variant(constructorDeclarations) =>
      walkConstructorDeclarations(constructorDeclarations, t, rest)
    }
    
    attach(t.trailing, td.ptype_loc, rest)
  }
  
  and walkLabelDeclarations = (lds, t, comments) =>
    visitListButContinueWithRemainingComments(
      ~getLoc=ld => ld.Parsetree.pld_loc,
      ~walkNode=walkLabelDeclaration,
      ~newlineDelimited=false,
      lds,
      t,
      comments,
    )
  
  and walkLabelDeclaration = (ld, t, comments) => {
    let (beforeName, rest) = partitionLeadingTrailing(comments, ld.pld_name.loc)
    attach(t.leading, ld.pld_name.loc, beforeName)
    let (afterName, rest) = partitionAdjacentTrailing(ld.pld_name.loc, rest)
    attach(t.trailing, ld.pld_name.loc, afterName)
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(
      rest,
      ld.pld_type.ptyp_loc,
    )
    attach(t.leading, ld.pld_type.ptyp_loc, beforeTyp)
    walkTypExpr(ld.pld_type, t, insideTyp)
    attach(t.trailing, ld.pld_type.ptyp_loc, afterTyp)
  }
  
  and walkConstructorDeclarations = (cds, t, comments) =>
    visitListButContinueWithRemainingComments(
      ~getLoc=cd => cd.Parsetree.pcd_loc,
      ~walkNode=walkConstructorDeclaration,
      ~newlineDelimited=false,
      cds,
      t,
      comments,
    )
  
  and walkConstructorDeclaration = (cd, t, comments) => {
    let (beforeName, rest) = partitionLeadingTrailing(comments, cd.pcd_name.loc)
    attach(t.leading, cd.pcd_name.loc, beforeName)
    let (afterName, rest) = partitionAdjacentTrailing(cd.pcd_name.loc, rest)
    attach(t.trailing, cd.pcd_name.loc, afterName)
    let rest = walkConstructorArguments(cd.pcd_args, t, rest)
    
    let rest = switch cd.pcd_res {
    | Some(typexpr) =>
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(
        rest,
        typexpr.ptyp_loc,
      )
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      let (afterTyp, rest) = partitionAdjacentTrailing(
        typexpr.Parsetree.ptyp_loc,
        afterTyp,
      )
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
      rest
    | None => rest
    }
    
    attach(t.trailing, cd.pcd_loc, rest)
  }
  
  and walkConstructorArguments = (args, t, comments) =>
    switch args {
    | Pcstr_tuple(typexprs) =>
      visitListButContinueWithRemainingComments(
        ~getLoc=n => n.Parsetree.ptyp_loc,
        ~walkNode=walkTypExpr,
        ~newlineDelimited=false,
        typexprs,
        t,
        comments,
      )
    | Pcstr_record(labelDeclarations) =>
      walkLabelDeclarations(labelDeclarations, t, comments)
    }
  
  and walkValueBinding = (vb, t, comments) => {
    open Location
    
    if vb.pvb_pat.ppat_loc.loc_ghost {
      walkExpr(vb.pvb_expr, t, comments)
    } else {
      let patternLoc = vb.Parsetree.pvb_pat.ppat_loc
      let exprLoc = vb.Parsetree.pvb_expr.pexp_loc
      
      let (leading, inside, trailing) = partitionByLoc(comments, patternLoc)
      
      attach(t.leading, patternLoc, leading)
      walkPattern(vb.Parsetree.pvb_pat, t, inside)
      
      let (afterPat, surroundingExpr) = partitionAdjacentTrailing(
        patternLoc,
        trailing,
      )
      
      attach(t.trailing, patternLoc, afterPat)
      let (beforeExpr, insideExpr, afterExpr) = partitionByLoc(
        surroundingExpr,
        exprLoc,
      )
      if isBlockExpr(vb.pvb_expr) {
        walkExpr(
          vb.pvb_expr,
          t,
          List.concat(list[beforeExpr, insideExpr, afterExpr]),
        )
      } else {
        attach(t.leading, exprLoc, beforeExpr)
        walkExpr(vb.Parsetree.pvb_expr, t, insideExpr)
        attach(t.trailing, exprLoc, afterExpr)
      }
    }
  }
  
  and walkExpr = (expr, t, comments) => {
    open Location
    switch expr.Parsetree.pexp_desc {
    | _ when comments == list[] => ()
    | Pexp_constant(_) =>
      let (leading, trailing) = partitionLeadingTrailing(
        comments,
        expr.pexp_loc,
      )
      attach(t.leading, expr.pexp_loc, leading)
      attach(t.trailing, expr.pexp_loc, trailing)
    | Pexp_ident(longident) =>
      let (leading, trailing) = partitionLeadingTrailing(
        comments,
        longident.loc,
      )
      attach(t.leading, longident.loc, leading)
      attach(t.trailing, longident.loc, trailing)
    | Pexp_let(_recFlag, valueBindings, expr2) =>
      let comments = visitListButContinueWithRemainingComments(
        ~getLoc=n =>
          if n.Parsetree.pvb_pat.ppat_loc.loc_ghost {
            n.pvb_expr.pexp_loc
          } else {
            n.Parsetree.pvb_loc
          },
        ~walkNode=walkValueBinding,
        ~newlineDelimited=true,
        valueBindings,
        t,
        comments,
      )
      
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, comments)
      } else {
        let (leading, inside, trailing) = partitionByLoc(
          comments,
          expr2.pexp_loc,
        )
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_open(_override, longident, expr2) =>
      let (leading, comments) = partitionLeadingTrailing(
        comments,
        expr.pexp_loc,
      )
      attach(
        t.leading,
        {...expr.pexp_loc, loc_end: longident.loc.loc_end},
        leading,
      )
      let (leading, trailing) = partitionLeadingTrailing(
        comments,
        longident.loc,
      )
      attach(t.leading, longident.loc, leading)
      let (afterLongident, rest) = partitionByOnSameLine(
        longident.loc,
        trailing,
      )
      attach(t.trailing, longident.loc, afterLongident)
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_extension(
        {txt: "bs.obj"},
        PStr(
          list[{
            pstr_desc: Pstr_eval({pexp_desc: Pexp_record(rows, _)}, list[]),
          }],
        ),
      ) =>
      walkList(
        ~getLoc=(
          (longident, expr): (Asttypes.loc<Longident.t>, Parsetree.expression),
        ) => {
          ...longident.loc,
          loc_end: expr.pexp_loc.loc_end,
        },
        ~walkNode=walkExprRecordRow,
        rows,
        t,
        comments,
      )
    | Pexp_extension(extension) => walkExtension(extension, t, comments)
    | Pexp_letexception(extensionConstructor, expr2) =>
      let (leading, comments) = partitionLeadingTrailing(
        comments,
        expr.pexp_loc,
      )
      attach(
        t.leading,
        {...expr.pexp_loc, loc_end: extensionConstructor.pext_loc.loc_end},
        leading,
      )
      let (leading, inside, trailing) = partitionByLoc(
        comments,
        extensionConstructor.pext_loc,
      )
      attach(t.leading, extensionConstructor.pext_loc, leading)
      walkExtConstr(extensionConstructor, t, inside)
      let (afterExtConstr, rest) = partitionByOnSameLine(
        extensionConstructor.pext_loc,
        trailing,
      )
      attach(t.trailing, extensionConstructor.pext_loc, afterExtConstr)
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_letmodule(stringLoc, modExpr, expr2) =>
      let (leading, comments) = partitionLeadingTrailing(
        comments,
        expr.pexp_loc,
      )
      attach(
        t.leading,
        {...expr.pexp_loc, loc_end: modExpr.pmod_loc.loc_end},
        leading,
      )
      let (leading, trailing) = partitionLeadingTrailing(
        comments,
        stringLoc.loc,
      )
      attach(t.leading, stringLoc.loc, leading)
      let (afterString, rest) = partitionAdjacentTrailing(
        stringLoc.loc,
        trailing,
      )
      attach(t.trailing, stringLoc.loc, afterString)
      let (beforeModExpr, insideModExpr, afterModExpr) = partitionByLoc(
        rest,
        modExpr.pmod_loc,
      )
      attach(t.leading, modExpr.pmod_loc, beforeModExpr)
      walkModExpr(modExpr, t, insideModExpr)
      let (afterModExpr, rest) = partitionByOnSameLine(
        modExpr.pmod_loc,
        afterModExpr,
      )
      attach(t.trailing, modExpr.pmod_loc, afterModExpr)
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_assert(expr) | Pexp_lazy(expr) =>
      if isBlockExpr(expr) {
        walkExpr(expr, t, comments)
      } else {
        let (leading, inside, trailing) = partitionByLoc(
          comments,
          expr.pexp_loc,
        )
        attach(t.leading, expr.pexp_loc, leading)
        walkExpr(expr, t, inside)
        attach(t.trailing, expr.pexp_loc, trailing)
      }
    | Pexp_constraint(expr, typexpr) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, leading)
      walkExpr(expr, t, inside)
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
      attach(t.trailing, expr.pexp_loc, afterExpr)
      let (leading, inside, trailing) = partitionByLoc(rest, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, leading)
      walkTypExpr(typexpr, t, inside)
      attach(t.trailing, typexpr.ptyp_loc, trailing)
    | (Pexp_tuple(list[]) | Pexp_array(list[]))
      | Pexp_construct({txt: Longident.Lident("[]")}, _) =>
      attach(t.inside, expr.pexp_loc, comments)
    | Pexp_construct({txt: Longident.Lident("::")}, _) =>
      walkList(
        ~getLoc=n => n.Parsetree.pexp_loc,
        ~walkNode=walkExpr,
        collectListExprs(list[], expr),
        t,
        comments,
      )
    | Pexp_construct(longident, args) =>
      let (leading, trailing) = partitionLeadingTrailing(
        comments,
        longident.loc,
      )
      attach(t.leading, longident.loc, leading)
      switch args {
      | Some(expr) =>
        let (afterLongident, rest) = partitionAdjacentTrailing(
          longident.loc,
          trailing,
        )
        attach(t.trailing, longident.loc, afterLongident)
        walkExpr(expr, t, rest)
      | None => attach(t.trailing, longident.loc, trailing)
      }
    | Pexp_array(exprs) | Pexp_tuple(exprs) =>
      walkList(
        ~getLoc=n => n.Parsetree.pexp_loc,
        ~walkNode=walkExpr,
        exprs,
        t,
        comments,
      )
    | Pexp_record(rows, spreadExpr) =>
      let comments = switch spreadExpr {
      | None => comments
      | Some(expr) =>
        let (leading, inside, trailing) = partitionByLoc(
          comments,
          expr.pexp_loc,
        )
        attach(t.leading, expr.pexp_loc, leading)
        walkExpr(expr, t, inside)
        let (afterExpr, rest) = partitionAdjacentTrailing(
          expr.pexp_loc,
          trailing,
        )
        attach(t.trailing, expr.pexp_loc, afterExpr)
        rest
      }
      
      walkList(
        ~getLoc=(
          (longident, expr): (Asttypes.loc<Longident.t>, Parsetree.expression),
        ) => {
          ...longident.loc,
          loc_end: expr.pexp_loc.loc_end,
        },
        ~walkNode=walkExprRecordRow,
        rows,
        t,
        comments,
      )
    | Pexp_field(expr, longident) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
      let trailing = if isBlockExpr(expr) {
        let (afterExpr, rest) = partitionAdjacentTrailing(
          expr.pexp_loc,
          trailing,
        )
        walkExpr(expr, t, List.concat(list[leading, inside, afterExpr]))
        rest
      } else {
        attach(t.leading, expr.pexp_loc, leading)
        walkExpr(expr, t, inside)
        trailing
      }
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
      attach(t.trailing, expr.pexp_loc, afterExpr)
      let (leading, trailing) = partitionLeadingTrailing(rest, longident.loc)
      attach(t.leading, longident.loc, leading)
      attach(t.trailing, longident.loc, trailing)
    | Pexp_setfield(expr1, longident, expr2) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr1.pexp_loc)
      let rest = if isBlockExpr(expr1) {
        let (afterExpr, rest) = partitionAdjacentTrailing(
          expr1.pexp_loc,
          trailing,
        )
        walkExpr(expr1, t, List.concat(list[leading, inside, afterExpr]))
        rest
      } else {
        let (afterExpr, rest) = partitionAdjacentTrailing(
          expr1.pexp_loc,
          trailing,
        )
        attach(t.leading, expr1.pexp_loc, leading)
        walkExpr(expr1, t, inside)
        attach(t.trailing, expr1.pexp_loc, afterExpr)
        rest
      }
      let (beforeLongident, afterLongident) = partitionLeadingTrailing(
        rest,
        longident.loc,
      )
      attach(t.leading, longident.loc, beforeLongident)
      let (afterLongident, rest) = partitionAdjacentTrailing(
        longident.loc,
        afterLongident,
      )
      attach(t.trailing, longident.loc, afterLongident)
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_ifthenelse(ifExpr, thenExpr, elseExpr) =>
      let (leading, inside, trailing) = partitionByLoc(
        comments,
        ifExpr.pexp_loc,
      )
      let comments = if isBlockExpr(ifExpr) {
        let (afterExpr, comments) = partitionAdjacentTrailing(
          ifExpr.pexp_loc,
          trailing,
        )
        walkExpr(ifExpr, t, List.concat(list[leading, inside, afterExpr]))
        comments
      } else {
        attach(t.leading, ifExpr.pexp_loc, leading)
        walkExpr(ifExpr, t, inside)
        let (afterExpr, comments) = partitionAdjacentTrailing(
          ifExpr.pexp_loc,
          trailing,
        )
        attach(t.trailing, ifExpr.pexp_loc, afterExpr)
        comments
      }
      let (leading, inside, trailing) = partitionByLoc(
        comments,
        thenExpr.pexp_loc,
      )
      let trailing = if isBlockExpr(thenExpr) {
        let (afterExpr, trailing) = partitionAdjacentTrailing(
          thenExpr.pexp_loc,
          trailing,
        )
        walkExpr(thenExpr, t, List.concat(list[leading, inside, afterExpr]))
        trailing
      } else {
        attach(t.leading, thenExpr.pexp_loc, leading)
        walkExpr(thenExpr, t, inside)
        let (afterExpr, comments) = partitionAdjacentTrailing(
          thenExpr.pexp_loc,
          trailing,
        )
        attach(t.trailing, thenExpr.pexp_loc, afterExpr)
        comments
      }
      switch elseExpr {
      | None => ()
      | Some(expr) =>
        if isBlockExpr(expr) {
          walkExpr(expr, t, comments)
        } else {
          let (leading, inside, trailing) = partitionByLoc(
            trailing,
            expr.pexp_loc,
          )
          attach(t.leading, expr.pexp_loc, leading)
          walkExpr(expr, t, inside)
          attach(t.trailing, expr.pexp_loc, trailing)
        }
      }
    | Pexp_while(expr1, expr2) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr1.pexp_loc)
      let rest = if isBlockExpr(expr1) {
        let (afterExpr, rest) = partitionAdjacentTrailing(
          expr1.pexp_loc,
          trailing,
        )
        walkExpr(expr1, t, List.concat(list[leading, inside, afterExpr]))
        rest
      } else {
        attach(t.leading, expr1.pexp_loc, leading)
        walkExpr(expr1, t, inside)
        let (afterExpr, rest) = partitionAdjacentTrailing(
          expr1.pexp_loc,
          trailing,
        )
        attach(t.trailing, expr1.pexp_loc, afterExpr)
        rest
      }
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_for(pat, expr1, expr2, _, expr3) =>
      let (leading, inside, trailing) = partitionByLoc(comments, pat.ppat_loc)
      attach(t.leading, pat.ppat_loc, leading)
      walkPattern(pat, t, inside)
      let (afterPat, rest) = partitionAdjacentTrailing(pat.ppat_loc, trailing)
      attach(t.trailing, pat.ppat_loc, afterPat)
      let (leading, inside, trailing) = partitionByLoc(rest, expr1.pexp_loc)
      attach(t.leading, expr1.pexp_loc, leading)
      walkExpr(expr1, t, inside)
      let (afterExpr, rest) = partitionAdjacentTrailing(
        expr1.pexp_loc,
        trailing,
      )
      attach(t.trailing, expr1.pexp_loc, afterExpr)
      let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
      attach(t.leading, expr2.pexp_loc, leading)
      walkExpr(expr2, t, inside)
      let (afterExpr, rest) = partitionAdjacentTrailing(
        expr2.pexp_loc,
        trailing,
      )
      attach(t.trailing, expr2.pexp_loc, afterExpr)
      if isBlockExpr(expr3) {
        walkExpr(expr3, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr3.pexp_loc)
        attach(t.leading, expr3.pexp_loc, leading)
        walkExpr(expr3, t, inside)
        attach(t.trailing, expr3.pexp_loc, trailing)
      }
    | Pexp_pack(modExpr) =>
      let (before, inside, after) = partitionByLoc(comments, modExpr.pmod_loc)
      attach(t.leading, modExpr.pmod_loc, before)
      walkModExpr(modExpr, t, inside)
      attach(t.trailing, modExpr.pmod_loc, after)
    | Pexp_match(expr, cases) | Pexp_try(expr, cases) =>
      let (before, inside, after) = partitionByLoc(comments, expr.pexp_loc)
      let after = if isBlockExpr(expr) {
        let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, after)
        walkExpr(expr, t, List.concat(list[before, inside, afterExpr]))
        rest
      } else {
        attach(t.leading, expr.pexp_loc, before)
        walkExpr(expr, t, inside)
        after
      }
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, after)
      attach(t.trailing, expr.pexp_loc, afterExpr)
      walkList(
        ~getLoc=n => {
          ...n.Parsetree.pc_lhs.ppat_loc,
          loc_end: n.pc_rhs.pexp_loc.loc_end,
        },
        ~walkNode=walkCase,
        cases,
        t,
        rest,
      )
    
    | Pexp_apply(
        {
          pexp_desc: 
            Pexp_ident(
              {
                txt: 
                  Longident.Lident(
                    (((("~+" | "~+.") | "~-") | "~-.") | "not") | "!",
                  ),
              },
            ),
        },
        list[(Nolabel, argExpr)],
      ) =>
      let (before, inside, after) = partitionByLoc(comments, argExpr.pexp_loc)
      attach(t.leading, argExpr.pexp_loc, before)
      walkExpr(argExpr, t, inside)
      attach(t.trailing, argExpr.pexp_loc, after)
    
    | Pexp_apply(
        {
          pexp_desc: 
            Pexp_ident(
              {
                txt: 
                  Longident.Lident(
                    (((((((((((((((((((((((":=" | "||") | "&&") | "=") | "==")
                    | "<")
                    | ">")
                    | "!=")
                    | "!==")
                    | "<=")
                    | ">=")
                    | "|>")
                    | "+")
                    | "+.")
                    | "-")
                    | "-.")
                    | "++")
                    | "^")
                    | "*")
                    | "*.")
                    | "/")
                    | "/.")
                    | "**")
                    | "|.")
                    | "<>",
                  ),
              },
            ),
        },
        list[(Nolabel, operand1), (Nolabel, operand2)],
      ) =>
      let (before, inside, after) = partitionByLoc(comments, operand1.pexp_loc)
      attach(t.leading, operand1.pexp_loc, before)
      walkExpr(operand1, t, inside)
      let (afterOperand1, rest) = partitionAdjacentTrailing(
        operand1.pexp_loc,
        after,
      )
      
      attach(t.trailing, operand1.pexp_loc, afterOperand1)
      let (before, inside, after) = partitionByLoc(rest, operand2.pexp_loc)
      attach(t.leading, operand2.pexp_loc, before)
      walkExpr(operand2, t, inside)
      attach(t.trailing, operand2.pexp_loc, after)
    | Pexp_apply(callExpr, arguments) =>
      let (before, inside, after) = partitionByLoc(comments, callExpr.pexp_loc)
      let after = if isBlockExpr(callExpr) {
        let (afterExpr, rest) = partitionAdjacentTrailing(
          callExpr.pexp_loc,
          after,
        )
        walkExpr(callExpr, t, List.concat(list[before, inside, afterExpr]))
        rest
      } else {
        attach(t.leading, callExpr.pexp_loc, before)
        walkExpr(callExpr, t, inside)
        after
      }
      let (afterExpr, rest) = partitionAdjacentTrailing(
        callExpr.pexp_loc,
        after,
      )
      attach(t.trailing, callExpr.pexp_loc, afterExpr)
      walkList(
        ~getLoc=((_argLabel, expr)) => {
          let loc = switch expr.Parsetree.pexp_attributes {
          | list[({Location.txt: "ns.jsxPropLoc", loc}, _), ...attrs] =>
            {...loc, loc_end: expr.pexp_loc.loc_end}
          | _ => expr.pexp_loc
          }
          
          loc
        },
        ~walkNode=walkExprArgument,
        arguments,
        t,
        rest,
      )
    | Pexp_fun(_, _, _, _) | Pexp_newtype(_) =>
      let (_, parameters, returnExpr) = funExpr(expr)
      let comments = visitListButContinueWithRemainingComments(
        ~newlineDelimited=false,
        ~walkNode=walkExprPararameter,
        ~getLoc=((_attrs, _argLbl, exprOpt, pattern)) => {
          open Parsetree
          switch exprOpt {
          | None => pattern.ppat_loc
          | Some(expr) =>
            {
              ...pattern.ppat_loc,
              loc_end: expr.pexp_loc.loc_end,
            }
          }
        },
        parameters,
        t,
        comments,
      )
      
      switch returnExpr.pexp_desc {
      | Pexp_constraint(expr, typ)
        when expr.pexp_loc.loc_start.pos_cnum >=
        typ.ptyp_loc.loc_end.pos_cnum =>
        let (leading, inside, trailing) = partitionByLoc(comments, typ.ptyp_loc)
        attach(t.leading, typ.ptyp_loc, leading)
        walkTypExpr(typ, t, inside)
        let (afterTyp, comments) = partitionAdjacentTrailing(
          typ.ptyp_loc,
          trailing,
        )
        attach(t.trailing, typ.ptyp_loc, afterTyp)
        if isBlockExpr(expr) {
          walkExpr(expr, t, comments)
        } else {
          let (leading, inside, trailing) = partitionByLoc(
            comments,
            expr.pexp_loc,
          )
          attach(t.leading, expr.pexp_loc, leading)
          walkExpr(expr, t, inside)
          attach(t.trailing, expr.pexp_loc, trailing)
        }
      | _ =>
        if isBlockExpr(returnExpr) {
          walkExpr(returnExpr, t, comments)
        } else {
          let (leading, inside, trailing) = partitionByLoc(
            comments,
            returnExpr.pexp_loc,
          )
          attach(t.leading, returnExpr.pexp_loc, leading)
          walkExpr(returnExpr, t, inside)
          attach(t.trailing, returnExpr.pexp_loc, trailing)
        }
      }
    | _ => ()
    }
  }
  
  and walkExprPararameter = (
    (_attrs, _argLbl, exprOpt, pattern),
    t,
    comments,
  ) => {
    let (leading, inside, trailing) = partitionByLoc(comments, pattern.ppat_loc)
    attach(t.leading, pattern.ppat_loc, leading)
    walkPattern(pattern, t, inside)
    switch exprOpt {
    | Some(expr) =>
      let (afterPat, rest) = partitionAdjacentTrailing(
        pattern.ppat_loc,
        trailing,
      )
      attach(t.trailing, pattern.ppat_loc, trailing)
      if isBlockExpr(expr) {
        walkExpr(expr, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr.pexp_loc)
        attach(t.leading, expr.pexp_loc, leading)
        walkExpr(expr, t, inside)
        attach(t.trailing, expr.pexp_loc, trailing)
      }
    | None => attach(t.trailing, pattern.ppat_loc, trailing)
    }
  }
  
  and walkExprArgument = ((_argLabel, expr), t, comments) =>
    switch expr.Parsetree.pexp_attributes {
    | list[({Location.txt: "ns.jsxPropLoc", loc}, _), ...attrs] =>
      let (leading, trailing) = partitionLeadingTrailing(comments, loc)
      attach(t.leading, loc, leading)
      let (afterLabel, rest) = partitionAdjacentTrailing(loc, trailing)
      attach(t.trailing, loc, afterLabel)
      let (before, inside, after) = partitionByLoc(rest, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, before)
      walkExpr(expr, t, inside)
      attach(t.trailing, expr.pexp_loc, after)
    | _ =>
      let (before, inside, after) = partitionByLoc(comments, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, before)
      walkExpr(expr, t, inside)
      attach(t.trailing, expr.pexp_loc, after)
    }
  
  and walkCase = (case, t, comments) => {
    let (before, inside, after) = partitionByLoc(comments, case.pc_lhs.ppat_loc)
    
    walkPattern(case.pc_lhs, t, List.concat(list[before, inside]))
    let (afterPat, rest) = partitionAdjacentTrailing(
      case.pc_lhs.ppat_loc,
      after,
    )
    attach(t.trailing, case.pc_lhs.ppat_loc, afterPat)
    let comments = switch case.pc_guard {
    | Some(expr) =>
      let (before, inside, after) = partitionByLoc(comments, expr.pexp_loc)
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, after)
      if isBlockExpr(expr) {
        walkExpr(expr, t, List.concat(list[before, inside, afterExpr]))
      } else {
        attach(t.leading, expr.pexp_loc, before)
        walkExpr(expr, t, inside)
        attach(t.trailing, expr.pexp_loc, afterExpr)
      }
      rest
    | None => rest
    }
    
    switch case.pc_rhs.pexp_desc {
    | (((Pexp_letmodule(_) | Pexp_letexception(_)) | Pexp_let(_))
      | Pexp_sequence(_))
      | Pexp_open(_) =>
      walkExpr(case.pc_rhs, t, comments)
    | _ =>
      let (before, inside, after) = partitionByLoc(
        comments,
        case.pc_rhs.pexp_loc,
      )
      attach(t.leading, case.pc_rhs.pexp_loc, before)
      walkExpr(case.pc_rhs, t, inside)
      attach(t.trailing, case.pc_rhs.pexp_loc, after)
    }
  }
  
  and walkExprRecordRow = ((longident, expr), t, comments) => {
    let (beforeLongident, afterLongident) = partitionLeadingTrailing(
      comments,
      longident.loc,
    )
    
    attach(t.leading, longident.loc, beforeLongident)
    let (afterLongident, rest) = partitionAdjacentTrailing(
      longident.loc,
      afterLongident,
    )
    attach(t.trailing, longident.loc, afterLongident)
    let (leading, inside, trailing) = partitionByLoc(rest, expr.pexp_loc)
    attach(t.leading, expr.pexp_loc, leading)
    walkExpr(expr, t, inside)
    attach(t.trailing, expr.pexp_loc, trailing)
  }
  
  and walkExtConstr = (extConstr, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(
      comments,
      extConstr.pext_name.loc,
    )
    attach(t.leading, extConstr.pext_name.loc, leading)
    let (afterName, rest) = partitionAdjacentTrailing(
      extConstr.pext_name.loc,
      trailing,
    )
    attach(t.trailing, extConstr.pext_name.loc, afterName)
    walkExtensionConstructorKind(extConstr.pext_kind, t, rest)
  }
  
  and walkExtensionConstructorKind = (kind, t, comments) =>
    switch kind {
    | Pext_rebind(longident) =>
      let (leading, trailing) = partitionLeadingTrailing(
        comments,
        longident.loc,
      )
      attach(t.leading, longident.loc, leading)
      attach(t.trailing, longident.loc, trailing)
    | Pext_decl(constructorArguments, maybeTypExpr) =>
      let rest = walkConstructorArguments(constructorArguments, t, comments)
      switch maybeTypExpr {
      | None => ()
      | Some(typexpr) =>
        let (before, inside, after) = partitionByLoc(rest, typexpr.ptyp_loc)
        attach(t.leading, typexpr.ptyp_loc, before)
        walkTypExpr(typexpr, t, inside)
        attach(t.trailing, typexpr.ptyp_loc, after)
      }
    }
  
  and walkModExpr = (modExpr, t, comments) =>
    switch modExpr.pmod_desc {
    | Pmod_ident(longident) =>
      let (before, after) = partitionLeadingTrailing(comments, longident.loc)
      attach(t.leading, longident.loc, before)
      attach(t.trailing, longident.loc, after)
    | Pmod_structure(structure) => walkStructure(structure, t, comments)
    | Pmod_extension(extension) => walkExtension(extension, t, comments)
    | Pmod_unpack(expr) =>
      let (before, inside, after) = partitionByLoc(comments, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, before)
      walkExpr(expr, t, inside)
      attach(t.trailing, expr.pexp_loc, after)
    | Pmod_constraint(modexpr, modtype) =>
      let (before, inside, after) = partitionByLoc(comments, modexpr.pmod_loc)
      attach(t.leading, modexpr.pmod_loc, before)
      walkModExpr(modexpr, t, inside)
      let (after, rest) = partitionAdjacentTrailing(modexpr.pmod_loc, after)
      attach(t.trailing, modexpr.pmod_loc, after)
      let (before, inside, after) = partitionByLoc(rest, modtype.pmty_loc)
      attach(t.leading, modtype.pmty_loc, before)
      walkModType(modtype, t, inside)
      attach(t.trailing, modtype.pmty_loc, after)
    | Pmod_apply(callModExpr, argModExpr) =>
      let modExprs = modExprApply(modExpr)
      walkList(
        ~getLoc=n => n.Parsetree.pmod_loc,
        ~walkNode=walkModExpr,
        modExprs,
        t,
        comments,
      )
    | Pmod_functor(_) =>
      let (parameters, returnModExpr) = modExprFunctor(modExpr)
      let comments = visitListButContinueWithRemainingComments(
        ~getLoc=((_, lbl, modTypeOption)) =>
          switch modTypeOption {
          | None => lbl.Asttypes.loc
          | Some(modType) =>
            {...lbl.loc, loc_end: modType.Parsetree.pmty_loc.loc_end}
          },
        ~walkNode=walkModExprParameter,
        ~newlineDelimited=false,
        parameters,
        t,
        comments,
      )
      
      switch returnModExpr.pmod_desc {
      | Pmod_constraint(modExpr, modType)
        when modType.pmty_loc.loc_end.pos_cnum <=
        modExpr.pmod_loc.loc_start.pos_cnum =>
        let (before, inside, after) = partitionByLoc(comments, modType.pmty_loc)
        attach(t.leading, modType.pmty_loc, before)
        walkModType(modType, t, inside)
        let (after, rest) = partitionAdjacentTrailing(modType.pmty_loc, after)
        attach(t.trailing, modType.pmty_loc, after)
        let (before, inside, after) = partitionByLoc(rest, modExpr.pmod_loc)
        attach(t.leading, modExpr.pmod_loc, before)
        walkModExpr(modExpr, t, inside)
        attach(t.trailing, modExpr.pmod_loc, after)
      | _ =>
        let (before, inside, after) = partitionByLoc(
          comments,
          returnModExpr.pmod_loc,
        )
        attach(t.leading, returnModExpr.pmod_loc, before)
        walkModExpr(returnModExpr, t, inside)
        attach(t.trailing, returnModExpr.pmod_loc, after)
      }
    }
  
  and walkModExprParameter = (parameter, t, comments) => {
    let (_attrs, lbl, modTypeOption) = parameter
    let (leading, trailing) = partitionLeadingTrailing(comments, lbl.loc)
    attach(t.leading, lbl.loc, leading)
    switch modTypeOption {
    | None => attach(t.trailing, lbl.loc, trailing)
    | Some(modType) =>
      let (afterLbl, rest) = partitionAdjacentTrailing(lbl.loc, trailing)
      attach(t.trailing, lbl.loc, afterLbl)
      let (before, inside, after) = partitionByLoc(rest, modType.pmty_loc)
      attach(t.leading, modType.pmty_loc, before)
      walkModType(modType, t, inside)
      attach(t.trailing, modType.pmty_loc, after)
    }
  }
  
  and walkModType = (modType, t, comments) =>
    switch modType.pmty_desc {
    | Pmty_ident(longident) | Pmty_alias(longident) =>
      let (leading, trailing) = partitionLeadingTrailing(
        comments,
        longident.loc,
      )
      attach(t.leading, longident.loc, leading)
      attach(t.trailing, longident.loc, trailing)
    | Pmty_signature(signature) => walkSignature(signature, t, comments)
    | Pmty_extension(extension) => walkExtension(extension, t, comments)
    | Pmty_typeof(modExpr) =>
      let (before, inside, after) = partitionByLoc(comments, modExpr.pmod_loc)
      attach(t.leading, modExpr.pmod_loc, before)
      walkModExpr(modExpr, t, inside)
      attach(t.trailing, modExpr.pmod_loc, after)
    | Pmty_with(modType, withConstraints) =>
      let (before, inside, after) = partitionByLoc(comments, modType.pmty_loc)
      attach(t.leading, modType.pmty_loc, before)
      walkModType(modType, t, inside)
      attach(t.trailing, modType.pmty_loc, after)
    
    | Pmty_functor(_) =>
      let (parameters, returnModType) = functorType(modType)
      let comments = visitListButContinueWithRemainingComments(
        ~getLoc=((_, lbl, modTypeOption)) =>
          switch modTypeOption {
          | None => lbl.Asttypes.loc
          | Some(modType) =>
            if lbl.txt == "_" {
              modType.Parsetree.pmty_loc
            } else {
              {...lbl.loc, loc_end: modType.Parsetree.pmty_loc.loc_end}
            }
          },
        ~walkNode=walkModTypeParameter,
        ~newlineDelimited=false,
        parameters,
        t,
        comments,
      )
      
      let (before, inside, after) = partitionByLoc(
        comments,
        returnModType.pmty_loc,
      )
      attach(t.leading, returnModType.pmty_loc, before)
      walkModType(returnModType, t, inside)
      attach(t.trailing, returnModType.pmty_loc, after)
    }
  
  and walkModTypeParameter = ((_, lbl, modTypeOption), t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(comments, lbl.loc)
    attach(t.leading, lbl.loc, leading)
    switch modTypeOption {
    | None => attach(t.trailing, lbl.loc, trailing)
    | Some(modType) =>
      let (afterLbl, rest) = partitionAdjacentTrailing(lbl.loc, trailing)
      attach(t.trailing, lbl.loc, afterLbl)
      let (before, inside, after) = partitionByLoc(rest, modType.pmty_loc)
      attach(t.leading, modType.pmty_loc, before)
      walkModType(modType, t, inside)
      attach(t.trailing, modType.pmty_loc, after)
    }
  }
  
  and walkPattern = (pat, t, comments) => {
    open Location
    switch pat.Parsetree.ppat_desc {
    | _ when comments == list[] => ()
    | Ppat_alias(pat, alias) =>
      let (leading, inside, trailing) = partitionByLoc(comments, pat.ppat_loc)
      attach(t.leading, pat.ppat_loc, leading)
      walkPattern(pat, t, inside)
      let (afterPat, rest) = partitionAdjacentTrailing(pat.ppat_loc, trailing)
      attach(t.leading, pat.ppat_loc, leading)
      attach(t.trailing, pat.ppat_loc, afterPat)
      let (beforeAlias, afterAlias) = partitionLeadingTrailing(rest, alias.loc)
      attach(t.leading, alias.loc, beforeAlias)
      attach(t.trailing, alias.loc, afterAlias)
    | (Ppat_tuple(list[]) | Ppat_array(list[]))
      | Ppat_construct({txt: Longident.Lident("[]")}, _) =>
      attach(t.inside, pat.ppat_loc, comments)
    | Ppat_array(patterns) =>
      walkList(
        ~getLoc=n => n.Parsetree.ppat_loc,
        ~walkNode=walkPattern,
        patterns,
        t,
        comments,
      )
    | Ppat_tuple(patterns) =>
      walkList(
        ~getLoc=n => n.Parsetree.ppat_loc,
        ~walkNode=walkPattern,
        patterns,
        t,
        comments,
      )
    | Ppat_construct({txt: Longident.Lident("::")}, _) =>
      walkList(
        ~getLoc=n => n.Parsetree.ppat_loc,
        ~walkNode=walkPattern,
        collectListPatterns(list[], pat),
        t,
        comments,
      )
    | Ppat_construct(constr, None) =>
      let (beforeConstr, afterConstr) = partitionLeadingTrailing(
        comments,
        constr.loc,
      )
      
      attach(t.leading, constr.loc, beforeConstr)
      attach(t.trailing, constr.loc, afterConstr)
    | Ppat_construct(constr, Some(pat)) =>
      let (leading, trailing) = partitionLeadingTrailing(comments, constr.loc)
      attach(t.leading, constr.loc, leading)
      let (leading, inside, trailing) = partitionByLoc(trailing, pat.ppat_loc)
      attach(t.leading, pat.ppat_loc, leading)
      walkPattern(pat, t, inside)
      attach(t.trailing, pat.ppat_loc, trailing)
    | Ppat_record(recordRows, _) =>
      walkList(
        ~getLoc=(
          (longidentLoc, pattern): (
            Asttypes.loc<Longident.t>,
            Parsetree.pattern,
          ),
        ) => {
          ...longidentLoc.loc,
          loc_end: pattern.Parsetree.ppat_loc.loc_end,
        },
        ~walkNode=walkPatternRecordRow,
        recordRows,
        t,
        comments,
      )
    | Ppat_or(pattern1, pattern2) =>
      let (beforePattern1, insidePattern1, afterPattern1) = partitionByLoc(
        comments,
        pattern1.ppat_loc,
      )
      
      attach(t.leading, pattern1.ppat_loc, beforePattern1)
      walkPattern(pattern1, t, insidePattern1)
      let (afterPattern1, rest) = partitionAdjacentTrailing(
        pattern1.ppat_loc,
        afterPattern1,
      )
      
      attach(t.trailing, pattern1.ppat_loc, afterPattern1)
      let (beforePattern2, insidePattern2, afterPattern2) = partitionByLoc(
        rest,
        pattern2.ppat_loc,
      )
      
      attach(t.leading, pattern2.ppat_loc, beforePattern2)
      walkPattern(pattern2, t, insidePattern2)
      attach(t.trailing, pattern2.ppat_loc, afterPattern2)
    | Ppat_constraint(pattern, typ) =>
      let (beforePattern, insidePattern, afterPattern) = partitionByLoc(
        comments,
        pattern.ppat_loc,
      )
      
      attach(t.leading, pattern.ppat_loc, beforePattern)
      walkPattern(pattern, t, insidePattern)
      let (afterPattern, rest) = partitionAdjacentTrailing(
        pattern.ppat_loc,
        afterPattern,
      )
      
      attach(t.trailing, pattern.ppat_loc, afterPattern)
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typ.ptyp_loc)
      
      attach(t.leading, typ.ptyp_loc, beforeTyp)
      walkTypExpr(typ, t, insideTyp)
      attach(t.trailing, typ.ptyp_loc, afterTyp)
    | Ppat_lazy(pattern) | Ppat_exception(pattern) =>
      let (leading, inside, trailing) = partitionByLoc(
        comments,
        pattern.ppat_loc,
      )
      attach(t.leading, pattern.ppat_loc, leading)
      walkPattern(pattern, t, inside)
      attach(t.trailing, pattern.ppat_loc, trailing)
    | Ppat_unpack(stringLoc) =>
      let (leading, trailing) = partitionLeadingTrailing(
        comments,
        stringLoc.loc,
      )
      attach(t.leading, stringLoc.loc, leading)
      attach(t.trailing, stringLoc.loc, trailing)
    | Ppat_extension(extension) => walkExtension(extension, t, comments)
    | _ => ()
    }
  }
  
  and walkPatternRecordRow = (row, t, comments) =>
    switch row {
    | (
        {Location.txt: Longident.Lident(ident), loc: longidentLoc},
        {Parsetree.ppat_desc: Ppat_var({txt, _})},
      ) when ident == txt =>
      let (beforeLbl, afterLbl) = partitionLeadingTrailing(
        comments,
        longidentLoc,
      )
      
      attach(t.leading, longidentLoc, beforeLbl)
      attach(t.trailing, longidentLoc, afterLbl)
    | (longident, pattern) =>
      let (beforeLbl, afterLbl) = partitionLeadingTrailing(
        comments,
        longident.loc,
      )
      
      attach(t.leading, longident.loc, beforeLbl)
      let (afterLbl, rest) = partitionAdjacentTrailing(longident.loc, afterLbl)
      attach(t.trailing, longident.loc, afterLbl)
      let (leading, inside, trailing) = partitionByLoc(rest, pattern.ppat_loc)
      attach(t.leading, pattern.ppat_loc, leading)
      walkPattern(pattern, t, inside)
      attach(t.trailing, pattern.ppat_loc, trailing)
    }
  
  and walkTypExpr = (typ, t, comments) =>
    switch typ.Parsetree.ptyp_desc {
    | _ when comments == list[] => ()
    | Ptyp_tuple(typexprs) =>
      walkList(
        ~getLoc=n => n.Parsetree.ptyp_loc,
        ~walkNode=walkTypExpr,
        typexprs,
        t,
        comments,
      )
    | Ptyp_extension(extension) => walkExtension(extension, t, comments)
    | Ptyp_package(packageType) => walkPackageType(packageType, t, comments)
    | Ptyp_alias(typexpr, alias) =>
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(
        comments,
        typexpr.ptyp_loc,
      )
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
    | Ptyp_poly(strings, typexpr) =>
      let comments = visitListButContinueWithRemainingComments(
        ~getLoc=n => n.Asttypes.loc,
        ~walkNode=(longident, t, comments) => {
          let (beforeLongident, afterLongident) = partitionLeadingTrailing(
            comments,
            longident.loc,
          )
          attach(t.leading, longident.loc, beforeLongident)
          attach(t.trailing, longident.loc, afterLongident)
        },
        ~newlineDelimited=false,
        strings,
        t,
        comments,
      )
      
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(
        comments,
        typexpr.ptyp_loc,
      )
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
    | Ptyp_constr(longident, typexprs) =>
      let (beforeLongident, afterLongident) = partitionLeadingTrailing(
        comments,
        longident.loc,
      )
      let (afterLongident, rest) = partitionAdjacentTrailing(
        longident.loc,
        comments,
      )
      attach(t.leading, longident.loc, beforeLongident)
      attach(t.trailing, longident.loc, afterLongident)
      walkList(
        ~getLoc=n => n.Parsetree.ptyp_loc,
        ~walkNode=walkTypExpr,
        typexprs,
        t,
        rest,
      )
    | Ptyp_arrow(_) =>
      let (_, parameters, typexpr) = arrowType(typ)
      let comments = walkTypeParameters(parameters, t, comments)
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(
        comments,
        typexpr.ptyp_loc,
      )
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
    | Ptyp_object(fields, _) => walkTypObjectFields(fields, t, comments)
    | _ => ()
    }
  
  and walkTypObjectFields = (fields, t, comments) =>
    walkList(
      ~getLoc=field =>
        switch field {
        | Parsetree.Otag(lbl, _, typ) =>
          {...lbl.loc, loc_end: typ.ptyp_loc.loc_end}
        | _ => Location.none
        },
      ~walkNode=walkTypObjectField,
      fields,
      t,
      comments,
    )
  
  and walkTypObjectField = (field, t, comments) =>
    switch field {
    | Otag(lbl, _, typexpr) =>
      let (beforeLbl, afterLbl) = partitionLeadingTrailing(comments, lbl.loc)
      attach(t.leading, lbl.loc, beforeLbl)
      let (afterLbl, rest) = partitionAdjacentTrailing(lbl.loc, afterLbl)
      attach(t.trailing, lbl.loc, afterLbl)
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(
        rest,
        typexpr.ptyp_loc,
      )
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
    | _ => ()
    }
  
  and walkTypeParameters = (typeParameters, t, comments) =>
    visitListButContinueWithRemainingComments(
      ~getLoc=((_, _, typexpr)) => typexpr.Parsetree.ptyp_loc,
      ~walkNode=walkTypeParameter,
      ~newlineDelimited=false,
      typeParameters,
      t,
      comments,
    )
  
  and walkTypeParameter = ((_attrs, _lbl, typexpr), t, comments) => {
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(
      comments,
      typexpr.ptyp_loc,
    )
    attach(t.leading, typexpr.ptyp_loc, beforeTyp)
    walkTypExpr(typexpr, t, insideTyp)
    attach(t.trailing, typexpr.ptyp_loc, afterTyp)
  }
  
  and walkPackageType = (packageType, t, comments) => {
    let (longident, packageConstraints) = packageType
    let (beforeLongident, afterLongident) = partitionLeadingTrailing(
      comments,
      longident.loc,
    )
    attach(t.leading, longident.loc, beforeLongident)
    let (afterLongident, rest) = partitionAdjacentTrailing(
      longident.loc,
      afterLongident,
    )
    attach(t.trailing, longident.loc, afterLongident)
    walkPackageConstraints(packageConstraints, t, rest)
  }
  
  and walkPackageConstraints = (packageConstraints, t, comments) =>
    walkList(
      ~getLoc=((longident, typexpr)) => {
        ...longident.Asttypes.loc,
        loc_end: typexpr.Parsetree.ptyp_loc.loc_end,
      },
      ~walkNode=walkPackageConstraint,
      packageConstraints,
      t,
      comments,
    )
  
  and walkPackageConstraint = (packageConstraint, t, comments) => {
    let (longident, typexpr) = packageConstraint
    let (beforeLongident, afterLongident) = partitionLeadingTrailing(
      comments,
      longident.loc,
    )
    attach(t.leading, longident.loc, beforeLongident)
    let (afterLongident, rest) = partitionAdjacentTrailing(
      longident.loc,
      afterLongident,
    )
    attach(t.trailing, longident.loc, afterLongident)
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(
      rest,
      typexpr.ptyp_loc,
    )
    attach(t.leading, typexpr.ptyp_loc, beforeTyp)
    walkTypExpr(typexpr, t, insideTyp)
    attach(t.trailing, typexpr.ptyp_loc, afterTyp)
  }
  
  and walkExtension = (extension, t, comments) => {
    let (id, payload) = extension
    let (beforeId, afterId) = partitionLeadingTrailing(comments, id.loc)
    attach(t.leading, id.loc, beforeId)
    let (afterId, rest) = partitionAdjacentTrailing(id.loc, afterId)
    attach(t.trailing, id.loc, afterId)
    walkPayload(payload, t, rest)
  }
  
  and walkAttribute = ((id, payload), t, comments) => {
    let (beforeId, afterId) = partitionLeadingTrailing(comments, id.loc)
    attach(t.leading, id.loc, beforeId)
    let (afterId, rest) = partitionAdjacentTrailing(id.loc, afterId)
    attach(t.trailing, id.loc, afterId)
    walkPayload(payload, t, rest)
  }
  
  and walkPayload = (payload, t, comments) =>
    switch payload {
    | PStr(s) => walkStructure(s, t, comments)
    | _ => ()
    }
}

module Printer = {
  let addParens = doc =>
    Doc.group(
      Doc.concat(list[
        Doc.lparen,
        Doc.indent(Doc.concat(list[Doc.softLine, doc])),
        Doc.softLine,
        Doc.rparen,
      ]),
    )
  
  let addBraces = doc =>
    Doc.group(Doc.concat(list[Doc.lbrace, doc, Doc.rbrace]))
  
  let getFirstLeadingComment = (tbl, loc) =>
    switch Hashtbl.find(tbl.CommentTable.leading, loc) {
    | list[comment, ..._] => Some(comment)
    | list[] => None
    | exception Not_found => None
    }
  
  let printMultilineCommentContent = txt => {
    let rec indentStars = (lines, acc) =>
      switch lines {
      | list[] => Doc.nil
      | list[lastLine] =>
        let line = String.trim(lastLine)
        let doc = Doc.text(" " ++ line)
        let trailingSpace = if String.length(line) > 0 {
          Doc.space
        } else {
          Doc.nil
        }
        List.rev(list[trailingSpace, doc, ...acc]) |> Doc.concat
      | list[line, ...lines] =>
        let line = String.trim(line)
        let len = String.length(line)
        if len > 0 && String.get(line, 0) === '*' {
          let doc = Doc.text(" " ++ String.trim(line))
          indentStars(lines, list[Doc.hardLine, doc, ...acc])
        } else if len === 0 {
          List.rev(acc) |> Doc.concat
        } else {
          let content = String.trim(txt)
          Doc.text(content ++ " ")
        }
      }
    
    let lines = String.split_on_char('\n', txt)
    switch lines {
    | list[] => Doc.text("/* */")
    | list[line] => Doc.text("/* " ++ String.trim(line) ++ " */")
    | list[first, ...rest] =>
      let firstLine = String.trim(first)
      Doc.concat(list[
        Doc.text("/*"),
        if String.length(firstLine) > 0 && !String.equal(firstLine, "*") {
          Doc.space
        } else {
          Doc.nil
        },
        indentStars(rest, list[Doc.hardLine, Doc.text(firstLine)]),
        Doc.text("*/"),
      ])
    }
  }
  
  let printTrailingComment = (nodeLoc: Location.t, comment) => {
    let singleLine = Comment.isSingleLineComment(comment)
    let content = {
      let txt = Comment.txt(comment)
      if singleLine {
        Doc.text("// " ++ String.trim(txt))
      } else {
        printMultilineCommentContent(txt)
      }
    }
    
    let diff = {
      let cmtStart = Comment.loc(comment).loc_start
      let prevTokEndPos = Comment.prevTokEndPos(comment)
      cmtStart.pos_lnum - prevTokEndPos.pos_lnum
    }
    
    let isBelow =
      Comment.loc(comment).loc_start.pos_lnum > nodeLoc.loc_end.pos_lnum
    if diff > 0 || isBelow {
      Doc.lineSuffix(
        Doc.concat(list[
          Doc.hardLine,
          if diff > 1 {
            Doc.hardLine
          } else {
            Doc.nil
          },
          content,
        ]),
      )
    } else if !singleLine {
      Doc.concat(list[Doc.space, content])
    } else {
      Doc.lineSuffix(Doc.concat(list[Doc.space, content]))
    }
  }
  
  let printLeadingComment = (~nextComment=?, comment) => {
    let singleLine = Comment.isSingleLineComment(comment)
    let content = {
      let txt = Comment.txt(comment)
      if singleLine {
        Doc.text("// " ++ String.trim(txt))
      } else {
        printMultilineCommentContent(txt)
      }
    }
    
    let separator = Doc.concat(list[
      if singleLine {
        Doc.concat(list[Doc.hardLine, Doc.breakParent])
      } else {
        Doc.nil
      },
      switch nextComment {
      | Some(next) =>
        let nextLoc = Comment.loc(next)
        let currLoc = Comment.loc(comment)
        let diff =
          nextLoc.Location.loc_start.pos_lnum -
          currLoc.Location.loc_end.pos_lnum
        
        let nextSingleLine = Comment.isSingleLineComment(next)
        if singleLine && nextSingleLine {
          if diff > 1 {
            Doc.hardLine
          } else {
            Doc.nil
          }
        } else if singleLine && !nextSingleLine {
          if diff > 1 {
            Doc.hardLine
          } else {
            Doc.nil
          }
        } else if diff > 1 {
          Doc.concat(list[Doc.hardLine, Doc.hardLine])
        } else if diff === 1 {
          Doc.hardLine
        } else {
          Doc.space
        }
      | None => Doc.nil
      },
    ])
    
    Doc.concat(list[content, separator])
  }
  
  let printCommentsInside = (cmtTbl, loc) =>
    switch Hashtbl.find(cmtTbl.CommentTable.inside, loc) {
    | exception Not_found => Doc.nil
    | comments =>
      Doc.group(
        Doc.join(~sep=Doc.line, List.map(printLeadingComment, comments)),
      )
    }
  
  let printLeadingComments = (node, tbl, loc) => {
    let rec loop = (acc, comments) =>
      switch comments {
      | list[] => node
      | list[comment] =>
        let cmtDoc = printLeadingComment(comment)
        let diff =
          loc.Location.loc_start.pos_lnum -
          Comment.loc(comment).Location.loc_end.pos_lnum
        
        let separator = if Comment.isSingleLineComment(comment) {
          if diff > 1 {
            Doc.hardLine
          } else {
            Doc.nil
          }
        } else if diff === 0 {
          Doc.space
        } else if diff > 1 {
          Doc.concat(list[Doc.hardLine, Doc.hardLine])
        } else {
          Doc.hardLine
        }
        
        let doc = Doc.group(
          Doc.concat(list[
            Doc.concat(List.rev(list[cmtDoc, ...acc])),
            separator,
            node,
          ]),
        )
        
        doc
      | list[comment, ...list[nextComment, ...comments] as rest] =>
        let cmtDoc = printLeadingComment(~nextComment, comment)
        loop(list[cmtDoc, ...acc], rest)
      }
    
    switch Hashtbl.find(tbl, loc) {
    | exception Not_found => node
    | comments =>
      Hashtbl.remove(tbl, loc)
      loop(list[], comments)
    }
  }
  
  let printTrailingComments = (node, tbl, loc) => {
    let rec loop = (acc, comments) =>
      switch comments {
      | list[] => Doc.concat(List.rev(acc))
      | list[comment, ...comments] =>
        let cmtDoc = printTrailingComment(loc, comment)
        loop(list[cmtDoc, ...acc], comments)
      }
    
    switch Hashtbl.find(tbl, loc) {
    | exception Not_found => node
    | list[] => node
    | list[first, ..._] as comments =>
      Hashtbl.remove(tbl, loc)
      let cmtsDoc = loop(list[], comments)
      Doc.concat(list[node, cmtsDoc])
    }
  }
  
  let printComments = (doc, tbl: CommentTable.t, loc) => {
    let docWithLeadingComments = printLeadingComments(doc, tbl.leading, loc)
    printTrailingComments(docWithLeadingComments, tbl.trailing, loc)
  }
  
  let printLoc = (loc: Location.t) =>
    Format.sprintf(
      "{start: %d-%d; end: %d-%d}",
      loc.loc_start.pos_lnum,
      loc.loc_start.pos_cnum - loc.loc_start.pos_bol,
      loc.loc_end.pos_lnum,
      loc.loc_end.pos_cnum - loc.loc_end.pos_bol,
    ) |> print_endline
  
  let printList = (~getLoc, ~nodes, ~print, ~forceBreak=false, t) => {
    let rec loop = (prevLoc: Location.t, acc, nodes) =>
      switch nodes {
      | list[] => (prevLoc, Doc.concat(List.rev(acc)))
      | list[node, ...nodes] =>
        let loc = getLoc(node)
        let startPos = switch getFirstLeadingComment(t, loc) {
        | None => loc.loc_start
        | Some(comment) => Comment.loc(comment).loc_start
        }
        
        let sep = if startPos.pos_lnum - prevLoc.loc_end.pos_lnum > 1 {
          Doc.concat(list[Doc.hardLine, Doc.hardLine])
        } else {
          Doc.hardLine
        }
        
        let doc = printComments(print(node, t), t, loc)
        loop(loc, list[doc, sep, ...acc], nodes)
      }
    
    switch nodes {
    | list[] => Doc.nil
    | list[node, ...nodes] =>
      let firstLoc = getLoc(node)
      let doc = printComments(print(node, t), t, firstLoc)
      let (lastLoc, docs) = loop(firstLoc, list[doc], nodes)
      let forceBreak =
        forceBreak || firstLoc.loc_start.pos_lnum !== lastLoc.loc_end.pos_lnum
      
      Doc.breakableGroup(~forceBreak, docs)
    }
  }
  
  let printListi = (~getLoc, ~nodes, ~print, ~forceBreak=false, t) => {
    let rec loop = (i, prevLoc: Location.t, acc, nodes) =>
      switch nodes {
      | list[] => (prevLoc, Doc.concat(List.rev(acc)))
      | list[node, ...nodes] =>
        let loc = getLoc(node)
        let startPos = switch getFirstLeadingComment(t, loc) {
        | None => loc.loc_start
        | Some(comment) => Comment.loc(comment).loc_start
        }
        
        let sep = if startPos.pos_lnum - prevLoc.loc_end.pos_lnum > 1 {
          Doc.concat(list[Doc.hardLine, Doc.hardLine])
        } else {
          Doc.line
        }
        
        let doc = printComments(print(node, t, i), t, loc)
        loop(i + 1, loc, list[doc, sep, ...acc], nodes)
      }
    
    switch nodes {
    | list[] => Doc.nil
    | list[node, ...nodes] =>
      let firstLoc = getLoc(node)
      let doc = printComments(print(node, t, 0), t, firstLoc)
      let (lastLoc, docs) = loop(1, firstLoc, list[doc], nodes)
      let forceBreak =
        forceBreak || firstLoc.loc_start.pos_lnum !== lastLoc.loc_end.pos_lnum
      
      Doc.breakableGroup(~forceBreak, docs)
    }
  }
  
  let interleaveWhitespace = (
    ~forceBreak=false,
    rows: list<(Location.t, Doc.t)>,
  ) => {
    let rec loop = (prevLoc, acc, rows) =>
      switch rows {
      | list[] => Doc.concat(List.rev(acc))
      | list[(loc, doc), ...rest] =>
        if (
          loc.Location.loc_start.pos_lnum -
          prevLoc.Location.loc_end.pos_lnum > 1
        ) {
          loop(loc, list[doc, Doc.line, Doc.line, ...acc], rest)
        } else {
          loop(loc, list[doc, Doc.line, ...acc], rest)
        }
      }
    
    switch rows {
    | list[] => Doc.nil
    | list[(firstLoc, firstDoc), ...rest] =>
      let forceBreak =
        forceBreak ||
        switch List.rev(rest) {
        | list[(lastLoc, _), ..._] =>
          firstLoc.loc_start.pos_lnum !== lastLoc.loc_end.pos_lnum
        | _ => false
        }
      
      Doc.breakableGroup(~forceBreak, loop(firstLoc, list[firstDoc], rest))
    }
  }
  
  let printLongident = l =>
    switch l {
    | Longident.Lident(lident) => Doc.text(lident)
    | Longident.Ldot(lident, txt) as l =>
      let txts = Longident.flatten(l)
      Doc.join(~sep=Doc.dot, List.map(Doc.text, txts))
    | _ => failwith("unsupported ident")
    }
  
  let printLongidentLocation = (l, cmtTbl) => {
    let doc = printLongident(l.Location.txt)
    printComments(doc, cmtTbl, l.loc)
  }
  
  let printStringLoc = (sloc, cmtTbl) => {
    let doc = Doc.text(sloc.Location.txt)
    printComments(doc, cmtTbl, sloc.loc)
  }
  
  let escapeStringContents = s => {
    let len = String.length(s)
    let b = Buffer.create(len)
    for i in 0 to len - 1 {
      let c = String.get(s, i)
      if c == '\b' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 'b')
      } else if c == '\t' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 't')
      } else if c == '\n' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 'n')
      } else if c == '\r' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 'r')
      } else if c == '"' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, '"')
      } else if c == '\\' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, '\\')
      } else {
        Buffer.add_char(b, c)
      }
    }
    Buffer.contents(b)
  }
  
  let printConstant = c =>
    switch c {
    | Parsetree.Pconst_integer(s, suffix) =>
      switch suffix {
      | Some(c) => Doc.text(s ++ Char.escaped(c))
      | None => Doc.text(s)
      }
    | Pconst_string(s, _) => Doc.text("\"" ++ escapeStringContents(s) ++ "\"")
    | Pconst_float(s, _) => Doc.text(s)
    | Pconst_char(c) => Doc.text("'" ++ Char.escaped(c) ++ "'")
    }
  
  let rec printStructure = (s: Parsetree.structure, t) =>
    printList(
      ~getLoc=s => s.Parsetree.pstr_loc,
      ~nodes=s,
      ~print=printStructureItem,
      t,
    )
  
  and printStructureItem = (si: Parsetree.structure_item, cmtTbl) =>
    switch si.pstr_desc {
    | Pstr_value(rec_flag, valueBindings) =>
      let recFlag = switch rec_flag {
      | Asttypes.Nonrecursive => Doc.nil
      | Asttypes.Recursive => Doc.text("rec ")
      }
      
      printValueBindings(~recFlag, valueBindings, cmtTbl)
    | Pstr_type(recFlag, typeDeclarations) =>
      let recFlag = switch recFlag {
      | Asttypes.Nonrecursive => Doc.nil
      | Asttypes.Recursive => Doc.text("rec ")
      }
      
      printTypeDeclarations(~recFlag, typeDeclarations, cmtTbl)
    | Pstr_primitive(valueDescription) =>
      printValueDescription(valueDescription, cmtTbl)
    | Pstr_eval(expr, attrs) =>
      let needsParens = switch expr {
      | {
          pexp_attributes: list[({txt: "ns.ternary"}, _)],
          pexp_desc: Pexp_ifthenelse(_),
        } =>
        false
      | _ when ParsetreeViewer.hasAttributes(expr.pexp_attributes) => true
      | _ => false
      }
      
      let exprDoc = {
        let doc = printExpressionWithComments(expr, cmtTbl)
        if needsParens {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.concat(list[printAttributes(attrs), exprDoc])
    | Pstr_attribute(attr) =>
      Doc.concat(list[Doc.text("@"), printAttributeWithComments(attr, cmtTbl)])
    | Pstr_extension(extension, attrs) =>
      Doc.concat(list[
        printAttributes(attrs),
        Doc.concat(list[
          Doc.text("%"),
          printExtensionWithComments(extension, cmtTbl),
        ]),
      ])
    | Pstr_include(includeDeclaration) =>
      printIncludeDeclaration(includeDeclaration, cmtTbl)
    | Pstr_open(openDescription) =>
      printOpenDescription(openDescription, cmtTbl)
    | Pstr_modtype(modTypeDecl) =>
      printModuleTypeDeclaration(modTypeDecl, cmtTbl)
    | Pstr_module(moduleBinding) =>
      printModuleBinding(~isRec=false, moduleBinding, cmtTbl, 0)
    | Pstr_recmodule(moduleBindings) =>
      printListi(
        ~getLoc=mb => mb.Parsetree.pmb_loc,
        ~nodes=moduleBindings,
        ~print=printModuleBinding(~isRec=true),
        cmtTbl,
      )
    | Pstr_exception(extensionConstructor) =>
      printExceptionDef(extensionConstructor, cmtTbl)
    | Pstr_typext(typeExtension) => printTypeExtension(typeExtension, cmtTbl)
    | Pstr_class(_) | Pstr_class_type(_) => Doc.nil
    }
  
  and printTypeExtension = (te: Parsetree.type_extension, cmtTbl) => {
    let prefix = Doc.text("type ")
    let name = printLongidentLocation(te.ptyext_path, cmtTbl)
    let typeParams = switch te.ptyext_params {
    | list[] => Doc.nil
    | typeParams =>
      Doc.group(
        Doc.concat(list[
          Doc.lessThan,
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                List.map(
                  typeParam => {
                    let doc = printTypeParam(typeParam, cmtTbl)
                    printComments(
                      doc,
                      cmtTbl,
                      fst(typeParam).Parsetree.ptyp_loc,
                    )
                  },
                  typeParams,
                ),
              ),
            ]),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.greaterThan,
        ]),
      )
    }
    
    let extensionConstructors = {
      let ecs = te.ptyext_constructors
      let forceBreak = switch (ecs, List.rev(ecs)) {
      | (list[first, ..._], list[last, ..._]) =>
        first.pext_loc.loc_start.pos_lnum >
        te.ptyext_path.loc.loc_end.pos_lnum ||
          first.pext_loc.loc_start.pos_lnum < last.pext_loc.loc_end.pos_lnum
      | _ => false
      }
      
      let privateFlag = switch te.ptyext_private {
      | Asttypes.Private => Doc.concat(list[Doc.text("private"), Doc.line])
      | Public => Doc.nil
      }
      
      let rows = printListi(
        ~getLoc=n => n.Parsetree.pext_loc,
        ~print=printExtensionConstructor,
        ~nodes=ecs,
        ~forceBreak,
        cmtTbl,
      )
      
      Doc.breakableGroup(
        ~forceBreak,
        Doc.indent(Doc.concat(list[Doc.line, privateFlag, rows])),
      )
    }
    
    Doc.group(
      Doc.concat(list[
        printAttributes(~loc=te.ptyext_path.loc, te.ptyext_attributes),
        prefix,
        name,
        typeParams,
        Doc.text(" +="),
        extensionConstructors,
      ]),
    )
  }
  
  and printModuleBinding = (~isRec, moduleBinding, cmtTbl, i) => {
    let prefix = if i == 0 {
      Doc.concat(list[
        Doc.text("module "),
        if isRec {
          Doc.text("rec ")
        } else {
          Doc.nil
        },
      ])
    } else {
      Doc.text("and ")
    }
    
    let (modExprDoc, modConstraintDoc) = switch moduleBinding.pmb_expr {
    | {pmod_desc: Pmod_constraint(modExpr, modType)} =>
      (
        printModExpr(modExpr, cmtTbl),
        Doc.concat(list[Doc.text(": "), printModType(modType, cmtTbl)]),
      )
    | modExpr => (printModExpr(modExpr, cmtTbl), Doc.nil)
    }
    
    let modName = {
      let doc = Doc.text(moduleBinding.pmb_name.Location.txt)
      printComments(doc, cmtTbl, moduleBinding.pmb_name.loc)
    }
    
    let doc = Doc.concat(list[
      printAttributes(
        ~loc=moduleBinding.pmb_name.loc,
        moduleBinding.pmb_attributes,
      ),
      prefix,
      modName,
      modConstraintDoc,
      Doc.text(" = "),
      modExprDoc,
    ])
    printComments(doc, cmtTbl, moduleBinding.pmb_loc)
  }
  
  and printModuleTypeDeclaration = (
    modTypeDecl: Parsetree.module_type_declaration,
    cmtTbl,
  ) => {
    let modName = {
      let doc = Doc.text(modTypeDecl.pmtd_name.txt)
      printComments(doc, cmtTbl, modTypeDecl.pmtd_name.loc)
    }
    
    Doc.concat(list[
      printAttributes(modTypeDecl.pmtd_attributes),
      Doc.text("module type "),
      modName,
      switch modTypeDecl.pmtd_type {
      | None => Doc.nil
      | Some(modType) =>
        Doc.concat(list[Doc.text(" = "), printModType(modType, cmtTbl)])
      },
    ])
  }
  
  and printModType = (modType, cmtTbl) => {
    let modTypeDoc = switch modType.pmty_desc {
    | Parsetree.Pmty_ident(longident) =>
      Doc.concat(list[
        printAttributes(~loc=longident.loc, modType.pmty_attributes),
        printLongidentLocation(longident, cmtTbl),
      ])
    | Pmty_signature(signature) =>
      let signatureDoc = Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list[
          Doc.lbrace,
          Doc.indent(
            Doc.concat(list[Doc.line, printSignature(signature, cmtTbl)]),
          ),
          Doc.line,
          Doc.rbrace,
        ]),
      )
      Doc.concat(list[printAttributes(modType.pmty_attributes), signatureDoc])
    | Pmty_functor(_) =>
      let (parameters, returnType) = ParsetreeViewer.functorType(modType)
      let parametersDoc = switch parameters {
      | list[] => Doc.nil
      | list[(attrs, {Location.txt: "_", loc}, Some(modType))] =>
        let cmtLoc = {...loc, loc_end: modType.Parsetree.pmty_loc.loc_end}
        
        let attrs = switch attrs {
        | list[] => Doc.nil
        | attrs =>
          Doc.concat(list[
            Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
            Doc.line,
          ])
        }
        let doc = Doc.concat(list[attrs, printModType(modType, cmtTbl)])
        printComments(doc, cmtTbl, cmtLoc)
      | params =>
        Doc.group(
          Doc.concat(list[
            Doc.lparen,
            Doc.indent(
              Doc.concat(list[
                Doc.softLine,
                Doc.join(
                  ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                  List.map(
                    ((attrs, lbl, modType)) => {
                      let cmtLoc = switch modType {
                      | None => lbl.Asttypes.loc
                      | Some(modType) =>
                        {
                          ...lbl.Asttypes.loc,
                          loc_end: modType.Parsetree.pmty_loc.loc_end,
                        }
                      }
                      
                      let attrs = switch attrs {
                      | list[] => Doc.nil
                      | attrs =>
                        Doc.concat(list[
                          Doc.join(
                            ~sep=Doc.line,
                            List.map(printAttribute, attrs),
                          ),
                          Doc.line,
                        ])
                      }
                      let lblDoc = if lbl.Location.txt == "_" {
                        Doc.nil
                      } else {
                        let doc = Doc.text(lbl.txt)
                        printComments(doc, cmtTbl, lbl.loc)
                      }
                      
                      let doc = Doc.concat(list[
                        attrs,
                        lblDoc,
                        switch modType {
                        | None => Doc.nil
                        | Some(modType) =>
                          Doc.concat(list[
                            if lbl.txt == "_" {
                              Doc.nil
                            } else {
                              Doc.text(": ")
                            },
                            printModType(modType, cmtTbl),
                          ])
                        },
                      ])
                      printComments(doc, cmtTbl, cmtLoc)
                    },
                    params,
                  ),
                ),
              ]),
            ),
            Doc.trailingComma,
            Doc.softLine,
            Doc.rparen,
          ]),
        )
      }
      
      let returnDoc = {
        let doc = printModType(returnType, cmtTbl)
        if Parens.modTypeFunctorReturn(returnType) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.group(
        Doc.concat(list[
          parametersDoc,
          Doc.group(Doc.concat(list[Doc.text(" =>"), Doc.line, returnDoc])),
        ]),
      )
    | Pmty_typeof(modExpr) =>
      Doc.concat(list[
        Doc.text("module type of "),
        printModExpr(modExpr, cmtTbl),
      ])
    | Pmty_extension(extension) => printExtensionWithComments(extension, cmtTbl)
    | Pmty_alias(longident) =>
      Doc.concat(list[
        Doc.text("module "),
        printLongidentLocation(longident, cmtTbl),
      ])
    | Pmty_with(modType, withConstraints) =>
      let operand = {
        let doc = printModType(modType, cmtTbl)
        if Parens.modTypeWithOperand(modType) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.group(
        Doc.concat(list[
          operand,
          Doc.indent(
            Doc.concat(list[Doc.line, printWithConstraints(withConstraints)]),
          ),
        ]),
      )
    }
    
    let attrsAlreadyPrinted = switch modType.pmty_desc {
    | (Pmty_functor(_) | Pmty_signature(_)) | Pmty_ident(_) => true
    | _ => false
    }
    
    let doc = Doc.concat(list[
      if attrsAlreadyPrinted {
        Doc.nil
      } else {
        printAttributes(modType.pmty_attributes)
      },
      modTypeDoc,
    ])
    printComments(doc, cmtTbl, modType.pmty_loc)
  }
  
  and printWithConstraints = withConstraints => {
    let rows = List.mapi(
      (i, withConstraint) =>
        Doc.group(
          Doc.concat(list[
            if i === 0 {
              Doc.text("with ")
            } else {
              Doc.text("and ")
            },
            printWithConstraint(withConstraint),
          ]),
        ),
      withConstraints,
    )
    
    Doc.join(~sep=Doc.line, rows)
  }
  
  and printWithConstraint = (withConstraint: Parsetree.with_constraint) =>
    switch withConstraint {
    | Pwith_type({txt: longident}, typeDeclaration) =>
      Doc.group(
        printTypeDeclaration(
          ~name=printLongident(longident),
          ~equalSign="=",
          ~recFlag=Doc.nil,
          0,
          typeDeclaration,
          CommentTable.empty,
        ),
      )
    
    | Pwith_module({txt: longident1}, {txt: longident2}) =>
      Doc.concat(list[
        Doc.text("module "),
        printLongident(longident1),
        Doc.text(" ="),
        Doc.indent(Doc.concat(list[Doc.line, printLongident(longident2)])),
      ])
    
    | Pwith_typesubst({txt: longident}, typeDeclaration) =>
      Doc.group(
        printTypeDeclaration(
          ~name=printLongident(longident),
          ~equalSign=":=",
          ~recFlag=Doc.nil,
          0,
          typeDeclaration,
          CommentTable.empty,
        ),
      )
    | Pwith_modsubst({txt: longident1}, {txt: longident2}) =>
      Doc.concat(list[
        Doc.text("module "),
        printLongident(longident1),
        Doc.text(" :="),
        Doc.indent(Doc.concat(list[Doc.line, printLongident(longident2)])),
      ])
    }
  
  and printSignature = (signature, cmtTbl) =>
    printList(
      ~getLoc=s => s.Parsetree.psig_loc,
      ~nodes=signature,
      ~print=printSignatureItem,
      cmtTbl,
    )
  
  and printSignatureItem = (si: Parsetree.signature_item, cmtTbl) =>
    switch si.psig_desc {
    | Parsetree.Psig_value(valueDescription) =>
      printValueDescription(valueDescription, cmtTbl)
    | Psig_type(recFlag, typeDeclarations) =>
      let recFlag = switch recFlag {
      | Asttypes.Nonrecursive => Doc.nil
      | Asttypes.Recursive => Doc.text("rec ")
      }
      
      printTypeDeclarations(~recFlag, typeDeclarations, cmtTbl)
    | Psig_typext(typeExtension) => printTypeExtension(typeExtension, cmtTbl)
    | Psig_exception(extensionConstructor) =>
      printExceptionDef(extensionConstructor, cmtTbl)
    | Psig_module(moduleDeclaration) =>
      printModuleDeclaration(moduleDeclaration, cmtTbl)
    | Psig_recmodule(moduleDeclarations) =>
      printRecModuleDeclarations(moduleDeclarations, cmtTbl)
    | Psig_modtype(modTypeDecl) =>
      printModuleTypeDeclaration(modTypeDecl, cmtTbl)
    | Psig_open(openDescription) =>
      printOpenDescription(openDescription, cmtTbl)
    | Psig_include(includeDescription) =>
      printIncludeDescription(includeDescription, cmtTbl)
    | Psig_attribute(attr) =>
      Doc.concat(list[Doc.text("@"), printAttributeWithComments(attr, cmtTbl)])
    | Psig_extension(extension, attrs) =>
      Doc.concat(list[
        printAttributes(attrs),
        Doc.concat(list[
          Doc.text("%"),
          printExtensionWithComments(extension, cmtTbl),
        ]),
      ])
    | Psig_class(_) | Psig_class_type(_) => Doc.nil
    }
  
  and printRecModuleDeclarations = (moduleDeclarations, cmtTbl) =>
    printListi(
      ~getLoc=n => n.Parsetree.pmd_loc,
      ~nodes=moduleDeclarations,
      ~print=printRecModuleDeclaration,
      cmtTbl,
    )
  
  and printRecModuleDeclaration = (md, cmtTbl, i) => {
    let body = switch md.pmd_type.pmty_desc {
    | Parsetree.Pmty_alias(longident) =>
      Doc.concat(list[
        Doc.text(" = "),
        printLongidentLocation(longident, cmtTbl),
      ])
    | _ =>
      let needsParens = switch md.pmd_type.pmty_desc {
      | Pmty_with(_) => true
      | _ => false
      }
      
      let modTypeDoc = {
        let doc = printModType(md.pmd_type, cmtTbl)
        if needsParens {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.concat(list[Doc.text(": "), modTypeDoc])
    }
    
    let prefix = if i < 1 {
      "module rec "
    } else {
      "and "
    }
    Doc.concat(list[
      printAttributes(~loc=md.pmd_name.loc, md.pmd_attributes),
      Doc.text(prefix),
      printComments(Doc.text(md.pmd_name.txt), cmtTbl, md.pmd_name.loc),
      body,
    ])
  }
  
  and printModuleDeclaration = (md: Parsetree.module_declaration, cmtTbl) => {
    let body = switch md.pmd_type.pmty_desc {
    | Parsetree.Pmty_alias(longident) =>
      Doc.concat(list[
        Doc.text(" = "),
        printLongidentLocation(longident, cmtTbl),
      ])
    | _ => Doc.concat(list[Doc.text(": "), printModType(md.pmd_type, cmtTbl)])
    }
    
    Doc.concat(list[
      printAttributes(~loc=md.pmd_name.loc, md.pmd_attributes),
      Doc.text("module "),
      printComments(Doc.text(md.pmd_name.txt), cmtTbl, md.pmd_name.loc),
      body,
    ])
  }
  
  and printOpenDescription = (openDescription: Parsetree.open_description, p) =>
    Doc.concat(list[
      printAttributes(openDescription.popen_attributes),
      Doc.text("open"),
      switch openDescription.popen_override {
      | Asttypes.Fresh => Doc.space
      | Asttypes.Override => Doc.text("! ")
      },
      printLongidentLocation(openDescription.popen_lid, p),
    ])
  
  and printIncludeDescription = (
    includeDescription: Parsetree.include_description,
    cmtTbl,
  ) =>
    Doc.concat(list[
      printAttributes(includeDescription.pincl_attributes),
      Doc.text("include "),
      printModType(includeDescription.pincl_mod, cmtTbl),
    ])
  
  and printIncludeDeclaration = (
    includeDeclaration: Parsetree.include_declaration,
    cmtTbl,
  ) =>
    Doc.concat(list[
      printAttributes(includeDeclaration.pincl_attributes),
      Doc.text("include "),
      printModExpr(includeDeclaration.pincl_mod, cmtTbl),
    ])
  
  and printValueBindings = (
    ~recFlag,
    vbs: list<Parsetree.value_binding>,
    cmtTbl,
  ) =>
    printListi(
      ~getLoc=vb => vb.Parsetree.pvb_loc,
      ~nodes=vbs,
      ~print=printValueBinding(~recFlag),
      cmtTbl,
    )
  
  and printValueDescription = (valueDescription, cmtTbl) => {
    let isExternal = switch valueDescription.pval_prim {
    | list[] => false
    | _ => true
    }
    
    Doc.group(
      Doc.concat(list[
        printAttributes(valueDescription.pval_attributes),
        Doc.text(
          if isExternal {
            "external "
          } else {
            "let "
          },
        ),
        printComments(
          Doc.text(valueDescription.pval_name.txt),
          cmtTbl,
          valueDescription.pval_name.loc,
        ),
        Doc.text(": "),
        printTypExpr(valueDescription.pval_type, cmtTbl),
        if isExternal {
          Doc.group(
            Doc.concat(list[
              Doc.text(" ="),
              Doc.indent(
                Doc.concat(list[
                  Doc.line,
                  Doc.join(
                    ~sep=Doc.line,
                    List.map(
                      s =>
                        Doc.concat(list[
                          Doc.text("\""),
                          Doc.text(s),
                          Doc.text("\""),
                        ]),
                      valueDescription.pval_prim,
                    ),
                  ),
                ]),
              ),
            ]),
          )
        } else {
          Doc.nil
        },
      ]),
    )
  }
  
  and printTypeDeclarations = (~recFlag, typeDeclarations, cmtTbl) =>
    printListi(
      ~getLoc=n => n.Parsetree.ptype_loc,
      ~nodes=typeDeclarations,
      ~print=printTypeDeclaration2(~recFlag),
      cmtTbl,
    )
  
  and printTypeDeclaration = (
    ~name,
    ~equalSign,
    ~recFlag,
    i,
    td: Parsetree.type_declaration,
    cmtTbl,
  ) => {
    let (hasGenType, attrs) = ParsetreeViewer.splitGenTypeAttr(
      td.ptype_attributes,
    )
    let attrs = printAttributes(~loc=td.ptype_loc, attrs)
    let prefix = if i > 0 {
      Doc.concat(list[
        Doc.text("and "),
        if hasGenType {
          Doc.text("export ")
        } else {
          Doc.nil
        },
      ])
    } else {
      Doc.concat(list[
        Doc.text(
          if hasGenType {
            "export type "
          } else {
            "type "
          },
        ),
        recFlag,
      ])
    }
    
    let typeName = name
    let typeParams = switch td.ptype_params {
    | list[] => Doc.nil
    | typeParams =>
      Doc.group(
        Doc.concat(list[
          Doc.lessThan,
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                List.map(tp => printTypeParam(tp, cmtTbl), typeParams),
              ),
            ]),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.greaterThan,
        ]),
      )
    }
    
    let manifestAndKind = switch td.ptype_kind {
    | Ptype_abstract =>
      switch td.ptype_manifest {
      | None => Doc.nil
      | Some(typ) =>
        Doc.concat(list[
          Doc.concat(list[Doc.space, Doc.text(equalSign), Doc.space]),
          printPrivateFlag(td.ptype_private),
          printTypExpr(typ, cmtTbl),
        ])
      }
    | Ptype_open =>
      Doc.concat(list[
        Doc.concat(list[Doc.space, Doc.text(equalSign), Doc.space]),
        printPrivateFlag(td.ptype_private),
        Doc.text(".."),
      ])
    | Ptype_record(lds) =>
      let manifest = switch td.ptype_manifest {
      | None => Doc.nil
      | Some(typ) =>
        Doc.concat(list[
          Doc.concat(list[Doc.space, Doc.text(equalSign), Doc.space]),
          printTypExpr(typ, cmtTbl),
        ])
      }
      
      Doc.concat(list[
        manifest,
        Doc.concat(list[Doc.space, Doc.text(equalSign), Doc.space]),
        printPrivateFlag(td.ptype_private),
        printRecordDeclaration(lds, cmtTbl),
      ])
    | Ptype_variant(cds) =>
      let manifest = switch td.ptype_manifest {
      | None => Doc.nil
      | Some(typ) =>
        Doc.concat(list[
          Doc.concat(list[Doc.space, Doc.text(equalSign), Doc.space]),
          printTypExpr(typ, cmtTbl),
        ])
      }
      
      Doc.concat(list[
        manifest,
        Doc.concat(list[Doc.space, Doc.text(equalSign)]),
        printConstructorDeclarations(
          ~privateFlag=td.ptype_private,
          cds,
          cmtTbl,
        ),
      ])
    }
    
    let constraints = printTypeDefinitionConstraints(td.ptype_cstrs)
    Doc.group(
      Doc.concat(list[
        attrs,
        prefix,
        typeName,
        typeParams,
        manifestAndKind,
        constraints,
      ]),
    )
  }
  
  and printTypeDeclaration2 = (
    ~recFlag,
    td: Parsetree.type_declaration,
    cmtTbl,
    i,
  ) => {
    let name = {
      let doc = Doc.text(td.Parsetree.ptype_name.txt)
      printComments(doc, cmtTbl, td.ptype_name.loc)
    }
    
    let equalSign = "="
    let (hasGenType, attrs) = ParsetreeViewer.splitGenTypeAttr(
      td.ptype_attributes,
    )
    let attrs = printAttributes(~loc=td.ptype_loc, attrs)
    let prefix = if i > 0 {
      Doc.concat(list[
        Doc.text("and "),
        if hasGenType {
          Doc.text("export ")
        } else {
          Doc.nil
        },
      ])
    } else {
      Doc.concat(list[
        Doc.text(
          if hasGenType {
            "export type "
          } else {
            "type "
          },
        ),
        recFlag,
      ])
    }
    
    let typeName = name
    let typeParams = switch td.ptype_params {
    | list[] => Doc.nil
    | typeParams =>
      Doc.group(
        Doc.concat(list[
          Doc.lessThan,
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                List.map(
                  typeParam => printTypeParam(typeParam, cmtTbl),
                  typeParams,
                ),
              ),
            ]),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.greaterThan,
        ]),
      )
    }
    
    let manifestAndKind = switch td.ptype_kind {
    | Ptype_abstract =>
      switch td.ptype_manifest {
      | None => Doc.nil
      | Some(typ) =>
        Doc.concat(list[
          Doc.concat(list[Doc.space, Doc.text(equalSign), Doc.space]),
          printPrivateFlag(td.ptype_private),
          printTypExpr(typ, cmtTbl),
        ])
      }
    | Ptype_open =>
      Doc.concat(list[
        Doc.concat(list[Doc.space, Doc.text(equalSign), Doc.space]),
        printPrivateFlag(td.ptype_private),
        Doc.text(".."),
      ])
    | Ptype_record(lds) =>
      let manifest = switch td.ptype_manifest {
      | None => Doc.nil
      | Some(typ) =>
        Doc.concat(list[
          Doc.concat(list[Doc.space, Doc.text(equalSign), Doc.space]),
          printTypExpr(typ, cmtTbl),
        ])
      }
      
      Doc.concat(list[
        manifest,
        Doc.concat(list[Doc.space, Doc.text(equalSign), Doc.space]),
        printPrivateFlag(td.ptype_private),
        printRecordDeclaration(lds, cmtTbl),
      ])
    | Ptype_variant(cds) =>
      let manifest = switch td.ptype_manifest {
      | None => Doc.nil
      | Some(typ) =>
        Doc.concat(list[
          Doc.concat(list[Doc.space, Doc.text(equalSign), Doc.space]),
          printTypExpr(typ, cmtTbl),
        ])
      }
      
      Doc.concat(list[
        manifest,
        Doc.concat(list[Doc.space, Doc.text(equalSign)]),
        printConstructorDeclarations(
          ~privateFlag=td.ptype_private,
          cds,
          cmtTbl,
        ),
      ])
    }
    
    let constraints = printTypeDefinitionConstraints(td.ptype_cstrs)
    Doc.group(
      Doc.concat(list[
        attrs,
        prefix,
        typeName,
        typeParams,
        manifestAndKind,
        constraints,
      ]),
    )
  }
  
  and printTypeDefinitionConstraints = cstrs =>
    switch cstrs {
    | list[] => Doc.nil
    | cstrs =>
      Doc.indent(
        Doc.group(
          Doc.concat(list[
            Doc.line,
            Doc.group(
              Doc.join(
                ~sep=Doc.line,
                List.map(printTypeDefinitionConstraint, cstrs),
              ),
            ),
          ]),
        ),
      )
    }
  
  and printTypeDefinitionConstraint = (
    (typ1, typ2, _loc): (Parsetree.core_type, Parsetree.core_type, Location.t),
  ) =>
    Doc.concat(list[
      Doc.text("constraint "),
      printTypExpr(typ1, CommentTable.empty),
      Doc.text(" = "),
      printTypExpr(typ2, CommentTable.empty),
    ])
  
  and printPrivateFlag = (flag: Asttypes.private_flag) =>
    switch flag {
    | Private => Doc.text("private ")
    | Public => Doc.nil
    }
  
  and printTypeParam = (
    param: (Parsetree.core_type, Asttypes.variance),
    cmtTbl,
  ) => {
    let (typ, variance) = param
    let printedVariance = switch variance {
    | Covariant => Doc.text("+")
    | Contravariant => Doc.text("-")
    | Invariant => Doc.nil
    }
    
    Doc.concat(list[printedVariance, printTypExpr(typ, cmtTbl)])
  }
  
  and printRecordDeclaration = (
    lds: list<Parsetree.label_declaration>,
    cmtTbl,
  ) => {
    let forceBreak = switch (lds, List.rev(lds)) {
    | (list[first, ..._], list[last, ..._]) =>
      first.pld_loc.loc_start.pos_lnum < last.pld_loc.loc_end.pos_lnum
    | _ => false
    }
    
    Doc.breakableGroup(
      ~forceBreak,
      Doc.concat(list[
        Doc.lbrace,
        Doc.indent(
          Doc.concat(list[
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list[Doc.comma, Doc.line]),
              List.map(
                ld => {
                  let doc = printLabelDeclaration(ld, cmtTbl)
                  printComments(doc, cmtTbl, ld.Parsetree.pld_loc)
                },
                lds,
              ),
            ),
          ]),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rbrace,
      ]),
    )
  }
  
  and printConstructorDeclarations = (
    ~privateFlag,
    cds: list<Parsetree.constructor_declaration>,
    cmtTbl,
  ) => {
    let forceBreak = switch (cds, List.rev(cds)) {
    | (list[first, ..._], list[last, ..._]) =>
      first.pcd_loc.loc_start.pos_lnum < last.pcd_loc.loc_end.pos_lnum
    | _ => false
    }
    
    let privateFlag = switch privateFlag {
    | Asttypes.Private => Doc.concat(list[Doc.text("private"), Doc.line])
    | Public => Doc.nil
    }
    
    let rows = printListi(
      ~getLoc=cd => cd.Parsetree.pcd_loc,
      ~nodes=cds,
      ~print=(cd, cmtTbl, i) => {
        let doc = printConstructorDeclaration2(i, cd, cmtTbl)
        printComments(doc, cmtTbl, cd.Parsetree.pcd_loc)
      },
      ~forceBreak,
      cmtTbl,
    )
    
    Doc.breakableGroup(
      ~forceBreak,
      Doc.indent(Doc.concat(list[Doc.line, privateFlag, rows])),
    )
  }
  
  and printConstructorDeclaration2 = (
    i,
    cd: Parsetree.constructor_declaration,
    cmtTbl,
  ) => {
    let attrs = printAttributes(cd.pcd_attributes)
    let bar = if i > 0 {
      Doc.text("| ")
    } else {
      Doc.ifBreaks(Doc.text("| "), Doc.nil)
    }
    
    let constrName = {
      let doc = Doc.text(cd.pcd_name.txt)
      printComments(doc, cmtTbl, cd.pcd_name.loc)
    }
    
    let constrArgs = printConstructorArguments(
      ~indent=true,
      cd.pcd_args,
      cmtTbl,
    )
    let gadt = switch cd.pcd_res {
    | None => Doc.nil
    | Some(typ) =>
      Doc.indent(Doc.concat(list[Doc.text(": "), printTypExpr(typ, cmtTbl)]))
    }
    
    Doc.concat(list[
      bar,
      Doc.group(Doc.concat(list[attrs, constrName, constrArgs, gadt])),
    ])
  }
  
  and printConstructorArguments = (
    ~indent,
    cdArgs: Parsetree.constructor_arguments,
    cmtTbl,
  ) =>
    switch cdArgs {
    | Pcstr_tuple(list[]) => Doc.nil
    | Pcstr_tuple(types) =>
      let args = Doc.concat(list[
        Doc.lparen,
        Doc.indent(
          Doc.concat(list[
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list[Doc.comma, Doc.line]),
              List.map(typexpr => printTypExpr(typexpr, cmtTbl), types),
            ),
          ]),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rparen,
      ])
      Doc.group(
        if indent {
          Doc.indent(args)
        } else {
          args
        },
      )
    | Pcstr_record(lds) =>
      let args = Doc.concat(list[
        Doc.lparen,
        Doc.lbrace,
        Doc.indent(
          Doc.concat(list[
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list[Doc.comma, Doc.line]),
              List.map(
                ld => {
                  let doc = printLabelDeclaration(ld, cmtTbl)
                  printComments(doc, cmtTbl, ld.Parsetree.pld_loc)
                },
                lds,
              ),
            ),
          ]),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rbrace,
        Doc.rparen,
      ])
      if indent {
        Doc.indent(args)
      } else {
        args
      }
    }
  
  and printLabelDeclaration = (ld: Parsetree.label_declaration, cmtTbl) => {
    let attrs = printAttributes(~loc=ld.pld_name.loc, ld.pld_attributes)
    let mutableFlag = switch ld.pld_mutable {
    | Mutable => Doc.text("mutable ")
    | Immutable => Doc.nil
    }
    
    let name = {
      let doc = Doc.text(ld.pld_name.txt)
      printComments(doc, cmtTbl, ld.pld_name.loc)
    }
    
    Doc.group(
      Doc.concat(list[
        attrs,
        mutableFlag,
        name,
        Doc.text(": "),
        printTypExpr(ld.pld_type, cmtTbl),
      ]),
    )
  }
  
  and printTypExpr = (typExpr: Parsetree.core_type, cmtTbl) => {
    let renderedType = switch typExpr.ptyp_desc {
    | Ptyp_any => Doc.text("_")
    | Ptyp_var(var) => Doc.text("'" ++ var)
    | Ptyp_extension(extension) => printExtensionWithComments(extension, cmtTbl)
    | Ptyp_alias(typ, alias) =>
      let typ = {
        let needsParens = switch typ.ptyp_desc {
        | Ptyp_arrow(_) => true
        | _ => false
        }
        
        let doc = printTypExpr(typ, cmtTbl)
        if needsParens {
          Doc.concat(list[Doc.lparen, doc, Doc.rparen])
        } else {
          doc
        }
      }
      
      Doc.concat(list[typ, Doc.text(" as "), Doc.text("'" ++ alias)])
    | Ptyp_constr(
        {txt: Longident.Ldot(Longident.Lident("Js"), "t")},
        list[typ],
      ) =>
      let bsObject = printTypExpr(typ, cmtTbl)
      switch typExpr.ptyp_attributes {
      | list[] => bsObject
      | attrs =>
        Doc.concat(list[
          Doc.group(Doc.join(~sep=Doc.line, List.map(printAttribute, attrs))),
          Doc.space,
          printTypExpr(typ, cmtTbl),
        ])
      }
    | Ptyp_constr(
        longidentLoc,
        list[{ptyp_desc: Parsetree.Ptyp_tuple(tuple)}],
      ) =>
      let constrName = printLongidentLocation(longidentLoc, cmtTbl)
      Doc.group(
        Doc.concat(list[
          constrName,
          Doc.lessThan,
          printTupleType(~inline=true, tuple, cmtTbl),
          Doc.greaterThan,
        ]),
      )
    | Ptyp_constr(longidentLoc, constrArgs) =>
      let constrName = printLongidentLocation(longidentLoc, cmtTbl)
      switch constrArgs {
      | list[] => constrName
      | list[{
          Parsetree.ptyp_desc: 
            Ptyp_constr(
              {txt: Longident.Ldot(Longident.Lident("Js"), "t")},
              list[{ptyp_desc: Ptyp_object(fields, openFlag)}],
            ),
        }] =>
        Doc.concat(list[
          constrName,
          Doc.lessThan,
          printBsObjectSugar(~inline=true, fields, openFlag, cmtTbl),
          Doc.greaterThan,
        ])
      | args =>
        Doc.group(
          Doc.concat(list[
            constrName,
            Doc.lessThan,
            Doc.indent(
              Doc.concat(list[
                Doc.softLine,
                Doc.join(
                  ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                  List.map(
                    typexpr => printTypExpr(typexpr, cmtTbl),
                    constrArgs,
                  ),
                ),
              ]),
            ),
            Doc.trailingComma,
            Doc.softLine,
            Doc.greaterThan,
          ]),
        )
      }
    | Ptyp_arrow(_) =>
      let (attrsBefore, args, returnType) = ParsetreeViewer.arrowType(typExpr)
      let returnTypeNeedsParens = switch returnType.ptyp_desc {
      | Ptyp_alias(_) => true
      | _ => false
      }
      
      let returnDoc = {
        let doc = printTypExpr(returnType, cmtTbl)
        if returnTypeNeedsParens {
          Doc.concat(list[Doc.lparen, doc, Doc.rparen])
        } else {
          doc
        }
      }
      
      let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(
        attrsBefore,
      )
      
      switch args {
      | list[] => Doc.nil
      | list[(list[], Nolabel, n)] when !isUncurried =>
        let hasAttrsBefore = !(attrs == list[])
        let attrs = if hasAttrsBefore {
          Doc.concat(list[
            Doc.join(~sep=Doc.line, List.map(printAttribute, attrsBefore)),
            Doc.space,
          ])
        } else {
          Doc.nil
        }
        
        Doc.group(
          Doc.concat(list[
            Doc.group(attrs),
            Doc.group(
              if hasAttrsBefore {
                Doc.concat(list[
                  Doc.lparen,
                  Doc.indent(
                    Doc.concat(list[
                      Doc.softLine,
                      printTypExpr(n, cmtTbl),
                      Doc.text(" => "),
                      returnDoc,
                    ]),
                  ),
                  Doc.softLine,
                  Doc.rparen,
                ])
              } else {
                Doc.concat(list[
                  printTypExpr(n, cmtTbl),
                  Doc.text(" => "),
                  returnDoc,
                ])
              },
            ),
          ]),
        )
      | args =>
        let attrs = switch attrs {
        | list[] => Doc.nil
        | attrs =>
          Doc.concat(list[
            Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
            Doc.space,
          ])
        }
        
        let renderedArgs = Doc.concat(list[
          attrs,
          Doc.text("("),
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              if isUncurried {
                Doc.concat(list[Doc.dot, Doc.space])
              } else {
                Doc.nil
              },
              Doc.join(
                ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                List.map(tp => printTypeParameter(tp, cmtTbl), args),
              ),
            ]),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.text(")"),
        ])
        Doc.group(Doc.concat(list[renderedArgs, Doc.text(" => "), returnDoc]))
      }
    | Ptyp_tuple(types) => printTupleType(~inline=false, types, cmtTbl)
    | Ptyp_object(fields, openFlag) =>
      printBsObjectSugar(~inline=false, fields, openFlag, cmtTbl)
    | Ptyp_poly(stringLocs, typ) =>
      Doc.concat(list[
        Doc.join(
          ~sep=Doc.space,
          List.map(
            ({Location.txt: txt, loc}) => {
              let doc = Doc.text("'" ++ txt)
              printComments(doc, cmtTbl, loc)
            },
            stringLocs,
          ),
        ),
        Doc.dot,
        Doc.space,
        printTypExpr(typ, cmtTbl),
      ])
    | Ptyp_package(packageType) =>
      printPackageType(~printModuleKeywordAndParens=true, packageType, cmtTbl)
    | Ptyp_class(_) => failwith("classes are not supported in types")
    | Ptyp_variant(_) =>
      failwith("Polymorphic variants currently not supported")
    }
    
    let shouldPrintItsOwnAttributes = switch typExpr.ptyp_desc {
    | Ptyp_arrow(_)
      | Ptyp_constr({txt: Longident.Ldot(Longident.Lident("Js"), "t")}, _) =>
      true
    | _ => false
    }
    
    let doc = switch typExpr.ptyp_attributes {
    | list[_, ..._] as attrs when !shouldPrintItsOwnAttributes =>
      Doc.group(Doc.concat(list[printAttributes(attrs), renderedType]))
    | _ => renderedType
    }
    
    printComments(doc, cmtTbl, typExpr.ptyp_loc)
  }
  
  and printBsObjectSugar = (~inline, fields, openFlag, cmtTbl) => {
    let flag = switch openFlag {
    | Asttypes.Closed => Doc.nil
    | Open => Doc.dotdot
    }
    
    let doc = Doc.concat(list[
      Doc.lbrace,
      flag,
      Doc.indent(
        Doc.concat(list[
          Doc.softLine,
          Doc.join(
            ~sep=Doc.concat(list[Doc.comma, Doc.line]),
            List.map(field => printObjectField(field, cmtTbl), fields),
          ),
        ]),
      ),
      Doc.trailingComma,
      Doc.softLine,
      Doc.rbrace,
    ])
    if inline {
      doc
    } else {
      Doc.group(doc)
    }
  }
  
  and printTupleType = (~inline, types: list<Parsetree.core_type>, cmtTbl) => {
    let tuple = Doc.concat(list[
      Doc.lparen,
      Doc.indent(
        Doc.concat(list[
          Doc.softLine,
          Doc.join(
            ~sep=Doc.concat(list[Doc.comma, Doc.line]),
            List.map(typexpr => printTypExpr(typexpr, cmtTbl), types),
          ),
        ]),
      ),
      Doc.trailingComma,
      Doc.softLine,
      Doc.rparen,
    ])
    
    if inline === false {
      Doc.group(tuple)
    } else {
      tuple
    }
  }
  
  and printObjectField = (field: Parsetree.object_field, cmtTbl) =>
    switch field {
    | Otag(labelLoc, attrs, typ) =>
      let lbl = {
        let doc = Doc.text("\"" ++ labelLoc.txt ++ "\"")
        printComments(doc, cmtTbl, labelLoc.loc)
      }
      
      let doc = Doc.concat(list[lbl, Doc.text(": "), printTypExpr(typ, cmtTbl)])
      let cmtLoc = {...labelLoc.loc, loc_end: typ.ptyp_loc.loc_end}
      printComments(doc, cmtTbl, cmtLoc)
    | _ => Doc.nil
    }
  
  and printTypeParameter = ((attrs, lbl, typ), cmtTbl) => {
    let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(attrs)
    let uncurried = if isUncurried {
      Doc.concat(list[Doc.dot, Doc.space])
    } else {
      Doc.nil
    }
    let attrs = switch attrs {
    | list[] => Doc.nil
    | attrs =>
      Doc.concat(list[
        Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
        Doc.line,
      ])
    }
    let label = switch lbl {
    | Asttypes.Nolabel => Doc.nil
    | Labelled(lbl) => Doc.text("~" ++ lbl ++ ": ")
    | Optional(lbl) => Doc.text("~" ++ lbl ++ ": ")
    }
    
    let optionalIndicator = switch lbl {
    | Asttypes.Nolabel | Labelled(_) => Doc.nil
    | Optional(lbl) => Doc.text("=?")
    }
    
    let doc = Doc.group(
      Doc.concat(list[
        uncurried,
        attrs,
        label,
        printTypExpr(typ, cmtTbl),
        optionalIndicator,
      ]),
    )
    printComments(doc, cmtTbl, typ.ptyp_loc)
  }
  
  and printValueBinding = (~recFlag, vb, cmtTbl, i) => {
    let (hasGenType, attrs) = ParsetreeViewer.splitGenTypeAttr(
      vb.pvb_attributes,
    )
    let attrs = printAttributes(~loc=vb.pvb_pat.ppat_loc, attrs)
    let isGhost = ParsetreeViewer.isGhostUnitBinding(i, vb)
    let header = if isGhost {
      Doc.nil
    } else if i === 0 {
      Doc.concat(list[
        if hasGenType {
          Doc.text("export ")
        } else {
          Doc.text("let ")
        },
        recFlag,
      ])
    } else {
      Doc.concat(list[
        Doc.text("and "),
        if hasGenType {
          Doc.text("export ")
        } else {
          Doc.nil
        },
      ])
    }
    
    let printedExpr = {
      let exprDoc = printExpressionWithComments(vb.pvb_expr, cmtTbl)
      let needsParens = switch vb.pvb_expr.pexp_desc {
      | Pexp_constraint(
          {pexp_desc: Pexp_pack(_)},
          {ptyp_desc: Ptyp_package(_)},
        ) =>
        false
      | Pexp_constraint(_) => true
      | _ => false
      }
      
      if needsParens {
        addParens(exprDoc)
      } else {
        exprDoc
      }
    }
    
    if isGhost {
      printedExpr
    } else {
      let shouldIndent =
        ParsetreeViewer.isBinaryExpression(vb.pvb_expr) ||
        switch vb.pvb_expr {
        | {
            pexp_attributes: list[({Location.txt: "ns.ternary"}, _)],
            pexp_desc: Pexp_ifthenelse(ifExpr, _, _),
          } =>
          ParsetreeViewer.isBinaryExpression(ifExpr) ||
          ParsetreeViewer.hasAttributes(ifExpr.pexp_attributes)
        | {pexp_desc: Pexp_newtype(_)} => false
        | e =>
          ParsetreeViewer.hasAttributes(e.pexp_attributes) ||
          ParsetreeViewer.isArrayAccess(e)
        }
      
      Doc.group(
        Doc.concat(list[
          attrs,
          header,
          printPattern(vb.pvb_pat, cmtTbl),
          Doc.text(" ="),
          if shouldIndent {
            Doc.indent(Doc.concat(list[Doc.line, printedExpr]))
          } else {
            Doc.concat(list[Doc.space, printedExpr])
          },
        ]),
      )
    }
  }
  
  and printPackageType = (
    ~printModuleKeywordAndParens,
    packageType: Parsetree.package_type,
    cmtTbl,
  ) => {
    let doc = switch packageType {
    | (longidentLoc, list[]) =>
      Doc.group(Doc.concat(list[printLongidentLocation(longidentLoc, cmtTbl)]))
    | (longidentLoc, packageConstraints) =>
      Doc.group(
        Doc.concat(list[
          printLongidentLocation(longidentLoc, cmtTbl),
          printPackageConstraints(packageConstraints, cmtTbl),
          Doc.softLine,
        ]),
      )
    }
    
    if printModuleKeywordAndParens {
      Doc.concat(list[Doc.text("module("), doc, Doc.rparen])
    } else {
      doc
    }
  }
  
  and printPackageConstraints = (packageConstraints, cmtTbl) =>
    Doc.concat(list[
      Doc.text(" with"),
      Doc.indent(
        Doc.concat(list[
          Doc.line,
          Doc.join(
            ~sep=Doc.line,
            List.mapi(
              (i, pc) => {
                let (longident, typexpr) = pc
                let cmtLoc = {
                  ...longident.Asttypes.loc,
                  loc_end: typexpr.Parsetree.ptyp_loc.loc_end,
                }
                let doc = printPackageConstraint(i, cmtTbl, pc)
                printComments(doc, cmtTbl, cmtLoc)
              },
              packageConstraints,
            ),
          ),
        ]),
      ),
    ])
  
  and printPackageConstraint = (i, cmtTbl, (longidentLoc, typ)) => {
    let prefix = if i === 0 {
      Doc.text("type ")
    } else {
      Doc.text("and type ")
    }
    Doc.concat(list[
      prefix,
      printLongidentLocation(longidentLoc, cmtTbl),
      Doc.text(" = "),
      printTypExpr(typ, cmtTbl),
    ])
  }
  
  and printExtensionWithComments = ((stringLoc, payload), cmtTbl) => {
    let extName = {
      let doc = Doc.text("%" ++ stringLoc.Location.txt)
      printComments(doc, cmtTbl, stringLoc.Location.loc)
    }
    
    switch payload {
    | Parsetree.PStr(list[{pstr_desc: Pstr_eval(expr, attrs)}]) =>
      let exprDoc = printExpressionWithComments(expr, cmtTbl)
      let needsParens = switch attrs {
      | list[] => false
      | _ => true
      }
      Doc.group(
        Doc.concat(list[
          extName,
          addParens(
            Doc.concat(list[
              printAttributes(attrs),
              if needsParens {
                addParens(exprDoc)
              } else {
                exprDoc
              },
            ]),
          ),
        ]),
      )
    | _ => extName
    }
  }
  
  and printPattern = (p: Parsetree.pattern, cmtTbl) => {
    let patternWithoutAttributes = switch p.ppat_desc {
    | Ppat_any => Doc.text("_")
    | Ppat_var(stringLoc) => Doc.text(stringLoc.txt)
    | Ppat_constant(c) => printConstant(c)
    | Ppat_tuple(patterns) =>
      Doc.group(
        Doc.concat(list[
          Doc.lparen,
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.text(","), Doc.line]),
                List.map(pat => printPattern(pat, cmtTbl), patterns),
              ),
            ]),
          ),
          Doc.ifBreaks(Doc.text(","), Doc.nil),
          Doc.softLine,
          Doc.rparen,
        ]),
      )
    | Ppat_array(list[]) =>
      Doc.concat(list[
        Doc.lbracket,
        printCommentsInside(cmtTbl, p.ppat_loc),
        Doc.rbracket,
      ])
    | Ppat_array(patterns) =>
      Doc.group(
        Doc.concat(list[
          Doc.text("["),
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.text(","), Doc.line]),
                List.map(pat => printPattern(pat, cmtTbl), patterns),
              ),
            ]),
          ),
          Doc.ifBreaks(Doc.text(","), Doc.nil),
          Doc.softLine,
          Doc.text("]"),
        ]),
      )
    | Ppat_construct({txt: Longident.Lident("[]")}, _) =>
      Doc.concat(list[
        Doc.text("list["),
        printCommentsInside(cmtTbl, p.ppat_loc),
        Doc.rbracket,
      ])
    | Ppat_construct({txt: Longident.Lident("::")}, _) =>
      let (patterns, tail) = ParsetreeViewer.collectPatternsFromListConstruct(
        list[],
        p,
      )
      let shouldHug = switch (patterns, tail) {
      | (
          list[pat],
          {ppat_desc: Ppat_construct({txt: Longident.Lident("[]")}, _)},
        ) when ParsetreeViewer.isHuggablePattern(pat) =>
        true
      | _ => false
      }
      
      let children = Doc.concat(list[
        if shouldHug {
          Doc.nil
        } else {
          Doc.softLine
        },
        Doc.join(
          ~sep=Doc.concat(list[Doc.text(","), Doc.line]),
          List.map(pat => printPattern(pat, cmtTbl), patterns),
        ),
        switch tail.Parsetree.ppat_desc {
        | Ppat_construct({txt: Longident.Lident("[]")}, _) => Doc.nil
        | _ =>
          let doc = Doc.concat(list[
            Doc.text("..."),
            printPattern(tail, cmtTbl),
          ])
          let tail = printComments(doc, cmtTbl, tail.ppat_loc)
          Doc.concat(list[Doc.text(","), Doc.line, tail])
        },
      ])
      Doc.group(
        Doc.concat(list[
          Doc.text("list["),
          if shouldHug {
            children
          } else {
            Doc.concat(list[
              Doc.indent(children),
              Doc.ifBreaks(Doc.text(","), Doc.nil),
              Doc.softLine,
            ])
          },
          Doc.rbracket,
        ]),
      )
    | Ppat_construct(constrName, constructorArgs) =>
      let constrName = printLongident(constrName.txt)
      switch constructorArgs {
      | None => constrName
      | Some({ppat_desc: Ppat_tuple(list[]), ppat_loc: loc}) =>
        Doc.group(
          Doc.concat(list[
            constrName,
            Doc.lparen,
            Doc.softLine,
            printCommentsInside(cmtTbl, loc),
            Doc.rparen,
          ]),
        )
      | Some(args) =>
        let args = switch args.ppat_desc {
        | Ppat_construct({txt: Longident.Lident("()")}, None) => list[Doc.nil]
        | Ppat_tuple(patterns) =>
          List.map(pat => printPattern(pat, cmtTbl), patterns)
        | _ => list[printPattern(args, cmtTbl)]
        }
        
        Doc.group(
          Doc.concat(list[
            constrName,
            Doc.text("("),
            Doc.indent(
              Doc.concat(list[
                Doc.softLine,
                Doc.join(~sep=Doc.concat(list[Doc.text(","), Doc.line]), args),
              ]),
            ),
            Doc.ifBreaks(Doc.text(","), Doc.nil),
            Doc.softLine,
            Doc.text(")"),
          ]),
        )
      }
    | Ppat_record(rows, openFlag) =>
      Doc.group(
        Doc.concat(list[
          Doc.text("{"),
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.text(","), Doc.line]),
                List.map(row => printPatternRecordRow(row, cmtTbl), rows),
              ),
              switch openFlag {
              | Open => Doc.concat(list[Doc.text(","), Doc.line, Doc.text("_")])
              | Closed => Doc.nil
              },
            ]),
          ),
          Doc.ifBreaks(Doc.text(","), Doc.nil),
          Doc.softLine,
          Doc.text("}"),
        ]),
      )
    
    | Ppat_exception(p) =>
      let needsParens = switch p.ppat_desc {
      | Ppat_or(_, _) | Ppat_alias(_, _) => true
      | _ => false
      }
      
      let pat = {
        let p = printPattern(p, cmtTbl)
        if needsParens {
          Doc.concat(list[Doc.text("("), p, Doc.text(")")])
        } else {
          p
        }
      }
      
      Doc.group(Doc.concat(list[Doc.text("exception"), Doc.line, pat]))
    | Ppat_or(p1, p2) =>
      let p1 = {
        let p = printPattern(p1, cmtTbl)
        switch p1.ppat_desc {
        | Ppat_or(_, _) => Doc.concat(list[Doc.text("("), p, Doc.text(")")])
        | _ => p
        }
      }
      
      let p2 = {
        let p = printPattern(p2, cmtTbl)
        switch p2.ppat_desc {
        | Ppat_or(_, _) => Doc.concat(list[Doc.text("("), p, Doc.text(")")])
        | _ => p
        }
      }
      
      Doc.group(Doc.concat(list[p1, Doc.line, Doc.text("| "), p2]))
    | Ppat_extension(ext) => printExtensionWithComments(ext, cmtTbl)
    | Ppat_lazy(p) =>
      let needsParens = switch p.ppat_desc {
      | Ppat_or(_, _) | Ppat_alias(_, _) => true
      | _ => false
      }
      
      let pat = {
        let p = printPattern(p, cmtTbl)
        if needsParens {
          Doc.concat(list[Doc.text("("), p, Doc.text(")")])
        } else {
          p
        }
      }
      
      Doc.concat(list[Doc.text("lazy "), pat])
    | Ppat_alias(p, aliasLoc) =>
      let needsParens = switch p.ppat_desc {
      | Ppat_or(_, _) | Ppat_alias(_, _) => true
      | _ => false
      }
      
      let renderedPattern = {
        let p = printPattern(p, cmtTbl)
        if needsParens {
          Doc.concat(list[Doc.text("("), p, Doc.text(")")])
        } else {
          p
        }
      }
      
      Doc.concat(list[
        renderedPattern,
        Doc.text(" as "),
        printStringLoc(aliasLoc, cmtTbl),
      ])
    
    | Ppat_constraint(
        {ppat_desc: Ppat_unpack(stringLoc)},
        {ptyp_desc: Ptyp_package(packageType), ptyp_loc},
      ) =>
      Doc.concat(list[
        Doc.text("module("),
        printComments(Doc.text(stringLoc.txt), cmtTbl, stringLoc.loc),
        Doc.text(": "),
        printComments(
          printPackageType(
            ~printModuleKeywordAndParens=false,
            packageType,
            cmtTbl,
          ),
          cmtTbl,
          ptyp_loc,
        ),
        Doc.rparen,
      ])
    | Ppat_constraint(pattern, typ) =>
      Doc.concat(list[
        printPattern(pattern, cmtTbl),
        Doc.text(": "),
        printTypExpr(typ, cmtTbl),
      ])
    
    | Ppat_unpack(stringLoc) =>
      Doc.concat(list[
        Doc.text("module("),
        printComments(Doc.text(stringLoc.txt), cmtTbl, stringLoc.loc),
        Doc.rparen,
      ])
    | ((Ppat_open(_) | Ppat_interval(_, _)) | Ppat_variant(_, _))
      | Ppat_type(_) =>
      Doc.nil
    }
    
    let doc = switch p.ppat_attributes {
    | list[] => patternWithoutAttributes
    | attrs =>
      Doc.group(
        Doc.concat(list[printAttributes(attrs), patternWithoutAttributes]),
      )
    }
    
    printComments(doc, cmtTbl, p.ppat_loc)
  }
  
  and printPatternRecordRow = (row, cmtTbl) =>
    switch row {
    | (
        {Location.txt: Longident.Lident(ident)} as longident,
        {Parsetree.ppat_desc: Ppat_var({txt, _})},
      ) when ident == txt =>
      printLongidentLocation(longident, cmtTbl)
    | (longident, pattern) =>
      let locForComments = {
        ...longident.loc,
        loc_end: pattern.Parsetree.ppat_loc.loc_end,
      }
      let doc = Doc.group(
        Doc.concat(list[
          printLongidentLocation(longident, cmtTbl),
          Doc.text(": "),
          Doc.indent(
            Doc.concat(list[Doc.softLine, printPattern(pattern, cmtTbl)]),
          ),
        ]),
      )
      printComments(doc, cmtTbl, locForComments)
    }
  
  and printExpressionWithComments = (expr, cmtTbl) => {
    let doc = printExpression(expr, cmtTbl)
    printComments(doc, cmtTbl, expr.Parsetree.pexp_loc)
  }
  
  and printExpression = (e: Parsetree.expression, cmtTbl) => {
    let printedExpression = switch e.pexp_desc {
    | Parsetree.Pexp_constant(c) => printConstant(c)
    | Pexp_construct(_)
      when ParsetreeViewer.hasJsxAttribute(e.pexp_attributes) =>
      printJsxFragment(e, cmtTbl)
    | Pexp_construct({txt: Longident.Lident("()")}, _) => Doc.text("()")
    | Pexp_construct({txt: Longident.Lident("[]")}, _) =>
      Doc.concat(list[
        Doc.text("list["),
        printCommentsInside(cmtTbl, e.pexp_loc),
        Doc.rbracket,
      ])
    | Pexp_construct({txt: Longident.Lident("::")}, _) =>
      let (expressions, spread) = ParsetreeViewer.collectListExpressions(e)
      let spreadDoc = switch spread {
      | Some(expr) =>
        Doc.concat(list[
          Doc.text(","),
          Doc.line,
          Doc.dotdotdot,
          printExpressionWithComments(expr, cmtTbl),
        ])
      | None => Doc.nil
      }
      
      Doc.group(
        Doc.concat(list[
          Doc.text("list["),
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.text(","), Doc.line]),
                List.map(
                  expr => printExpressionWithComments(expr, cmtTbl),
                  expressions,
                ),
              ),
              spreadDoc,
            ]),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rbracket,
        ]),
      )
    | Pexp_construct(longidentLoc, args) =>
      let constr = printLongidentLocation(longidentLoc, cmtTbl)
      let args = switch args {
      | None => Doc.nil
      | Some({pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, _)}) =>
        Doc.text("()")
      | Some({pexp_desc: Pexp_tuple(args)}) =>
        Doc.concat(list[
          Doc.lparen,
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                List.map(
                  expr => printExpressionWithComments(expr, cmtTbl),
                  args,
                ),
              ),
            ]),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        ])
      | Some(arg) =>
        let argDoc = printExpressionWithComments(arg, cmtTbl)
        let shouldHug = ParsetreeViewer.isHuggableExpression(arg)
        Doc.concat(list[
          Doc.lparen,
          if shouldHug {
            argDoc
          } else {
            Doc.concat(list[
              Doc.indent(Doc.concat(list[Doc.softLine, argDoc])),
              Doc.trailingComma,
              Doc.softLine,
            ])
          },
          Doc.rparen,
        ])
      }
      
      Doc.group(Doc.concat(list[constr, args]))
    | Pexp_ident(longident) => printLongidentLocation(longident, cmtTbl)
    | Pexp_tuple(exprs) =>
      Doc.group(
        Doc.concat(list[
          Doc.lparen,
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.text(","), Doc.line]),
                List.map(
                  expr => printExpressionWithComments(expr, cmtTbl),
                  exprs,
                ),
              ),
            ]),
          ),
          Doc.ifBreaks(Doc.text(","), Doc.nil),
          Doc.softLine,
          Doc.rparen,
        ]),
      )
    | Pexp_array(list[]) =>
      Doc.concat(list[
        Doc.lbracket,
        printCommentsInside(cmtTbl, e.pexp_loc),
        Doc.rbracket,
      ])
    | Pexp_array(exprs) =>
      Doc.group(
        Doc.concat(list[
          Doc.lbracket,
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.text(","), Doc.line]),
                List.map(
                  expr => printExpressionWithComments(expr, cmtTbl),
                  exprs,
                ),
              ),
            ]),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rbracket,
        ]),
      )
    | Pexp_record(rows, spreadExpr) =>
      let spread = switch spreadExpr {
      | None => Doc.nil
      | Some(expr) =>
        Doc.concat(list[
          Doc.dotdotdot,
          printExpression(expr, cmtTbl),
          Doc.comma,
          Doc.line,
        ])
      }
      
      let forceBreak =
        e.pexp_loc.loc_start.pos_lnum < e.pexp_loc.loc_end.pos_lnum
      
      Doc.breakableGroup(
        ~forceBreak,
        Doc.concat(list[
          Doc.lbrace,
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              spread,
              Doc.join(
                ~sep=Doc.concat(list[Doc.text(","), Doc.line]),
                List.map(row => printRecordRow(row, cmtTbl), rows),
              ),
            ]),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rbrace,
        ]),
      )
    | Pexp_extension(extension) =>
      switch extension {
      | (
          {txt: "bs.obj"},
          PStr(
            list[{
              pstr_loc: loc,
              pstr_desc: Pstr_eval({pexp_desc: Pexp_record(rows, _)}, list[]),
            }],
          ),
        ) =>
        let forceBreak = loc.loc_start.pos_lnum < loc.loc_end.pos_lnum
        
        Doc.breakableGroup(
          ~forceBreak,
          Doc.concat(list[
            Doc.lbrace,
            Doc.indent(
              Doc.concat(list[
                Doc.softLine,
                Doc.join(
                  ~sep=Doc.concat(list[Doc.text(","), Doc.line]),
                  List.map(row => printBsObjectRow(row, cmtTbl), rows),
                ),
              ]),
            ),
            Doc.trailingComma,
            Doc.softLine,
            Doc.rbrace,
          ]),
        )
      | extension => printExtensionWithComments(extension, cmtTbl)
      }
    | Pexp_apply(_) =>
      if ParsetreeViewer.isUnaryExpression(e) {
        printUnaryExpression(e, cmtTbl)
      } else if ParsetreeViewer.isBinaryExpression(e) {
        printBinaryExpression(e, cmtTbl)
      } else {
        printPexpApply(e, cmtTbl)
      }
    | Pexp_unreachable => Doc.dot
    | Pexp_field(expr, longidentLoc) =>
      let lhs = {
        let doc = printExpressionWithComments(expr, cmtTbl)
        if Parens.fieldExpr(expr) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.concat(list[
        lhs,
        Doc.dot,
        printLongidentLocation(longidentLoc, cmtTbl),
      ])
    | Pexp_setfield(expr1, longidentLoc, expr2) =>
      printSetFieldExpr(
        e.pexp_attributes,
        expr1,
        longidentLoc,
        expr2,
        e.pexp_loc,
        cmtTbl,
      )
    | Pexp_ifthenelse(ifExpr, thenExpr, elseExpr) =>
      if ParsetreeViewer.isTernaryExpr(e) {
        let (parts, alternate) = ParsetreeViewer.collectTernaryParts(e)
        let ternaryDoc = switch parts {
        | list[(condition1, consequent1), ...rest] =>
          Doc.group(
            Doc.concat(list[
              printTernaryOperand(condition1, cmtTbl),
              Doc.indent(
                Doc.concat(list[
                  Doc.line,
                  Doc.indent(
                    Doc.concat(list[
                      Doc.text("? "),
                      printTernaryOperand(consequent1, cmtTbl),
                    ]),
                  ),
                  Doc.concat(
                    List.map(
                      ((condition, consequent)) =>
                        Doc.concat(list[
                          Doc.line,
                          Doc.text(": "),
                          printTernaryOperand(condition, cmtTbl),
                          Doc.line,
                          Doc.text("? "),
                          printTernaryOperand(consequent, cmtTbl),
                        ]),
                      rest,
                    ),
                  ),
                  Doc.line,
                  Doc.text(": "),
                  Doc.indent(printTernaryOperand(alternate, cmtTbl)),
                ]),
              ),
            ]),
          )
        | _ => Doc.nil
        }
        
        let attrs = ParsetreeViewer.filterTernaryAttributes(e.pexp_attributes)
        let needsParens = switch attrs {
        | list[] => false
        | _ => true
        }
        Doc.concat(list[
          printAttributes(attrs),
          if needsParens {
            addParens(ternaryDoc)
          } else {
            ternaryDoc
          },
        ])
      } else {
        let (ifs, elseExpr) = ParsetreeViewer.collectIfExpressions(e)
        let ifDocs = Doc.join(
          ~sep=Doc.space,
          List.mapi(
            (i, (ifExpr, thenExpr)) => {
              let ifTxt = if i > 0 {
                Doc.text("else if ")
              } else {
                Doc.text("if ")
              }
              let condition = if ParsetreeViewer.isBlockExpr(ifExpr) {
                printExpressionBlock(~braces=true, ifExpr, cmtTbl)
              } else {
                let doc = printExpressionWithComments(ifExpr, cmtTbl)
                Doc.group(Doc.ifBreaks(addParens(doc), doc))
              }
              
              Doc.concat(list[
                ifTxt,
                condition,
                Doc.space,
                printExpressionBlock(~braces=true, thenExpr, cmtTbl),
              ])
            },
            ifs,
          ),
        )
        let elseDoc = switch elseExpr {
        | None => Doc.nil
        | Some(expr) =>
          Doc.concat(list[
            Doc.text(" else "),
            printExpressionBlock(~braces=true, expr, cmtTbl),
          ])
        }
        
        Doc.concat(list[printAttributes(e.pexp_attributes), ifDocs, elseDoc])
      }
    | Pexp_while(expr1, expr2) =>
      let condition = printExpressionWithComments(expr1, cmtTbl)
      Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list[
          Doc.text("while "),
          if ParsetreeViewer.isBlockExpr(expr1) {
            condition
          } else {
            Doc.group(Doc.ifBreaks(addParens(condition), condition))
          },
          Doc.space,
          printExpressionBlock(~braces=true, expr2, cmtTbl),
        ]),
      )
    | Pexp_for(pattern, fromExpr, toExpr, directionFlag, body) =>
      Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list[
          Doc.text("for "),
          printPattern(pattern, cmtTbl),
          Doc.text(" in "),
          printExpressionWithComments(fromExpr, cmtTbl),
          printDirectionFlag(directionFlag),
          printExpressionWithComments(toExpr, cmtTbl),
          Doc.space,
          printExpressionBlock(~braces=true, body, cmtTbl),
        ]),
      )
    | Pexp_constraint(
        {pexp_desc: Pexp_pack(modExpr)},
        {ptyp_desc: Ptyp_package(packageType), ptyp_loc},
      ) =>
      Doc.group(
        Doc.concat(list[
          Doc.text("module("),
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              printModExpr(modExpr, cmtTbl),
              Doc.text(": "),
              printComments(
                printPackageType(
                  ~printModuleKeywordAndParens=false,
                  packageType,
                  cmtTbl,
                ),
                cmtTbl,
                ptyp_loc,
              ),
            ]),
          ),
          Doc.softLine,
          Doc.rparen,
        ]),
      )
    
    | Pexp_constraint(expr, typ) =>
      Doc.concat(list[
        printExpressionWithComments(expr, cmtTbl),
        Doc.text(": "),
        printTypExpr(typ, cmtTbl),
      ])
    | Pexp_letmodule({txt: modName}, modExpr, expr) =>
      printExpressionBlock(~braces=true, e, cmtTbl)
    | Pexp_letexception(extensionConstructor, expr) =>
      printExpressionBlock(~braces=true, e, cmtTbl)
    | Pexp_assert(expr) =>
      let rhs = {
        let doc = printExpressionWithComments(expr, cmtTbl)
        if Parens.lazyOrAssertExprRhs(expr) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.concat(list[Doc.text("assert "), rhs])
    | Pexp_lazy(expr) =>
      let rhs = {
        let doc = printExpressionWithComments(expr, cmtTbl)
        if Parens.lazyOrAssertExprRhs(expr) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.group(Doc.concat(list[Doc.text("lazy "), rhs]))
    | Pexp_open(overrideFlag, longidentLoc, expr) =>
      printExpressionBlock(~braces=true, e, cmtTbl)
    | Pexp_pack(modExpr) =>
      Doc.group(
        Doc.concat(list[
          Doc.text("module("),
          Doc.indent(
            Doc.concat(list[Doc.softLine, printModExpr(modExpr, cmtTbl)]),
          ),
          Doc.softLine,
          Doc.rparen,
        ]),
      )
    | Pexp_sequence(_) => printExpressionBlock(~braces=true, e, cmtTbl)
    | Pexp_let(_) => printExpressionBlock(~braces=true, e, cmtTbl)
    | Pexp_fun(_) | Pexp_newtype(_) =>
      let (attrsOnArrow, parameters, returnExpr) = ParsetreeViewer.funExpr(e)
      let (uncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(
        attrsOnArrow,
      )
      
      let (returnExpr, typConstraint) = switch returnExpr.pexp_desc {
      | Pexp_constraint(expr, typ) => (expr, Some(typ))
      | _ => (returnExpr, None)
      }
      
      let hasConstraint = switch typConstraint {
      | Some(_) => true
      | None => false
      }
      let parametersDoc = printExprFunParameters(
        ~inCallback=false,
        ~uncurried,
        ~hasConstraint,
        parameters,
        cmtTbl,
      )
      
      let returnExprDoc = {
        let shouldInline = switch returnExpr.pexp_desc {
        | ((Pexp_array(_) | Pexp_tuple(_)) | Pexp_construct(_, Some(_)))
          | Pexp_record(_) =>
          true
        | _ => false
        }
        
        let shouldIndent = switch returnExpr.pexp_desc {
        | (((Pexp_sequence(_) | Pexp_let(_)) | Pexp_letmodule(_))
          | Pexp_letexception(_))
          | Pexp_open(_) =>
          false
        | _ => true
        }
        
        let returnDoc = printExpressionWithComments(returnExpr, cmtTbl)
        if shouldInline {
          Doc.concat(list[Doc.space, returnDoc])
        } else {
          Doc.group(
            if shouldIndent {
              Doc.indent(Doc.concat(list[Doc.line, returnDoc]))
            } else {
              Doc.concat(list[Doc.space, returnDoc])
            },
          )
        }
      }
      
      let typConstraintDoc = switch typConstraint {
      | Some(typ) => Doc.concat(list[Doc.text(": "), printTypExpr(typ, cmtTbl)])
      | _ => Doc.nil
      }
      
      let attrs = switch attrs {
      | list[] => Doc.nil
      | attrs =>
        Doc.concat(list[
          Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
          Doc.space,
        ])
      }
      
      Doc.group(
        Doc.concat(list[
          attrs,
          parametersDoc,
          typConstraintDoc,
          Doc.text(" =>"),
          returnExprDoc,
        ]),
      )
    | Pexp_try(expr, cases) =>
      Doc.concat(list[
        Doc.text("try "),
        printExpressionWithComments(expr, cmtTbl),
        Doc.text(" catch "),
        printCases(cases, cmtTbl),
      ])
    | Pexp_match(expr, cases) =>
      Doc.concat(list[
        Doc.text("switch "),
        printExpressionWithComments(expr, cmtTbl),
        Doc.space,
        printCases(cases, cmtTbl),
      ])
    | Pexp_function(cases) =>
      Doc.concat(list[Doc.text("x => switch x "), printCases(cases, cmtTbl)])
    | _ => failwith("expression not yet implemented in printer")
    }
    
    let shouldPrintItsOwnAttributes = switch e.pexp_desc {
    | (((Pexp_apply(_) | Pexp_fun(_)) | Pexp_newtype(_)) | Pexp_setfield(_))
      | Pexp_ifthenelse(_) =>
      true
    | Pexp_construct(_)
      when ParsetreeViewer.hasJsxAttribute(e.pexp_attributes) =>
      true
    | _ => false
    }
    
    switch e.pexp_attributes {
    | list[] => printedExpression
    | attrs when !shouldPrintItsOwnAttributes =>
      Doc.group(Doc.concat(list[printAttributes(attrs), printedExpression]))
    | _ => printedExpression
    }
  }
  
  and printPexpFun = (~inCallback, e, cmtTbl) => {
    let (attrsOnArrow, parameters, returnExpr) = ParsetreeViewer.funExpr(e)
    let (uncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(
      attrsOnArrow,
    )
    
    let (returnExpr, typConstraint) = switch returnExpr.pexp_desc {
    | Pexp_constraint(expr, typ) => (expr, Some(typ))
    | _ => (returnExpr, None)
    }
    
    let parametersDoc = printExprFunParameters(
      ~inCallback,
      ~uncurried,
      ~hasConstraint=switch typConstraint {
      | Some(_) => true
      | None => false
      },
      parameters,
      cmtTbl,
    )
    let returnShouldIndent = switch returnExpr.pexp_desc {
    | (((Pexp_sequence(_) | Pexp_let(_)) | Pexp_letmodule(_))
      | Pexp_letexception(_))
      | Pexp_open(_) =>
      false
    | _ => true
    }
    
    let returnExprDoc = {
      let shouldInline = switch returnExpr.pexp_desc {
      | ((Pexp_array(_) | Pexp_tuple(_)) | Pexp_construct(_, Some(_)))
        | Pexp_record(_) =>
        true
      | _ => false
      }
      
      let returnDoc = printExpressionWithComments(returnExpr, cmtTbl)
      if shouldInline {
        Doc.concat(list[Doc.space, returnDoc])
      } else {
        Doc.group(
          if returnShouldIndent {
            Doc.concat(list[
              Doc.indent(Doc.concat(list[Doc.line, returnDoc])),
              if inCallback {
                Doc.softLine
              } else {
                Doc.nil
              },
            ])
          } else {
            Doc.concat(list[Doc.space, returnDoc])
          },
        )
      }
    }
    
    let typConstraintDoc = switch typConstraint {
    | Some(typ) => Doc.concat(list[Doc.text(": "), printTypExpr(typ, cmtTbl)])
    | _ => Doc.nil
    }
    
    let attrs = switch attrs {
    | list[] => Doc.nil
    | attrs =>
      Doc.concat(list[
        Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
        Doc.space,
      ])
    }
    
    Doc.group(
      Doc.concat(list[
        attrs,
        parametersDoc,
        typConstraintDoc,
        Doc.text(" =>"),
        returnExprDoc,
      ]),
    )
  }
  
  and printTernaryOperand = (expr, cmtTbl) => {
    let doc = printExpressionWithComments(expr, cmtTbl)
    if Parens.ternaryOperand(expr) {
      addParens(doc)
    } else {
      doc
    }
  }
  
  and printSetFieldExpr = (attrs, lhs, longidentLoc, rhs, loc, cmtTbl) => {
    let rhsDoc = {
      let doc = printExpressionWithComments(rhs, cmtTbl)
      if Parens.setFieldExprRhs(rhs) {
        addParens(doc)
      } else {
        doc
      }
    }
    
    let lhsDoc = {
      let doc = printExpressionWithComments(lhs, cmtTbl)
      if Parens.fieldExpr(lhs) {
        addParens(doc)
      } else {
        doc
      }
    }
    
    let shouldIndent = ParsetreeViewer.isBinaryExpression(rhs)
    let doc = Doc.concat(list[
      lhsDoc,
      Doc.dot,
      printLongidentLocation(longidentLoc, cmtTbl),
      Doc.text(" ="),
      if shouldIndent {
        Doc.group(Doc.indent(Doc.concat(list[Doc.line, rhsDoc])))
      } else {
        Doc.concat(list[Doc.space, rhsDoc])
      },
    ])
    let doc = switch attrs {
    | list[] => doc
    | attrs => Doc.group(Doc.concat(list[printAttributes(attrs), doc]))
    }
    
    printComments(doc, cmtTbl, loc)
  }
  
  and printUnaryExpression = (expr, cmtTbl) => {
    let printUnaryOperator = op =>
      Doc.text(
        switch op {
        | "~+" => "+"
        | "~+." => "+."
        | "~-" => "-"
        | "~-." => "-."
        | "not" => "!"
        | "!" => "&"
        | _ => assert false
        },
      )
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list[(Nolabel, operand)],
      ) =>
      let printedOperand = {
        let doc = printExpressionWithComments(operand, cmtTbl)
        if Parens.unaryExprOperand(operand) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      let doc = Doc.concat(list[printUnaryOperator(operator), printedOperand])
      printComments(doc, cmtTbl, expr.pexp_loc)
    | _ => assert false
    }
  }
  
  and printBinaryExpression = (expr: Parsetree.expression, cmtTbl) => {
    let printBinaryOperator = (~inlineRhs, operator) => {
      let operatorTxt = switch operator {
      | "|." => "->"
      | "^" => "++"
      | "=" => "=="
      | "==" => "==="
      | "<>" => "!="
      | "!=" => "!=="
      | txt => txt
      }
      
      let spacingBeforeOperator = if operator == "|." {
        Doc.softLine
      } else if operator == "|>" {
        Doc.line
      } else {
        Doc.space
      }
      
      let spacingAfterOperator = if operator == "|." {
        Doc.nil
      } else if operator == "|>" {
        Doc.space
      } else if inlineRhs {
        Doc.space
      } else {
        Doc.line
      }
      
      Doc.concat(list[
        spacingBeforeOperator,
        Doc.text(operatorTxt),
        spacingAfterOperator,
      ])
    }
    
    let printOperand = (~isLhs, expr, parentOperator) => {
      let rec flatten = (~isLhs, expr, parentOperator) =>
        if ParsetreeViewer.isBinaryExpression(expr) {
          switch expr {
          | {
              pexp_desc: 
                Pexp_apply(
                  {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
                  list[(_, left), (_, right)],
                ),
            } =>
            if (
              ParsetreeViewer.flattenableOperators(parentOperator, operator) &&
              !ParsetreeViewer.hasAttributes(expr.pexp_attributes)
            ) {
              let leftPrinted = flatten(~isLhs=true, left, operator)
              let rightPrinted = {
                let (
                  _,
                  rightAttrs,
                ) = ParsetreeViewer.partitionPrinteableAttributes(
                  right.pexp_attributes,
                )
                
                let doc = printExpressionWithComments(
                  {...right, pexp_attributes: rightAttrs},
                  cmtTbl,
                )
                
                let doc = if Parens.flattenOperandRhs(parentOperator, right) {
                  Doc.concat(list[Doc.lparen, doc, Doc.rparen])
                } else {
                  doc
                }
                
                let printeableAttrs = ParsetreeViewer.filterPrinteableAttributes(
                  right.pexp_attributes,
                )
                
                Doc.concat(list[printAttributes(printeableAttrs), doc])
              }
              
              let doc = Doc.concat(list[
                leftPrinted,
                printBinaryOperator(~inlineRhs=false, operator),
                rightPrinted,
              ])
              printComments(doc, cmtTbl, expr.pexp_loc)
            } else {
              let doc = printExpressionWithComments(
                {...expr, pexp_attributes: list[]},
                cmtTbl,
              )
              let doc = if (
                Parens.subBinaryExprOperand(parentOperator, operator) ||
                ((expr.pexp_attributes != list[]) &&
                  (ParsetreeViewer.isBinaryExpression(expr) ||
                  ParsetreeViewer.isTernaryExpr(expr)))
              ) {
                Doc.concat(list[Doc.lparen, doc, Doc.rparen])
              } else {
                doc
              }
              Doc.concat(list[printAttributes(expr.pexp_attributes), doc])
            }
          | _ => assert false
          }
        } else {
          switch expr.pexp_desc {
          | Pexp_setfield(lhs, field, rhs) =>
            let doc = printSetFieldExpr(
              expr.pexp_attributes,
              lhs,
              field,
              rhs,
              expr.pexp_loc,
              cmtTbl,
            )
            if isLhs {
              addParens(doc)
            } else {
              doc
            }
          | Pexp_apply(
              {pexp_desc: Pexp_ident({txt: Longident.Lident("#=")})},
              list[(Nolabel, lhs), (Nolabel, rhs)],
            ) =>
            let rhsDoc = printExpressionWithComments(rhs, cmtTbl)
            let lhsDoc = printExpressionWithComments(lhs, cmtTbl)
            
            let shouldIndent = ParsetreeViewer.isBinaryExpression(rhs)
            let doc = Doc.group(
              Doc.concat(list[
                lhsDoc,
                Doc.text(" ="),
                if shouldIndent {
                  Doc.group(Doc.indent(Doc.concat(list[Doc.line, rhsDoc])))
                } else {
                  Doc.concat(list[Doc.space, rhsDoc])
                },
              ]),
            )
            let doc = switch expr.pexp_attributes {
            | list[] => doc
            | attrs => Doc.group(Doc.concat(list[printAttributes(attrs), doc]))
            }
            
            if isLhs {
              addParens(doc)
            } else {
              doc
            }
          | _ =>
            let doc = {
              let doc = printExpressionWithComments(expr, cmtTbl)
              doc
            }
            
            if Parens.binaryExprOperand(~isLhs, expr, parentOperator) {
              addParens(doc)
            } else {
              doc
            }
          }
        }
      
      flatten(~isLhs, expr, parentOperator)
    }
    
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(("|." | "|>") as op)})},
        list[(Nolabel, lhs), (Nolabel, rhs)],
      )
      when !(
        ParsetreeViewer.isBinaryExpression(lhs) ||
        ParsetreeViewer.isBinaryExpression(rhs)
      ) =>
      let lhsDoc = printOperand(~isLhs=true, lhs, op)
      let rhsDoc = printOperand(~isLhs=false, rhs, op)
      Doc.concat(list[
        lhsDoc,
        switch op {
        | "|." => Doc.text("->")
        | "|>" => Doc.text(" |> ")
        | _ => assert false
        },
        rhsDoc,
      ])
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list[(Nolabel, lhs), (Nolabel, rhs)],
      ) =>
      let right = {
        let operatorWithRhs = Doc.concat(list[
          printBinaryOperator(
            ~inlineRhs=ParsetreeViewer.shouldInlineRhsBinaryExpr(rhs),
            operator,
          ),
          printOperand(~isLhs=false, rhs, operator),
        ])
        if ParsetreeViewer.shouldIndentBinaryExpr(expr) {
          Doc.group(Doc.indent(operatorWithRhs))
        } else {
          operatorWithRhs
        }
      }
      
      let doc = Doc.group(
        Doc.concat(list[printOperand(~isLhs=true, lhs, operator), right]),
      )
      Doc.concat(list[
        printAttributes(expr.pexp_attributes),
        if Parens.binaryExpr(expr) {
          addParens(doc)
        } else {
          doc
        },
      ])
    | _ => Doc.nil
    }
  }
  
  and printPexpApply = (expr, cmtTbl) =>
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident("##")})},
        list[(Nolabel, parentExpr), (Nolabel, memberExpr)],
      ) =>
      let member = {
        let memberDoc = printExpressionWithComments(memberExpr, cmtTbl)
        Doc.concat(list[Doc.text("\""), memberDoc, Doc.text("\"")])
      }
      
      Doc.group(
        Doc.concat(list[
          printAttributes(expr.pexp_attributes),
          printExpressionWithComments(parentExpr, cmtTbl),
          Doc.lbracket,
          member,
          Doc.rbracket,
        ]),
      )
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident("#=")})},
        list[(Nolabel, lhs), (Nolabel, rhs)],
      ) =>
      let rhsDoc = printExpressionWithComments(rhs, cmtTbl)
      
      let shouldIndent = ParsetreeViewer.isBinaryExpression(rhs)
      let doc = Doc.group(
        Doc.concat(list[
          printExpressionWithComments(lhs, cmtTbl),
          Doc.text(" ="),
          if shouldIndent {
            Doc.group(Doc.indent(Doc.concat(list[Doc.line, rhsDoc])))
          } else {
            Doc.concat(list[Doc.space, rhsDoc])
          },
        ]),
      )
      switch expr.pexp_attributes {
      | list[] => doc
      | attrs => Doc.group(Doc.concat(list[printAttributes(attrs), doc]))
      }
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Ldot(Lident("Array"), "get")})},
        list[(Nolabel, parentExpr), (Nolabel, memberExpr)],
      ) =>
      let member = {
        let memberDoc = printExpressionWithComments(memberExpr, cmtTbl)
        let shouldInline = switch memberExpr.pexp_desc {
        | Pexp_constant(_) | Pexp_ident(_) => true
        | _ => false
        }
        
        if shouldInline {
          memberDoc
        } else {
          Doc.concat(list[
            Doc.indent(Doc.concat(list[Doc.softLine, memberDoc])),
            Doc.softLine,
          ])
        }
      }
      
      Doc.group(
        Doc.concat(list[
          printAttributes(expr.pexp_attributes),
          printExpressionWithComments(parentExpr, cmtTbl),
          Doc.lbracket,
          member,
          Doc.rbracket,
        ]),
      )
    
    | Pexp_apply({pexp_desc: Pexp_ident(lident)}, args)
      when ParsetreeViewer.isJsxExpression(expr) =>
      printJsxExpression(lident, args, cmtTbl)
    | Pexp_apply(callExpr, args) =>
      let (uncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(
        expr.pexp_attributes,
      )
      
      let callExprDoc = printExpressionWithComments(callExpr, cmtTbl)
      if ParsetreeViewer.requiresSpecialCallbackPrinting(args) {
        let argsDoc = printArgumentsWithCallback(~uncurried, args, cmtTbl)
        Doc.concat(list[printAttributes(attrs), callExprDoc, argsDoc])
      } else {
        let argsDoc = printArguments(~uncurried, args, cmtTbl)
        Doc.concat(list[printAttributes(attrs), callExprDoc, argsDoc])
      }
    | _ => assert false
    }
  
  and printJsxExpression = (lident, args, cmtTbl) => {
    let name = printJsxName(lident, cmtTbl)
    let (formattedProps, children) = formatJsxProps(args, cmtTbl)
    
    let isSelfClosing = switch children {
    | list[] => true
    | _ => false
    }
    Doc.group(
      Doc.concat(list[
        Doc.group(
          Doc.concat(list[
            Doc.lessThan,
            name,
            formattedProps,
            if isSelfClosing {
              Doc.concat(list[Doc.line, Doc.text("/>")])
            } else {
              Doc.nil
            },
          ]),
        ),
        if isSelfClosing {
          Doc.nil
        } else {
          Doc.concat(list[
            Doc.greaterThan,
            Doc.indent(
              Doc.concat(list[Doc.line, printJsxChildren(children, cmtTbl)]),
            ),
            Doc.line,
            Doc.text("</"),
            name,
            Doc.greaterThan,
          ])
        },
      ]),
    )
  }
  
  and printJsxFragment = (expr, cmtTbl) => {
    let opening = Doc.text("<>")
    let closing = Doc.text("</>")
    let (children, _) = ParsetreeViewer.collectListExpressions(expr)
    Doc.group(
      Doc.concat(list[
        opening,
        switch children {
        | list[] => Doc.nil
        | children =>
          Doc.indent(
            Doc.concat(list[Doc.line, printJsxChildren(children, cmtTbl)]),
          )
        },
        Doc.line,
        closing,
      ]),
    )
  }
  
  and printJsxChildren = (children: list<Parsetree.expression>, cmtTbl) =>
    Doc.group(
      Doc.join(
        ~sep=Doc.line,
        List.map(
          expr => {
            let exprDoc = printExpressionWithComments(expr, cmtTbl)
            if Parens.jsxChildExpr(expr) {
              addBraces(exprDoc)
            } else {
              exprDoc
            }
          },
          children,
        ),
      ),
    )
  
  and formatJsxProps = (args, cmtTbl) => {
    let rec loop = (props, args) =>
      switch args {
      | list[] => (Doc.nil, list[])
      | list[
          (Asttypes.Labelled("children"), children),
          (
            Asttypes.Nolabel,
            {
              Parsetree.pexp_desc: 
                Pexp_construct({txt: Longident.Lident("()")}, None),
            },
          ),
        ] =>
        let formattedProps = Doc.indent(
          switch props {
          | list[] => Doc.nil
          | props =>
            Doc.concat(list[
              Doc.line,
              Doc.group(Doc.join(~sep=Doc.line, props |> List.rev)),
            ])
          },
        )
        let (children, _) = ParsetreeViewer.collectListExpressions(children)
        (formattedProps, children)
      | list[arg, ...args] =>
        let propDoc = formatJsxProp(arg, cmtTbl)
        loop(list[propDoc, ...props], args)
      }
    
    loop(list[], args)
  }
  
  and formatJsxProp = (arg, cmtTbl) =>
    switch arg {
    | (
        (Asttypes.Labelled(lblTxt) | Optional(lblTxt)) as lbl,
        {
          Parsetree.pexp_attributes: 
            list[({Location.txt: "ns.jsxPropLoc", loc: argLoc}, _)],
          pexp_desc: Pexp_ident({txt: Longident.Lident(ident)}),
        },
      ) when lblTxt == ident =>
      switch lbl {
      | Nolabel => Doc.nil
      | Labelled(lbl) => printComments(Doc.text(ident), cmtTbl, argLoc)
      | Optional(lbl) => printComments(Doc.text("?" ++ ident), cmtTbl, argLoc)
      }
    | (lbl, expr) =>
      let (argLoc, expr) = switch expr.pexp_attributes {
      | list[({Location.txt: "ns.jsxPropLoc", loc}, _), ...attrs] =>
        (loc, {...expr, pexp_attributes: attrs})
      | _ => (Location.none, expr)
      }
      
      let lblDoc = switch lbl {
      | Asttypes.Labelled(lbl) =>
        let lbl = printComments(Doc.text(lbl), cmtTbl, argLoc)
        Doc.concat(list[lbl, Doc.text("=")])
      | Asttypes.Optional(lbl) =>
        let lbl = printComments(Doc.text(lbl), cmtTbl, argLoc)
        Doc.concat(list[lbl, Doc.text("=?")])
      | Nolabel => Doc.nil
      }
      
      let exprDoc = printExpression(expr, cmtTbl)
      let fullLoc = {...argLoc, loc_end: expr.pexp_loc.loc_end}
      printComments(
        Doc.concat(list[
          lblDoc,
          if Parens.jsxPropExpr(expr) {
            addBraces(exprDoc)
          } else {
            exprDoc
          },
        ]),
        cmtTbl,
        fullLoc,
      )
    }
  
  and printJsxName = ({txt: lident, loc}, cmtTbl) => {
    let rec flatten = (acc, lident) =>
      switch lident {
      | Longident.Lident(txt) => list[txt, ...acc]
      | Ldot(lident, txt) =>
        let acc = if txt == "createElement" {
          acc
        } else {
          list[txt, ...acc]
        }
        flatten(acc, lident)
      | _ => acc
      }
    
    let doc = switch lident {
    | Longident.Lident(txt) => Doc.text(txt)
    | _ as lident =>
      let segments = flatten(list[], lident)
      Doc.join(~sep=Doc.dot, List.map(Doc.text, segments))
    }
    
    printComments(doc, cmtTbl, loc)
  }
  
  and printArgumentsWithCallback = (~uncurried, args, cmtTbl) => {
    let rec loop = (acc, args) =>
      switch args {
      | list[] => (Doc.nil, Doc.nil)
      | list[(_lbl, expr)] =>
        let callback = printPexpFun(~inCallback=true, expr, cmtTbl)
        (Doc.concat(List.rev(acc)), callback)
      | list[arg, ...args] =>
        let argDoc = printArgument(arg, cmtTbl)
        loop(list[Doc.line, Doc.comma, argDoc, ...acc], args)
      }
    
    let (printedArgs, callback) = loop(list[], args)
    
    let fitsOnOneLine = Doc.concat(list[
      if uncurried {
        Doc.text("(.")
      } else {
        Doc.lparen
      },
      Doc.concat(list[printedArgs, callback]),
      Doc.rparen,
    ])
    
    let arugmentsFitOnOneLine = Doc.concat(list[
      if uncurried {
        Doc.text("(.")
      } else {
        Doc.lparen
      },
      Doc.concat(list[
        Doc.softLine,
        printedArgs,
        Doc.breakableGroup(~forceBreak=true, callback),
      ]),
      Doc.softLine,
      Doc.rparen,
    ])
    
    let breakAllArgs = printArguments(~uncurried, args, cmtTbl)
    Doc.customLayout(list[fitsOnOneLine, arugmentsFitOnOneLine, breakAllArgs])
  }
  
  and printArguments = (
    ~uncurried,
    args: list<(Asttypes.arg_label, Parsetree.expression)>,
    cmtTbl,
  ) =>
    switch args {
    | list[(
        Nolabel,
        {pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, _)},
      )] =>
      if uncurried {
        Doc.text("(.)")
      } else {
        Doc.text("()")
      }
    | list[(Nolabel, arg)] when ParsetreeViewer.isHuggableExpression(arg) =>
      Doc.concat(list[
        if uncurried {
          Doc.text("(.")
        } else {
          Doc.lparen
        },
        printExpressionWithComments(arg, cmtTbl),
        Doc.rparen,
      ])
    | args =>
      Doc.group(
        Doc.concat(list[
          if uncurried {
            Doc.text("(.")
          } else {
            Doc.lparen
          },
          Doc.indent(
            Doc.concat(list[
              if uncurried {
                Doc.line
              } else {
                Doc.softLine
              },
              Doc.join(
                ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                List.map(arg => printArgument(arg, cmtTbl), args),
              ),
            ]),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        ]),
      )
    }
  
  and printArgument = ((argLbl, arg), cmtTbl) =>
    switch (argLbl, arg) {
    | (
        Asttypes.Labelled(lbl),
        {pexp_desc: Pexp_ident({txt: Longident.Lident(name)})},
      ) when lbl == name =>
      Doc.text("~" ++ lbl)
    
    | (
        Asttypes.Optional(lbl),
        {pexp_desc: Pexp_ident({txt: Longident.Lident(name)})},
      ) when lbl == name =>
      Doc.text("~" ++ lbl ++ "?")
    | (lbl, expr) =>
      let printedLbl = switch argLbl {
      | Asttypes.Nolabel => Doc.nil
      | Asttypes.Labelled(lbl) => Doc.text("~" ++ lbl ++ "=")
      | Asttypes.Optional(lbl) => Doc.text("~" ++ lbl ++ "=?")
      }
      
      let printedExpr = printExpressionWithComments(expr, cmtTbl)
      Doc.concat(list[printedLbl, printedExpr])
    }
  
  and printCases = (cases: list<Parsetree.case>, cmtTbl) =>
    Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat(list[
        Doc.lbrace,
        Doc.concat(list[
          Doc.line,
          printList(
            ~getLoc=n => {
              ...n.Parsetree.pc_lhs.ppat_loc,
              loc_end: n.pc_rhs.pexp_loc.loc_end,
            },
            ~print=printCase,
            ~nodes=cases,
            cmtTbl,
          ),
        ]),
        Doc.line,
        Doc.rbrace,
      ]),
    )
  
  and printCase = (case: Parsetree.case, cmtTbl) => {
    let rhs = switch case.pc_rhs.pexp_desc {
    | (((Pexp_let(_) | Pexp_letmodule(_)) | Pexp_letexception(_))
      | Pexp_open(_))
      | Pexp_sequence(_) =>
      printExpressionBlock(~braces=false, case.pc_rhs, cmtTbl)
    | _ => printExpressionWithComments(case.pc_rhs, cmtTbl)
    }
    
    let guard = switch case.pc_guard {
    | None => Doc.nil
    | Some(expr) =>
      Doc.group(
        Doc.concat(list[
          Doc.line,
          Doc.text("when "),
          printExpressionWithComments(expr, cmtTbl),
        ]),
      )
    }
    
    let shouldInlineRhs = switch case.pc_rhs.pexp_desc {
    | Pexp_construct({txt: Longident.Lident("()")}, _) => true
    | _ => false
    }
    
    Doc.group(
      Doc.concat(list[
        Doc.text("| "),
        Doc.indent(
          Doc.concat(list[
            printPattern(case.pc_lhs, cmtTbl),
            guard,
            Doc.text(" =>"),
            if shouldInlineRhs {
              Doc.space
            } else {
              Doc.line
            },
            rhs,
          ]),
        ),
      ]),
    )
  }
  
  and printExprFunParameters = (
    ~inCallback,
    ~uncurried,
    ~hasConstraint,
    parameters,
    cmtTbl,
  ) =>
    switch parameters {
    | list[(list[], Asttypes.Nolabel, None, {Parsetree.ppat_desc: Ppat_any})]
      when !uncurried =>
      if hasConstraint {
        Doc.text("(_)")
      } else {
        Doc.text("_")
      }
    
    | list[(
        list[],
        Asttypes.Nolabel,
        None,
        {Parsetree.ppat_desc: Ppat_var(stringLoc)},
      )] when !uncurried =>
      let txtDoc = Doc.text(
        if hasConstraint {
          "(" ++ stringLoc.txt ++ ")"
        } else {
          stringLoc.txt
        },
      )
      
      printComments(txtDoc, cmtTbl, stringLoc.loc)
    
    | list[(
        list[],
        Nolabel,
        None,
        {ppat_desc: Ppat_construct({txt: Longident.Lident("()")}, None)},
      )] when !uncurried =>
      Doc.text("()")
    
    | parameters =>
      let lparen = if uncurried {
        Doc.text("(. ")
      } else {
        Doc.lparen
      }
      let shouldHug = ParsetreeViewer.parametersShouldHug(parameters)
      let printedParamaters = Doc.concat(list[
        if shouldHug || inCallback {
          Doc.nil
        } else {
          Doc.softLine
        },
        Doc.join(
          ~sep=Doc.concat(list[
            Doc.comma,
            if inCallback {
              Doc.space
            } else {
              Doc.line
            },
          ]),
          List.map(p => printExpFunParameter(p, cmtTbl), parameters),
        ),
      ])
      Doc.group(
        Doc.concat(list[
          lparen,
          if shouldHug || inCallback {
            printedParamaters
          } else {
            Doc.indent(printedParamaters)
          },
          if shouldHug || inCallback {
            Doc.nil
          } else {
            Doc.concat(list[Doc.trailingComma, Doc.softLine])
          },
          Doc.rparen,
        ]),
      )
    }
  
  and printExpFunParameter = ((attrs, lbl, defaultExpr, pattern), cmtTbl) => {
    let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(attrs)
    let uncurried = if isUncurried {
      Doc.concat(list[Doc.dot, Doc.space])
    } else {
      Doc.nil
    }
    let attrs = switch attrs {
    | list[] => Doc.nil
    | attrs =>
      Doc.concat(list[
        Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
        Doc.line,
      ])
    }
    
    let defaultExprDoc = switch defaultExpr {
    | Some(expr) =>
      Doc.concat(list[Doc.text("="), printExpressionWithComments(expr, cmtTbl)])
    | None => Doc.nil
    }
    
    let labelWithPattern = switch (lbl, pattern) {
    | (Asttypes.Nolabel, pattern) => printPattern(pattern, cmtTbl)
    | (Asttypes.Labelled(lbl) | Optional(lbl), {ppat_desc: Ppat_var(stringLoc)})
      when lbl == stringLoc.txt =>
      Doc.concat(list[Doc.text("~"), Doc.text(lbl)])
    | (Asttypes.Labelled(lbl) | Optional(lbl), pattern) =>
      Doc.concat(list[
        Doc.text("~"),
        Doc.text(lbl),
        Doc.text(" as "),
        printPattern(pattern, cmtTbl),
      ])
    }
    
    let optionalLabelSuffix = switch (lbl, defaultExpr) {
    | (Asttypes.Optional(_), None) => Doc.text("=?")
    | _ => Doc.nil
    }
    
    let doc = Doc.group(
      Doc.concat(list[
        uncurried,
        attrs,
        labelWithPattern,
        defaultExprDoc,
        optionalLabelSuffix,
      ]),
    )
    let cmtLoc = switch defaultExpr {
    | None => pattern.ppat_loc
    | Some(expr) =>
      {
        ...pattern.ppat_loc,
        loc_end: expr.pexp_loc.loc_end,
      }
    }
    printComments(doc, cmtTbl, cmtLoc)
  }
  
  and printExpressionBlock = (~braces, expr, cmtTbl) => {
    let rec collectRows = (acc, expr) =>
      switch expr.Parsetree.pexp_desc {
      | Parsetree.Pexp_letmodule(modName, modExpr, expr2) =>
        let name = {
          let doc = Doc.text(modName.txt)
          printComments(doc, cmtTbl, modName.loc)
        }
        
        let letModuleDoc = Doc.concat(list[
          Doc.text("module "),
          name,
          Doc.text(" = "),
          printModExpr(modExpr, cmtTbl),
        ])
        let loc = {...expr.pexp_loc, loc_end: modExpr.pmod_loc.loc_end}
        collectRows(list[(loc, letModuleDoc), ...acc], expr2)
      | Pexp_letexception(extensionConstructor, expr2) =>
        let loc = {
          let loc = {
            ...expr.pexp_loc,
            loc_end: extensionConstructor.pext_loc.loc_end,
          }
          switch getFirstLeadingComment(cmtTbl, loc) {
          | None => loc
          | Some(comment) =>
            let cmtLoc = Comment.loc(comment)
            {...cmtLoc, loc_end: loc.loc_end}
          }
        }
        
        let letExceptionDoc = printExceptionDef(extensionConstructor, cmtTbl)
        collectRows(list[(loc, letExceptionDoc), ...acc], expr2)
      | Pexp_open(overrideFlag, longidentLoc, expr2) =>
        let openDoc = Doc.concat(list[
          Doc.text("open"),
          printOverrideFlag(overrideFlag),
          Doc.space,
          printLongidentLocation(longidentLoc, cmtTbl),
        ])
        let loc = {...expr.pexp_loc, loc_end: longidentLoc.loc.loc_end}
        collectRows(list[(loc, openDoc), ...acc], expr2)
      | Pexp_sequence(expr1, expr2) =>
        let exprDoc = {
          let doc = printExpressionWithComments(expr1, cmtTbl)
          if Parens.blockExpr(expr1) {
            addParens(doc)
          } else {
            doc
          }
        }
        
        let loc = expr1.pexp_loc
        collectRows(list[(loc, exprDoc), ...acc], expr2)
      | Pexp_let(recFlag, valueBindings, expr2) =>
        let loc = {
          let loc = switch valueBindings {
          | list[] => Location.none
          | list[vb, ..._] => vb.pvb_loc
          }
          
          switch getFirstLeadingComment(cmtTbl, loc) {
          | None => loc
          | Some(comment) =>
            let cmtLoc = Comment.loc(comment)
            {...cmtLoc, loc_end: loc.loc_end}
          }
        }
        
        let recFlag = switch recFlag {
        | Asttypes.Nonrecursive => Doc.nil
        | Asttypes.Recursive => Doc.text("rec ")
        }
        
        let letDoc = printValueBindings(~recFlag, valueBindings, cmtTbl)
        collectRows(list[(loc, letDoc), ...acc], expr2)
      | _ =>
        let exprDoc = {
          let doc = printExpression(expr, cmtTbl)
          if Parens.blockExpr(expr) {
            addParens(doc)
          } else {
            doc
          }
        }
        
        List.rev(list[(expr.pexp_loc, exprDoc), ...acc])
      }
    
    let rows = collectRows(list[], expr)
    let block = printList(
      ~getLoc=fst,
      ~nodes=rows,
      ~print=((_, doc), _) => doc,
      ~forceBreak=true,
      cmtTbl,
    )
    
    Doc.breakableGroup(
      ~forceBreak=true,
      if braces {
        Doc.concat(list[
          Doc.lbrace,
          Doc.indent(Doc.concat(list[Doc.line, block])),
          Doc.line,
          Doc.rbrace,
        ])
      } else {
        block
      },
    )
  }
  
  and printOverrideFlag = overrideFlag =>
    switch overrideFlag {
    | Asttypes.Override => Doc.text("!")
    | Fresh => Doc.nil
    }
  
  and printDirectionFlag = flag =>
    switch flag {
    | Asttypes.Downto => Doc.text(" downto ")
    | Asttypes.Upto => Doc.text(" to ")
    }
  
  and printRecordRow = ((lbl, expr), cmtTbl) => {
    let cmtLoc = {...lbl.loc, loc_end: expr.pexp_loc.loc_end}
    let doc = Doc.concat(list[
      printLongidentLocation(lbl, cmtTbl),
      Doc.text(": "),
      printExpressionWithComments(expr, cmtTbl),
    ])
    printComments(doc, cmtTbl, cmtLoc)
  }
  
  and printBsObjectRow = ((lbl, expr), cmtTbl) => {
    let cmtLoc = {...lbl.loc, loc_end: expr.pexp_loc.loc_end}
    let lblDoc = {
      let doc = Doc.concat(list[
        Doc.text("\""),
        printLongident(lbl.txt),
        Doc.text("\""),
      ])
      printComments(doc, cmtTbl, lbl.loc)
    }
    
    let doc = Doc.concat(list[
      lblDoc,
      Doc.text(": "),
      printExpressionWithComments(expr, cmtTbl),
    ])
    printComments(doc, cmtTbl, cmtLoc)
  }
  
  and printAttributes = (~loc=?, attrs: Parsetree.attributes) =>
    switch attrs {
    | list[] => Doc.nil
    | attrs =>
      let lineBreak = switch loc {
      | None => Doc.line
      | Some(loc) =>
        switch List.rev(attrs) {
        | list[({loc: firstLoc}, _), ..._]
          when loc.loc_start.pos_lnum > firstLoc.loc_end.pos_lnum =>
          Doc.hardLine
        | _ => Doc.line
        }
      }
      
      Doc.concat(list[
        Doc.group(Doc.join(~sep=Doc.line, List.map(printAttribute, attrs))),
        lineBreak,
      ])
    }
  
  and printAttribute = ((id, payload): Parsetree.attribute) => {
    let attrName = Doc.text("@" ++ id.txt)
    switch payload {
    | PStr(list[{pstr_desc: Pstr_eval(expr, attrs)}]) =>
      let exprDoc = printExpression(expr, CommentTable.empty)
      let needsParens = switch attrs {
      | list[] => false
      | _ => true
      }
      Doc.group(
        Doc.concat(list[
          attrName,
          addParens(
            Doc.concat(list[
              printAttributes(attrs),
              if needsParens {
                addParens(exprDoc)
              } else {
                exprDoc
              },
            ]),
          ),
        ]),
      )
    | _ => attrName
    }
  }
  
  and printAttributeWithComments = (
    (id, payload): Parsetree.attribute,
    cmtTbl,
  ) => {
    let attrName = Doc.text("@" ++ id.txt)
    switch payload {
    | PStr(list[{pstr_desc: Pstr_eval(expr, attrs)}]) =>
      let exprDoc = printExpressionWithComments(expr, cmtTbl)
      let needsParens = switch attrs {
      | list[] => false
      | _ => true
      }
      Doc.group(
        Doc.concat(list[
          attrName,
          addParens(
            Doc.concat(list[
              printAttributes(attrs),
              if needsParens {
                addParens(exprDoc)
              } else {
                exprDoc
              },
            ]),
          ),
        ]),
      )
    | _ => attrName
    }
  }
  
  and printModExpr = (modExpr, cmtTbl) => {
    let doc = switch modExpr.pmod_desc {
    | Pmod_ident(longidentLoc) => printLongidentLocation(longidentLoc, cmtTbl)
    | Pmod_structure(structure) =>
      Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list[
          Doc.lbrace,
          Doc.indent(
            Doc.concat(list[Doc.softLine, printStructure(structure, cmtTbl)]),
          ),
          Doc.softLine,
          Doc.rbrace,
        ]),
      )
    | Pmod_unpack(expr) =>
      let shouldHug = switch expr.pexp_desc {
      | Pexp_let(_) => true
      | Pexp_constraint(
          {pexp_desc: Pexp_let(_)},
          {ptyp_desc: Ptyp_package(packageType)},
        ) =>
        true
      | _ => false
      }
      
      let (expr, moduleConstraint) = switch expr.pexp_desc {
      | Pexp_constraint(
          expr,
          {ptyp_desc: Ptyp_package(packageType), ptyp_loc},
        ) =>
        let packageDoc = {
          let doc = printPackageType(
            ~printModuleKeywordAndParens=false,
            packageType,
            cmtTbl,
          )
          printComments(doc, cmtTbl, ptyp_loc)
        }
        
        let typeDoc = Doc.group(
          Doc.concat(list[
            Doc.text(":"),
            Doc.indent(Doc.concat(list[Doc.line, packageDoc])),
          ]),
        )
        (expr, typeDoc)
      | _ => (expr, Doc.nil)
      }
      
      let unpackDoc = Doc.group(
        Doc.concat(list[
          printExpressionWithComments(expr, cmtTbl),
          moduleConstraint,
        ]),
      )
      Doc.group(
        Doc.concat(list[
          Doc.text("unpack("),
          if shouldHug {
            unpackDoc
          } else {
            Doc.concat(list[
              Doc.indent(Doc.concat(list[Doc.softLine, unpackDoc])),
              Doc.softLine,
            ])
          },
          Doc.rparen,
        ]),
      )
    | Pmod_extension(extension) => printExtensionWithComments(extension, cmtTbl)
    | Pmod_apply(_) =>
      let (args, callExpr) = ParsetreeViewer.modExprApply(modExpr)
      let isUnitSugar = switch args {
      | list[{pmod_desc: Pmod_structure(list[])}] => true
      | _ => false
      }
      
      let shouldHug = switch args {
      | list[{pmod_desc: Pmod_structure(_)}] => true
      | _ => false
      }
      
      Doc.group(
        Doc.concat(list[
          printModExpr(callExpr, cmtTbl),
          if isUnitSugar {
            printModApplyArg(List.hd(args), cmtTbl)
          } else {
            Doc.concat(list[
              Doc.lparen,
              if shouldHug {
                printModApplyArg(List.hd(args), cmtTbl)
              } else {
                Doc.indent(
                  Doc.concat(list[
                    Doc.softLine,
                    Doc.join(
                      ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                      List.map(
                        modArg => printModApplyArg(modArg, cmtTbl),
                        args,
                      ),
                    ),
                  ]),
                )
              },
              if !shouldHug {
                Doc.concat(list[Doc.trailingComma, Doc.softLine])
              } else {
                Doc.nil
              },
              Doc.rparen,
            ])
          },
        ]),
      )
    | Pmod_constraint(modExpr, modType) =>
      Doc.concat(list[
        printModExpr(modExpr, cmtTbl),
        Doc.text(": "),
        printModType(modType, cmtTbl),
      ])
    | Pmod_functor(_) => printModFunctor(modExpr, cmtTbl)
    }
    
    printComments(doc, cmtTbl, modExpr.pmod_loc)
  }
  
  and printModFunctor = (modExpr, cmtTbl) => {
    let (parameters, returnModExpr) = ParsetreeViewer.modExprFunctor(modExpr)
    
    let (returnConstraint, returnModExpr) = switch returnModExpr.pmod_desc {
    | Pmod_constraint(modExpr, modType) =>
      let constraintDoc = {
        let doc = printModType(modType, cmtTbl)
        if Parens.modExprFunctorConstraint(modType) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      let modConstraint = Doc.concat(list[Doc.text(": "), constraintDoc])
      (modConstraint, printModExpr(modExpr, cmtTbl))
    | _ => (Doc.nil, printModExpr(returnModExpr, cmtTbl))
    }
    
    let parametersDoc = switch parameters {
    | list[(attrs, {txt: "*"}, None)] =>
      let attrs = switch attrs {
      | list[] => Doc.nil
      | attrs =>
        Doc.concat(list[
          Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
          Doc.line,
        ])
      }
      Doc.group(Doc.concat(list[attrs, Doc.text("()")]))
    | list[(list[], {txt: lbl}, None)] => Doc.text(lbl)
    | parameters =>
      Doc.group(
        Doc.concat(list[
          Doc.lparen,
          Doc.indent(
            Doc.concat(list[
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list[Doc.comma, Doc.line]),
                List.map(
                  param => printModFunctorParam(param, cmtTbl),
                  parameters,
                ),
              ),
            ]),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        ]),
      )
    }
    
    Doc.group(
      Doc.concat(list[
        parametersDoc,
        returnConstraint,
        Doc.text(" => "),
        returnModExpr,
      ]),
    )
  }
  
  and printModFunctorParam = ((attrs, lbl, optModType), cmtTbl) => {
    let cmtLoc = switch optModType {
    | None => lbl.Asttypes.loc
    | Some(modType) =>
      {
        ...lbl.loc,
        loc_end: modType.Parsetree.pmty_loc.loc_end,
      }
    }
    
    let attrs = switch attrs {
    | list[] => Doc.nil
    | attrs =>
      Doc.concat(list[
        Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
        Doc.line,
      ])
    }
    let lblDoc = {
      let doc = Doc.text(lbl.txt)
      printComments(doc, cmtTbl, lbl.loc)
    }
    
    let doc = Doc.group(
      Doc.concat(list[
        attrs,
        lblDoc,
        switch optModType {
        | None => Doc.nil
        | Some(modType) =>
          Doc.concat(list[Doc.text(": "), printModType(modType, cmtTbl)])
        },
      ]),
    )
    printComments(doc, cmtTbl, cmtLoc)
  }
  
  and printModApplyArg = (modExpr, cmtTbl) =>
    switch modExpr.pmod_desc {
    | Pmod_structure(list[]) => Doc.text("()")
    | _ => printModExpr(modExpr, cmtTbl)
    }
  
  and printExceptionDef = (constr: Parsetree.extension_constructor, cmtTbl) => {
    let kind = switch constr.pext_kind {
    | Pext_rebind(longident) =>
      Doc.indent(
        Doc.concat(list[
          Doc.text(" ="),
          Doc.line,
          printLongidentLocation(longident, cmtTbl),
        ]),
      )
    | Pext_decl(Pcstr_tuple(list[]), None) => Doc.nil
    | Pext_decl(args, gadt) =>
      let gadtDoc = switch gadt {
      | Some(typ) => Doc.concat(list[Doc.text(": "), printTypExpr(typ, cmtTbl)])
      | None => Doc.nil
      }
      
      Doc.concat(list[
        printConstructorArguments(~indent=false, args, cmtTbl),
        gadtDoc,
      ])
    }
    
    let name = printComments(
      Doc.text(constr.pext_name.txt),
      cmtTbl,
      constr.pext_name.loc,
    )
    
    let doc = Doc.group(
      Doc.concat(list[
        printAttributes(constr.pext_attributes),
        Doc.text("exception "),
        name,
        kind,
      ]),
    )
    printComments(doc, cmtTbl, constr.pext_loc)
  }
  
  and printExtensionConstructor = (
    constr: Parsetree.extension_constructor,
    cmtTbl,
    i,
  ) => {
    let attrs = printAttributes(constr.pext_attributes)
    let bar = if i > 0 {
      Doc.text("| ")
    } else {
      Doc.ifBreaks(Doc.text("| "), Doc.nil)
    }
    
    let kind = switch constr.pext_kind {
    | Pext_rebind(longident) =>
      Doc.indent(
        Doc.concat(list[
          Doc.text(" ="),
          Doc.line,
          printLongidentLocation(longident, cmtTbl),
        ]),
      )
    | Pext_decl(Pcstr_tuple(list[]), None) => Doc.nil
    | Pext_decl(args, gadt) =>
      let gadtDoc = switch gadt {
      | Some(typ) => Doc.concat(list[Doc.text(": "), printTypExpr(typ, cmtTbl)])
      | None => Doc.nil
      }
      
      Doc.concat(list[
        printConstructorArguments(~indent=false, args, cmtTbl),
        gadtDoc,
      ])
    }
    
    let name = printComments(
      Doc.text(constr.pext_name.txt),
      cmtTbl,
      constr.pext_name.loc,
    )
    
    Doc.concat(list[bar, Doc.group(Doc.concat(list[attrs, name, kind]))])
  }
  
  let printImplementation = (s: Parsetree.structure, comments) => {
    let cmtTbl = CommentTable.make()
    CommentTable.walkStructure(s, cmtTbl, comments)
    
    let doc = printStructure(s, cmtTbl)
    
    let stringDoc = Doc.toString(~width=80, doc)
    print_string(stringDoc)
  }
  
  let printInterface = (s: Parsetree.signature, comments) => {
    let cmtTbl = CommentTable.make()
    CommentTable.walkSignature(s, cmtTbl, comments)
    let stringDoc = Doc.toString(~width=80, printSignature(s, cmtTbl))
    print_string(stringDoc)
  }
}

module Clflags: {
  let recover: ref<bool>
  let profile: ref<bool>
  let bench: ref<bool>
  let print: ref<string>
  let files: ref<list<string>>
  
  let parse: unit => unit
} = {
  let recover = ref(false)
  let profile = ref(false)
  let bench = ref(false)
  let setRecover = () => recover := true
  
  let files = ref(list[])
  let addFilename = filename => files := list[filename, ...&files]
  
  let print = ref("")
  
  let usage = "Usage: napkinscript <options> <file>\nOptions are:"
  
  let spec = list[
    ("-recover", Arg.Unit(() => recover := true), "Emit partial ast"),
    ("-bench", Arg.Unit(() => bench := true), "Run internal benchmarks"),
    (
      "-print",
      Arg.String(txt => print := txt),
      "Print either binary, ocaml or ast",
    ),
    (
      "-profile",
      Arg.Unit(() => profile := true),
      "Enable performance profiling",
    ),
  ]
  
  let parse = () => Arg.parse(spec, addFilename, usage)
}

module Driver: {
  let processFile: (~recover: bool, ~target: string, string) => unit
} = {
  type rec file_kind<'a> =
    | Structure: file_kind<Parsetree.structure>
    | Signature: file_kind<Parsetree.signature>
  
  let parse = (type a, kind: file_kind<a>, p): a =>
    switch kind {
    | Structure => NapkinScript.parseStructure(p)
    | Signature => NapkinScript.parseSignature(p)
    }
  
  let parseFile = (kind, filename) => {
    let src = if String.length(filename) > 0 {
      IO.readFile(filename)
    } else {
      IO.readStdin()
    }
    
    let p = Parser.make(src, filename)
    let ast = parse(kind, p)
    let report = switch p.diagnostics {
    | list[] => None
    | diagnostics =>
      Some(
        Diagnostics.makeReport(p.diagnostics, Bytes.to_string(p.scanner.src)),
      )
    }
    
    (ast, report, p)
  }
  
  let parseImplementation = filename => parseFile(Structure, filename)
  
  let parseInterface = filename => parseFile(Signature, filename)
  
  let process = (parseFn, printFn, recover, filename) => {
    let (ast, report, parserState) = Profile.record(~name="parser", () =>
      parseFn(filename)
    )
    
    switch report {
    | Some(report) when recover == true =>
      printFn(ast, parserState)
      prerr_string(report)
    | Some(report) =>
      prerr_string(report)
      exit(1)
    | None => printFn(ast, parserState)
    }
  }
  
  type rec action =
    | ProcessImplementation
    | ProcessInterface
  
  let printImplementation = (~target, filename, ast, _parserState) =>
    switch target {
    | "ml" | "ocaml" => Pprintast.structure(Format.std_formatter, ast)
    | "ns" | "napkinscript" =>
      Printer.printImplementation(ast, List.rev(_parserState.Parser.comments))
    | "ast" => Printast.implementation(Format.std_formatter, ast)
    | _ =>
      output_string(stdout, Config.ast_impl_magic_number)
      output_value(stdout, filename)
      output_value(stdout, ast)
    }
  
  let printInterface = (~target, filename, ast, _parserState) =>
    switch target {
    | "ml" | "ocaml" => Pprintast.signature(Format.std_formatter, ast)
    | "ns" | "napkinscript" =>
      Printer.printInterface(ast, List.rev(_parserState.Parser.comments))
    | "ast" => Printast.interface(Format.std_formatter, ast)
    | _ =>
      output_string(stdout, Config.ast_intf_magic_number)
      output_value(stdout, filename)
      output_value(stdout, ast)
    }
  
  let processFile = (~recover, ~target, filename) =>
    try {
      let len = String.length(filename)
      let action = if len > 0 && String.get(filename, len - 1) == 'i' {
        ProcessInterface
      } else {
        ProcessImplementation
      }
      
      switch action {
      | ProcessImplementation =>
        process(
          parseImplementation,
          printImplementation(~target, filename),
          recover,
          filename,
        )
      | ProcessInterface =>
        process(
          parseInterface,
          printInterface(~target, filename),
          recover,
          filename,
        )
      }
    } catch {
    | Failure(txt) =>
      prerr_string(txt)
      prerr_newline()
      exit(1)
    | _ => exit(1)
    }
}

module Benchmarks: {
  let run: unit => unit
} = {
  type rec action = Parse | Print
  let string_of_action = action =>
    switch action {
    | Parse => "parser"
    | Print => "printer"
    }
  
  type rec lang = Ocaml | Napkin
  let string_of_lang = lang =>
    switch lang {
    | Ocaml => "ocaml"
    | Napkin => "napkinscript"
    }
  
  let parseOcaml = (src, filename) => {
    let lexbuf = Lexing.from_string(src)
    Location.init(lexbuf, filename)
    Parse.implementation(lexbuf)
  }
  
  let parseNapkin = (src, filename) => {
    let p = Parser.make(src, filename)
    NapkinScript.parseStructure(p)
  }
  
  let benchmark = (~filename, ~lang, ~action) => {
    let src = IO.readFile(filename)
    let name =
      filename ++ " " ++ string_of_lang(lang) ++ " " ++ string_of_action(action)
    
    let benchmarkFn = switch (lang, action) {
    | (Napkin, Parse) =>
      _ => {
        let _ = Sys.opaque_identity(parseNapkin(src, filename))
        ()
      }
    | (Ocaml, Parse) =>
      _ => {
        let _ = Sys.opaque_identity(parseOcaml(src, filename))
        ()
      }
    | (Napkin, Print) =>
      let p = Parser.make(src, filename)
      let ast = NapkinScript.parseStructure(p)
      _ => {
        let _ = Sys.opaque_identity(
          {
            let cmtTbl = CommentTable.make()
            let comments = List.rev(p.Parser.comments)
            let () = CommentTable.walkStructure(ast, cmtTbl, comments)
            Doc.toString(~width=80, Printer.printStructure(ast, cmtTbl))
          },
        )
        ()
      }
    | _ => _ => ()
    }
    
    let b = Benchmark.make(~name, ~f=benchmarkFn, ())
    Benchmark.launch(b)
    Benchmark.report(b)
  }
  
  let run = () => {
    benchmark("./benchmarks/RedBlackTreeNapkin.ml", Napkin, Parse)
    benchmark("./benchmarks/RedBlackTreeOcaml.ml", Ocaml, Parse)
    
    benchmark("./benchmarks/RedBlackTreeNapkin.ml", Napkin, Print)
    benchmark("./benchmarks/RedBlackTreeNoCommentsNapkin.ml", Napkin, Print)
  }
}

let () = {
  Clflags.parse()
  if &Clflags.bench {
    Benchmarks.run()
    exit(0)
  }
  let () = switch &Clflags.files {
  | list[file, ..._] as files =>
    List.iter(
      filename =>
        Driver.processFile(
          ~recover=&Clflags.recover,
          ~target=&Clflags.print,
          filename,
        ),
      files,
    )
  | list[] =>
    Driver.processFile(~recover=&Clflags.recover, ~target=&Clflags.print, "")
  }
  
  if &Clflags.profile {
    Profile.print()
  }
  if &Clflags.bench {
    Benchmarks.run()
  }
  exit(0)
}


