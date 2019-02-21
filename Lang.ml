module CharacterCodes = struct
  let eol = -1

  let space = 0x0020
  let newline = 0x0A (* \n *)
  let lineFeed = 0x0A (* \n *)
  let carriageReturn = 0x0D  (* \r *)
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

  module Lower = struct
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
  end

  module Upper = struct
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
  end

  let isLetter ch =
    Lower.a <= ch && ch <= Lower.z ||
    Upper.a <= ch && ch <= Upper.z

  let isUpperCase ch =
    Upper.a <= ch && ch <= Upper.z

  let isDigit ch = _0 <= ch && ch <= _9

    (*
      // ES5 7.3:
      // The ECMAScript line terminator characters are listed in Table 3.
      //     Table 3: Line Terminator Characters
      //     Code Unit Value     Name                    Formal Name
      //     \u000A              Line Feed               <LF>
      //     \u000D              Carriage Return         <CR>
      //     \u2028              Line separator          <LS>
      //     \u2029              Paragraph separator     <PS>
      // Only the characters in Table 3 are treated as line terminators. Other new line or line
      // breaking characters are treated as white space but not as line terminators.
  *)
  let isLineBreak ch =
       ch == lineFeed
    || ch == carriageReturn
    || ch == lineSeparator
    || ch == paragraphSeparator

end

module Token = struct
  type t =
    | Open
    | True | False
    | Char of char
    | Int of string
    | Float of string
    | String of string
    | Lident of string
    | Uident of string
    | As
    | Dot | DotDot | DotDotDot
    | Bang
    | Semicolon
    | Let
    | And
    | Rec
    | Underscore
    | SingleQuote
    | Equal | EqualEqual | EqualEqualEqual
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
    | Forwardslash | ForwardslashDot | TupleEnding
    | Asterisk | AsteriskDot | Exponentiation
    | Minus | MinusDot
    | Plus | PlusDot | PlusPlus
    | GreaterThan
    | LessThan
    | LessThanSlash
    | Hash | HashEqual | HashHash
    | Assert
    | Lazy
    | Tilde
    | Question
    | If | Else | For | In | To | Downto | While | Switch
    | When
    | EqualGreater | MinusGreater
    | External
    | Typ
    | Private
    | Mutable
    | Constraint
    | Include
    | Module
    | Of
    | With
    | Mod | Land | Lor | Lxor
    | Lsl | Lsr | Asr
    | Band (* Bitwise and: & *)
    | BangEqual | BangEqualEqual
    | LessEqual | GreaterEqual
    | ColonEqual
    | At
    | Comment of string
    | List
    | TemplateTail of string
    | TemplatePart of string
    | Backtick
    | Export
    | BarGreater

  let precedence = function
    | HashEqual | ColonEqual -> 1
    | Lor -> 2
    | Land -> 3
    | EqualEqual | EqualEqualEqual | LessThan | GreaterThan
    | BangEqual | BangEqualEqual | LessEqual | GreaterEqual | BarGreater -> 4
    | Plus | PlusDot | Minus | MinusDot | Lxor | PlusPlus -> 5
    | Asterisk | AsteriskDot | Forwardslash | ForwardslashDot  | Lsl | Lsr | Mod -> 6
    | Exponentiation -> 7
    | Hash | HashHash | MinusGreater -> 8
    | Dot -> 9
    | _ -> 0

  let toString = function
    | Open -> "open"
    | True -> "true" | False -> "false"
    | Char c -> "'" ^ (Char.escaped c) ^ "'"
    | String s -> s
    | Lident str -> str
    | Uident str -> str
    | Dot -> "." | DotDot -> ".." | DotDotDot -> "..."
    | Int i -> "int " ^ i
    | Float f -> "Float: " ^ f
    | Bang -> "!"
    | Semicolon -> ";"
    | Let -> "let"
    | And -> "and"
    | Rec -> "rec"
    | Underscore -> "_"
    | SingleQuote -> "'"
    | Equal -> "=" | EqualEqual -> "==" | EqualEqualEqual -> "==="
    | Eof -> "eof"
    | Bar -> "|"
    | As -> "as"
    | Lparen -> "(" | Rparen -> ")"
    | Lbracket -> "[" | Rbracket -> "]"
    | Lbrace -> "{" | Rbrace -> "}"
    | Colon -> ":"
    | Comma -> ","
    | Minus -> "-" | MinusDot -> "-."
    | Plus -> "+" | PlusDot -> "+." | PlusPlus -> "++"
    | Backslash -> "\\"
    | Forwardslash -> "/" | ForwardslashDot -> "/."
    | TupleEnding -> "/ (tuple ending)"
    | Exception -> "exception"
    | Hash -> "#" | HashHash -> "##" | HashEqual -> "#="
    | GreaterThan -> ">"
    | LessThan -> "<"
    | LessThanSlash -> "</"
    | Asterisk -> "*" | AsteriskDot -> "*." | Exponentiation -> "**"
    | Assert -> "assert"
    | Lazy -> "lazy"
    | Tilde -> "tilde"
    | Question -> "?"
    | If -> "if"
    | Else -> "else"
    | For -> "for"
    | In -> "in"
    | To -> "to"
    | Downto -> "downto"
    | While -> "while"
    | Switch -> "switch"
    | When -> "when"
    | EqualGreater -> "=>" | MinusGreater -> "->"
    | External -> "external"
    | Typ -> "type"
    | Private -> "private"
    | Constraint -> "constraint"
    | Mutable -> "mutable"
    | Include -> "include"
    | Module -> "module"
    | Of -> "of"
    | With -> "with"
    | Mod -> "mod"  | Lor -> "||" | Lxor -> "lxor"
    | Lsl -> "lsl"| Lsr -> "lsr" | Asr -> "asr"
    | Band -> "&" | Land -> "&&"
    | BangEqual -> "!=" | BangEqualEqual -> "!=="
    | GreaterEqual -> ">=" | LessEqual -> "<="
    | ColonEqual -> ":="
    | At -> "@"
    | Comment text -> "Comment(" ^ text ^ ")"
    | List -> "list"
    | TemplatePart text -> text ^ "${"
    | TemplateTail text -> "TemplateTail(" ^ text ^ ")"
    | Backtick -> "`"
    | Export -> "export"
    | BarGreater -> "|>"

  let keywordTable =
    let keywords = [|
      "true", True;
      "false", False;
      "open", Open;
      "let", Let;
      "rec", Rec;
      "and", And;
      "as", As;
      "exception", Exception;
      "assert", Assert;
      "lazy", Lazy;
      "if", If;
      "else", Else;
      "for", For;
      "in", In;
      "to", To;
      "downto", Downto;
      "while", While;
      "switch", Switch;
      "when", When;
      "external", External;
      "type", Typ;
      "private", Private;
      "mutable", Mutable;
      "constraint", Constraint;
      "include", Include;
      "module", Module;
      "of", Of;
      "mod", Mod; "land", Land; "lor", Lor; "lxor", Lxor;
      "lsl", Lsl;
      "lsr", Lsr;
      "asr", Asr;
      "list", List;
      "export", Export;
    |] in
    let t = Hashtbl.create 50 in
    Array.iter (fun (k, v) ->
      Hashtbl.add t k v;
    ) keywords;
    t

  let isKeyword = function
    | True | False | Open | Let | Rec | And | As
    | Exception | Assert | Lazy | If | Else | For | In | To
    | Downto | While | Switch | When | External | Typ | Private
    | Mutable | Constraint | Include | Module | Of | Mod
    | Land | Lor | Lxor | Lsl | Lsr | Asr | List | Export -> true
    | _ -> false

  let lookupKeyword str =
    try Hashtbl.find keywordTable str with
    | Not_found ->
      if CharacterCodes.isUpperCase (int_of_char str.[0]) then
        Uident str
      else Lident str
end

module Lex = struct
  type mode = Normal | Template | Tuple | Jsx

  type lexbuf = {
    filename: string;
    src: bytes;
    mutable ch: int; (* current character *)
    mutable offset: int; (* character offset *)
    mutable rdOffset: int; (* reading offset (position after current character) *)
    mutable lineOffset: int; (* current line offset *)
    mutable lnum: int; (* current line number *)
    mutable mode: mode;
  }

  let setNormalMode lexbuf =
    lexbuf.mode <- Normal

  let setTemplateMode lexbuf =
    lexbuf.mode <- Template

  let setTupleMode lexbuf =
    lexbuf.mode <- Tuple

  let setJsxMode lexbuf =
    lexbuf.mode <- Jsx

  let position lexbuf = Lexing.{
    pos_fname = lexbuf.filename;
    (* line number *)
    pos_lnum = lexbuf.lnum;
    (* offset of the beginning of the line (number
       of characters between the beginning of the lexbuf and the beginning
       of the line) *)
    pos_bol = lexbuf.lineOffset;
    (* [pos_cnum] is the offset of the position (number of
       characters between the beginning of the lexbuf and the position). *)
    pos_cnum = lexbuf.offset;
  }

  let printPos p =
    print_endline ("cnum: " ^ (string_of_int p.Lexing.pos_cnum));
    print_endline ("lnum: " ^ (string_of_int p.Lexing.pos_lnum));
    print_endline ("beginning of line: " ^ (string_of_int p.Lexing.pos_bol));
    print_endline ("-------------------")

  let next lexbuf =
    if lexbuf.rdOffset < (Bytes.length lexbuf.src) then begin
      lexbuf.offset <- lexbuf.rdOffset;
      let ch = Bytes.get lexbuf.src lexbuf.rdOffset in
      if ch = '\n' then begin
        lexbuf.lineOffset <- lexbuf.offset + 1;
        lexbuf.lnum <- lexbuf.lnum + 1
      end;
      lexbuf.rdOffset <- lexbuf.rdOffset + 1;
      lexbuf.ch <- int_of_char ch
    end else begin
      lexbuf.offset <- Bytes.length lexbuf.src;
      lexbuf.ch <- -1
    end

  let peek lexbuf =
    if lexbuf.rdOffset < (Bytes.length lexbuf.src) then
      int_of_char (Bytes.unsafe_get lexbuf.src lexbuf.rdOffset)
    else
      -1

  let make b filename =
    let lexbuf = {
      filename;
      src = b;
      ch = CharacterCodes.space;
      offset = 0;
      rdOffset = 0;
      lineOffset = 0;
      lnum = 1;
      mode = Normal;
    } in
    next lexbuf;
    lexbuf


  (* black magic, use sparingly! *)
  let lookahead lexbuf callback =
    let lexbufCopy = {lexbuf with filename = lexbuf.filename} in
    callback lexbufCopy

  let skipWhitespace lexbuf =
    while
      (lexbuf.ch == CharacterCodes.space) ||
      (lexbuf.ch == CharacterCodes.tab) ||
      (lexbuf.ch == CharacterCodes.newline)
    do next lexbuf
    done

  let lexIdentifier lexbuf =
    let startOff = lexbuf.offset in
    while (
      CharacterCodes.isLetter lexbuf.ch ||
      CharacterCodes.isDigit lexbuf.ch ||
      lexbuf.ch == CharacterCodes.underscore
    ) do
      next lexbuf
    done;
    let str = Bytes.sub_string lexbuf.src startOff (lexbuf.offset - startOff) in
    Token.lookupKeyword str

  (* float: (0…9) { 0…9∣ _ } [. { 0…9∣ _ }] [(e∣ E) [+∣ -] (0…9) { 0…9∣ _ }]   *)
  let lexNumber lexbuf =
    let startOff = lexbuf.offset in
    while CharacterCodes.isDigit lexbuf.ch do
      next lexbuf
    done;
    (* floats *)
    if CharacterCodes.dot == lexbuf.ch then (
      next lexbuf;
      while CharacterCodes.isDigit lexbuf.ch do
        next lexbuf
      done;
      let str = Bytes.sub_string lexbuf.src startOff (lexbuf.offset - startOff) in
      Token.Float str
    ) else (
      let str = Bytes.sub_string lexbuf.src startOff (lexbuf.offset - startOff) in
      Token.Int str
    )

  let lexString lexbuf =
    let didEnd = ref false in

    let buffer = Buffer.create 256 in
    while not !didEnd do
      if lexbuf.ch == CharacterCodes.doubleQuote then (
        didEnd := true;
      ) else if lexbuf.ch == CharacterCodes.backslash then (
        next lexbuf;

        let char_for_backslash = function
          | 110 -> '\010'
          | 114 -> '\013'
          | 98 -> '\008'
          | 116 -> '\009'
          | c   -> Char.chr c
        in

        Buffer.add_char buffer (char_for_backslash lexbuf.ch);
      ) else (
        Buffer.add_char buffer (Char.chr lexbuf.ch)
      );
      next lexbuf;
    done;
    Token.String (
      Buffer.contents buffer
    )

  let lexSingleLineComment lexbuf =
    let startOff = lexbuf.offset in
    while not (CharacterCodes.isLineBreak lexbuf.ch) &&
      lexbuf.rdOffset < (Bytes.length lexbuf.src)
    do
      next lexbuf
    done;
    next lexbuf;
    Token.Comment (
      Bytes.sub_string lexbuf.src startOff (lexbuf.offset - 1 - startOff)
    )

  (* TODO: error handling unclosed multi-line comment, i.e. missing */ *)
  let lexMultiLineComment lexbuf =
    let startOff = lexbuf.offset in
    while not (
      lexbuf.ch == CharacterCodes.asterisk &&
      peek lexbuf == CharacterCodes.forwardslash
    ) do next lexbuf done;
    next lexbuf;
    next lexbuf;
    Token.Comment (
      Bytes.sub_string lexbuf.src startOff (lexbuf.offset - 1 - startOff)
    )

  exception Unknown_token of int

  let lexTemplate lexbuf =
    let startOff = lexbuf.offset in

    let rec scan lexbuf =
      if lexbuf.ch == CharacterCodes.backtick then (
        next lexbuf;
        let contents =
          Bytes.sub_string lexbuf.src startOff (lexbuf.offset - 1 - startOff)
        in
        setNormalMode lexbuf;
        Token.TemplateTail contents
      ) else if lexbuf.ch == CharacterCodes.dollar &&
                peek lexbuf == CharacterCodes.lbrace
        then (
          next lexbuf; (* consume $ *)
          next lexbuf; (* consume { *)
          let contents =
            Bytes.sub_string lexbuf.src startOff (lexbuf.offset - 2 - startOff)
          in
          setNormalMode lexbuf;
          Token.TemplatePart contents
      ) else (
        next lexbuf;
        scan lexbuf
      )
    in
    scan lexbuf

  let rec lex lexbuf =
    skipWhitespace lexbuf;
    let startPos = position lexbuf in
    let ch = lexbuf.ch in
    let token = if lexbuf.mode = Template then
      lexTemplate lexbuf
    else if ch == CharacterCodes.underscore then (
      let nextCh = peek lexbuf in
      if nextCh == CharacterCodes.underscore || CharacterCodes.isLetter nextCh then
        lexIdentifier lexbuf
      else (
        next lexbuf;
        Token.Underscore
      )
    ) else if CharacterCodes.isLetter ch then
      lexIdentifier lexbuf
    else if CharacterCodes.isDigit ch then
      lexNumber lexbuf
    else begin
      next lexbuf;
      if ch == CharacterCodes.dot then
        if lexbuf.ch == CharacterCodes.dot then (
          next lexbuf;
          if lexbuf.ch == CharacterCodes.dot then (
            next lexbuf;
            Token.DotDotDot
          ) else (
            Token.DotDot
          )
        ) else (
          Token.Dot
        )
      else if ch == CharacterCodes.doubleQuote then
        lexString lexbuf
      else if ch == CharacterCodes.singleQuote then
        Token.SingleQuote
      else if ch == CharacterCodes.bang then
        if lexbuf.ch == CharacterCodes.equal then (
          next lexbuf;
          if lexbuf.ch == CharacterCodes.equal then (
            next lexbuf;
            Token.BangEqualEqual
          ) else (
            Token.BangEqual
          )
        ) else (
          Token.Bang
        )
      else if ch == CharacterCodes.semicolon then
        Token.Semicolon
      else if ch == CharacterCodes.equal then (
        if lexbuf.ch == CharacterCodes.greaterThan then (
          next lexbuf;
          Token.EqualGreater
        ) else if lexbuf.ch == CharacterCodes.equal then (
          next lexbuf;
          if lexbuf.ch == CharacterCodes.equal then (
            next lexbuf;
            Token.EqualEqualEqual
          ) else (
            Token.EqualEqual
          )
        ) else (
          Token.Equal
        )
      ) else if ch == CharacterCodes.bar then
        if lexbuf.ch == CharacterCodes.bar then (
          next lexbuf;
          Token.Lor
        ) else if lexbuf.ch == CharacterCodes.greaterThan then (
          next lexbuf;
          Token.BarGreater
        ) else (
          Token.Bar
        )
      else if ch == CharacterCodes.ampersand then
        if lexbuf.ch == CharacterCodes.ampersand then (
          next lexbuf;
          Token.Land
        ) else (
          Token.Band
        )
      else if ch == CharacterCodes.lparen then
        Token.Lparen
      else if ch == CharacterCodes.rparen then
        Token.Rparen
      else if ch == CharacterCodes.lbracket then
        Token.Lbracket
      else if ch == CharacterCodes.rbracket then
        Token.Rbracket
      else if ch == CharacterCodes.lbrace then
        Token.Lbrace
      else if ch == CharacterCodes.rbrace then
        Token.Rbrace
      else if ch == CharacterCodes.comma then
        Token.Comma
      else if ch == CharacterCodes.colon then
       if lexbuf.ch == CharacterCodes.equal then(
          next lexbuf;
          Token.ColonEqual
        ) else (
          Token.Colon
        )
      else if ch == CharacterCodes.backslash then
        Token.Backslash
      else if ch == CharacterCodes.forwardslash then
        if lexbuf.ch == CharacterCodes.forwardslash then (
          next lexbuf;
          lexSingleLineComment lexbuf
        ) else if (lexbuf.ch == CharacterCodes.asterisk) then (
          next lexbuf;
          lexMultiLineComment lexbuf
        ) else (
          if lexbuf.mode = Tuple then
            lexForwardSlashOrTupleEnding lexbuf
          else
          Token.Forwardslash
        )
      else if ch == CharacterCodes.minus then
        if lexbuf.ch == CharacterCodes.dot then (
          next lexbuf;
          Token.MinusDot
        ) else if lexbuf.ch == CharacterCodes.greaterThan then (
          next lexbuf;
          Token.MinusGreater;
        ) else (
          Token.Minus
        )
      else if ch == CharacterCodes.plus then
        if lexbuf.ch == CharacterCodes.dot then (
          next lexbuf;
          Token.PlusDot
        ) else if lexbuf.ch == CharacterCodes.plus then (
          next lexbuf;
          Token.PlusPlus
        ) else (
          Token.Plus
        )
      else if ch == CharacterCodes.greaterThan then
        if lexbuf.ch == CharacterCodes.equal then(
          next lexbuf;
          Token.GreaterEqual
        ) else (
          Token.GreaterThan
        )
      else if ch == CharacterCodes.lessThan then
        if lexbuf.mode = Jsx then (
          if lexbuf.ch == CharacterCodes.forwardslash then
            let () = next lexbuf in
            Token.LessThanSlash
          else
            Token.LessThan
        ) else if lexbuf.ch == CharacterCodes.equal then (
          next lexbuf;
          Token.LessEqual
        ) else (
          Token.LessThan
        )
      else if ch == CharacterCodes.hash then
        if lexbuf.ch == CharacterCodes.hash then(
          next lexbuf;
          Token.HashHash
        ) else if lexbuf.ch == CharacterCodes.equal then(
          next lexbuf;
          Token.HashEqual
        ) else (
          Token.Hash
        )
      else if ch == CharacterCodes.asterisk then
        if lexbuf.ch == CharacterCodes.asterisk then (
          next lexbuf;
          Token.Exponentiation;
        ) else (
          Token.Asterisk
        )
      else if ch == CharacterCodes.tilde then
        Token.Tilde
      else if ch == CharacterCodes.question then
        Token.Question
      else if ch == CharacterCodes.at then
        Token.At
      else if ch == CharacterCodes.backtick  then
        Token.Backtick
      else if ch == -1 then
        Token.Eof
      else
        raise (Unknown_token lexbuf.ch)
    end
    in
    let endPos = position lexbuf in
    (startPos, endPos, token)

  and lexForwardSlashOrTupleEnding lexbuf =
    let cb lexbuf =
      let (_, _, token) = lex lexbuf in
      match token with
      | Int _ | Lident _ | Uident _ | Lparen | Lbrace | Minus | Plus
      | Lazy | If | For | While | Switch | At -> Token.Forwardslash
      | _ -> TupleEnding
    in
    let result = lookahead lexbuf cb in
    setNormalMode lexbuf;
    result

  let rescanLessThan lexbuf =
    if lexbuf.ch == CharacterCodes.forwardslash then
      let () = next lexbuf in
      Token.LessThanSlash
    else
      Token.LessThan

end

module Report = struct
  type circumstance =
    | OpenDescription (* open Belt *)
    | ModuleLongIdent (* Foo or Foo.Bar *)
    | Ternary (* condExpr ? trueExpr : falseExpr *)
    | Es6ArrowExpr
    | Jsx
    | JsxAttribute
    | ExprOperand
    | ExprUnary
    | ExprSetField
    | ExprBinaryAfterOp of Token.t
    | SeqExpr
    | ExprCall
    | ExprArrayAccess
    | ExprArrayMutation
    | ExprIf
    | IfCondition | IfBranch | ElseBranch
    | TypeExpression
    | External
    | PatternMatching
    | LetBinding

  (* type context = circumstance * token *)

  let circumstanceToString = function
    | OpenDescription -> "an open description"
    | ModuleLongIdent -> "a module identifier"
    | Ternary -> "a ternary expression"
    | Es6ArrowExpr -> "an es6 arrow function"
    | Jsx -> "a jsx expression"
    | JsxAttribute -> "a jsx attribute"
    | ExprOperand -> "a basic expression"
    | ExprUnary -> "a unary expression"
    | ExprBinaryAfterOp op -> "an expression after the operator \"" ^ Token.toString op  ^ "\""
    | ExprIf -> "an if expression"
    | IfCondition -> "the condition of an if expression"
    | IfBranch -> "the true-branch of an if expression"
    | ElseBranch -> "the else-branch of an if expression"
    | TypeExpression -> "a type"
    | External -> "an external"
    | PatternMatching -> "the cases of a pattern match"
    | SeqExpr -> "a block with expressions"
    | ExprSetField -> "a record field mutation"
    | ExprCall -> "a function application"
    | ExprArrayAccess -> "an array access expression"
    | ExprArrayMutation -> "an array mutation"
    | LetBinding -> "a let binding"

  let parseContext stack =
    match stack with
    | ((ExprOperand, _)::cs) ->
        begin match cs with
        | (ExprBinaryAfterOp _ as c, _)::cs ->
          circumstanceToString c
        | _ -> "a basic expression"
        end
    | ((c, _)::cs) ->
        circumstanceToString c
    | [] -> "your code"

  (* let parseMeaningFullCircumstance stack = *)
    (* let peek stack  *)


  (* let parseCircumstances css = *)


  type problem =
    | Unexpected of Token.t
    | Expected of (Token.t * circumstance option)
    | OneOf of Token.t list
    | Message of string
    | Uident
    | Lident
    | Unbalanced of Token.t

  type parseError = Lexing.position * problem

end

module LangParser = struct
  let mkLoc startLoc endLoc = Location.{
    loc_start = startLoc;
    loc_end = endLoc;
    loc_ghost = false;
  }

  module Parser = struct
    type t = {
      lexbuf: Lex.lexbuf;
      mutable token: Token.t;
      mutable startPos: Lexing.position;
      mutable endPos: Lexing.position;
      mutable prevEndPos: Lexing.position;
      mutable breadcrumbs: (Report.circumstance * Lexing.position) list;
      mutable errors: Report.parseError list
    }

    let rec next p =
      let (startPos, endPos, token) = Lex.lex p.lexbuf in
      p.prevEndPos <- p.endPos;
      p.token <- token;
      p.startPos <- startPos;
      p.endPos <- endPos;
      match p.token with
      | Comment _ -> next p
      | _ -> ()

    let make src filename =
      let lexbuf = Lex.make (Bytes.of_string src) filename in
      let parserState = {
        lexbuf;
        token = Token.Eof;
        startPos = Lexing.dummy_pos;
        prevEndPos = Lexing.dummy_pos;
        endPos = Lexing.dummy_pos;
        breadcrumbs = [];
        errors = [];
      } in
      next parserState;
      parserState

    let leaveBreadcrumb p circumstance =
      let crumb = (circumstance, p.startPos) in
      p.breadcrumbs <- crumb::p.breadcrumbs

    let eatBreadcrumb p =
      match p.breadcrumbs with
      | [] -> ()
      | _::crumbs -> p.breadcrumbs <- crumbs

    let optional p token =
      if p.token = token then
        let () = next p in true
      else
        false

    exception ParseError of Report.parseError

    (* ?circumstance indicates an optional circumstance
     * The current reasoning is to be able to say:
     *   "I'm expecting a ":", which signals the start of a type"
     * the "which signals the start of a type", can be deduced from the
     * circumstance (if there's one) *)
    let expectExn ?circumstance token p =
      if p.token = token then
        next p
      else
        raise (ParseError (p.startPos, Report.Expected (token, circumstance)))

    let expect ?circumstance token p =
      if p.token = token then
        next p
      else
        let error = (p.startPos, Report.Expected (token, circumstance)) in
        p.errors <- error::p.errors

    let lookahead p callback =
      (* is this copying correct? *)
      let lexbufCopy = {p.lexbuf with filename = p.lexbuf.filename} in
      let parserStateCopy = {p with lexbuf = lexbufCopy} in
      callback parserStateCopy
  end


type context =
  | StructureExpr
  | OrdinaryExpr
  | TernaryTrueBranchExpr
  | WhenExpr

let getClosingToken = function
  | Token.Lparen -> Token.Rparen
  | Lbrace -> Rbrace
  | Lbracket -> Rbracket
  | _ -> assert false

let rec goToClosing closingToken state =
  match (state.Parser.token, closingToken) with
  | (Rparen, Token.Rparen) | (Rbrace, Rbrace) | (Rbracket, Rbracket) ->
    Parser.next state; ()
  | (Token.Lbracket | Lparen | Lbrace) as t, _ ->
    Parser.next state;
    goToClosing (getClosingToken t) state;
    goToClosing closingToken state
  | ((Rparen | Token.Rbrace | Rbracket | Eof), _)  ->
    raise (Parser.ParseError (state.startPos, Report.Unbalanced closingToken))
  | _ ->
    Parser.next state;
    goToClosing closingToken state

  (* Madness *)
  let isEs6ArrowExpression ~inTernary p =
    Parser.lookahead p (fun state ->
      match state.Parser.token with
      | Lident _ | Underscore ->
        Parser.next state;
        begin match state.Parser.token with
        (* Don't think that this valid
         * Imagine: let x = (a: int)
         * This is a parenthesized expression with a type constraint, wait for
         * the arrow *)
        (* | Colon when not inTernary -> true *)
        | EqualGreater -> true
        | _ -> false
        end
      | Lparen ->
        Parser.next state;
        begin match state.token with
        | Rparen ->
          Parser.next state;
          begin match state.Parser.token with
          | Colon when not inTernary -> true
          | EqualGreater -> true
          | _ -> false
          end
        | Dot (* uncurried *) -> true
        | Tilde -> true
        | Backtick -> false (* (` always indicates the start of an expr, can't be es6 parameter *)
        | _ ->
          goToClosing Rparen state;
          begin match state.Parser.token with
          | EqualGreater | Lbrace -> true
          | Colon when not inTernary -> true
          | _ -> false
          end
        end
      | _ -> false)

  let isEs6ArrowType p =
    Parser.lookahead p (fun state ->
      match state.Parser.token with
      | Lparen ->
        Parser.next state;
        begin match state.Parser.token with
        | Rparen ->
          Parser.next state;
          begin match state.Parser.token with
          | EqualGreater -> true
          | _ -> false
          end
        | Tilde | Dot -> true
        | _ ->
          goToClosing Rparen state;
          begin match state.Parser.token with
          | EqualGreater  -> true
          | _ -> false
          end
        end
      | _ -> false
    )

  let buildLongident words = match List.rev words with
    | [] -> assert false
    | hd::tl -> List.fold_left (fun p s -> Longident.Ldot (p, s)) (Lident hd) tl

  let makeInfixOperator token startPos endPos =
    let stringifiedToken =
      if token = Token.MinusGreater then "|."
      else if token = Token.PlusPlus then "^"
      else Token.toString token
    in
    let operator = Location.mkloc
      (Longident.Lident stringifiedToken) (mkLoc startPos endPos)
    in
    Ast_helper.Exp.ident operator

  let negateString s =
    if String.length s > 0 && s.[0] = '-'
    then String.sub s 1 (String.length s - 1)
    else "-" ^ s

  let makeUnaryExpr token expr =
    match token, expr.Parsetree.pexp_desc with
    | (Token.Plus | PlusDot), Pexp_constant((Pconst_integer _ | Pconst_float _)) -> expr
    | (Minus | MinusDot), Pexp_constant(Pconst_integer (n,m)) ->
      {expr with pexp_desc = Pexp_constant(Pconst_integer (negateString n,m))}
    | (Minus | MinusDot), Pexp_constant(Pconst_float (n,m)) ->
      {expr with pexp_desc = Pexp_constant(Pconst_float (negateString n,m))}
    | (Token.Plus | PlusDot | Minus | MinusDot ), _ ->
       let operator = "~" ^ Token.toString token in
       Ast_helper.Exp.apply
         (Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident operator)))
         [Nolabel, expr]
    | Token.Bang, _ ->
      Ast_helper.Exp.apply
        (Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident "not")))
        [Nolabel, expr]
    | _ -> expr

  let makeListExpression loc seq extOpt =
    let rec handleSeq = function
      | [] ->
        begin match extOpt with
        | Some ext -> ext
        | None ->
          let loc = {loc with Location.loc_ghost = true} in
          let nil = Location.mkloc (Longident.Lident "[]") loc in
          Ast_helper.Exp.construct ~loc nil None
        end
      | e1 :: el ->
        let exp_el = handleSeq el in
        let loc = mkLoc
          e1.Parsetree.pexp_loc.Location.loc_start
          exp_el.pexp_loc.loc_end
        in
        let arg = Ast_helper.Exp.tuple ~loc [e1; exp_el] in
        Ast_helper.Exp.construct ~loc
          (Location.mkloc (Longident.Lident "::") loc)
          (Some arg)
    in
    handleSeq seq

  let makeListPattern loc seq ext_opt =
    let rec handle_seq = function
      [] ->
        let base_case = match ext_opt with
          | Some ext ->
            ext
          | None ->
            let loc = { loc with Location.loc_ghost = true} in
            let nil = { Location.txt = Longident.Lident "[]"; loc } in
            Ast_helper.Pat.construct ~loc nil None
        in
        base_case
    | p1 :: pl ->
        let pat_pl = handle_seq pl in
        let loc = mkLoc p1.Parsetree.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
        let arg = Ast_helper.Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
        Ast_helper.Pat.mk ~loc (Ppat_construct(Location.mkloc (Longident.Lident "::") loc, Some arg))
    in
    handle_seq seq

    let verifyJsxOpeningClosingName nameExpr closingName =
      match nameExpr.Parsetree.pexp_desc with
      | Pexp_ident openingIdent ->
          let openingName = List.hd (Longident.flatten openingIdent.txt) in
          openingName = closingName
      | _ -> assert false

    let string_of_pexp_ident nameExpr =
      match nameExpr.Parsetree.pexp_desc with
      | Pexp_ident openingIdent ->
        List.hd (Longident.flatten openingIdent.txt)
      | _ -> ""

  (* Ldot (Ldot (Lident "Foo", "Bar"), "baz") *)
  let parseValuePath p =
    let startPos = p.Parser.startPos in
    let rec aux p path =
      match p.Parser.token with
      | List -> Longident.Ldot(path, "list")
      | Lident ident -> Longident.Ldot(path, ident)
      | Uident uident ->
        Parser.next p;
        Parser.expectExn Dot p;
        aux p (Ldot (path, uident))
      | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))
    in
    let ident = match p.Parser.token with
    | List -> Longident.Lident "list"
    | Lident ident -> Longident.Lident ident
    | Uident ident ->
      Parser.next p;
      Parser.expectExn Dot p;
      aux p (Lident ident)
    | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))
    in
    Parser.next p;
    Location.mkloc ident (mkLoc startPos p.prevEndPos)

  (* Parses module identifiers:
       Foo
       Foo.Bar *)
  let parseModuleLongIdent p =
    (* Parser.leaveBreadcrumb p Report.ModuleLongIdent; *)
    let startPos = p.Parser.startPos in
    let rec aux p acc =
      match p.Parser.token with
      | Uident ident ->
        let endPos = p.endPos in
        Parser.next p;
        let lident = (Longident.Ldot (acc, ident)) in
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          aux p lident
        | _ -> Location.mkloc lident (mkLoc startPos endPos)
        end
      | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))
    in
    let moduleIdent = match p.Parser.token with
    | Uident ident ->
      let lident = Longident.Lident ident in
      let endPos = p.endPos in
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        aux p lident
      | _ -> Location.mkloc lident (mkLoc startPos endPos)
      end
    | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))
    in
    (* Parser.eatBreadcrumb p; *)
    moduleIdent

  let parseOpenDescription ~attrs p =
    Parser.leaveBreadcrumb p Report.OpenDescription;
    let startPos = p.Parser.startPos in
    Parser.expectExn Open p;
    let override = if Parser.optional p Token.Bang then
      Asttypes.Override
    else
      Asttypes.Fresh
    in
    let modident = parseModuleLongIdent p in
    Parser.next p;
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    Parser.eatBreadcrumb p;
    Ast_helper.Opn.mk ~loc ~attrs ~override modident

  (* constant	::=	integer-literal   *)
   (* ∣	 float-literal   *)
   (* ∣	 string-literal   *)
  let parseConstant p =
    let constant = match p.Parser.token with
    | Int i -> Parsetree.Pconst_integer (i, None)
    | Float i -> Parsetree.Pconst_float (i, None)
    | String s -> Pconst_string(s, None)
    | _ ->
      raise (Parser.ParseError (p.startPos, Report.Message "I'm expecting a constant like: 1 or \"a string\""))
    in
    Parser.next p;
    constant

  let parseCommaSeparatedList ~closing:closingToken ~f p =
    let rec parse f p nodes =
      let node = f p in
      let hasComma = Parser.optional p Comma in
      if hasComma then
        if p.Parser.token = closingToken then
          List.rev (node::nodes)
        else
          parse f p (node::nodes)
      else if p.token = closingToken || p.token = Eof then
        List.rev (node::nodes)
      else (
        Parser.expect Comma p;
        if p.token = Semicolon then Parser.next p;
        parse f p (node::nodes)
      )
    in
    if p.Parser.token = closingToken then
      []
    else
      parse f p []

  (* let-binding	::=	pattern =  expr   *)
     (* ∣	 value-name  { parameter }  [: typexpr]  [:> typexpr] =  expr   *)
     (* ∣	 value-name :  poly-typexpr =  expr   *)
  (* let parseLetBinding p = *)
    (* let   *)

   (* pattern	::=	value-name   *)
     (* ∣	 _   *)
     (* ∣	 constant   *)
     (* ∣	 pattern as  value-name   *)
     (* ∣	 ( pattern )   *)
     (* ∣	 ( pattern :  typexpr )   *)
     (* ∣	 pattern |  pattern   *)
     (* ∣	 constr  pattern   *)
     (* ∣	 `tag-name  pattern   *)
     (* ∣	 #typeconstr   *)
     (* ∣	 pattern  { , pattern }+   *)
     (* ∣	 { field  [: typexpr]  [= pattern] { ; field  [: typexpr]  [= pattern] }  [; _ ] [ ; ] }   *)
     (* ∣	 [ pattern  { ; pattern }  [ ; ] ]   *)
     (* ∣	 pattern ::  pattern   *)
     (* ∣	 [| pattern  { ; pattern }  [ ; ] |]   *)
     (* ∣	 char-literal ..  char-literal *)
     (*	∣	 exception pattern  *)
  let rec parsePattern p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let pat = match p.Parser.token with
    | Int _ | String _ ->
      let endPos = p.endPos in
      let c = parseConstant p in
      let loc = mkLoc startPos endPos in
      Ast_helper.Pat.constant ~loc c
    | Lparen ->
      Parser.next p;
      begin match p.token with
      | Rparen ->
        Parser.next p;
        let endPos = p.endPos in
        let loc = mkLoc startPos endPos in
        let lid = Location.mkloc (Longident.Lident "()") loc in
        Ast_helper.Pat.construct lid None
      | _ ->
        let pat = parseConstrainedPattern p in
        Parser.expectExn Token.Rparen p;
        let loc = mkLoc startPos p.prevEndPos in
        {pat with ppat_loc = loc}
      end
    | Lbracket ->
      parseArrayPattern ~attrs p
    | Lbrace ->
      parseRecordPattern ~attrs p
    | Forwardslash ->
      parseTuplePattern ~attrs p
    | Underscore ->
      let endPos = p.endPos in
      let loc = mkLoc startPos endPos in
      Parser.next p;
      Ast_helper.Pat.any ~loc ~attrs ()
    | Lident ident ->
      let endPos = p.endPos in
      let loc = mkLoc startPos endPos in
      Parser.next p;
      Ast_helper.Pat.var ~loc ~attrs (Location.mkloc ident loc)
    | Uident _ ->
      let constr = parseModuleLongIdent p in
      begin match p.Parser.token with
      | Lparen ->
        parseConstructorPatternWithArgs p constr startPos attrs
      | _ ->
        Ast_helper.Pat.construct ~loc:constr.loc ~attrs constr None
      end
    (* is this safe in a let-context? let exception Foo
     * probably, local exceptions in seq expressions are
     * exception Foo *)
    | Exception ->
      Parser.next p;
      let pat = parsePattern p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Pat.exception_ ~loc ~attrs pat
    | Lazy ->
      Parser.next p;
      let pat = parsePattern p in
      let loc = mkLoc startPos pat.ppat_loc.loc_end in
      Ast_helper.Pat.lazy_ ~loc ~attrs pat
    | List ->
      parseListPattern ~attrs p
    | unknownToken ->
      raise (Parser.ParseError (p.startPos, Report.Unexpected unknownToken))
    in
    begin match p.token with
    | As ->
      Parser.next p;
      let startLoc = p.startPos in
      begin match p.token with
      | Lident ident ->
        Parser.next p;
        let endLoc = p.startPos in
        let loc = mkLoc startLoc endLoc in
        Ast_helper.Pat.alias ~attrs pat (Location.mkloc ident loc)
      | _ -> raise (Parser.ParseError (p.startPos, Report.Lident))
      end
    | _ -> pat
    end

  and parseConstrainedPattern p =
    let pat = parsePattern p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let typ = parseTypExpr p in
      let loc = mkLoc pat.ppat_loc.loc_start typ.Parsetree.ptyp_loc.loc_end in
      Ast_helper.Pat.constraint_ ~loc pat typ
    | _ -> pat

  (* field  [: typexpr]  [: pattern] *)
  and parseRecordPatternField p =
    let startPos = p.Parser.startPos in
    let label = parseValuePath p in
    let pattern = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parsePattern p
    | _ ->
      Ast_helper.Pat.var
        ~loc:label.loc
        (Location.mkloc (Longident.last label.txt) label.loc)
    in
    (Location.mkloc label.txt (mkLoc startPos pattern.ppat_loc.loc_end), pattern)


   (* { field  [: typexpr]  [= pattern] { ; field  [: typexpr]  [= pattern] }  [; _ ] [ ; ] }  *)
  and parseRecordPattern ~attrs p =
    let startPos = p.startPos in
    Parser.expectExn Lbrace p;
    let firstField = parseRecordPatternField p in
    let rec loop p fields =
      match p.Parser.token with
      | Rbrace ->
        (List.rev fields, Asttypes.Closed)
      | Underscore ->
        Parser.next p;
        ignore (Parser.optional p Comma);
        (List.rev fields, Asttypes.Open)
      | Comma ->
        Parser.next p;
        loop p fields
      | Uident _ | Lident _ ->
        let field = parseRecordPatternField p in
        loop p (field::fields)
      | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))
    in
    let (fields, closedFlag) = loop p [firstField] in
    let endPos = p.endPos in
    Parser.expectExn Rbrace p;
    let loc = mkLoc startPos endPos in
    Ast_helper.Pat.record ~loc ~attrs fields closedFlag

  and parseTuplePattern ~attrs p =
    let startPos = p.startPos in
    Parser.expectExn Forwardslash p;
    let patterns =
      parseCommaSeparatedList ~closing:Forwardslash ~f:parseConstrainedPattern p
    in
    Parser.expectExn Forwardslash p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.tuple ~loc ~attrs patterns

  and parseListPattern ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expectExn List p;
    Parser.expectExn Lparen p;
    let rec loop p patterns = match p.Parser.token with
    | Rparen ->
      Parser.next p;
      patterns
    | Comma -> Parser.next p; loop p patterns
    | DotDotDot ->
      Parser.next p;
      let pattern = parseConstrainedPattern p in
      loop p ((true, pattern)::patterns)
    | _ ->
      let pattern = parseConstrainedPattern p in
      loop p ((false, pattern)::patterns)
    in
    let listPatterns = loop p [] in
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    match listPatterns with
    | (true, pattern)::patterns ->
      let patterns =
        patterns
        |> List.map snd
        |> List.rev
      in
      makeListPattern loc patterns (Some pattern)
    | patterns ->
     let patterns =
        patterns
        |> List.map snd
        |> List.rev
      in
      makeListPattern loc patterns None

  and parseArrayPattern ~attrs p =
    let startPos = p.startPos in
    Parser.expectExn Lbracket p;
    let patterns =
      parseCommaSeparatedList ~closing:Rbracket ~f:parseConstrainedPattern p
    in
    Parser.expect Rbracket p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.array ~loc ~attrs patterns

  and parseConstructorPatternWithArgs p constr startPos attrs =
    let lparen = p.startPos in
    Parser.expectExn Lparen p;
    let args = match
      parseCommaSeparatedList ~closing:Rparen ~f:parseConstrainedPattern p
    with
    | [pattern] -> Some pattern
    | patterns ->
        Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen p.prevEndPos) patterns)
    in
    Parser.expect Rparen p;
    Ast_helper.Pat.construct ~loc:(mkLoc startPos p.prevEndPos) ~attrs constr args

  and parseExpr ?(context=OrdinaryExpr) p =
    let startPos = p.Parser.startPos in
    let expr = match p.token with
    | Assert ->
      Parser.next p;
      let expr = parseExpr p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.assert_ ~loc expr
    | Lazy ->
      Parser.next p;
      let expr = parseExpr p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.lazy_ ~loc expr
    | If ->
      parseIfExpression p
    | For ->
      parseForExpression p
    | While ->
      parseWhileExpression p
    | Switch ->
      parseSwitchExpression p
    | _ ->
      if (context != WhenExpr) &&
         isEs6ArrowExpression ~inTernary:(context=TernaryTrueBranchExpr) p
      then
        parseEs6ArrowExpression p
      else if context != StructureExpr then
        parseAttributedExpr p
      else
        parseUnaryExpr p
    in
    let expr = parseBinaryExpr ~allowAttrs:(context != StructureExpr) ~a:expr p 1 in
    parseTernaryExpr expr p

  (* expr ? expr : expr *)
  and parseTernaryExpr leftOperand p =
    match p.Parser.token with
    | Question ->
      Parser.leaveBreadcrumb p Report.Ternary;
      Parser.next p;
      let trueBranch = parseExpr ~context:TernaryTrueBranchExpr p in
      Parser.expectExn Colon p;
      let falseBranch = parseExpr p in
      Parser.eatBreadcrumb p;
      let loc = {leftOperand.Parsetree.pexp_loc with
        loc_start = leftOperand.pexp_loc.loc_start;
        loc_end = falseBranch.Parsetree.pexp_loc.loc_end;
      } in
      Ast_helper.Exp.ifthenelse ~loc leftOperand trueBranch (Some falseBranch)
    | _ ->
      leftOperand

  and parseEs6ArrowExpression p =
    let startPos = p.Parser.startPos in
    Parser.leaveBreadcrumb p Report.Es6ArrowExpr;
    let parameters = parseParameters p in
    let returnType = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parseTypExpr p)
    | _ ->
      None
    in
    Parser.expectExn EqualGreater p;
    let body =
      let expr = parseExpr p in
      match returnType with
      | Some typ ->
        Ast_helper.Exp.constraint_
          ~loc:(mkLoc expr.pexp_loc.loc_start typ.Parsetree.ptyp_loc.loc_end) expr typ
      | None -> expr
    in
    Parser.eatBreadcrumb p;
    let arrowExpr = List.fold_right (fun (attrs, lbl, defaultExpr, pat) expr ->
      Ast_helper.Exp.fun_ ~attrs lbl defaultExpr pat expr
    ) parameters body
    in
    {arrowExpr with pexp_loc = {arrowExpr.pexp_loc with loc_start = startPos}}

  and parseParameter p =
    let startPos = p.Parser.startPos in
    (* TODO: this is a shift reduce conflict, we reduce here :)
     * let f = ( @attr x ) => x + 1; -> on pattern x or on Pexp_fun? *)
    let attrs = parseAttributes p in
    let (lbl, pat) = match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let lblName = match p.Parser.token with
      | Lident ident ->
        Parser.next p;
        ident
      | _ -> raise (Parser.ParseError (p.startPos, Report.Message "Function parameters labels should be lowercased like ~a "))
      in
      begin match p.Parser.token with
      | Comma | Equal | Rparen ->
        let loc = mkLoc startPos p.prevEndPos in
        (
          Asttypes.Labelled lblName,
          Ast_helper.Pat.var ~loc (Location.mkloc lblName loc)
        )
      | Colon ->
        let lblEnd = p.prevEndPos in
        Parser.next p;
        let typ = parseTypExpr p in
        let loc = mkLoc startPos lblEnd in
        let pat =
          let pat = Ast_helper.Pat.var ~loc (Location.mkloc lblName loc) in
          let loc = mkLoc startPos p.prevEndPos in
          Ast_helper.Pat.constraint_ ~loc pat typ in
        (Asttypes.Labelled lblName, pat)
      | As ->
        Parser.next p;
        let pat = parseConstrainedPattern p in
        (Asttypes.Labelled lblName, pat)
      | _ -> raise (Parser.ParseError (p.startPos,
          Report.OneOf [Token.Comma; Token.Equal; Token.As]
        ))
      end
    | _ ->
      (Asttypes.Nolabel, parseConstrainedPattern p)
    in
    match p.Parser.token with
    | Equal ->
      Parser.next p;
      begin match p.Parser.token with
      | Question ->
        Parser.next p;
        let lbl = match lbl with
        | Asttypes.Labelled lblName -> Asttypes.Optional lblName
        | Asttypes.Optional _ as lbl -> lbl
        | Asttypes.Nolabel -> assert false
        in
        (attrs, lbl, None, pat)
      | _ ->
        let expr = parseExpr p in
        (attrs, lbl, Some expr, pat)
      end
    | _ ->
      (attrs, lbl, None, pat)

  and parseParameterList p =
    let parameters = parseCommaSeparatedList ~closing:Rparen ~f:parseParameter p in
    Parser.expect Rparen p;
    parameters

  (* parameters ::=
   *   | _
   *   | lident
   *   | ()
   *   | ( parameter {, parameter} [,] )
   *)
  and parseParameters p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      let loc = mkLoc startPos p.Parser.prevEndPos in
      [(
        [],
        Asttypes.Nolabel,
        None,
        Ast_helper.Pat.var ~loc (Location.mkloc ident loc)
      )]
    | Underscore ->
      Parser.next p;
      let loc = mkLoc startPos p.Parser.prevEndPos in
      [[], Asttypes.Nolabel, None, Ast_helper.Pat.any ~loc ()]
    | Lparen ->
      Parser.next p;
      begin match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mkLoc startPos p.Parser.prevEndPos in
        let unitPattern = Ast_helper.Pat.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
        in
        [[], Asttypes.Nolabel, None, unitPattern]
      | _ -> parseParameterList p
      end
    | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))

  and parseConstrainedExpr p =
    let expr = parseExpr p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let typ = parseTypExpr p in
      let loc = mkLoc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
      Ast_helper.Exp.constraint_ ~loc expr typ
    | _ -> expr

  (* Atomic expressions represent unambiguous expressions.
   * This means that regardless of the context, these expressions
   * are always interpreted correctly. *)
  and parseAtomicExpr p =
    Parser.leaveBreadcrumb p ExprOperand;
    let startPos = p.Parser.startPos in
    let expr = match p.Parser.token with
      | (True | False) as token ->
        let endPos = p.endPos in
        Parser.next p;
        let loc = mkLoc startPos endPos in
        Ast_helper.Exp.construct
          (Location.mkloc (Longident.Lident (Token.toString token)) loc) None
      | Int _ | String _ | Float _ ->
        let c = parseConstant p in
        Ast_helper.Exp.constant c
      | Backtick ->
        parseTemplateExpr p
      | Uident _ | Lident _ ->
        parseValueOrConstructor p
      | Lparen ->
        Parser.next p;
        begin match p.Parser.token with
        | Rparen ->
          Parser.next p;
          let loc = mkLoc startPos p.prevEndPos in
          Ast_helper.Exp.construct
            ~loc (Location.mkloc (Longident.Lident "()") loc) None
        | t ->
          let expr = parseConstrainedExpr p in
          Parser.expectExn Rparen p;
          {expr with pexp_loc = mkLoc startPos p.startPos}
        end
      | List ->
        parseListExpr p
      | Lbracket ->
        parseArrayExp p
      | Lbrace ->
        Parser.next p;
        let e = parseBracedOrRecordExpr p in
        Parser.expectExn Rbrace p;
        e
      | Forwardslash ->
        parseTupleExpr p
      | LessThan ->
        parseJsx p
      | unknownToken ->
        raise (Parser.ParseError (p.startPos, Report.Unexpected unknownToken))
    in
    Parser.eatBreadcrumb p;
    expr

  (* TODO: is this "operand"-arg a clear API? probably not with positions… *)
  and parsePrimaryExpr ?operand ?(noCall=false) p =
    let startPos = p.Parser.startPos in
    let e1 = match operand with
      | Some e -> e
      | None -> parseAtomicExpr p
    in
    let rec loop p expr =
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        let lident = parseValuePath p in
        begin match p.Parser.token with
        | Equal ->
          Parser.leaveBreadcrumb p ExprSetField;
          Parser.next p;
          let endPos = p.prevEndPos in
          let loc = mkLoc startPos endPos in
          let setfield = Ast_helper.Exp.setfield ~loc expr lident (parseExpr p) in
          Parser.eatBreadcrumb p;
          setfield
        | _ ->
          let endPos = p.prevEndPos in
          let loc = mkLoc startPos endPos in
          loop p (Ast_helper.Exp.field ~loc expr lident)
        end
      | Lbracket when noCall = false ->
        Parser.leaveBreadcrumb p ExprArrayAccess;
        let lbracket = p.startPos in
        Parser.next p;
        let accessExpr = parseConstrainedExpr p in
        Parser.expectExn Rbracket p;
        let rbracket = p.prevEndPos in
        let arrayLoc = mkLoc lbracket rbracket in
        begin match p.token with
        | Equal ->
          Parser.leaveBreadcrumb p ExprArrayMutation;
          Parser.next p;
          let rhsExpr = parseExpr p in
          let arraySet = Location.mkloc
            (Longident.Ldot(Lident "Array", "set"))
            arrayLoc
          in
          let endPos = p.prevEndPos in
          let arraySet = Ast_helper.Exp.apply
            ~loc:(mkLoc startPos endPos)
            (Ast_helper.Exp.ident ~loc:arrayLoc arraySet)
            [Nolabel, expr; Nolabel, accessExpr; Nolabel, rhsExpr]
          in
          Parser.eatBreadcrumb p;
          arraySet
        | _ ->
          let endPos = p.prevEndPos in
          let e = loop p
            (Ast_helper.Exp.apply
              ~loc:(mkLoc startPos endPos)
              (Ast_helper.Exp.ident
                ~loc:arrayLoc
                (Location.mkloc (Longident.Ldot(Lident "Array", "get")) arrayLoc)
              )
              [Nolabel, expr; Nolabel, accessExpr])
          in
          Parser.eatBreadcrumb p;
          e
        end
      | Lparen when noCall = false ->
        loop p (parseCallExpr p expr)
      | _ -> expr
    in
    let expr = loop p e1 in
    {expr with pexp_loc = mkLoc startPos p.startPos}


  (* a unary expression is an expression with only one operand and
   * unary operator. Example:
   * -> -1
   * -> !condition
   * -> -. 1.6
   *)
  and parseUnaryExpr p =
    match p.Parser.token with
    | (Minus | MinusDot | Plus | PlusDot | Bang) as token ->
      Parser.leaveBreadcrumb p Report.ExprUnary;
      Parser.next p;
      let unaryExpr = makeUnaryExpr token (parseUnaryExpr p) in
      Parser.eatBreadcrumb p;
      unaryExpr
    | Band (* & *) ->
      Parser.leaveBreadcrumb p Report.ExprUnary;
      let startPos = p.startPos in
      Parser.next p;
      let refAccess =
        let loc = mkLoc startPos p.prevEndPos in
        let op = Location.mkloc (Longident.Lident "!") loc in
        Ast_helper.Exp.ident ~loc op
      in
      let arg = parseUnaryExpr p in
      let loc = mkLoc startPos arg.pexp_loc.loc_end in
      let unaryExpr = Ast_helper.Exp.apply
        ~loc
        refAccess
        [Nolabel, arg]
      in
      Parser.eatBreadcrumb p;
      unaryExpr
    | _ ->
      parsePrimaryExpr p

  and parseAttributedExpr p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let unaryExpr = parseUnaryExpr p in
    let endPos = p.Parser.prevEndPos in
    {unaryExpr with pexp_attributes = attrs; pexp_loc = mkLoc startPos endPos}

  (* a binary expression is an expression that combines two expressions with an
   * operator. Examples:
   * -> a + b
   * -> f(x) |> g(y)
   *)
  and parseBinaryExpr ?(allowAttrs=true) ?a p prec =
    let a = match a with
    | Some e -> e
    | None -> if allowAttrs then parseAttributedExpr p else parseUnaryExpr p
    in
    let rec loop a =
      let token = p.Parser.token in
      let tokenPrec = Token.precedence token in
      if tokenPrec < prec then a
      else begin
        Parser.leaveBreadcrumb p (Report.ExprBinaryAfterOp token);
        let startPos = p.startPos in
        Parser.next p;
        let endPos = p.startPos in
        let b = parseBinaryExpr p (tokenPrec + 1) in
        let expr = Ast_helper.Exp.apply
          (makeInfixOperator token startPos endPos)
          [Nolabel, a; Nolabel, b]
        in
        loop expr
      end
    in
    loop a

  and parseTemplateExpr p =
    let hiddenOperator =
      let op = Location.mknoloc (Longident.Lident "^") in
      Ast_helper.Exp.ident op
    in
    let rec loop acc p =
      match p.Parser.token with
      | TemplateTail txt ->
        Parser.next p;
        if String.length txt > 0 then
          let str = Ast_helper.Exp.constant (Pconst_string(txt, None)) in
          Ast_helper.Exp.apply hiddenOperator
            [Nolabel, acc; Nolabel, str]
        else
          acc
      | TemplatePart txt ->
        Parser.next p;
        let expr = parseSeqExpr p in
        let () = match p.Parser.token with
        | Rbrace ->
          Lex.setTemplateMode p.lexbuf;
          Parser.next p
        | _ -> raise (Parser.ParseError (p.startPos, Report.Expected (Rbrace, None)))
        in
        let str = Ast_helper.Exp.constant (Pconst_string(txt, None)) in
        let next =
          let a = if String.length txt > 0 then
              Ast_helper.Exp.apply hiddenOperator [Nolabel, acc; Nolabel, str]
            else acc
          in
          Ast_helper.Exp.apply hiddenOperator
            [Nolabel, a; Nolabel, expr]
        in
        loop next p
      | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))
    in
    Lex.setTemplateMode p.lexbuf;
    Parser.expectExn Backtick p;
    match p.Parser.token with
    | TemplateTail txt ->
      Parser.next p;
      Ast_helper.Exp.constant (Pconst_string(txt, None))
    | TemplatePart txt ->
      Parser.next p;
      let expr = parseSeqExpr p in
      let () = match p.Parser.token with
      | Rbrace ->
        Lex.setTemplateMode p.lexbuf;
        Parser.next p
      | _ -> raise (Parser.ParseError (p.startPos, Report.Expected (Rbrace, None)))
      in
      let str = Ast_helper.Exp.constant (Pconst_string(txt, None)) in
      let next =
        if String.length txt > 0 then
          Ast_helper.Exp.apply hiddenOperator [Nolabel, str; Nolabel, expr]
        else
          expr
      in
      loop next p
   | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))

  and parseLetBindingBody ~attrs p =
    Parser.leaveBreadcrumb p LetBinding;
    let pat =
      let pat = parsePattern p in
      match p.Parser.token with
      | Colon ->
        Parser.next p;
        let polyType = parsePolyTypeExpr p in
        let loc = {pat.ppat_loc with loc_end = polyType.Parsetree.ptyp_loc.loc_end} in
        Ast_helper.Pat.constraint_ ~loc pat polyType
      | _ -> pat
    in
    Parser.expectExn Token.Equal p;
    let exp = parseExpr p in
    let loc = {pat.ppat_loc with
      loc_end = exp.Parsetree.pexp_loc.loc_end
    } in
    let vb = Ast_helper.Vb.mk ~loc ~attrs pat exp in
    Parser.eatBreadcrumb p;
    vb

  (* definition	::=	let [rec] let-binding  { and let-binding }   *)
  and parseLetBindings ~attrs p =
    Parser.expectExn Let p;
    let recFlag = if Parser.optional p Token.Rec then
      Asttypes.Recursive
    else
      Asttypes.Nonrecursive
    in
    let first = parseLetBindingBody ~attrs p in

    let rec loop p bindings =
      let attrs = parseAttributes p in
      match p.Parser.token with
      | And ->
        Parser.next p;
        let letBinding = parseLetBindingBody ~attrs p in
        loop p (letBinding::bindings)
      | _ ->
        List.rev bindings
    in
    (recFlag, loop p [first])

  (*
   * div -> div
   * Foo -> Foo.createElement
   * Foo.Bar -> Foo.Bar.createElement
   *)
  and parseJsxName p =
    let (ident, loc) = match p.Parser.token with
    | Lident ident ->
      let identStart = p.startPos in
      Parser.next p;
      let identEnd = p.prevEndPos in
      let loc = mkLoc identStart identEnd in
      (Longident.Lident ident, loc)
    | Uident _ ->
      let longident = parseModuleLongIdent p in
      let identName = (Longident.Ldot (longident.txt, "createElement")) in
      (identName, longident.loc)
    | _ ->
        raise (Parser.ParseError (p.startPos,
      Report.Message "A jsx name should start with a lowercase or uppercase identifier, like: div in <div /> or Navbar in <Navbar />"))
    in
    Ast_helper.Exp.ident ~loc (Location.mkloc ident loc)

  (*
   *  jsx ::=
   *    | <> {primary-expr} </>
   *    | <element-name {jsx-attribute} />
   *    | <element-name {jsx-attribute}> {primary-expr} </element-name>
   *)
  and parseJsx p =
    Parser.leaveBreadcrumb p Report.Jsx;
    let jsxStartPos = p.Parser.startPos in
    Parser.expectExn LessThan p;
    match p.Parser.token with
    | Lident _ | Uident _ ->
      let name = parseJsxName p in
      let jsxAttrs = parseJsxAttributes p in
      let children = match p.Parser.token with
      | Forwardslash -> (* <foo a=b /> *)
        let childrenStartPos = p.Parser.startPos in
        Parser.next p;
        let childrenEndPos = p.Parser.startPos in
        Parser.expectExn GreaterThan p;
        let loc = mkLoc childrenStartPos childrenEndPos in
        makeListExpression loc [] None (* no children *)
      | GreaterThan -> (* <foo a=b> bar </foo> *)
        let childrenStartPos = p.Parser.startPos in
        Lex.setJsxMode p.lexbuf;
        Parser.next p;
        let children = parseJsxChildren p in
        let childrenEndPos = p.Parser.startPos in
        let () = match p.token with
        | LessThanSlash ->
          Parser.next p;
          ()
        | LessThan -> Parser.next p; Parser.expectExn Forwardslash p
        | _ -> Parser.expectExn LessThanSlash p
        in
        begin match p.Parser.token with
        | Lident closingIdent | Uident closingIdent ->
          if verifyJsxOpeningClosingName name closingIdent then (
            Parser.next p;
            Parser.expectExn GreaterThan p;
            let loc = mkLoc childrenStartPos childrenEndPos in
            makeListExpression loc children None
          ) else
            let opening = "</" ^ (string_of_pexp_ident name) ^ ">" in
            let message = "Closing jsx name should be the same as the opening name. Did you mean " ^ opening ^ " ?" in
            raise (Parser.ParseError (p.startPos, Report.Message message))
        | _ ->
          let opening = "</" ^ (string_of_pexp_ident name) ^ ">" in
          let message = "Closing jsx name should be the same as the opening name. Did you mean " ^ opening ^ " ?" in
          raise (Parser.ParseError (p.startPos, Report.Message message))
        end
      | unknownToken -> raise (Parser.ParseError (p.startPos, Report.Unexpected unknownToken))
      in
      let jsxEndPos = p.prevEndPos in
      let loc = mkLoc jsxStartPos jsxEndPos in
      Ast_helper.Exp.apply
        ~loc
        name
        (jsxAttrs @ [
          (Asttypes.Labelled "childen", children);
          (Asttypes.Nolabel, Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None)
        ])
    | GreaterThan -> (* fragment: <> foo </> *)
      parseJsxFragment p
    | _ ->
        parseJsxName p

  and parseJsxFragment p =
    let childrenStartPos = p.Parser.startPos in
    Parser.expectExn GreaterThan p;
    let children = parseJsxChildren p in
    let childrenEndPos = p.Parser.startPos in
    Parser.expectExn LessThanSlash p;
    Parser.expectExn GreaterThan p;
    let loc = mkLoc childrenStartPos childrenEndPos in
    let fragment = makeListExpression loc children None in
    let jsxAttr = (Location.mknoloc "JSX", Parsetree.PStr []) in
    {fragment with pexp_attributes = [jsxAttr]}

  (*
   * jsx-attribute ::=
   *   | [?] LIDENT
   *   | LIDENT = [?] jsx_expr
   *)
  and parseJsxAttribute p =
    Parser.leaveBreadcrumb p Report.JsxAttribute;
    let optional = Parser.optional p Question in
    let name = match p.Parser.token with
    | Lident ident -> Parser.next p; ident
    | _ ->
      raise (Parser.ParseError (p.startPos, Report.Lident))
    in
    (* optional punning: <foo ?a /> *)
    if optional then
      (Asttypes.Labelled name, Ast_helper.Exp.ident (Location.mknoloc
        (Longident.Lident name)))
    else begin
      match p.Parser.token with
      | Equal ->
        Parser.next p;
        (* no punning *)
        let optional = Parser.optional p Question in
        let attrExpr = parsePrimaryExpr p in
        let label =
          if optional then Asttypes.Optional name else Asttypes.Labelled name
        in
        (label, attrExpr)
      | _ ->
        let optional = Parser.optional p Question in
        let attrExpr = Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident
        name)) in
        let label =
          if optional then Asttypes.Optional name else Asttypes.Labelled name
        in
        (label, attrExpr)
    end

  and parseJsxAttributes p =
    let rec loop p attrs =
      match p.Parser.token with
      | Token.Eof | Forwardslash | GreaterThan -> List.rev attrs
      | _ ->
        let attr = parseJsxAttribute p in
        loop p (attr::attrs)
    in
    loop p []

  and parseJsxChildren p =
    let rec loop p children =
      match p.Parser.token  with
      | Token.Eof | LessThanSlash ->
        Lex.setNormalMode p.lexbuf;
        List.rev children
      | LessThan ->
        let token = Lex.rescanLessThan p.lexbuf in
        if token = LessThan then
          let child = parsePrimaryExpr ~noCall:true p in
          loop p (child::children)
        else
          let () = p.token <- token in
          let () = Lex.setNormalMode p.lexbuf in
          List.rev children
      | _ ->
        let child = parsePrimaryExpr ~noCall:true p in
        loop p (child::children)
    in
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      [parsePrimaryExpr ~noCall:true p]
    | _ -> loop p []

  and parseBracedOrRecordExpr p =
    (* opening brace consumed *)
    match p.Parser.token with
    | DotDotDot ->
      (* beginning of record spread, parse record *)
      Parser.next p;
      let spreadExpr = parseExpr p in
      Parser.expect Comma p;
      parseRecordExpr ~spread:(Some spreadExpr) [] p
    | Uident _ | Lident _ ->
      let recordFieldOrPexpIdent = parseValuePath p in
      let pathIdent = recordFieldOrPexpIdent in
      begin match p.Parser.token with
      | Comma ->
        Parser.next p;
        parseRecordExpr [(pathIdent, Ast_helper.Exp.ident pathIdent)] p
      | Colon ->
        Parser.next p;
        let fieldExpr = parseExpr p in
        Parser.expect Comma p;
        parseRecordExpr [(pathIdent, fieldExpr)] p
      | Semicolon ->
        Parser.next p;
        parseSeqExpr ~first:(Ast_helper.Exp.ident pathIdent) p
      | Rbrace ->
        Ast_helper.Exp.ident pathIdent
      | _ ->
        let a = parsePrimaryExpr ~operand:(Ast_helper.Exp.ident pathIdent) p in
        let e = parseBinaryExpr ~a p 1 in
        begin match p.Parser.token with
        | Semicolon ->
          Parser.next p;
          parseSeqExpr ~first:e p
        | Rbrace -> e
        | _ -> parseSeqExpr ~first:e p
        end
      end
    | _ ->
      parseSeqExpr p

  and parseRecordRow p =
    let field = parseValuePath p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let fieldExpr = parseExpr p in
      (field, fieldExpr)
    | _ ->
      (field, Ast_helper.Exp.ident ~loc:field.loc  field)

  and parseRecordExpr ?(spread=None) rows p =
    let exprs = parseCommaSeparatedList ~closing:Rbrace ~f:parseRecordRow p in
    Ast_helper.Exp.record (rows @ exprs) spread

  and parseSeqExprItem p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Module ->
      Parser.next p;
      let name = match p.Parser.token with
      | Uident ident ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        Location.mkloc ident loc
      | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))
      in
      let body = parseModuleBindingBody p in
      Parser.optional p Semicolon |> ignore;
      let expr = parseSeqExpr p in
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in
      Ast_helper.Exp.letmodule ~loc name body expr
    | Exception ->
      let extensionConstructor = parseExceptionDef ~attrs:[] p in
      Parser.optional p Semicolon |> ignore;
      let seqExpr = parseSeqExpr  p in
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in
      Ast_helper.Exp.letexception ~loc extensionConstructor seqExpr
    | Open ->
      let od = parseOpenDescription ~attrs:[] p in
      Parser.optional p Semicolon |> ignore;
      let seqExpr = parseSeqExpr p in
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in
      Ast_helper.Exp.open_ ~loc od.popen_override od.popen_lid seqExpr
    | Let ->
      let (recFlag, letBindings) = parseLetBindings ~attrs:[] p in
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in

      let next = match p.Parser.token with
      | Semicolon ->
        Parser.next p;
        begin match p.Parser.token with
        (* seq expr start *)
        | At | Minus | MinusDot | Plus | PlusDot | Bang | Band
        | True | False | Int _ | Float _ | String _ | Lident _ | Uident _
        | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
        | Lazy | If | For | While | Switch | Open | Module | Exception | Let
        | LessThan | Backtick ->
          parseSeqExpr p
        | _ ->
          Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None
        end
      (* High danger, TODO check if we really can omit semi in these case*)
      | Bang | Band
      | True | False | Int _ | Float _ | String _ | Lident _ | Uident _
      | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
      | Lazy | If | For | While | Switch | Open | Module | Exception | Let
      | LessThan | Backtick ->
        parseSeqExpr p
      | _ ->
        Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None
      in
      Ast_helper.Exp.let_ ~loc recFlag letBindings next
    | _ ->
      let e1 = parseExpr p in
      ignore (Parser.optional p Semicolon);
      begin match p.Parser.token with
      (* seq expr start *)
      | At | Minus | MinusDot | Plus | PlusDot | Bang | Band
      | True | False | Int _ | Float _ | String _ | Lident _ | Uident _
      | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
      | Lazy | If | For | While | Switch | Open | Module | Exception | Let
      | LessThan | Backtick ->
        let e2 = parseSeqExpr p in
        Ast_helper.Exp.sequence e1 e2
      | _ -> e1
      end

  (* TODO; improve? perf? *)

  (* seq_expr ::= expr
   *           |  expr          ;
   *           |  expr          ; seq_expr
   *           |  module    ... ; seq_expr
   *           |  open      ... ; seq_expr
   *           |  exception ... ; seq_expr
   *           |  let       ...
   *           |  let       ... ;
   *           |  let       ... ; seq_expr
   *)
  and parseSeqExpr ?first p =
      Parser.leaveBreadcrumb p Report.SeqExpr;
      let item = match first with
      | Some e -> e
      | None -> parseSeqExprItem p
      in
      let seqExpr = match p.Parser.token with
      | Semicolon ->
        Parser.next p;
        begin match p.Parser.token with
        (* seq expr start *)
        | At | Minus | MinusDot | Plus | PlusDot | Bang | Band
        | True | False | Int _ | String _ | Lident _ | Uident _
        | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
        | Lazy | If | For | While | Switch | Open | Module | Exception | Let
        | LessThan | Backtick ->
          let next = parseSeqExprItem p in
          ignore(Parser.optional p Semicolon);
          Ast_helper.Exp.sequence item next
        | _ -> item
        end
      (* semicolon recovery *)
      | token when
          begin match token with
          | Bang | Band
          | True | False | Int _ | String _ | Lident _ | Uident _
          | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
          | Lazy | If | For | While | Switch | Open | Module | Exception | Let
          | LessThan | Backtick -> true
          | _ -> false
          end
        ->
          begin match p.Parser.token with
          (* seq expr start *)
          | At | Minus | MinusDot | Plus | PlusDot | Bang | Band
          | True | False | Int _ | String _ | Lident _ | Uident _
          | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
          | Lazy | If | For | While | Switch | Open | Module | Exception | Let
          | LessThan | Backtick ->
            let next = parseSeqExprItem p in
            ignore(Parser.optional p Semicolon);
            Ast_helper.Exp.sequence item next
          | _ -> item
          end
      | _ ->
        item
      in
      Parser.eatBreadcrumb p;
      seqExpr

  and parseIfExpression p =
    Parser.leaveBreadcrumb p ExprIf;
    let startPos = p.Parser.startPos in
    Parser.expectExn If p;
    Parser.leaveBreadcrumb p IfCondition;
    (* doesn't make sense to try es6 arrow here? *)
    let conditionExpr = parseExpr ~context:WhenExpr p in
    Parser.eatBreadcrumb p;
    Parser.leaveBreadcrumb p IfBranch;
    Parser.expectExn Lbrace p;
    let thenExpr = parseSeqExpr p in
    Parser.expectExn Rbrace p;
    Parser.eatBreadcrumb p;
    let elseExpr = match p.Parser.token with
    | Else ->
      Parser.leaveBreadcrumb p ElseBranch;
      Parser.next p;
      Parser.expectExn  Lbrace p;
      let elseExpr = parseSeqExpr p in
      Parser.expectExn Rbrace p;
      Parser.eatBreadcrumb p;
      Some elseExpr
    | _ ->
      None
    in
    let loc = mkLoc startPos p.prevEndPos in
    Parser.eatBreadcrumb p;
    Ast_helper.Exp.ifthenelse ~loc conditionExpr thenExpr elseExpr

  and parseForExpression p =
    let startPos = p.Parser.startPos in
    Parser.expectExn For p;
    let hasOpeningParen = Parser.optional p Lparen in
    let pattern = parsePattern p in
    Parser.expectExn In p;
    let e1 = parseExpr p in
    let direction = match p.Parser.token with
    | To -> Asttypes.Upto
    | Downto -> Asttypes.Downto
    | _ ->
      raise (Parser.ParseError (p.startPos, Report.OneOf [Token.To; Token.Downto]))
    in
    Parser.next p;
    let e2 = parseExpr p in
    if hasOpeningParen then Parser.expectExn Rparen p;
    Parser.expectExn Lbrace p;
    let bodyExpr = parseSeqExpr p in
    Parser.expectExn Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.for_ ~loc pattern e1 e2 direction bodyExpr

  and parseWhileExpression p =
    let startPos = p.Parser.startPos in
    Parser.expectExn While p;
    let expr1 = parseExpr ~context:WhenExpr p in
    Parser.expectExn Lbrace p;
    let expr2 = parseSeqExpr p in
    Parser.expectExn Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.while_ ~loc expr1 expr2

  and parsePatternMatching p =
    Parser.leaveBreadcrumb p Report.PatternMatching;
    (* '{' consumed *)
    let rec loop p cases =
      match p.Parser.token with
      | Rbrace ->
        List.rev cases
      | Bar ->
        Parser.next p;
        let lhs = parsePattern p in
        let guard = match p.Parser.token with
        | When ->
          Parser.next p;
          Some (parseExpr ~context:WhenExpr p)
        | _ ->
          None
        in
        Parser.expectExn EqualGreater p;
        let rhs = parseSeqExpr p in
        let case = Ast_helper.Exp.case lhs ?guard rhs in
        loop p (case::cases)
      | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))
    in
    let cases = loop p [] in
    Parser.eatBreadcrumb p;
    cases

  and parseSwitchExpression p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Switch p;
    let switchExpr = parseExpr ~context:WhenExpr p in
    Parser.expectExn Lbrace p;
    let cases = parsePatternMatching p in
    Parser.expectExn Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.match_ ~loc switchExpr cases

  (*
   * argument ::=
   *   | expr
   *   | expr : type
   *   | ~ label-name
   *   | ~ label-name
   *   | ~ label-name ?
   *   | ~ label-name =   expr
   *   | ~ label-name =   expr : type
   *   | ~ label-name = ? expr
   *   | ~ label-name = ? expr : type
   *)
  and parseArgument p =
    match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let startPos = p.startPos in
      begin match p.Parser.token with
      | Lident ident ->
        Parser.next p;
        let endPos = p.startPos in
        let loc = mkLoc startPos endPos in
        let identExpr = Ast_helper.Exp.ident (
          Location.mkloc (Longident.Lident ident) loc
        ) in
        begin match p.Parser.token with
        | Question ->
          Parser.next p;
          (Asttypes.Optional ident, identExpr)
        | Equal ->
          Parser.next p;
          let label = match p.Parser.token with
          | Question ->
            Parser.next p;
            Asttypes.Optional ident
          | _ ->
            Labelled ident
          in
          (label, parseConstrainedExpr p)
        | _ ->
          (Labelled ident, identExpr)
        end
      | _ -> raise (Parser.ParseError (p.startPos, Report.Lident))
      end
    | _ -> (Nolabel, parseConstrainedExpr p)

  and parseCallExpr p funExpr =
    Parser.expectExn Lparen p;
    let startPos = p.Parser.startPos in
    Parser.leaveBreadcrumb p ExprCall;
    let args = parseCommaSeparatedList ~closing:Rparen ~f:parseArgument p in
    Parser.expectExn Rparen p;
    let args = match args with
    | [] ->
      let loc = mkLoc startPos p.prevEndPos in
     (* No args -> unit sugar: `foo()` *)
      [
        Asttypes.Nolabel,
        Ast_helper.Exp.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
      ]
    | args -> args
    in
    let loc = {funExpr.pexp_loc with loc_end = p.prevEndPos } in
    let apply = Ast_helper.Exp.apply ~loc funExpr args in
    Parser.eatBreadcrumb p;
    apply

  and parseValueOrConstructor p =
    let startPos = p.Parser.startPos in
    let rec aux p acc =
      match p.Parser.token with
      | Uident ident ->
        Parser.next p;
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          aux p (ident::acc)
        | Lparen ->
          let lparen = p.startPos in
          let args = parseConstructorArgs p in
          let rparen = p.prevEndPos in
          let lident = buildLongident (ident::acc) in
          let tail = match args with
          | [] -> None
          | [arg] -> Some arg
          | args ->
            let loc = mkLoc lparen rparen in
            Some (Ast_helper.Exp.tuple ~loc args)
          in
          let loc = mkLoc startPos p.prevEndPos in
          Ast_helper.Exp.construct (Location.mkloc lident loc) tail
        | _ ->
          let loc = mkLoc startPos p.prevEndPos in
          let lident = buildLongident (ident::acc) in
          Ast_helper.Exp.construct ~loc (Location.mkloc lident loc) None
        end
      | Lident ident ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        let lident = buildLongident (ident::acc) in
        Ast_helper.Exp.ident ~loc (Location.mkloc lident loc)
      | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))
    in
    aux p []

  and parseConstructorArgs p =
    let lparen = p.Parser.startPos in
    Parser.expectExn Lparen p;
    let args =
      parseCommaSeparatedList ~closing:Rparen ~f:parseConstrainedExpr p
    in
    Parser.expectExn Rparen p;
    match args with
    | [] ->
      let loc = mkLoc lparen p.prevEndPos in
      [Ast_helper.Exp.construct
        ~loc (Location.mkloc (Longident.Lident "()") loc) None]
    | args -> args

  and parseTupleExpr p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Forwardslash p;
    Lex.setTupleMode p.lexbuf;
    let exprs =
      parseCommaSeparatedList ~closing:TupleEnding ~f:parseConstrainedExpr p
    in
    Parser.expect TupleEnding p;
    Ast_helper.Exp.tuple ~loc:(mkLoc startPos p.prevEndPos) exprs

  and parseListExpr p =
    let startPos = p.Parser.startPos in
    Parser.expectExn List p;
    Parser.expectExn Lparen p;
    let rec loop p exprs = match p.Parser.token with
    | Rparen ->
      Parser.next p;
      exprs
    | Comma -> Parser.next p; loop p exprs
    | DotDotDot ->
      Parser.next p;
      let expr = parseConstrainedExpr p in
      loop p ((true, expr)::exprs)
    | _ ->
      let expr = parseConstrainedExpr p in
      loop p ((false, expr)::exprs)
    in
    let listExprs = loop p [] in
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    match listExprs with
    | (true, expr)::exprs ->
      let exprs =
        exprs
        |> List.map snd
        |> List.rev
      in
      makeListExpression loc exprs (Some expr)
    | exprs ->
     let exprs =
        exprs
        |> List.map snd
        |> List.rev
      in
      makeListExpression loc exprs None

  and parseArrayExp p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Lbracket p;
    let exprs =
      parseCommaSeparatedList ~closing:Rbracket ~f:parseConstrainedExpr p
    in
    Parser.expect Rbracket p;
    Ast_helper.Exp.array ~loc:(mkLoc startPos p.prevEndPos) exprs

  (* TODO: check attributes in the case of poly type vars,
   * might be context dependend: parseFieldDeclaration (see ocaml) *)
  and parsePolyTypeExpr p =
    match p.Parser.token with
    | SingleQuote ->
      let vars = parseTypeVarList p in
      begin match vars with
      | _v1::_v2::_ ->
        Parser.expectExn Dot p;
        let typ = parseTypExpr p in
        Ast_helper.Typ.poly vars typ
      | [var] ->
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          let typ = parseTypExpr p in
          Ast_helper.Typ.poly vars typ
        | _ ->
          Ast_helper.Typ.var ~loc:var.loc var.txt
        end
      | _ -> assert false
      end
    | _ ->
      parseTypExpr p

  (* 'a 'b 'c *)
  and parseTypeVarList p =
    let rec loop p vars =
      let startPos = p.Parser.startPos in
      match p.Parser.token with
      | SingleQuote ->
        Parser.next p;
        begin match p.Parser.token with
        | Lident ident ->
          let endPos = p.endPos in
          Parser.next p;
          let var = Location.mkloc ident (mkLoc startPos endPos) in
          loop p (var::vars)
        | _ -> raise (Parser.ParseError (p.startPos, Report.Lident))
        end
      | _ ->
        List.rev vars
    in
    loop p []

  and parseAtomicTypExpr ~attrs p =
    let startPos = p.Parser.startPos in
    let typ = match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      begin match p.Parser.token with
      | Lident ident ->
        let endPos = p.endPos in
        Parser.next p;
        Ast_helper.Typ.var ~loc:(mkLoc startPos endPos) ~attrs ident
      | _ -> raise (Parser.ParseError (p.startPos, Report.Lident))
      end
    | Underscore ->
      let endPos = p.endPos in
      Parser.next p;
      Ast_helper.Typ.any ~loc:(mkLoc startPos endPos) ~attrs ()
    | Forwardslash ->
      parseTupleType p
    | Lparen ->
      Parser.next p;
      let t = parseTypExpr p in
      let endPos = p.endPos in
      Parser.expectExn Rparen p;
      {t with ptyp_loc = mkLoc startPos endPos}
    | Uident _ | Lident _ | List ->
      let constr = parseValuePath p in
      (* TODO extract this whole block into reusable logic, cf. type equation *)
      begin match p.Parser.token with
      | LessThan ->
        Parser.next p;
        let (endPos, args) = parseConstructorTypeArgs p in
        Ast_helper.Typ.constr ~loc:(mkLoc startPos endPos) ~attrs constr args
      | Lparen ->
        raise (Parser.ParseError (p.startPos, Report.Message "Type constructor args require diamonds, like: Belt.Map.String.t<int>"))
      | _ ->
        Ast_helper.Typ.constr ~loc:constr.loc ~attrs constr []
      end
    | t ->
      raise (Parser.ParseError (p.startPos, Report.Unexpected t))
    in
    typ

  (* TODO: check associativity in combination with attributes *)
  and parseTypeAlias p typ =
    match p.Parser.token with
    | As ->
      Parser.next p;
      Parser.expectExn SingleQuote p;
      begin match p.token with
      | Lident ident ->
        Parser.next p;
        (* TODO: how do we parse attributes here? *)
        Ast_helper.Typ.alias ~loc:(mkLoc typ.Parsetree.ptyp_loc.loc_start p.prevEndPos) typ ident
      | _ -> raise (Parser.ParseError (p.startPos, Report.Lident))
      end
    | _ -> typ


  (* type_parameter ::=
    *  | type_expr
    *  | ~ident: type_expr
    *  | ~ident: type_expr=?
    *)
  and parseTypeParameter p =
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let name = match p.Parser.token with
      | Lident ident -> Parser.next p; ident
      | _ ->
        raise (Parser.ParseError (
          p.startPos,
          Report.Lident
        ))
      in
      Parser.expectExn Colon p;
      let typ = parseTypExpr p in
      begin match p.Parser.token with
      | Equal ->
        Parser.next p;
        Parser.expectExn Question p;
        (attrs, Asttypes.Optional name, typ)
      | _ -> (attrs, Asttypes.Labelled name, typ)
      end
    | _ ->
      (attrs, Asttypes.Nolabel, parseTypExpr p)

  (* (int, ~x:string, float) *)
  and parseTypeParameters p =
    Parser.expectExn Lparen p;
    let rec loop p params =
      match p.Parser.token with
      | Rparen -> Parser.next p; List.rev params
      | Comma -> Parser.next p; loop p params
      | _ ->
        let param = parseTypeParameter p in
        loop p (param::params)
    in
    loop p []

  and parseEs6ArrowType p =
    (* let startPos = p.Parser.startPos in *)
    let parameters = parseTypeParameters p in
    Parser.expectExn EqualGreater p;
    let returnType = parseTypExpr p in
    List.fold_right (fun (attrs, argLbl, typ) t ->
      Ast_helper.Typ.arrow ~attrs argLbl typ t
    ) parameters returnType

  and parseTypExpr p =
    Parser.leaveBreadcrumb p Report.TypeExpression;
    let attrs = parseAttributes p in
    let typ = if isEs6ArrowType p then
      parseEs6ArrowType p
    else
      let typ = parseAtomicTypExpr ~attrs p in
      (* TODO: loop *)
      match p.Parser.token with
      | EqualGreater ->
        Parser.next p;
        (* TODO: we're making a decision about shifting/reducing alias es *)
        let returnType = parseTypExpr p in
        let loc = mkLoc typ.Parsetree.ptyp_loc.loc_start p.prevEndPos in
        Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ returnType
      | _ -> typ
    in
    let typ = parseTypeAlias p typ in
    Parser.eatBreadcrumb p;
    typ

  and parseTupleType p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Forwardslash p;
    let types = parseCommaSeparatedList ~closing:Forwardslash ~f:parseTypExpr p in
    Parser.expect Forwardslash p;
    Ast_helper.Typ.tuple ~loc:(mkLoc startPos p.prevEndPos) types

  and parseConstructorTypeArgs p =
    let rec loop p types =
      match p.Parser.token with
      | GreaterThan ->
        let endPos = p.endPos in
        Parser.next p;
        (endPos, List.rev types)
      | _ ->
        let typ = parseTypExpr p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (typ::types)
        | GreaterThan ->
          let endPos = p.endPos in
          Parser.next p;
          (endPos, List.rev (typ::types))
        | _ -> raise (Parser.ParseError (p.startPos, Report.OneOf [Lparen; Comma]))
        end
    in
    loop p []

  and parseFieldDeclaration p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let mut = if Parser.optional p Token.Mutable then
      Asttypes.Mutable
    else
      Asttypes.Immutable
    in
    let name = match p.Parser.token with
    | Lident ident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc ident loc
    | _ -> raise (Parser.ParseError (p.startPos, Report.Lident))
    in
    Parser.expectExn Colon p;
    let typ = parsePolyTypeExpr p in
    let loc = mkLoc startPos typ.ptyp_loc.loc_end in
    Ast_helper.Type.field ~attrs ~loc ~mut name typ

  and parseRecordDeclaration p =
    Parser.expectExn Lbrace p;
    let rows =
      parseCommaSeparatedList ~closing:Rbrace ~f:parseFieldDeclaration p
    in
    Parser.expect Rbrace p;
    rows

  and parseConstrDeclArgs p =
    let args = match p.Parser.token with
    | Lparen ->
     Parser.next p;
     begin match p.Parser.token with
     | Lbrace ->
       let recordDecl = parseRecordDeclaration p in
       Parser.expectExn Rparen p;
       Parsetree.Pcstr_record recordDecl
     | _ ->
       let (_, args) = parseConstructorTypeArgs p in
       Parsetree.Pcstr_tuple args
      end
    | Lbrace ->
     Parsetree.Pcstr_record (parseRecordDeclaration p)
    | _ ->
     Pcstr_tuple []
    in
    let res = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parseTypExpr p)
    | _ -> None
    in
    (args, res)

  and parseTypeConstructorDeclaration p =
     match p.Parser.token with
     | Uident uident ->
       Parser.next p;
       let (args, res) = parseConstrDeclArgs p in
       Ast_helper.Type.constructor ?res ~args (Location.mknoloc uident)
     | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))

   (* [|] constr-decl  { | constr-decl }   *)
   and parseTypeConstructorDeclarations ?first p =
    let firstConstrDecl = match first with
    | None ->
      ignore (Parser.optional p Token.Bar);
      parseTypeConstructorDeclaration p
    | Some firstConstrDecl ->
      firstConstrDecl
    in
    let rec loop p acc =
      match p.Parser.token with
      | Bar ->
        Parser.next p;
        let constrDecl = parseTypeConstructorDeclaration p in
        loop p (constrDecl::acc)
      | _ ->
        List.rev acc
    in
    loop p [firstConstrDecl]

  (*
   * type-representation ::=
      ∣	 = private [ | ] constr-decl  { | constr-decl }
      ∣	 = private record-decl
      |  = ..
  *)
and parseTypeRepresentation p =
    (* = consumed *)
    let privateFlag =
      if Parser.optional p Token.Private
      then Asttypes.Private
      else Asttypes.Public
    in
    let kind = match p.Parser.token with
    | Bar | Uident _ ->
      Parsetree.Ptype_variant (parseTypeConstructorDeclarations p)
    | Lbrace ->
      Parsetree.Ptype_record (parseRecordDeclaration p)
    | DotDot ->
      Parser.next p;
      Ptype_open
    | unknownToken -> raise (Parser.ParseError (p.startPos, Report.Unexpected unknownToken))
    in
    (privateFlag, kind)

  and parseTypeParam p =
    let variance = match p.Parser.token with
    | Plus -> Parser.next p; Asttypes.Covariant
    | Minus -> Parser.next p; Contravariant
    | _ -> Invariant
    in
    let param = match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      begin match p.Parser.token with
      | Lident ident ->
        Parser.next p;
        (Ast_helper.Typ.var ident, variance)
      | _ -> raise (Parser.ParseError (p.startPos, Report.Lident))
      end
    | Underscore ->
      Parser.next p;
      (Ast_helper.Typ.any (), variance)
    | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))
    in
    Parser.expectExn GreaterThan p;
    param

  and parseTypeParams p =
    let params = match p.Parser.token with
    | LessThan -> Parser.next p; [parseTypeParam p]
    | _ -> []
    in
    params

  and parseTypeConstraint p =
    Parser.expectExn SingleQuote p;
    begin match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      Parser.expectExn Equal p;
      let typ = parseTypExpr p in
      (Ast_helper.Typ.var ident, typ, Location.none)
    | _ -> raise (Parser.ParseError (p.startPos, Report.Lident))
    end

  and parseTypeConstraints p =
    let rec loop p constraints =
      match p.Parser.token with
      | Constraint ->
        Parser.next p;
        let constraint_ = parseTypeConstraint p in
        (constraint_::constraints)
      | _ -> List.rev constraints
    in
    loop p []

  and parseTypeEquationOrConstrDecl p =
    match p.Parser.token with
    | Uident uident ->
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        let rec loop p path =
          match p.Parser.token with
         | Lident ident ->
           Parser.next p;
           Longident.Ldot(path, ident)
         | Uident uident ->
           Parser.next p;
           Parser.expectExn Dot p;
           loop p (Longident.Ldot (path, uident))
         | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))
        in
        let typeConstr = Location.mknoloc (loop p (Longident.Lident uident)) in
        let typ = match p.Parser.token with
        | Lparen ->
          Parser.next p;
          let (_endPos, args) = parseConstructorTypeArgs p in
          Ast_helper.Typ.constr typeConstr args
        | _ ->
          Ast_helper.Typ.constr typeConstr []
        in
        (Some typ, Parsetree.Ptype_abstract)
      | _ ->
        Parser.next p;
        let (args, res) = parseConstrDeclArgs p in
        let first = Some (
          Ast_helper.Type.constructor ?res ~args (Location.mknoloc uident)
        ) in
        (None, Parsetree.Ptype_variant (parseTypeConstructorDeclarations p ?first))
      end
    | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))

  and parseTypeEquationAndRepresentation p =
    match p.Parser.token with
    | Equal ->
      Parser.next p;
      begin match p.Parser.token with
      | Uident ident ->
        let (manifest, kind) = parseTypeEquationOrConstrDecl p in
        (manifest, Asttypes.Public, kind)
      (* start of type representation, beware Uident
       * type t = Foo.t indicates type-equation, not representation! *)
      | Bar | Lbrace | Private | Dot ->
        let manifest = None in
        let (priv, kind) = parseTypeRepresentation p in
        (manifest, priv, kind)
      | _ ->
        let manifest = Some (parseTypExpr p) in
        begin match p.Parser.token with
        | Equal ->
          Parser.next p;
          let (priv, kind) = parseTypeRepresentation p in
          (manifest, priv, kind)
        | _ ->
          (manifest, Public, Parsetree.Ptype_abstract)
        end
      end
    | _ -> (None, Public, Parsetree.Ptype_abstract)

  (* let parseConstrDef p = *)
    (* match p.Parser.token with *)
    (* | Uident ident -> *)
      (* Parser.next p; *)



  (* let parseTypeExtensionDef p = *)
    (* + consumed *)
    (* Parser.expectExn p Equal; *)
    (* let privateFlag = *)
      (* if Parser.optional p Token.Private *)
      (* then Asttypes.Private *)
      (* else Asttypes.Public *)
    (* in *)
    (* ignore (Parser.optional p Token.Bar); *)
    (* let firstConstrDef = parseConstrDef p in *)


(*
  type-definition	::=	type [nonrec] typedef  { and typedef }

  typedef	::=	[type-params]  typeconstr-name  type-information

  type-information	::=	[type-equation]  [type-representation]  { type-constraint }

  type-equation	::=	= typexpr

  type-representation	::=	= [|] constr-decl  { | constr-decl }
    ∣	 = record-decl
    ∣	 = |
*)

  and parseTypeDef ?attrs p =
    let startPos = p.Parser.startPos in
    let attrs = match attrs with | Some attrs -> attrs | None -> parseAttributes p in
    let typeConstrName = match p.Parser.token with
    | Lident ident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      (Location.mkloc ident loc)
    | _ -> raise (Parser.ParseError (p.startPos, Report.Message "Type constructor name should be lowercase"))
    in
    let params = parseTypeParams p in
    match p.Parser.token with
    (* | Plus -> *)
      (* Parser.next p; *)
      (* let (priv, kind) = parseTypeExtensionDef p in *)
      (* Ast_helper.Type.mk ~priv ~kind typeConstrName *)
    | _ ->
      let (manifest, priv, kind) = parseTypeEquationAndRepresentation p in
      let cstrs = parseTypeConstraints p in
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in
      Ast_helper.Type.mk
        ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest typeConstrName

  and parseTypeDefinition ~attrs p =
    Parser.expectExn Token.Typ p;
    let recFlag =
      if Parser.optional p Token.Rec
        then Asttypes.Recursive
        else Asttypes.Nonrecursive
    in
    let typeDef = parseTypeDef ~attrs p in
    let rec loop p defs =
      match p.Parser.token with
      | And ->
        Parser.next p;
        let typeDef = parseTypeDef p in
        loop p (typeDef::defs)
      | _ ->
        List.rev defs
    in
    (recFlag, loop p [typeDef])

  and parsePrimitive p =
    let rec loop p prims =
      match p.Parser.token with
      | String s ->
        Parser.next p;
        loop p (s::prims)
      | _ ->
        begin match prims with
        | [] -> raise (Parser.ParseError (p.startPos, Report.Message "An external definition should have at least one primitive. Example: \"setTimeout\""))
        | prims -> List.rev prims
        end
    in
    loop p []

  and parseExternalDef ~attrs p =
    Parser.leaveBreadcrumb p Report.External;
    let startPos = p.Parser.startPos in
    Parser.expectExn Token.External p;
    let name = match p.Parser.token with
    | Lident ident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc ident loc
    | _ -> raise (Parser.ParseError (p.startPos, Report.Lident))
    in
    Parser.expectExn ~circumstance:(Report.TypeExpression) Colon p;
    let typExpr = parseTypExpr p in
    Parser.expectExn Equal p;
    let prim = parsePrimitive p in
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    let vb = Ast_helper.Val.mk ~loc ~attrs ~prim name typExpr in
    Parser.eatBreadcrumb p;
    vb

  and parseConstrDeclOrName p =
    let name = match p.Parser.token with
    | Uident name ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc name loc
    | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))
    in
    let kind = match p.Parser.token with
    | Lparen | Lbrace ->
      let (args, res) = parseConstrDeclArgs p in
      Parsetree.Pext_decl (args, res)
    | Equal ->
      Parser.next p;
      let longident = parseModuleLongIdent p in
      Parsetree.Pext_rebind longident
    | _ ->
      Parsetree.Pext_decl (Pcstr_tuple [], None)
    in
    (name, kind)

  and parseExceptionDef ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Token.Exception p;
    let (name, kind) = parseConstrDeclOrName p in
    let endPos = p.Parser.prevEndPos in
    let loc = mkLoc startPos endPos in
    Ast_helper.Te.constructor ~loc ~attrs name kind

  and parseStructure p =
    let rec parse p acc = match p.Parser.token with
      | Eof | Rbrace -> acc
      | prevToken ->
        let item = parseStructureItem p in
        ignore (Parser.optional p Semicolon);
        (* let () = match p.Parser.token with *)
        (* | Semicolon -> Parser.next p *)
        (* (* TODO: this is shady *) *)
        (* | Open | Let | Typ | External | Include | Module | Eof *)
        (* | String _ | Int _ | Float _ | Lbrace | Lparen | True | False *)
        (* | Backtick | Uident _ | Lident _ | Lbracket | Assert | Lazy *)
        (* | If | For | While | Switch | LessThan | Rbrace -> () *)
        (* | At when prevToken <> Let && prevToken <> Module -> () *)
        (* | _ -> Parser.expectExn Semicolon p *)
        (* in *)
        parse p (item::acc)
    in
    let structure = parse p [] in
    List.rev structure

  and parseStructureItem p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let item = match p.Parser.token with
    | Open ->
      Ast_helper.Str.open_ (parseOpenDescription ~attrs p)
    | Let ->
      let (recFlag, letBindings) = parseLetBindings ~attrs p in
      Ast_helper.Str.value recFlag letBindings
    | Typ ->
      let (recFlag, typeDecls) = parseTypeDefinition ~attrs p in
      Ast_helper.Str.type_ recFlag typeDecls
    | External ->
      Ast_helper.Str.primitive (parseExternalDef ~attrs p)
    | Exception ->
      Ast_helper.Str.exception_ (parseExceptionDef ~attrs p)
    | Include ->
      Ast_helper.Str.include_ (parseIncludeStatement ~attrs p)
    | Module -> parseModuleOrModuleTypeImpl p
    | _ ->
      Ast_helper.Str.eval ~attrs (parseExpr ~context:StructureExpr p)
    in
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    {item with pstr_loc = loc}

  and parseIncludeStatement ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Token.Include p;
    let modExpr = parseModuleExpr p in
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    Ast_helper.Incl.mk ~loc ~attrs modExpr

  and parseModuleOperand p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in
      Ast_helper.Mod.ident ~loc (Location.mkloc (Longident.Lident ident) loc)
    | Lbrace ->
      Parser.next p;
      let structure = Ast_helper.Mod.structure (parseStructure p) in
      Parser.expectExn Rbrace p;
      let endPos = p.prevEndPos in
      {structure with pmod_loc = mkLoc startPos endPos}
    | Lparen ->
      parseParenthesizedOrFunctorModuleExpr p
    | unknownToken -> raise (Parser.ParseError (p.startPos, Report.Unexpected unknownToken))

  and parseParenthesizedOrFunctorModuleExpr p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Lparen p;
    match p.Parser.token with
    | Underscore -> (* functor arg name, parse functor *)
      parseFunctorModuleExpr p []
    | Rparen ->
      Parser.next p;
      let rparenEnd = p.prevEndPos in
      Parser.expectExn EqualGreater p;
      let rhs = parseModuleExpr p in
      let loc = mkLoc startPos p.prevEndPos in
      let arg = Location.mkloc "*" (mkLoc startPos rparenEnd) in
      Ast_helper.Mod.functor_ ~loc arg None rhs
    | _ ->
      let moduleExpression = parseModuleExpr p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let moduleType = parseModuleType p in
        begin match p.Parser.token with
        | Comma -> (* comma hints possible es6 style arrow: (A: Foo, ) => module_expr *)
          Parser.next p;
          let argName = match moduleExpression.pmod_desc with
            | Parsetree.Pmod_ident ({Location.txt} as lident) ->
              {lident with txt = Longident.last txt}
            | _ -> raise (Parser.ParseError (p.startPos,
              Report.Message "A functor arg should be a module name, like List
              or Array"
              ))
            in
          let arg = (
            argName,
            Some moduleType
          ) in
          parseFunctorModuleExpr p [arg]
        | Rparen ->
          Parser.next p;
          let rparenEnd = p.prevEndPos in
          begin match p.Parser.token with
          | EqualGreater ->
            Parser.next p;
            let rhs = parseModuleExpr p in
            let argName = match moduleExpression.pmod_desc with
            | Parsetree.Pmod_ident ({Location.txt} as lident) ->
              {lident with txt = Longident.last txt}
            | _ -> raise (Parser.ParseError (p.startPos,
              Report.Message "A functor arg should be a module name, like List
              or Array"
              ))
            in
            let loc = mkLoc startPos rhs.pmod_loc.loc_end in
            Ast_helper.Mod.functor_ ~loc argName (Some moduleType) rhs
          | _ ->
            let loc = mkLoc startPos rparenEnd in
            Ast_helper.Mod.constraint_ ~loc moduleExpression moduleType
          end
        | _ -> raise (Parser.ParseError (p.startPos, Report.OneOf [Rparen; Comma]))
        end
      | Rparen ->
        Parser.next p;
        let rparenEnd = p.prevEndPos in
        begin match p.Parser.token with
        | EqualGreater ->
          raise (Parser.ParseError (
            p.startPos,
            Report.Message "A functor arg needs a module type"
          ))
        | _ ->
          {moduleExpression with pmod_loc = mkLoc startPos rparenEnd}
        end
      | _ -> raise (Parser.ParseError (p.startPos, Report.OneOf [Rparen; Comma]))
      end

  and parseFunctorArgName p =
    let startPos = p.Parser.startPos in
    let ident = match p.Parser.token with
    | Uident ident -> ident
    | Underscore -> "_"
    | _ -> raise (Parser.ParseError (p.startPos,
      Report.Message "a functor arg name should be Uident or _"
      ))
    in
    Parser.next p;
    Location.mkloc ident (mkLoc startPos p.prevEndPos)

  and parseFunctorArgs p args =
    let rec loop p args =
      match p.Parser.token with
      | Rparen -> Parser.next p; args
      | _ ->
        let functorArgName = parseFunctorArgName p in
        Parser.expectExn Colon p;
        let moduleType = parseModuleType p in
        let arg = (functorArgName, Some moduleType) in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (arg::args)
        | Rparen ->
          Parser.next p;
          (arg::args)
        | _ -> raise (Parser.ParseError (p.startPos, Report.OneOf [Rparen; Comma]))
        end
    in
    loop p args

  and parseFunctorModuleExpr p args =
    let args = parseFunctorArgs p args in
    Parser.expectExn EqualGreater p;
    let rhsModuleExpr = parseModuleExpr p in
    List.fold_left (fun acc (name, moduleType) ->
      Ast_helper.Mod.functor_ name moduleType acc
    ) rhsModuleExpr args

  and parseModuleExpr p =
    let modExpr = parseModuleOperand p in
    let rec loop p modExpr =
      match p.Parser.token with
      | Lparen ->
        loop p (parseModuleApplication p modExpr)
      | EqualGreater ->
        Parser.next p;
        let arg = match modExpr.Parsetree.pmod_desc with
        | Parsetree.Pmod_ident {Location.loc; txt} ->
          {Location.loc; txt = Longident.last txt}
        | _ -> raise (Parser.ParseError (p.startPos,
          Report.Message "A functor arg should be a lowercased module identifier"
          ))
        in
        let functorBody = parseModuleExpr p in
        let loc = mkLoc modExpr.pmod_loc.loc_start p.Parser.prevEndPos in
        Ast_helper.Mod.functor_ ~loc arg None functorBody
      | _ ->
        modExpr
    in loop p modExpr

  and parseModuleApplication p modExpr =
    Parser.expectExn Lparen p;
    let arg = parseModuleExpr p in
    Parser.expectExn Rparen p;
    let endPos = p.prevEndPos in
    let loc = mkLoc modExpr.pmod_loc.loc_start endPos in
    Ast_helper.Mod.apply ~loc modExpr arg

  and parseModuleOrModuleTypeImpl p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Module p;
    match p.Parser.token with
    | Typ -> parseModuleTypeImpl startPos p
    | _ -> parseMaybeRecModuleBinding p

  and parseModuleTypeImpl startPos p =
    Parser.expectExn Typ p;
    let nameStart = p.Parser.startPos in
    let name = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      let loc = mkLoc nameStart p.prevEndPos in
      Location.mkloc ident loc
    | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))
    in
    Parser.expectExn Equal p;
    let moduleType = parseModuleType p in
    let moduleTypeDeclaration =
      Ast_helper.Mtd.mk
        ~loc:(mkLoc name.loc.loc_start p.prevEndPos)
        ~typ:moduleType
        name
    in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Str.modtype ~loc moduleTypeDeclaration

  (* definition	::=
    ∣	 module rec module-name :  module-type =  module-expr   { and module-name :  module-type =  module-expr }
    |  module module-name  { ( module-name :  module-type ) }  [ : module-type ]  =  module-expr *)
  and parseMaybeRecModuleBinding p =
    if Parser.optional p Token.Rec then
      Ast_helper.Str.rec_module (parseModuleBindings p)
    else
      Ast_helper.Str.module_ (parseModuleBinding p)

  and parseModuleBinding p =
    let startPos = p.Parser.startPos in
    let name = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      Location.mkloc ident loc
    | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))
    in
    let body = parseModuleBindingBody p in
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    Ast_helper.Mb.mk ~loc name body

  and parseModuleBindingBody p =
    Parser.expect Equal p;
    parseModuleExpr p

  and parseModuleBindings p =
    (* module-name :  module-type =  module-expr *)
    (* { and module-name :  module-type =  module-expr } *)
    let rec loop p acc =
      match p.Parser.token with
      | And ->
        Parser.next p;
        let modBinding = parseModuleBinding p in
        loop p (modBinding::acc)
      | _ -> List.rev acc
    in
    let first = parseModuleBinding p in
    match p.Parser.token with
    | And -> loop p [first]
    | _ -> [first]

  (* Ocaml allows module types to end with lowercase: module Foo : bar = { ... }
   * lets go with uppercase terminal for now *)
   and parseModuleTypePath p =
    Ast_helper.Mty.ident (parseModuleLongIdent p)

  (* Module types are the module-level equivalent of type expressions: they
   * specify the general shape and type properties of modules. *)
 and parseModuleType p =
    let startPos = p.Parser.startPos in
    let moduleType = match p.Parser.token with
    | Uident _ ->
      parseModuleTypePath p
    | Lparen ->
      Parser.next p;
      let mty = parseModuleType p in
      Parser.expectExn Rparen p;
      mty
    | Lbrace ->
      Parser.next p;
      let spec = parseSpecification p in
      Parser.expectExn Rbrace p;
      spec
    | Module ->
      parseModuleTypeOf p
    | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))
    in
    let moduleTypeLoc = mkLoc startPos p.prevEndPos in
    let moduleType = {moduleType with pmty_loc = moduleTypeLoc} in
    match p.Parser.token with
    | With ->
      let constraints = parseWithConstraints p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Mty.with_ ~loc moduleType constraints
    | _ ->
      moduleType

  and parseWithConstraints p =
    Parser.expectExn With p;
    let first = parseWithConstraint p in
    let rec loop p acc =
      match p.Parser.token with
      | And ->
        Parser.next p;
        loop p ((parseWithConstraint p)::acc)
      | _ ->
        List.rev acc
    in loop p [first]

  and parseWithConstraint p =
    match p.Parser.token with
    | Module ->
      Parser.next p;
      let modulePath = parseModuleLongIdent p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        Parser.expectExn Equal p;
        let lident = parseModuleLongIdent p in
        Parsetree.Pwith_modsubst (modulePath, lident)
      | Equal ->
        Parser.next p;
        let lident = parseModuleLongIdent p in
        Parsetree.Pwith_module (modulePath, lident)
      | _ -> raise (Parser.ParseError (p.startPos, Report.OneOf [Equal; ColonEqual]))
      end
    | Typ ->
      Parser.next p;
      let typeConstr = parseValuePath p in
      let params = parseTypeParams p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        Parser.expectExn Equal p;
        let typExpr = parseTypExpr p in
        Parsetree.Pwith_typesubst (
          typeConstr,
          Ast_helper.Type.mk
            ~params
            ~manifest:typExpr
            (Location.mkloc (Longident.last typeConstr.txt) typeConstr.loc)
        )
      | Equal ->
        Parser.next p;
        let typExpr = parseTypExpr p in
        let typeConstraints = parseTypeConstraints p in
        Parsetree.Pwith_type (
          typeConstr,
          Ast_helper.Type.mk
            ~params
            ~manifest:typExpr
            ~cstrs:typeConstraints
            (Location.mkloc (Longident.last typeConstr.txt) typeConstr.loc)
        )
      | _ -> raise (Parser.ParseError (p.startPos, Report.OneOf [Equal; ColonEqual]))
      end
    | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))

  and parseModuleTypeOf p =
    Parser.expectExn Module p;
    Parser.expectExn Typ p;
    Parser.expectExn Of p;
    let moduleExpr = parseModuleExpr p in
    Ast_helper.Mty.typeof_ moduleExpr

  and parseSpecification p =
    (* { consumed *)
    let rec loop p spec =
      let item = parseSignatureItem p in
      let () = match p.Parser.token with
      | Semicolon -> Parser.next p
      | Let | Typ | External | Exception | Open | Include | Module | Eof -> ()
      | _ -> Parser.expectExn Semicolon p
      in
      let spec = (item::spec) in
      match p.Parser.token with
      | Rbrace | Eof -> spec
      | _ -> loop p spec
    in
    Ast_helper.Mty.signature (loop p [])

  and parseSignatureItem p =
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Let ->
      parseSignLetDesc p
    | Typ ->
      let (recFlag, typeDecls) = parseTypeDefinition ~attrs p in
      Ast_helper.Sig.type_ recFlag typeDecls
    | External ->
      Ast_helper.Sig.value (parseExternalDef ~attrs p)
    | Exception ->
      Ast_helper.Sig.exception_ (parseExceptionDef ~attrs p)
    | Open ->
      Ast_helper.Sig.open_ (parseOpenDescription ~attrs p)
    | Include ->
      Parser.next p;
      let moduleType = parseModuleType p in
      let includeDescription = Ast_helper.Incl.mk moduleType in
      Ast_helper.Sig.include_ includeDescription
    | Module ->
      Parser.next p;
      begin match p.Parser.token with
      | Uident _ ->
        parseModuleDeclarationOrAlias ~attrs p
      | Typ ->
        parseModuleTypeDeclaration ~attrs p
      | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))
      end
    | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))

  and parseModuleDeclarationOrAlias ~attrs p =
    let moduleName = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))
    in
    let body = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parseModuleType p
    | Equal ->
      Parser.next p;
      let lident = parseModuleLongIdent p in
      Ast_helper.Mty.alias lident
    | _ -> raise (Parser.ParseError (p.startPos, Report.OneOf [Colon; Equal]))
    in
    Ast_helper.Sig.module_ (Ast_helper.Md.mk ~attrs moduleName body)

  and parseModuleTypeDeclaration ~attrs p =
    Parser.expectExn Typ p;
    let moduleName = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.ParseError (p.startPos, Report.Uident))
    in
    let typ = match p.Parser.token with
    | Equal ->
      Parser.next p;
      Some (parseModuleType p)
    | _ -> None
    in
    let moduleDecl = Ast_helper.Mtd.mk ~attrs ?typ moduleName in
    Ast_helper.Sig.modtype moduleDecl

  and parseSignLetDesc p =
    Parser.expectExn Let p;
    let name = match p.Parser.token with
    | Lident ident ->
      let nameStartPos = p.startPos in
      Parser.next p;
      Location.mkloc ident (mkLoc nameStartPos p.prevEndPos)
    | _ -> raise (Parser.ParseError (p.startPos, Report.Lident))
    in
    Parser.expectExn Colon p;
    let typExpr = parsePolyTypeExpr p in
    let valueDesc = Ast_helper.Val.mk name typExpr in
    Ast_helper.Sig.value valueDesc

(*    attr-id	::=	lowercase-ident
 	∣	  capitalized-ident
 	∣	  attr-id .  attr-id   *)
  and parseAttributeId p =
    let startPos = p.Parser.startPos in
    let rec loop p acc =
      match p.Parser.token with
      (* TODO keywords! *)
      | Lident ident | Uident ident ->
        Parser.next p;
        let id = acc ^ ident in
        begin match p.Parser.token with
        | Dot -> Parser.next p; loop p (id ^ ".")
        | _ -> id
        end
      | token when Token.isKeyword token ->
        Parser.next p;
        let id = acc ^ (Token.toString token) in
        begin match p.Parser.token with
        | Dot -> Parser.next p; loop p (id ^ ".")
        | _ -> id
        end
      | token -> raise (Parser.ParseError (p.startPos, Report.Unexpected token))
    in
    let id = loop p "" in
    let endPos = p.prevEndPos in
    Location.mkloc id (mkLoc startPos endPos)

  (*
   * payload ::=  empty
   *          |  ( structure-item )
   *
   * TODO: what about multiple structure items?
   * @attr({let x = 1; let x = 2})
   *
   * Also what about type-expressions and specifications?
   * @attr(:myType) ???
   *)
  and parsePayload cnumEndAttrId p =
    let structure = match p.Parser.token with
    | Lparen when p.startPos.pos_cnum = cnumEndAttrId + 1 ->
      Parser.next p;
      let item = parseStructureItem p in
      Parser.expectExn Rparen p;
      [item]
    | _ -> []
    in
    Parsetree.PStr structure

  (* type attribute = string loc * payload *)
  and parseAttribute p =
    Parser.expectExn At p;
    let attrId = parseAttributeId p in
    let cnumEndAttrId = p.Parser.prevEndPos.pos_cnum - 1 in
    let payload = parsePayload cnumEndAttrId p in
    (attrId, payload)

  and parseAttributes p =
    let rec loop p attrs =
      match p.Parser.token with
      | At ->
        let attr = parseAttribute p in
        loop p (attr::attrs)
      | _ ->
        List.rev attrs
    in
    loop p []

  let extractExportedType structureItem =
    let loc = structureItem.Parsetree.pstr_loc in
    match structureItem.Parsetree.pstr_desc with
    | Pstr_primitive valueDescription ->
      Ast_helper.Sig.value ~loc valueDescription
    | Pstr_type (recFlag, typeDecls) ->
      Ast_helper.Sig.type_ ~loc recFlag typeDecls
    | Pstr_exception extensionConstructor ->
      Ast_helper.Sig.exception_ ~loc extensionConstructor
    | Pstr_open openDescription ->
      Ast_helper.Sig.open_ ~loc openDescription
    (* | Pstr_include includeDeclaration -> *)
      (* Ast_helper.Sig.include_ ~loc includeDeclaration *)
    | Pstr_value (Asttypes.Nonrecursive, [vb]) ->
      let (var, typ) = match vb.pvb_pat.ppat_desc with
      | Ppat_constraint ({ppat_desc= Ppat_var stringLoc}, coreType) ->
        (stringLoc, coreType)
      | _ -> raise (Parser.ParseError (vb.pvb_loc.loc_start, Report.Message "Hey! If you want to export an item, you need to add a type."))
      in
      let vd = Ast_helper.Val.mk var typ in
      Ast_helper.Sig.value ~loc vd
    | _ -> raise (Parser.ParseError (loc.loc_start, Report.Message "Hey! we don't support exporting here (yet?)"))

  let parseExportItem p =
    match p.Parser.token with
    | Export ->
      Parser.next p;
      let structureItem = parseStructureItem p in
      (structureItem, Some (extractExportedType structureItem))
    | _ -> (parseStructureItem p, None)

  let compileExports items =
    List.fold_right (fun curr (items, exports) ->
      match curr with
      | (structureItem, Some export) ->
        (structureItem::items, export::exports)
      | (structureItem, None) ->
        (structureItem::items, exports)
    ) items ([], [])

  let parseFile p =
    let rec loop p items =
      begin match p.Parser.token with
      | Eof ->
        List.rev items
      | _ ->
        let item = parseExportItem p in
        let () = match p.Parser.token with
        | Semicolon -> Parser.next p
        | Open | Let | Typ | External | Include | Module | Eof
        | String _ | Int _ | Float _ | Lbrace | Lparen | True | False
        | Backtick | Uident _ | Lident _ | Lbracket | Assert | Lazy
        | If | For | While | Switch | LessThan | Export -> ()
        | _ -> Parser.expectExn Semicolon p
        in
        loop p (item::items)
      end
    in
    let items = loop p [] in
    let (structure, exports) = compileExports items in
    let modExpr = Ast_helper.Mod.constraint_
      (Ast_helper.Mod.structure structure)
      (Ast_helper.Mty.signature exports)
    in
    [Ast_helper.Str.include_ (Ast_helper.Incl.mk modExpr)]

  let read_file filename =
    let txt = ref "" in
    let chan = open_in filename in
    try
      while true; do
        txt := !txt ^ input_line chan ^ "\n"
      done; String.sub !txt 0 ((String.length !txt) - 1)
    with End_of_file ->
      close_in chan;
      String.sub !txt 0 ((String.length !txt) - 1);;

  let () =
    let filename = Sys.argv.(1) in
    let src = read_file filename in
    let p = Parser.make src filename in
    try
      let ast = parseStructure p in
      Pprintast.structure Format.std_formatter ast;
      Format.pp_print_flush Format.std_formatter ();
      print_newline();
      (* Printast.implementation Format.std_formatter ast; *)
      (* Format.pp_print_flush Format.std_formatter (); *)
      (* print_newline(); *)
      List.iter (fun (pos, problem) ->
        Printf.eprintf "Parse error\n";
        Printf.eprintf "Line: %d, Column: %d\n" (pos.Lexing.pos_lnum) (pos.pos_cnum - pos.pos_bol + 1);
        Printf.eprintf "%s\n"
        (match problem with
        | Report.Expected (t, _) ->
          "Missing " ^ Token.toString t
        | _ -> "Todo: pretty print parse error")
      ) p.errors;
    with
    | Parser.ParseError (pos, problem) ->
      Printf.eprintf "Parse error\n";
      Printf.eprintf "Line: %d, Column: %d\n" (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol + 1);
      Printf.eprintf "Problem encountered while trying to parse %s.\n"
      (Report.parseContext p.breadcrumbs);
      Printf.eprintf "%s\n"
      (match problem with
      | Report.Uident -> "I'm expecting an uppercased identifier like Foo or Bar"
      | Report.Lident -> "I'm expecting an lowercased identifier like foo or bar"
      | Report.OneOf tokens ->
          "I'm expecting one of the following tokens:\n"
          ^ (String.concat "\n" (List.map (fun t -> "• " ^ (Token.toString t)) tokens))
      | Message msg -> msg
      | Unexpected t ->
          let name = (Token.toString t) in
          begin match p.breadcrumbs with
          | (ExprOperand, _)::breadcrumbs ->
              begin match breadcrumbs, p.token with
              | (SeqExpr, _) :: _, Rbrace ->
                "It seems that this expression block is empty"
              | (ExprSetField, _) :: _, _ ->
                "It seems that this record field mutation misses an expression"
              | (ExprArrayMutation, _) :: _, _ ->
                "Seems that an expression is missing, with what do I mutate the array?"
              | ((ExprBinaryAfterOp _ | ExprUnary), _) ::_, _ ->
                "Did you forget to write an expression here?"
              | _ ->
                "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
              end
          | _ ->
            (* TODO: match on circumstance to verify Lident needed ? *)
            if Token.isKeyword t then
              name ^ " is a reserved keyword, it cannot be used as an identifier."
            else
            "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
          end
      | Expected (t, circumstance) ->
          "I'm expecting a \"" ^ (Token.toString t) ^ "\" here."
          ^ (match circumstance with
          | Some c -> "It signals the start of " ^ (Report.circumstanceToString c)
          | None -> "")
      | Unbalanced t ->
          "Closing \"" ^ (Token.toString t) ^ "\" seems to be missing."
      )
end
