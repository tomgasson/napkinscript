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
    | Forwardslash | ForwardslashDot
    | Asterisk | AsteriskDot | Exponentiation
    | Minus | MinusDot
    | Plus | PlusDot | PlusPlus
    | GreaterThan
    | LessThan
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
    | Lident str -> "Lident " ^ str
    | Uident str -> "Lident " ^ str
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
    | Exception -> "exception"
    | Hash -> "#" | HashHash -> "##" | HashEqual -> "#="
    | GreaterThan -> ">"
    | LessThan -> "<"
    | Asterisk -> "*" | AsteriskDot -> "*." | Exponentiation -> "**"
    | Assert -> "assert"
    | Lazy -> "lazy"
    | Tilde -> "tilde"
    | Question -> "question"
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

  let lookupKeyword str =
    try Hashtbl.find keywordTable str with
    | Not_found ->
      if CharacterCodes.isUpperCase (int_of_char str.[0]) then
        Uident str
      else Lident str
end

module Lex = struct
  type mode = Normal | Template

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
    let startOff = lexbuf.offset in
    while not (CharacterCodes.doubleQuote == lexbuf.ch) do
      next lexbuf
    done;
    next lexbuf;
    Token.String (
      Bytes.sub_string lexbuf.src startOff (lexbuf.offset - 1 - startOff)
    )

  let lexSingleLineComment lexbuf =
    let startOff = lexbuf.offset in
    while not (CharacterCodes.isLineBreak lexbuf.ch) do
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

  let lex lexbuf =
    skipWhitespace lexbuf;
    let startPos = position lexbuf in
    let ch = lexbuf.ch in
    let token = if lexbuf.mode = Template then
        lexTemplate lexbuf
    else if CharacterCodes.isLetter ch then
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
      else if ch == CharacterCodes.underscore then
        Token.Underscore
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
        if lexbuf.ch == CharacterCodes.equal then(
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
        endPos = Lexing.dummy_pos
      } in
      next parserState;
      parserState


    let optional p token =
      if p.token = token then
        let () = next p in true
      else
        false

    exception Expected of (Lexing.position * string)

    let expect p token =
      if p.token = token then
        next p
      else
        raise (Expected (p.startPos, ("expected: " ^ (Token.toString token))))

    let lookahead p callback =
      (* is this copying correct? *)
      let lexbufCopy = {p.lexbuf with filename = p.lexbuf.filename} in
      let parserStateCopy = {p with lexbuf = lexbufCopy} in
      callback parserStateCopy
  end


    exception Unbalanced

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
  | (Rparen, Token.Rparen) | (Rbrace, Rbrace) | (Rbracket, Rbracket)-> Parser.next state; ()
  | (Token.Lbracket | Lparen | Lbrace), _ ->
     goToClosing (getClosingToken state.Parser.token) state;
     Parser.next state;
     goToClosing closingToken state
  | ((Rparen | Token.Rbrace | Rbracket | Eof), _)  -> raise Unbalanced
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
        | Colon when not inTernary -> true
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
        | _ ->
          goToClosing Rparen state;
          begin match state.Parser.token with
          | EqualGreater -> true
          | Colon when not inTernary -> true
          | _ -> false
          end
        end
      | _ -> false)

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



  (* Ldot (Ldot (Lident "Foo", "Bar"), "baz") *)
  let parseValuePath p =
    let startPos = p.Parser.startPos in
    let rec aux p path =
      match p.Parser.token with
      | Lident ident -> Longident.Ldot(path, ident)
      | Uident uident ->
        Parser.next p;
        Parser.expect p Dot;
        aux p (Ldot (path, uident))
      | _ -> raise (Parser.Expected (p.startPos, "value path"))
    in
    let ident = match p.Parser.token with
    | Lident ident -> Longident.Lident ident
    | Uident ident ->
      Parser.next p;
      Parser.expect p Dot;
      aux p (Lident ident)
    | _ -> raise (Parser.Expected (p.startPos, "value path"))
    in
    let endPos = p.endPos in
    Parser.next p;
    Location.mkloc ident (mkLoc startPos endPos)

  (* Parses module identifiers:
       Foo
       Foo.Bar *)
  let parseModuleLongIdent p =
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
      | _ -> raise (Parser.Expected (p.startPos, "expected Uident"))
    in
    match p.Parser.token with
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
    | _ -> raise (Parser.Expected (p.startPos, "expected Uident"))

  let parseOpenDescription ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect p Open;
    let override = if Parser.optional p Token.Bang then
      Asttypes.Override
    else
      Asttypes.Fresh
    in
    let modident = match p.token with
    | Uident _ ->
      parseModuleLongIdent p
    | _ -> raise (Parser.Expected (p.startPos, "expected Uident"))
    in
    Parser.next p;
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    Ast_helper.Opn.mk ~loc ~attrs ~override modident

  (* constant	::=	integer-literal   *)
   (* ∣	 float-literal   *)
   (* ∣	 char-literal   *)
   (* ∣	 string-literal   *)
   (* ∣	 constr   *)
   (* ∣	 false   *)
   (* ∣	 true   *)
   (* ∣	 ()   *)
   (* ∣	 begin end   *)
   (* ∣	 []   *)
   (* ∣	 [||]   *)
   (* ∣	 `tag-name *)
  let parseConstant p =
    let constant = match p.Parser.token with
    | Int i -> Parsetree.Pconst_integer (i, None)
    | Float i -> Parsetree.Pconst_float (i, None)
    | String s -> Pconst_string(s, None)
    (* | Char c -> Pconst_char c *)
    | _ ->
      raise (Parser.Expected (p.startPos, "constant"))
    in
    Parser.next p;
    constant


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

      begin match p.token with
      | Dot ->
        Parser.next p;
        Parser.expect p Dot;
        let c2 = parseConstant p in
        Ast_helper.Pat.interval c c2
      | _ ->
        let loc = mkLoc startPos endPos in
        Ast_helper.Pat.constant ~loc c
      end
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
        let pat = parsePattern p in
        Parser.expect p Token.Rparen;
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
    | Exception ->
      Parser.next p;
      let pat = parsePattern p in
      let loc = mkLoc startPos pat.ppat_loc.loc_end in
      Ast_helper.Pat.exception_ ~loc ~attrs pat
    | Lazy ->
      Parser.next p;
      let pat = parsePattern p in
      let loc = mkLoc startPos pat.ppat_loc.loc_end in
      Ast_helper.Pat.lazy_ ~loc ~attrs pat
    | List ->
      parseListPattern ~attrs p
    | _ ->
      raise (Parser.Expected (p.startPos, "pattern"))
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
      | _ -> raise (Parser.Expected (p.startPos, "ident plz"))
      end
    | _ -> pat
    end

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
    Parser.expect p Lbrace;
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
      | _ ->
        raise (Parser.Expected (p.startPos, "record pattern field should be lident or uident"))
    in
    let (fields, closedFlag) = loop p [firstField] in
    let endPos = p.endPos in
    Parser.expect p Rbrace;
    let loc = mkLoc startPos endPos in
    Ast_helper.Pat.record ~loc ~attrs fields closedFlag

  and parseTuplePattern ~attrs p =
    (* '/' consumed *)
    let startPos = p.startPos in
    Parser.expect p Forwardslash;
    let rec loop p patterns =
      match p.Parser.token with
      | Forwardslash ->
        let endPos = p.endPos in
        Parser.next p;
        let patterns = List.rev patterns in
        let loc = mkLoc startPos endPos in
        Ast_helper.Pat.tuple ~loc~attrs patterns
      | _ ->
        let pattern = parsePattern p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (pattern::patterns)
        | Forwardslash ->
          let endPos = p.endPos in
          Parser.next p;
          let patterns = List.rev (pattern::patterns) in
          let loc = mkLoc startPos endPos in
          Ast_helper.Pat.tuple ~loc~attrs patterns
        | _ -> raise (Parser.Expected (p.startPos, "unexpected tuple pattern thing: need / or ,"))
        end
    in
    loop p []

  and parseListPattern ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect p List;
    Parser.expect p Lparen;
    let rec loop p patterns = match p.Parser.token with
    | Rparen ->
      Parser.next p;
      patterns
    | Comma -> Parser.next p; loop p patterns
    | DotDotDot ->
      Parser.next p;
      let pattern = parsePattern p in
      loop p ((true, pattern)::patterns)
    | _ ->
      let pattern = parsePattern p in
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
    Parser.expect p Lbracket;
    let rec loop p patterns =
      match p.Parser.token with
      | Rbracket ->
        let endPos = p.endPos in
        Parser.next p;
        let patterns = List.rev patterns in
        let loc = mkLoc startPos endPos in
        Ast_helper.Pat.array ~loc ~attrs patterns
      | _ ->
        let pattern = parsePattern p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (pattern::patterns)
        | Rbracket ->
          let endPos = p.endPos in
          Parser.next p;
          let patterns = List.rev (pattern::patterns) in
          let loc = mkLoc startPos endPos in
          Ast_helper.Pat.array ~loc ~attrs patterns
        | _ -> raise (Parser.Expected (p.startPos, "unexpected array pattern thing: need ] or ,"))
        end
    in
    loop p []


  and parseConstructorPatternWithArgs p constr startPos attrs =
    let lparen = p.startPos in
    Parser.expect p Lparen;
    let rec loop p patterns =
      match p.Parser.token with
      | Rparen ->
        let endPos = p.endPos in
        Parser.next p;
        (endPos, List.rev patterns)
      | _ ->
        let pattern = parsePattern p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (pattern::patterns)
        | Rparen ->
          let endPos = p.endPos in
          Parser.next p;
          (endPos, List.rev (pattern::patterns))
        | _ -> raise (Parser.Expected (p.startPos, "expected pattern constructor arg thing: ) or ,"))
        end
    in
    let (endPos, args) = match loop p [] with
    | (endPos, [pattern]) -> endPos, Some pattern
    | (endPos, patterns) ->
        endPos, Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen endPos) patterns)
    in
    Ast_helper.Pat.construct ~loc:(mkLoc startPos endPos) ~attrs constr args

  and parseExpr ?(context=OrdinaryExpr) p =
    let expr = if (context != WhenExpr) && isEs6ArrowExpression ~inTernary:(context=TernaryTrueBranchExpr) p then
      parseEs6ArrowExpression p
    else
      parseBinaryExpr ~allowAttrs:(context != StructureExpr) p 1
    in
    parseTernaryTrueBranchExpr expr p

  (* expr ? expr : expr *)
  and parseTernaryTrueBranchExpr leftOperand p =
    match p.Parser.token with
    | Question ->
      Parser.next p;
      let trueBranch = parseExpr ~context:TernaryTrueBranchExpr p in
      Parser.expect p Colon;
      let falseBranch = parseExpr p in
      Ast_helper.Exp.ifthenelse leftOperand trueBranch (Some falseBranch)
    | _ ->
      leftOperand

  and parseEs6ArrowExpression p =
    let parameters = parseParameters p in
    let returnType = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parseTypExpr p)
    | _ ->
      None
    in
    Parser.expect p EqualGreater;
    let body =
      let expr = parseExpr p in
      match returnType with
      | Some typ ->
        Ast_helper.Exp.constraint_ expr typ
      | None -> expr
    in
    List.fold_right (fun (lbl, defaultExpr, pat) expr ->
      Ast_helper.Exp.fun_ lbl defaultExpr pat expr
    ) parameters body

  and parseParameter p =
    let (lbl, pat) = match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let lblName = match p.Parser.token with
      | Lident ident ->
        Parser.next p;
        ident
      | _ -> raise (Parser.Expected (p.startPos, "label-name should be lowercase"))
      in
      begin match p.Parser.token with
      | Comma | Equal | Rparen ->
        (Asttypes.Labelled lblName, Ast_helper.Pat.var (Location.mknoloc lblName))
      | As ->
        Parser.next p;
        let pat = parsePattern p in
        (Asttypes.Labelled lblName, pat)
      | _ -> raise (Parser.Expected (p.startPos, "Expectected , or = or as"))
      end
    | _ ->
      (Asttypes.Nolabel, parsePattern p)
    in
    (* TODO: explicitly passed optional and constraints *)
    match p.Parser.token with
    | Equal ->
      Parser.next p;
      let expr = parseExpr p in
      (lbl, Some expr, pat)
    | _ ->
      (lbl, None, pat)

  and parseParameterList p =
    (* ( consumed *)
    let rec loop p parameters =
      match p.Parser.token with
      | Comma ->
        Parser.next p;
        loop p parameters
      | Rparen ->
        Parser.next p;
        List.rev parameters
      | _ ->
        let parameter = parseParameter p in
        loop p (parameter::parameters)
    in
    loop p []

  (*
   * parameters ::= _
   *             | lident
   *             | ( parameter {, parameter} [,] )
   *)
  and parseParameters p =
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      [Asttypes.Nolabel, None, Ast_helper.Pat.var (Location.mknoloc ident)]
    | Underscore ->
      Parser.next p;
      [Asttypes.Nolabel, None, Ast_helper.Pat.any ()]
    | Lparen ->
      Parser.next p;
      begin match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let unitPattern = Ast_helper.Pat.construct (Location.mknoloc (Longident.Lident "()")) None in
        [Asttypes.Nolabel, None, unitPattern]
      | _ -> parseParameterList p
      end

    | _ -> raise (Parser.Expected (p.startPos, "Unsupported parameter"))

  and parseOperand p =
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
      | Lident ident ->
        let endPos = p.endPos in
        Parser.next p;
        let loc = mkLoc startPos endPos in
        Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident ident) loc)
      | Uident _ ->
        parseValueOrConstructor p
      | Lparen ->
        Parser.next p;
        begin match p.Parser.token with
        | Rparen ->
          Parser.next p;
          Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None
        | _ ->
          let expr = parseExpr p in
          Parser.expect p Rparen;
          expr
        end
      | List ->
        parseListExpr p
      | Lbracket ->
        parseArrayExp p
      | Lbrace ->
        Parser.next p;
        let e = parseBracedOrRecordExpr p in
        Parser.expect p Rbrace;
        e
      | Forwardslash ->
        Parser.next p;
        let expr = parseTuple p in
        expr
      (* TODO: check if Assert/Lazy/If/For/While/Switch belong here,
       * they might be not 100% simple/atomic *)
      | Assert ->
        Parser.next p;
        let expr = parseExpr p in
        Ast_helper.Exp.assert_ expr
      | Lazy ->
        Parser.next p;
        let expr = parseExpr p in
        Ast_helper.Exp.lazy_ expr
      | If ->
        Parser.next p;
        parseIfExpression p
      | For ->
        Parser.next p;
        parseForExpression p
      | While ->
        Parser.next p;
        parseWhileExpression p
      | Switch ->
        Parser.next p;
        parseSwitchExpression p
      | LessThan ->
        parseJsx p
      | _ ->
        raise (Parser.Expected (p.startPos, "unsupported expresion"))
    in
    expr
    (* {expr with pexp_loc = mkLoc startPos p.startPos} *)

  (* TODO: is this "operand"-arg a clear API? probably not with positions… *)
  and parsePrimaryExpr ?operand p =
    let startPos = p.Parser.startPos in
    let e1 = match operand with
      | Some e -> e
      | None -> parseOperand p
    in
    let rec loop p expr =
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        let lident = parseValuePath p in
        begin match p.Parser.token with
        | Equal ->
          Parser.next p;
          let endPos = p.prevEndPos in
          let loc = mkLoc startPos endPos in
          Ast_helper.Exp.setfield ~loc expr lident (parseExpr p)
        | _ ->
          let endPos = p.prevEndPos in
          let loc = mkLoc startPos endPos in
          loop p (Ast_helper.Exp.field ~loc expr lident)
        end
      | Lbracket ->
        let lbracket = p.startPos in
        Parser.next p;
        let accessExpr = parseExpr p in
        Parser.expect p Rbracket;
        let rbracket = p.prevEndPos in
        let arrayLoc = mkLoc lbracket rbracket in

        begin match p.token with
        | Equal ->
          Parser.next p;
          let rhsExpr = parseExpr p in
          let arraySet = Location.mkloc
            (Longident.Ldot(Lident "Array", "set"))
            arrayLoc
          in
          let endPos = p.prevEndPos in
          Ast_helper.Exp.apply
            ~loc:(mkLoc startPos endPos)
            (Ast_helper.Exp.ident ~loc:arrayLoc arraySet)
            [Nolabel, expr; Nolabel, accessExpr; Nolabel, rhsExpr]
        | _ ->
          let endPos = p.prevEndPos in
          loop p
            (Ast_helper.Exp.apply
              ~loc:(mkLoc startPos endPos)
              (Ast_helper.Exp.ident
                ~loc:arrayLoc
                (Location.mkloc (Longident.Ldot(Lident "Array", "get")) arrayLoc)
              )
              [Nolabel, expr; Nolabel, accessExpr])
        end
      | Lparen ->
        Parser.next p;
        loop p (parseCallExpr p expr)
      | _ -> expr
    in
    let expr = loop p e1 in
    {expr with pexp_loc = mkLoc startPos p.startPos}


  and parseUnaryExpr p =
    match p.Parser.token with
    | (Minus | MinusDot | Plus | PlusDot | Bang) as token ->
      Parser.next p;
      makeUnaryExpr token (parseUnaryExpr p)
    | Band (* & *) ->
      let startPos = p.startPos in
      Parser.next p;
      let refAccess =
        let loc = mkLoc startPos p.prevEndPos in
        let op = Location.mkloc (Longident.Lident "!") loc in
        Ast_helper.Exp.ident ~loc op
      in
      let arg = parseUnaryExpr p in
      let loc = mkLoc startPos arg.pexp_loc.loc_end in
      Ast_helper.Exp.apply
        ~loc
        refAccess
        [Nolabel, arg]
    | _ ->
      parsePrimaryExpr p

  and parseAttributedExpr p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let unaryExpr = parseUnaryExpr p in
    let endPos = p.Parser.prevEndPos in
    {unaryExpr with pexp_attributes = attrs; pexp_loc = mkLoc startPos endPos}

  and parseBinaryExpr ?(allowAttrs=true) p prec =
    let a = if allowAttrs then parseAttributedExpr p else parseUnaryExpr p in
    let rec loop a =
      let token = p.Parser.token in
      let tokenPrec = Token.precedence token in
      if tokenPrec < prec then a
      else begin
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
        | _ -> raise (Parser.Expected (p.startPos, "close with } plz"))
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
      | _ -> raise (Parser.Expected (p.startPos, "invalid template expression stuff"))
    in
    Lex.setTemplateMode p.lexbuf;
    Parser.expect p Backtick;
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
      | _ -> raise (Parser.Expected (p.startPos, "close with } plz"))
      in
      let str = Ast_helper.Exp.constant (Pconst_string(txt, None)) in
      let next =
        if String.length txt > 0 then
          Ast_helper.Exp.apply hiddenOperator [Nolabel, str; Nolabel, expr]
        else
          expr
      in
      loop next p
   | _ -> raise (Parser.Expected (p.startPos, "invalid template expression stuff"))

  and parseLetBindingBody ~attrs p =
    let pat =
      let pat = parsePattern p in
      match p.Parser.token with
      | Colon ->
        Parser.next p;
        let polyType = parsePolyTypeExpr p in
        Ast_helper.Pat.constraint_ pat polyType
      | _ -> pat
    in
    Parser.expect p Token.Equal;
    let exp = parseExpr p in
    Ast_helper.Vb.mk ~attrs pat exp

  (* definition	::=	let [rec] let-binding  { and let-binding }   *)
  and parseLetBindings ~attrs p =
    Parser.expect p Let;
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
    | _ -> raise (Parser.Expected (p.startPos, "Expected lident or uident jsx name"))
    in
    Ast_helper.Exp.ident ~loc (Location.mkloc ident loc)

  (*
   *  jsx ::=
   *    | <> {primary-expr} </>
   *    | <element-name {jsx-attribute} />
   *    | <element-name {jsx-attribute}> {primary-expr} </element-name>
   *)
  and parseJsx p =
    let attr = (Location.mknoloc "JSX", Parsetree.PStr []) in
    let jsxStartPos = p.Parser.startPos in
    Parser.expect p LessThan;
    match p.Parser.token with
    | Lident _ | Uident _ ->
      let name = parseJsxName p in
      let jsxAttrs = parseJsxAttributes p in
      let children = match p.Parser.token with
      | Forwardslash -> (* <foo a=b /> *)
        let childrenStartPos = p.Parser.startPos in
        Parser.next p;
        let childrenEndPos = p.Parser.startPos in
        Parser.expect p GreaterThan;
        let loc = mkLoc childrenStartPos childrenEndPos in
        makeListExpression loc [] None (* no children *)
      | GreaterThan -> (* <foo a=b> bar </foo> *)
        let childrenStartPos = p.Parser.startPos in
        Parser.next p;
        let children = parseJsxChildren p in
        let childrenEndPos = p.Parser.startPos in
        Parser.expect p LessThan;
        Parser.expect p Forwardslash;
        begin match p.Parser.token with
        (* TODO: better error messages *)
        | Lident closingIdent | Uident closingIdent ->
          Parser.next p;
          Parser.expect p GreaterThan;
          let loc = mkLoc childrenStartPos childrenEndPos in
          makeListExpression loc children None
        | _ -> raise (Parser.Expected (p.startPos, "Closing jsx element should match"))
        end
      | _ -> raise (Parser.Expected (p.startPos, "jsx opening invalid"))
      in
      let jsxEndPos = p.prevEndPos in
      let loc = mkLoc jsxStartPos jsxEndPos in
      Ast_helper.Exp.apply
        ~loc
        ~attrs:[attr]
        name
        (jsxAttrs @ [
          (Asttypes.Labelled "childen", children);
          (Asttypes.Nolabel, Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None)
        ])
    | GreaterThan -> (* fragment: <> foo </> *)
      let childrenStartPos = p.Parser.startPos in
      Parser.next p;
      let children = parseJsxChildren p in
      let childrenEndPos = p.Parser.startPos in
      Parser.expect p LessThan;
      Parser.expect p Forwardslash;
      Parser.expect p GreaterThan;
      let loc = mkLoc childrenStartPos childrenEndPos in
      let fragment = makeListExpression loc children None in
      {fragment with pexp_attributes = [attr]}
    | _ -> raise (Parser.Expected (p.startPos, "Expected jsx name"))

  (*
   * jsx-attribute ::=
   *   | [?] LIDENT
   *   | LIDENT = [?] jsx_expr
   *)
  and parseJsxAttribute p =
    let optional = Parser.optional p Question in
    let name = match p.Parser.token with
    | Lident ident -> Parser.next p; ident
    | _ ->
      raise (Parser.Expected (p.startPos, "jsx attr new should be lowercase"))
    in
    (* optional punning: <foo ?a /> *)
    if optional then
      (Asttypes.Labelled name, Ast_helper.Exp.ident (Location.mknoloc
        (Longident.Lident name)))
    else begin
      (* no punning *)
      Parser.expect p Equal;
      let optional = Parser.optional p Question in
      let attrExpr = parsePrimaryExpr p in
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
      | Token.Eof | LessThan -> List.rev children
      | _ ->
        let child = parsePrimaryExpr p in
        loop p (child::children)
    in
    loop p []

  and parseBracedOrRecordExpr p =
    (* opening brace consumed *)
    match p.Parser.token with
    | Dot ->
      (* beginning of record spread, parse record, todo maybe lex DOTDOTDOT *)
      Parser.next p;
      Parser.expect p Dot;
      Parser.expect p Dot;
      let spreadExpr = parseExpr p in
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
        parseRecordExpr [(pathIdent, fieldExpr)] p
      | Semicolon ->
        Parser.next p;
        parseSeqExpr ~first:(Ast_helper.Exp.ident pathIdent) p
      | Rbrace ->
        Ast_helper.Exp.ident pathIdent
      | _ ->
        let firstExpr = parsePrimaryExpr ~operand:(Ast_helper.Exp.ident
        pathIdent) p in
        begin match p.Parser.token with
        | Semicolon ->
          Parser.next p;
          parseSeqExpr ~first:firstExpr p
        | Rbrace ->
          firstExpr
        | _ -> raise (Parser.Expected (p.startPos, "Expeced } or ;"))
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
    let rec loop p rows =
      match p.Parser.token with
      | Comma ->
        Parser.next p;
        loop p rows
      | Rbrace ->
        rows
      | _ ->
        let row = parseRecordRow p in
        loop p (row::rows)
    in
    Ast_helper.Exp.record (loop p rows) spread

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
      | _ -> raise (Parser.Expected (p.startPos, "Expected module name"))
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
      | _ ->
        Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None
      in
      Ast_helper.Exp.let_ ~loc recFlag letBindings next
    | _ ->
      parseExpr p

  (* TODO; improve? perf? *)
  and parseSeqExpr ?first p =
      let item = match first with
      | Some e -> e
      | None -> parseSeqExprItem p
      in
      match p.Parser.token with
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
          Ast_helper.Exp.sequence item next
        | _ -> item
        end
      | _ ->
        item

  and parseIfExpression p =
    (* If token already consumed *)
    let conditionExpr = parseExpr p in
    Parser.expect p Lbrace;
    let thenExpr = parseSeqExpr p in
    Parser.expect p Rbrace;
    let elseExpr = match p.Parser.token with
    | Else ->
      Parser.next p;
      Parser.expect p Lbrace;
      let elseExpr = parseSeqExpr p in
      Parser.expect p Rbrace;
      Some elseExpr
    | _ ->
      None
    in
    Ast_helper.Exp.ifthenelse conditionExpr thenExpr elseExpr

  and parseForExpression p =
    (* For token consumed *)
    Parser.expect p Lparen;
    let pattern = parsePattern p in
    Parser.expect p In;
    let e1 = parseExpr p in
    let direction = match p.Parser.token with
    | To -> Asttypes.Upto
    | Downto -> Asttypes.Downto
    | _ ->
      raise (Parser.Expected (p.startPos, "Expected \"to\" or \"downto\""))
    in
    Parser.next p;
    let e2 = parseExpr p in
    Parser.expect p Rparen;
    Parser.expect p Lbrace;
    let bodyExpr = parseSeqExpr p in
    Parser.expect p Rbrace;
    Ast_helper.Exp.for_ pattern e1 e2 direction bodyExpr

  and parseWhileExpression p =
    (* While token consumed *)
    let expr1 = parseExpr p in
    Parser.expect p Lbrace;
    let expr2 = parseSeqExpr p in
    Parser.expect p Rbrace;
    Ast_helper.Exp.while_ expr1 expr2

  and parsePatternMatching p =
    (* '{' consumed *)
    let rec loop p cases =
      match p.Parser.token with
      | Rbrace ->
        Parser.next p;
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
        Parser.expect p EqualGreater;
        let rhs = parseSeqExpr p in
        let case = Ast_helper.Exp.case lhs ?guard rhs in
        loop p (case::cases)
      | _ -> raise (Parser.Expected (p.startPos, "case problem"))
    in
    loop p []

  and parseSwitchExpression p =
    (* Switch token consumed *)
    let switchExpr = parseExpr p in
    Parser.expect p Lbrace;
    let cases = parsePatternMatching p in
    Ast_helper.Exp.match_ switchExpr cases

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
          (label, parseExpr p)
        | _ ->
          (Labelled ident, identExpr)
        end
      | _ -> raise (Parser.Expected (p.startPos, "label name should be lowercase ident"))
      end
    | _ -> (Nolabel, parseExpr p)

  and parseCallExpr p funExpr =
    (* left `(` already consumed *)
    let rec loop p args =
      begin match p.Parser.token with
      | Rparen | Eof ->
        Parser.next p;
        List.rev args
      | _ ->
        let arg = parseArgument p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (arg::args)
        | Rparen | Eof ->
          Parser.next p;
          List.rev (arg::args)
        | _ -> raise (Parser.Expected (p.startPos, "parsing function args, need ) or ,"))
        end
      end
    in
    let args = match loop p [] with
    | [] ->
      (* No args -> unit sugar: `foo()` *)
      [
        Asttypes.Nolabel,
        Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None
      ]
    | args -> args
    in
    let loc = {funExpr.pexp_loc with
      loc_end = p.endPos
    } in
    Ast_helper.Exp.apply ~loc funExpr args


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
          Parser.next p;
          let args = parseConstructorArgs p in
          let lident = buildLongident (ident::acc) in
          let tail = match args with
          | [] -> None
          | [arg] -> Some arg
          | args -> Some (Ast_helper.Exp.tuple args)
          in
          let loc = mkLoc startPos p.startPos in
          Ast_helper.Exp.construct (Location.mkloc lident loc) tail
        | _ ->
          let loc = mkLoc startPos p.startPos in
          let lident = buildLongident (ident::acc) in
          Ast_helper.Exp.construct (Location.mkloc lident loc) None
        end
      | Lident ident ->
        Parser.next p;
        let loc = mkLoc startPos p.startPos in
        let lident = buildLongident (ident::acc) in
        Ast_helper.Exp.ident (Location.mkloc lident loc)
      | _ -> raise (Parser.Expected (p.startPos, "Trying to parse a value or a constructor"))
    in
    aux p []

  and parseConstructorArgs p =
    let rec aux p acc =
      match p.Parser.token with
      | Rparen ->
        Parser.next p;
        acc
      | _ ->
        let exp = parseExpr p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          aux p (exp::acc)
        | Rparen ->
          Parser.next p;
          (exp::acc)
        | _ -> raise (Parser.Expected (p.startPos, "expected constructor arg thing: ) or ,"))
        end
    in
    List.rev (aux p [])

  and parseTuple p =
    let rec aux p acc =
      match p.Parser.token with
      | Forwardslash ->
        Parser.next p;
        acc
      | _ ->
        let exp = parseExpr p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          aux p (exp::acc)
        | Forwardslash ->
          Parser.next p;
          (exp::acc)
        | _ -> raise (Parser.Expected (p.startPos, "expected tuple thing: / or ,"))
        end
    in
    let exprs = aux p [] in
    Ast_helper.Exp.tuple (List.rev exprs)

  and parseListExpr p =
    let startPos = p.Parser.startPos in
    Parser.expect p List;
    Parser.expect p Lparen;
    let rec loop p exprs = match p.Parser.token with
    | Rparen ->
      Parser.next p;
      exprs
    | Comma -> Parser.next p; loop p exprs
    | DotDotDot ->
      Parser.next p;
      let expr = parseExpr p in
      loop p ((true, expr)::exprs)
    | _ ->
      let expr = parseExpr p in
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
    Parser.expect p Lbracket;
    let rec aux p acc =
      match p.Parser.token with
      | Rbracket ->
        let endPos = p.endPos in
        Parser.next p;
        (endPos, acc)
      | _ ->
        let exp = parseExpr p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          aux p (exp::acc)
        | Rbracket ->
          let endPos = p.endPos in
          Parser.next p;
          (endPos, (exp::acc))
        | _ -> raise (Parser.Expected (p.startPos, "expected array thing: ] or ,"))
        end
    in
    let (endPos, exprs) = aux p [] in
    Ast_helper.Exp.array ~loc:(mkLoc startPos endPos) (List.rev exprs)

  (* TODO: check attributes in the case of poly type vars,
   * might be context dependend: parseFieldDeclaration (see ocaml) *)
  and parsePolyTypeExpr p =
    match p.Parser.token with
    | SingleQuote ->
      let vars = parseTypeVarList p in
      begin match vars with
      | _v1::_v2::_ ->
        Parser.expect p Dot;
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
        | _ ->
          raise (Parser.Expected (p.startPos, "Expected lowercase ident"))
        end
      | _ ->
        List.rev vars
    in
    loop p []

  and parseTypExpr p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let typ = match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      begin match p.Parser.token with
      | Lident ident ->
        let endPos = p.endPos in
        Parser.next p;
        Ast_helper.Typ.var ~loc:(mkLoc startPos endPos) ~attrs ident
      | _ -> raise (Parser.Expected (p.startPos, "Expected lowercase ident"))
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
      Parser.expect p Rparen;
      {t with ptyp_loc = mkLoc startPos endPos}
    | Uident _ | Lident _ ->
      let constr = parseValuePath p in
      (* TODO extract this whole block into reusable logic, cf. type equation *)
      begin match p.Parser.token with
      | Lparen ->
        Parser.next p;
        let (endPos, args) = parseConstructorTypeArgs p in
        Ast_helper.Typ.constr ~loc:(mkLoc startPos endPos) ~attrs constr args
      | _ ->
        Ast_helper.Typ.constr ~loc:constr.loc ~attrs constr []
      end
    | _ ->
      raise (Parser.Expected (p.startPos, "hmm we're going to parse a typexpr"))
    in
    (* TODO: check associativity in combination with attributes *)
    match p.Parser.token with
    | As ->
      Parser.next p;
      Parser.expect p SingleQuote;
      begin match p.token with
      | Lident ident ->
        let endPos = p.endPos in
        Parser.next p;
        Ast_helper.Typ.alias ~loc:(mkLoc startPos endPos) ~attrs typ ident
      | _ -> raise (Parser.Expected (p.startPos, "ident plz"))
      end
    | _ -> typ

  and parseTupleType p =
    let startPos = p.Parser.startPos in
    Parser.expect p Forwardslash;
    let rec loop p acc =
      match p.Parser.token with
      | Forwardslash ->
        let endPos = p.endPos in
        Parser.next p;
        Ast_helper.Typ.tuple ~loc:(mkLoc startPos endPos) (List.rev acc)
      | _ ->
        let typ = parseTypExpr p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (typ::acc)
        | Forwardslash ->
        let endPos = p.endPos in
          Parser.next p;
          let types = List.rev (typ::acc) in
          Ast_helper.Typ.tuple ~loc:(mkLoc startPos endPos) types
        | _ -> raise (Parser.Expected (p.startPos, "expected tuple type thing: / or ,"))
        end
    in
    loop p []

  and parseConstructorTypeArgs p =
    let rec loop p types =
      match p.Parser.token with
      | Rparen ->
        let endPos = p.endPos in
        Parser.next p;
        (endPos, List.rev types)
      | _ ->
        let typ = parseTypExpr p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (typ::types)
        | Rparen ->
          let endPos = p.endPos in
          Parser.next p;
          (endPos, List.rev (typ::types))
        | _ -> raise (Parser.Expected (p.startPos, "expected pattern constructor arg thing: ) or ,"))
        end
    in
    loop p []

  and parseFieldDeclaration p =
    let startPos = p.Parser.startPos in
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
    | _ -> raise (Parser.Expected (p.startPos, "need lowercase type name"))
    in
    Parser.expect p Colon;
    let typ = parsePolyTypeExpr p in
    let loc = mkLoc startPos typ.ptyp_loc.loc_end in
    Ast_helper.Type.field ~loc ~mut name typ

  and parseRecordDeclaration p =
    Parser.expect p Lbrace;
    let rec loop p fields =
      match p.Parser.token with
      | Rbrace -> Parser.next p; List.rev fields
      | _ ->
        let field = parseFieldDeclaration p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (field::fields)
        | Rbrace ->
          Parser.next p;
          List.rev (field::fields)
        | _ -> raise (Parser.Expected (p.startPos, "Expected comma or rbrace"))
        end
    in
    loop p []

  and parseConstrDeclArgs p =
    let args = match p.Parser.token with
    | Lparen ->
     Parser.next p;
     begin match p.Parser.token with
     | Lbrace ->
       let recordDecl = parseRecordDeclaration p in
       Parser.expect p Rparen;
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
     | _ -> raise (Parser.Expected (p.startPos, "expected constr name"))

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
    | Dot ->
      Parser.next p;
      Parser.expect p Dot;
      Ptype_open
    | _ -> raise (Parser.Expected (p.startPos, "expected constr-decl or record-decl"))
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
      | _ -> raise (Parser.Expected (p.startPos, "type param needs lident"))
      end
    | Underscore ->
      Parser.next p;
      (Ast_helper.Typ.any (), variance)
    | _ -> raise (Parser.Expected (p.startPos, "invalid type param"))
    in
    Parser.expect p GreaterThan;
    param

  and parseTypeParams p =
    let params = match p.Parser.token with
    | LessThan -> Parser.next p; [parseTypeParam p]
    | _ -> []
    in
    params

  and parseTypeConstraint p =
    Parser.expect p SingleQuote;
    begin match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      Parser.expect p Equal;
      let typ = parseTypExpr p in
      (Ast_helper.Typ.var ident, typ, Location.none)
    | _ -> raise (Parser.Expected (p.startPos, "Expected lowercase ident"))
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
           Parser.expect p Dot;
           loop p (Longident.Ldot (path, uident))
         | _ -> raise (Parser.Expected (p.startPos, "value path"))
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
    | _ -> raise (Parser.Expected (p.startPos, "Expected Uident"))

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
    (* Parser.expect p Equal; *)
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

  and parseTypeDef p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let typeConstrName = match p.Parser.token with
    | Lident ident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      (Location.mkloc ident loc)
    | _ -> raise (Parser.Expected (p.startPos, "Type constructor name should be lowercase"))
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

  and parseTypeDefinition p =
    Parser.expect p Token.Typ;
    let recFlag =
      if Parser.optional p Token.Rec
        then Asttypes.Recursive
        else Asttypes.Nonrecursive
    in
    let typeDef = parseTypeDef p in
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
        | [] -> raise (Parser.Expected (p.startPos, "should have at least one string"))
        | prims -> List.rev prims
        end
    in
    loop p []

  and parseExternalDef ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect p Token.External;
    let name = match p.Parser.token with
    | Lident ident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc ident loc
    | _ -> raise (Parser.Expected (p.startPos, "external name should be lident"))
    in
    Parser.expect p Colon;
    let typExpr = parseTypExpr p in
    Parser.expect p Equal;
    let prim = parsePrimitive p in
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    Ast_helper.Val.mk ~loc ~attrs ~prim name typExpr

  and parseConstrDeclOrName p =
    let name = match p.Parser.token with
    | Uident name ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc name loc
    | _ -> raise (Parser.Expected (p.startPos, "Expected constructor name"))
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
    Parser.expect p Token.Exception;
    let (name, kind) = parseConstrDeclOrName p in
    let endPos = p.Parser.prevEndPos in
    let loc = mkLoc startPos endPos in
    Ast_helper.Te.constructor ~loc ~attrs name kind

  and parseStructure p =
    let rec parse p acc = match p.Parser.token with
      | Eof | Rbrace -> acc
      | _ ->
        let item = parseStructureItem p in
        let () = match p.Parser.token with
        | Semicolon -> Parser.next p
        | Open | Let | Typ | External | Include | Module | Eof
        | String _ | Int _ | Float _ | Lbrace | Lparen | True | False
        | Backtick | Uident _ | Lident _ | Lbracket | Assert | Lazy
        | If | For | While | Switch | LessThan -> ()
        | _ -> raise (Parser.Expected (p.startPos, "Expected semicolon"))
        in
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
      let (recFlag, typeDecls) = parseTypeDefinition p in
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
    Parser.expect p Token.Include;
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
      Parser.expect p Rbrace;
      let endPos = p.prevEndPos in
      {structure with pmod_loc = mkLoc startPos endPos}
    | Lparen ->
      parseParenthesizedOrFunctorModuleExpr p
    | _ -> raise (Parser.Expected (p.startPos, "Unsupport module expression"))

  and parseParenthesizedOrFunctorModuleExpr p =
    let startPos = p.Parser.startPos in
    Parser.expect p Lparen;
    match p.Parser.token with
    | Underscore -> (* functor arg name, parse functor *)
      parseFunctorModuleExpr p []
    | Rparen ->
      Parser.next p;
      let rparenEnd = p.prevEndPos in
      Parser.expect p EqualGreater;
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
            | _ -> raise (Parser.Expected (
                p.startPos,
                "A functor arg param should be a Pmod_ident"
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
            | _ -> raise (Parser.Expected (
                p.startPos,
                "A functor arg param should be a Pmod_ident"
              ))
            in
            let loc = mkLoc startPos rhs.pmod_loc.loc_end in
            Ast_helper.Mod.functor_ ~loc argName (Some moduleType) rhs
          | _ ->
            let loc = mkLoc startPos rparenEnd in
            Ast_helper.Mod.constraint_ ~loc moduleExpression moduleType
          end
        | _ -> raise (Parser.Expected (p.startPos, "Expected ) or ,"))
        end
      | Rparen ->
        Parser.next p;
        let rparenEnd = p.prevEndPos in
        begin match p.Parser.token with
        | EqualGreater ->
          raise (Parser.Expected (
            p.startPos,
            "A functor arg needs a module type"
          ))
        | _ ->
          {moduleExpression with pmod_loc = mkLoc startPos rparenEnd}
        end
      | _ -> raise (Parser.Expected (p.startPos, "Expected , or rparen"))
      end

  and parseFunctorArgName p =
    let startPos = p.Parser.startPos in
    let ident = match p.Parser.token with
    | Uident ident -> ident
    | Underscore -> "_"
    | _ -> raise (Parser.Expected (p.startPos, "functor arg name should be Uident or _"))
    in
    Parser.next p;
    Location.mkloc ident (mkLoc startPos p.prevEndPos)

  and parseFunctorArgs p args =
    let rec loop p args =
      match p.Parser.token with
      | Rparen -> Parser.next p; args
      | _ ->
        let functorArgName = parseFunctorArgName p in
        Parser.expect p Colon;
        let moduleType = parseModuleType p in
        let arg = (functorArgName, Some moduleType) in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (arg::args)
        | Rparen ->
          Parser.next p;
          (arg::args)
        | _ -> raise (Parser.Expected (p.startPos, "Expected ) or ,"))
        end
    in
    loop p args

  and parseFunctorModuleExpr p args =
    let args = parseFunctorArgs p args in
    Parser.expect p EqualGreater;
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
        | _ -> raise (Parser.Expected (p.startPos, "TODO"))
        in
        let functorBody = parseModuleExpr p in
        let loc = mkLoc modExpr.pmod_loc.loc_start p.Parser.prevEndPos in
        Ast_helper.Mod.functor_ ~loc arg None functorBody
      | _ ->
        modExpr
    in loop p modExpr

  and parseModuleApplication p modExpr =
    Parser.expect p Lparen;
    let arg = parseModuleExpr p in
    Parser.expect p Rparen;
    let endPos = p.prevEndPos in
    let loc = mkLoc modExpr.pmod_loc.loc_start endPos in
    Ast_helper.Mod.apply ~loc modExpr arg

  and parseModuleOrModuleTypeImpl p =
    let startPos = p.Parser.startPos in
    Parser.expect p Module;
    match p.Parser.token with
    | Typ -> parseModuleTypeImpl startPos p
    | _ -> parseMaybeRecModuleBinding p

  and parseModuleTypeImpl startPos p =
    Parser.expect p Typ;
    let nameStart = p.Parser.startPos in
    let name = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      let loc = mkLoc nameStart p.prevEndPos in
      Location.mkloc ident loc
    | _ -> raise (Parser.Expected (p.startPos, "Expected module name"))
    in
    Parser.expect p Equal;
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
    | _ -> raise (Parser.Expected (p.startPos, "Expected module name"))
    in
    let body = parseModuleBindingBody p in
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    Ast_helper.Mb.mk ~loc name body

  and parseModuleBindingBody p =
    match p.Parser.token with
    | Equal ->
      Parser.next p;
      parseModuleExpr p
    | _ -> raise (Parser.Expected (p.startPos, "Unexpected module body"))

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
      Parser.expect p Rparen;
      mty
    | Lbrace ->
      Parser.next p;
      let spec = parseSpecification p in
      Parser.expect p Rbrace;
      spec
    | Module ->
      parseModuleTypeOf p
    | _ ->
      raise (Parser.Expected (p.startPos, "need a module type"))
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
    Parser.expect p With;
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
        Parser.expect p Equal;
        let lident = parseModuleLongIdent p in
        Parsetree.Pwith_modsubst (modulePath, lident)
      | Equal ->
        Parser.next p;
        let lident = parseModuleLongIdent p in
        Parsetree.Pwith_module (modulePath, lident)
      | _ -> raise (Parser.Expected (p.startPos, "Expected = or :="))
      end
    | Typ ->
      Parser.next p;
      let typeConstr = parseValuePath p in
      let params = parseTypeParams p in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        Parser.expect p Equal;
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
      | _ -> raise (Parser.Expected (p.startPos, "Expected = or := to complete typeconstr"))
      end
    | _ -> raise (Parser.Expected (p.startPos, "Unsupported with mod constraint"))

  and parseModuleTypeOf p =
    Parser.expect p Module;
    Parser.expect p Typ;
    Parser.expect p Of;
    let moduleExpr = parseModuleExpr p in
    Ast_helper.Mty.typeof_ moduleExpr

  and parseSpecification p =
    (* { consumed *)
    let rec loop p spec =
      let item = parseSignatureItem p in
      Parser.expect p Semicolon;
      let () = match p.Parser.token with
      | Semicolon -> Parser.next p
      | Let | Typ | External | Exception | Open | Include | Module | Eof -> ()
      | _ -> raise (Parser.Expected (p.startPos, "expected semi"))
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
      let (recFlag, typeDecls) = parseTypeDefinition p in
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
      | _ -> raise (Parser.Expected (p.startPos, "need type or uident"))
      end
    | _ -> raise (Parser.Expected (p.startPos, "signature item"))

  and parseModuleDeclarationOrAlias ~attrs p =
    let moduleName = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.Expected (p.startPos, "Module name should start with uident"))
    in
    let body = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parseModuleType p
    | Equal ->
      Parser.next p;
      let lident = parseModuleLongIdent p in
      Ast_helper.Mty.alias lident
    | _ -> raise (Parser.Expected (p.startPos, "Expected : or ="))
    in
    Ast_helper.Sig.module_ (Ast_helper.Md.mk ~attrs moduleName body)

  and parseModuleTypeDeclaration ~attrs p =
    Parser.expect p Typ;
    let moduleName = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.Expected (p.startPos, "Module type name should be uident"))
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
    Parser.expect p Let;
    let name = match p.Parser.token with
    | Lident ident -> Parser.next p; Location.mknoloc ident
    | _ -> raise (Parser.Expected (p.startPos, "name of value should be lowercase"))
    in
    Parser.expect p Colon;
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
        | Dot -> Parser.next p; loop p id
        | _ -> id
        end
      | _ -> raise (Parser.Expected (p.startPos, "Expected uident, lident"))
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
      Parser.expect p Rparen;
      [item]
    | _ -> []
    in
    Parsetree.PStr structure

  (* type attribute = string loc * payload *)
  and parseAttribute p =
    Parser.expect p At;
    let cnumEndAttrId = p.Parser.startPos.pos_cnum in
    let attrId = parseAttributeId p in
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
      | _ -> raise (Parser.Expected (vb.pvb_loc.loc_start, "TODO"))
      in
      let vd = Ast_helper.Val.mk var typ in
      Ast_helper.Sig.value ~loc vd
    | _ -> raise (Parser.Expected (loc.loc_start, "Hey! we don't support exporting here (yet?)"))

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
        | _ -> raise (Parser.Expected (p.startPos, "Expected semicolon"))
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
    let src =  read_file filename in
    let p = Parser.make src filename in
    try
      let ast = parseFile p in
      Pprintast.structure Format.std_formatter ast;
      Format.pp_print_flush Format.std_formatter ();
      print_newline();
      (* Printast.implementation Format.std_formatter ast; *)
      (* Format.pp_print_flush Format.std_formatter (); *)
      (* print_newline(); *)
      (* print_newline() *)
    with
    | Parser.Expected (pos, trace) ->
      print_endline ("pos_lnum: " ^ (string_of_int pos.pos_lnum));
      print_endline ("pos_cnum: " ^ (string_of_int pos.pos_cnum));
      print_endline "something threw an exception";
      print_endline "current token:";
      print_endline (Token.toString p.Parser.token);
      print_endline "trace";
      print_endline trace
end


(*  let arr1 = [1, 2, 3]
  * let arr2 = [4, 5, 6,] *)

      (* let t1 = /a, b/ *)
      (* let t2 = /a, b,/ *)

      (* let v = Foo.Bar.Baz.x *)

      (* let c = A(a, b) *)

      (* let d = Foo.Lala.Hihi(a, b) *)

      (* let x = A(a) *)
      (* let binaryApply = foo.bar(~a=?1, ~b, c,) *)

      (* let ifThenElse = if foo { *)
        (* lala *)
      (* } else { *)
        (* doStuff(x, y, z,) *)
      (* } *)

      (* let x = for (x in xStart downto xEnd) { *)
        (* print_int(x) *)
      (* } *)

  (* let y = while (break) { *)
    (* omg(1) *)
  (* } *)
      (* type foo<-_> = equation = Foo :int | Bar :string constraint 'a = x *)

      (*
    exception Foo
    exception Foo{n: int}
    exception Foo({n: int})
    exception Foo(string, bar, baz)
    exception Lala = Foo.Bar.Baz
    *)
