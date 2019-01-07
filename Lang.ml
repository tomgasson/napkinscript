module CharacterCodes = struct
  let eol = -1

  let space = 0x0020
  let newline = 0x0A
  let tab = 0x09

  let bang = 0x21
  let dot = 0x2E
  let colon = 0x3A
  let comma = 0x2C
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
end

module Token = struct
  type t =
    | Open
    | Char of char
    | Int of string
    | String of string
    | Lident of string
    | Uident of string
    | As
    | Dot
    | Bang
    | Semicolon
    | Let
    | And
    | Rec | Nonrec
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

  let precedence = function
    | HashEqual | ColonEqual -> 1
    | Lor -> 2
    | Land -> 3
    | EqualEqual | EqualEqualEqual | LessThan | GreaterThan
    | BangEqual | BangEqualEqual | LessEqual | GreaterEqual -> 4
    | Plus | PlusDot | Minus | MinusDot | Lxor | PlusPlus -> 5
    | Asterisk | AsteriskDot | Forwardslash | ForwardslashDot  | Lsl | Lsr | Mod -> 6
    | Exponentiation -> 7
    | Hash | HashHash | MinusGreater -> 8
    | Dot -> 9
    | _ -> 0

  let toString = function
    | Open -> "open"
    | Char c -> "'" ^ (Char.escaped c) ^ "'"
    | String s -> s
    | Lident str -> "Lident " ^ str
    | Uident str -> "Lident " ^ str
    | Dot -> "."
    | Int i -> "int " ^ i
    | Bang -> "!"
    | Semicolon -> ";"
    | Let -> "let"
    | And -> "and"
    | Rec -> "rec" | Nonrec -> "nonrec"
    | Underscore -> "_"
    | SingleQuote -> "'"
    | Equal -> "=" | EqualEqual -> "==" | EqualEqualEqual -> "==="
    | Eof -> "eof"
    | Bar -> "bar"
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

  let keywordTable =
    let keywords = [|
      "open", Open;
      "let", Let;
      "rec", Rec;
      "nonrec", Nonrec;
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
  type lexbuf = {
    filename: string;
    src: bytes;
    mutable ch: int; (* current character *)
    mutable offset: int; (* character offset *)
    mutable rdOffset: int; (* reading offset (position after current character) *)
    mutable lineOffset: int; (* current line offset *)
    mutable lnum: int; (* current line number *)
  }

  let position lexbuf = Lexing.{
    pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.lnum;
    pos_bol = lexbuf.lineOffset;
    pos_cnum = lexbuf.offset;
  }

  let printPos p =
    print_endline ("cnum: " ^ (string_of_int p.Lexing.pos_cnum))

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
      lnum = 0;
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

  let lexNumber lexbuf =
    let startOff = lexbuf.offset in
    while CharacterCodes.isDigit lexbuf.ch do
      next lexbuf
    done;
    let str = Bytes.sub_string lexbuf.src startOff (lexbuf.offset - startOff) in
    Token.Int str

  (* we don't support CHAR for now *)
  (* let lexChar lexbuf = *)
    (* let startOff = lexbuf.offset in *)
    (* if peek lexbuf == CharacterCodes.singleQuote then ( *)
      (* next lexbuf; next lexbuf; *)
      (* Token.Char (Bytes.unsafe_get lexbuf.src startOff) *)
    (* ) else ( *)
      (* Token.SingleQuote *)
    (* ) *)

  let lexString lexbuf =
    let startOff = lexbuf.offset in
    while not (CharacterCodes.doubleQuote == lexbuf.ch) do
      next lexbuf
    done;
    next lexbuf;
    Token.String (
      Bytes.sub_string lexbuf.src startOff (lexbuf.offset - 1 - startOff)
    )


  exception Unknown_token of int

  let lex lexbuf =
    skipWhitespace lexbuf;
    let ch = lexbuf.ch in
    if CharacterCodes.isLetter ch then
      lexIdentifier lexbuf
    else if CharacterCodes.isDigit ch then
      lexNumber lexbuf
    else begin
      next lexbuf;
      if ch == CharacterCodes.dot then
        Token.Dot
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
        Token.Forwardslash
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
      else if ch == -1 then
        Token.Eof
      else
        raise (Unknown_token lexbuf.ch)
    end
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
      mutable pos: Lexing.position;
    }

    let make src filename =
      let lexbuf = Lex.make (Bytes.of_string src) filename in
      {
        lexbuf;
        token = Lex.lex lexbuf;
        pos = Lex.position lexbuf;
      }

    let next p =
      p.token <- Lex.lex p.lexbuf;
      p.pos <- Lex.position p.lexbuf

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
        raise (Expected (p.pos, ("expected: " ^ (Token.toString token))))
  end

  let buildLongident words = match List.rev words with
    | [] -> assert false
    | hd::tl -> List.fold_left (fun p s -> Longident.Ldot (p, s)) (Lident hd) tl


  let array_function str name = Longident.Ldot(Lident str, name)

  let makeInfixOperator token startPos endPos =
    let stringifiedToken =
      if token = Token.MinusGreater then "|."
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

  let makeListExpression seq extOpt =
    let rec handleSeq = function
      | [] ->
        begin match extOpt with
        | Some ext -> ext
        | None ->
          let nil = Location.mknoloc (Longident.Lident "[]") in
          Ast_helper.Exp.construct nil None
        end
      | e1 :: el ->
        let exp_el = handleSeq el in
        let arg = Ast_helper.Exp.tuple [e1; exp_el] in
        Ast_helper.Exp.construct
          (Location.mknoloc (Longident.Lident "::"))
          (Some arg)
    in
    handleSeq seq

  (* Ldot (Ldot (Lident "Foo", "Bar"), "baz") *)
  let parseValuePath p =
    let rec aux p path =
      match p.Parser.token with
      | Lident ident -> Longident.Ldot(path, ident)
      | Uident uident ->
        Parser.next p;
        Parser.expect p Dot;
        aux p (Ldot (path, uident))
      | _ -> raise (Parser.Expected (p.pos, "value path"))
    in
    let ident = match p.Parser.token with
    | Lident ident -> Longident.Lident ident
    | Uident ident ->
      Parser.next p;
      Parser.expect p Dot;
      aux p (Lident ident)
    | _ -> raise (Parser.Expected (p.pos, "value path"))
    in
    Parser.next p;
    ident

  (* Parses module identifiers:
       Foo
       Foo.Bar *)
  let parseModuleLongIdent p =
    let rec aux p acc =
      match p.Parser.token with
      | Uident ident ->
        Parser.next p;
        let lident = (Longident.Ldot (acc, ident)) in
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          aux p lident
        | _ -> lident
        end
      | _ -> raise (Parser.Expected (p.pos, "expected Uident"))
    in
    match p.Parser.token with
    | Uident ident ->
      let lident = Longident.Lident ident in
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        aux p lident
      | _ -> lident
      end
    | _ -> raise (Parser.Expected (p.pos, "expected Uident"))

  let parseOpenDescription ~attrs p =
    Parser.expect p Open;
    let override = if Parser.optional p Token.Bang then
      Asttypes.Override
    else
      Asttypes.Fresh
    in
    let startIdentLoc = p.pos in
    let modident = match p.token with
    | Uident _ ->
      parseModuleLongIdent p
    | _ -> raise (Parser.Expected (p.pos, "expected Uident"))
    in
    let identLoc = mkLoc startIdentLoc p.pos in
    Parser.next p;
    Parser.optional p Token.Semicolon |> ignore;
    Ast_helper.Opn.mk ~attrs ~override (
      Location.mkloc modident identLoc
    )

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
    | String s -> Pconst_string(s, None)
    (* | Char c -> Pconst_char c *)
    | _ ->
      raise (Parser.Expected (p.pos, "constant"))
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
    let pat = match p.Parser.token with
    (* TODO inline or refactor into "maintainable"/"reusable code"? *)
    | Int _ | String _ ->
      let c = parseConstant p in

      begin match p.token with
      | Dot ->
        Parser.next p;
        Parser.expect p Dot;
        let c2 = parseConstant p in
        Ast_helper.Pat.interval c c2
      | _ -> Ast_helper.Pat.constant c
      end
    | Lparen ->
      Parser.next p;
      (* let pat = begin match p.Parser.token with *)
      (* | Uident ident -> *)
      (* | _ -> *)
      let pat = parsePattern p in
      (* end *)
      Parser.expect p Token.Rparen;
      pat
    | Lbracket ->
      Parser.next p;
      parseArrayPattern p
    | Lbrace ->
      parseRecordPattern p
    | Forwardslash ->
      Parser.next p;
      parseTuplePattern p
    | Underscore ->
      Parser.next p;
      Ast_helper.Pat.any ()
    | Lident ident ->
      Parser.next p;
      Ast_helper.Pat.var (Location.mkloc ident Location.none)
    | Uident _ ->
      let startPos = p.pos in
      let lident = parseModuleLongIdent p in
      let endPos = p.pos in
      let loc = mkLoc startPos endPos in
      let constr = Location.mkloc lident loc in
      begin match p.Parser.token with
      | Lparen ->
        Parser.next p;
        Ast_helper.Pat.construct constr (parseConstructorPatternArgs p)
      | _ ->
        Ast_helper.Pat.construct constr None
      end
    | Exception ->
      Parser.next p;
      let pat = parsePattern p in
      Ast_helper.Pat.exception_ pat
    | Lazy ->
      Parser.next p;
      let pat = parsePattern p in
      Ast_helper.Pat.lazy_ pat
    | _ ->
      raise (Parser.Expected (p.pos, "pattern"))
    in

    begin match p.token with
    | As ->
      Parser.next p;
      let startLoc = p.pos in
      begin match p.token with
      | Lident ident ->
        Parser.next p;
        let endLoc = p.pos in
        let loc = mkLoc startLoc endLoc in
        Ast_helper.Pat.alias pat (Location.mkloc ident loc)
      | _ -> raise (Parser.Expected (p.pos, "ident plz"))
      end
    | _ -> pat
    end

  (* field  [: typexpr]  [: pattern] *)
  and parseRecordPatternField p =
    let label = parseValuePath p in
    let pattern = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parsePattern p
    | _ ->
      Ast_helper.Pat.var (Longident.last label |> Location.mknoloc)
    in
    (Location.mknoloc label, pattern)


   (* { field  [: typexpr]  [= pattern] { ; field  [: typexpr]  [= pattern] }  [; _ ] [ ; ] }  *)
  and parseRecordPattern p =
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
        raise (Parser.Expected (p.pos, "record pattern field should be lident or uident"))
    in
    let (fields, closedFlag) = loop p [firstField] in
    Parser.expect p Rbrace;
    Ast_helper.Pat.record fields closedFlag

  and parseTuplePattern p =
    (* '/' consumed *)
    let rec loop p patterns =
      match p.Parser.token with
      | Forwardslash ->
        Parser.next p;
        List.rev patterns;
      | _ ->
        let pattern = parsePattern p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (pattern::patterns)
        | Forwardslash ->
          Parser.next p;
          List.rev (pattern::patterns)
        | _ -> raise (Parser.Expected (p.pos, "unexpected tuple pattern thing: need / or ,"))
        end
    in
    let patterns = loop p [] in
    Ast_helper.Pat.tuple patterns

  and parseArrayPattern p =
    (* '[' consumed *)
    let rec loop p patterns =
      match p.Parser.token with
      | Rbracket ->
        Parser.next p;
        List.rev patterns;
      | _ ->
        let pattern = parsePattern p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (pattern::patterns)
        | Rbracket ->
          Parser.next p;
          List.rev (pattern::patterns)
        | _ -> raise (Parser.Expected (p.pos, "unexpected array pattern thing: need / or ,"))
        end
    in
    let patterns = loop p [] in
    Ast_helper.Pat.tuple patterns

  and parseConstructorPatternArgs p =
    let rec loop p patterns =
      match p.Parser.token with
      | Rparen ->
        Parser.next p;
        List.rev patterns
      | _ ->
        let pattern = parsePattern p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (pattern::patterns)
        | Rparen ->
          Parser.next p;
          List.rev (pattern::patterns)
        | _ -> raise (Parser.Expected (p.pos, "expected pattern constructor arg thing: ) or ,"))
        end
    in
    match loop p [] with
    | [pattern] -> Some pattern
    | patterns -> Some (Ast_helper.Pat.tuple patterns)



  let rec parseExpr ?(allowAttrs=true) p =
    parseBinaryExpr ~allowAttrs p 1

  and parseOperand p =
    let expr = match p.Parser.token with
      | Int _ | String _ ->
        let c = parseConstant p in
        Ast_helper.Exp.constant c
      | Lident ident ->
        let startLoc = p.pos in
        Parser.next p;
        let loc = mkLoc startLoc p.pos in
        Ast_helper.Exp.ident (Location.mkloc (Longident.Lident ident) loc)
      | Uident _ ->
        parseValueOrConstructor p
      | Lparen ->
        Parser.next p;
        let expr = parseExpr p in
        Parser.expect p Rparen;
        expr
      | Lbracket ->
        Parser.next p;
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
       * they might be not 100% simple *)
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
        raise (Parser.Expected (p.pos, "unsupported expresion"))
    in
    expr

  (* TODO: is this "operand"-arg a clear API? *)
  and parsePrimaryExpr ?operand p =
    let e1 = match operand with
      | Some e -> e
      | None -> parseOperand p
    in
    let rec loop p expr =
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        let startLoc = p.pos in
        let lident = parseValuePath p in
        let loc = mkLoc startLoc p.pos in
        loop p (Ast_helper.Exp.field expr (Location.mkloc lident loc))
      | Lbracket ->
        Parser.next p;
        let accessExpr = parseExpr p in
        Parser.expect p Rbracket;
        loop p (Ast_helper.Exp.apply
          (Ast_helper.Exp.ident
            (Location.mknoloc (Longident.Ldot(Lident "Array", "get"))))
            [Nolabel, expr; Nolabel, accessExpr])
      | Lparen ->
        Parser.next p;
        loop p (parseCallExpr p expr)
      | _ -> expr
    in
    loop p e1

  and parseUnaryExpr p =
    match p.Parser.token with
    | (Minus | MinusDot | Plus | PlusDot | Bang) as token ->
      Parser.next p;
      makeUnaryExpr token (parseUnaryExpr p)
    | _ ->
      parsePrimaryExpr p

  and parseAttributedExpr p =
    let attrs = parseAttributes p in
    let unaryExpr = parseUnaryExpr p in
    {unaryExpr with pexp_attributes = attrs}

  and parseBinaryExpr ?(allowAttrs=true) p prec =
    let a = if allowAttrs then parseAttributedExpr p else parseUnaryExpr p in
    let rec loop a =
      let token = p.Parser.token in
      let tokenPrec = Token.precedence token in
      if tokenPrec < prec then a
      else begin
        let startPos = p.pos in
        Parser.next p;
        let endPos = p.pos in
        let b = parseBinaryExpr p (tokenPrec + 1) in
        let expr = Ast_helper.Exp.apply
          (makeInfixOperator token startPos endPos)
          [Nolabel, a; Nolabel, b]
        in
        loop expr
      end
    in
    loop a

  and parseLetBindingBody p =
    let pat = parsePattern p in
    Parser.expect p Token.Equal;
    let exp = parseExpr p in
    Ast_helper.Vb.mk pat exp

  (* definition	::=	let [rec] let-binding  { and let-binding }   *)
  and parseLetBindings p =
    Parser.expect p Let;
    let recFlag = if Parser.optional p Token.Rec then
      Asttypes.Recursive
    else
      Asttypes.Nonrecursive
    in
    let first = parseLetBindingBody p in

    let rec loop p bindings =
      match p.Parser.token with
      | And ->
        Parser.next p;
        let letBinding = parseLetBindingBody p in
        loop p (letBinding::bindings)
      | _ ->
        List.rev bindings
    in
    (recFlag, loop p [first])

  (*
   *  jsx ::=
   *    | <> {primary-expr} </>
   *    | <element-name {jsx-attribute} />
   *    | <element-name {jsx-attribute}> {primary-expr} </element-name>
   *)
  and parseJsx p =
    let attr = (Location.mknoloc "JSX", Parsetree.PStr []) in
    Parser.expect p LessThan;
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      let name = Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident ident)) in
      let jsxAttrs = parseJsxAttributes p in
      let children = match p.Parser.token with
      | Forwardslash -> (* <foo a=b /> *)
        Parser.next p;
        Parser.expect p GreaterThan;
        [] (* no children *)
      | GreaterThan -> (* <foo a=b> bar </foo> *)
        Parser.next p;
        let children = parseJsxChildren p in
        Parser.expect p LessThan;
        Parser.expect p Forwardslash;
        begin match p.Parser.token with
        | Lident closingIdent when closingIdent = ident ->
          Parser.next p;
          Parser.expect p GreaterThan;
          children
        | _ -> raise (Parser.Expected (p.pos, "Closing jsx element should match"))
        end
      | _ -> raise (Parser.Expected (p.pos, "jsx opening invalid"))
      in
      Ast_helper.Exp.apply
        ~attrs:[attr]
        name
        (jsxAttrs @ [
          (Asttypes.Labelled "childen", makeListExpression children None);
          (Asttypes.Nolabel, Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None)
        ])
    | GreaterThan -> (* fragment: <> foo </> *)
      Parser.next p;
      let children = parseJsxChildren p in
      Parser.expect p LessThan;
      Parser.expect p Forwardslash;
      Parser.expect p GreaterThan;
      let fragment = makeListExpression children None in
      {fragment with pexp_attributes = [attr]}
    | _ -> raise (Parser.Expected (p.pos, "Expected jsx name"))

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
      raise (Parser.Expected (p.pos, "jsx attr new should be lowercase"))
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
      let pathIdent = Location.mknoloc recordFieldOrPexpIdent in
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
        parseSeqExpr [Ast_helper.Exp.ident pathIdent] p
      | Rbrace ->
        Ast_helper.Exp.ident pathIdent
      | _ ->
        let firstExpr = parsePrimaryExpr ~operand:(Ast_helper.Exp.ident
        pathIdent) p in
        begin match p.Parser.token with
        | Semicolon ->
          Parser.next p;
          parseSeqExpr [firstExpr] p
        | Rbrace ->
          firstExpr
        | _ -> raise (Parser.Expected (p.pos, "Expeced } or ;"))
        end
      end
    | _ ->
      parseSeqExpr [] p

  and parseRecordRow p =
    let field = parseValuePath p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let fieldExpr = parseExpr p in
      (Location.mknoloc field, fieldExpr)
    | _ ->
      (Location.mknoloc field, Ast_helper.Exp.ident (Location.mknoloc field))

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

  and parseSeqExpr rows p =
    let rec loop p rows =
      let expr = match p.Parser.token with
      | Let ->
        let (recFlag, letBindings) = parseLetBindings p in
        ignore (Parser.optional p Semicolon);
        let expr = begin match p.Parser.token with
        | Rbrace ->
          Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None
        | _ ->
          parseSeqExpr [] p
        end in
        Ast_helper.Exp.let_ recFlag letBindings expr
      | _ -> parseExpr p in
      match p.Parser.token with
      | Semicolon -> Parser.next p; loop p (expr::rows)
      | Rbrace -> expr::rows
      | _ -> raise (Parser.Expected (p.pos, "Expected ; or }"))
    in
    let exprs = loop p rows in
    match exprs with
    | first::tl ->
      List.fold_left (fun acc curr -> Ast_helper.Exp.sequence curr acc) first tl
    | [] -> assert false

  and parseIfExpression p =
    (* If token already consumed *)
    let conditionExpr = parseExpr p in
    Parser.expect p Lbrace;
    let thenExpr = parseSeqExpr [] p in
    Parser.expect p Rbrace;
    let elseExpr = match p.Parser.token with
    | Else ->
      Parser.next p;
      Parser.expect p Lbrace;
      let elseExpr = parseSeqExpr [] p in
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
      raise (Parser.Expected (p.pos, "Expected \"to\" or \"downto\""))
    in
    Parser.next p;
    let e2 = parseExpr p in
    Parser.expect p Rparen;
    Parser.expect p Lbrace;
    let bodyExpr = parseSeqExpr [] p in
    Parser.expect p Rbrace;
    Ast_helper.Exp.for_ pattern e1 e2 direction bodyExpr

  and parseWhileExpression p =
    (* While token consumed *)
    let expr1 = parseExpr p in
    Parser.expect p Lbrace;
    let expr2 = parseSeqExpr [] p in
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
          Some (parseExpr p)
        | _ ->
          None
        in
        Parser.expect p EqualGreater;
        let rhs = parseSeqExpr [] p in
        let case = Ast_helper.Exp.case lhs ?guard rhs in
        loop p (case::cases)
      | _ -> raise (Parser.Expected (p.pos, "case problem"))
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
      let startPos = p.pos in
      begin match p.Parser.token with
      | Lident ident ->
        Parser.next p;
        let endPos = p.pos in
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
      | _ -> raise (Parser.Expected (p.pos, "label name should be lowercase ident"))
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
        | _ -> raise (Parser.Expected (p.pos, "parsing function args, need ) or ,"))
        end
      end
    in
    let args = loop p [] in
    Ast_helper.Exp.apply funExpr args


  and parseValueOrConstructor p =
    let startPos = p.Parser.pos in
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
          let loc = mkLoc startPos p.pos in
          Ast_helper.Exp.construct (Location.mkloc lident loc) tail
        | _ ->
          let loc = mkLoc startPos p.pos in
          let lident = buildLongident (ident::acc) in
          Ast_helper.Exp.construct (Location.mkloc lident loc) None
        end
      | Lident ident ->
        Parser.next p;
        let loc = mkLoc startPos p.pos in
        let lident = buildLongident (ident::acc) in
        Ast_helper.Exp.ident (Location.mkloc lident loc)
      | _ -> raise (Parser.Expected (p.pos, "Trying to parse a value or a constructor"))
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
        | _ -> raise (Parser.Expected (p.pos, "expected constructor arg thing: ) or ,"))
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
        | _ -> raise (Parser.Expected (p.pos, "expected tuple thing: / or ,"))
        end
    in
    let exprs = aux p [] in
    Ast_helper.Exp.tuple (List.rev exprs)

  and parseArrayExp p =
    let rec aux p acc =
      match p.Parser.token with
      | Rbracket ->
        Parser.next p;
        acc
      | _ ->
        let exp = parseExpr p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          aux p (exp::acc)
        | Rbracket ->
          Parser.next p;
          (exp::acc)
        | _ -> raise (Parser.Expected (p.pos, "expected array thing: ] or ,"))
        end
    in
    let exprs = aux p [] in
    Ast_helper.Exp.array (List.rev exprs)

  and parseTypExpr p =
    let typ = match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      begin match p.Parser.token with
      | Lident ident ->
        Parser.next p;
        Ast_helper.Typ.var ident
      | _ -> raise (Parser.Expected (p.pos, "Expected lowercase ident"))
      end
    | Underscore ->
      Parser.next p;
      Ast_helper.Typ.any ()
    | Forwardslash ->
      Parser.next p;
      parseTupleType p
    | Lparen ->
      Parser.next p;
      let t = parseTypExpr p in
      Parser.expect p Rparen;
      t
    | Uident _ | Lident _ ->
      let startPos = p.pos in
      let lident = parseValuePath p in
      let endPos = p.pos in
      let loc = mkLoc startPos endPos in
      let constr = Location.mkloc lident loc in
      (* TODO extract this whole block into reusable logic, cf. type equation *)
      begin match p.Parser.token with
      | Lparen ->
        Parser.next p;
        Ast_helper.Typ.constr constr (parseConstructorTypeArgs p)
      | _ ->
        Ast_helper.Typ.constr constr []
      end
    | _ ->
      raise (Parser.Expected (p.pos, "hmm we're going to parse a typexpr"))
    in
    match p.Parser.token with
    | As ->
      Parser.next p;
      (* TODO parse quote here *)
      (* Parser.expect p Quote; *)
      begin match p.token with
      | Lident ident ->
        Parser.next p;
        Ast_helper.Typ.alias typ ident
      | _ -> raise (Parser.Expected (p.pos, "ident plz"))
      end
    | _ -> typ


  and parseTupleType p =
    (* / consumed *)
    let rec loop p acc =
      match p.Parser.token with
      | Forwardslash ->
        Parser.next p;
        List.rev acc
      | _ ->
        let typ = parseTypExpr p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (typ::acc)
        | Forwardslash ->
          Parser.next p;
          List.rev (typ::acc)
        | _ -> raise (Parser.Expected (p.pos, "expected tuple type thing: / or ,"))
        end
    in
    let types = loop p [] in
    Ast_helper.Typ.tuple types

  and parseConstructorTypeArgs p =
    let rec loop p types =
      match p.Parser.token with
      | Rparen ->
        Parser.next p;
        List.rev types
      | _ ->
        let typ = parseTypExpr p in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          loop p (typ::types)
        | Rparen ->
          Parser.next p;
          List.rev (typ::types)
        | _ -> raise (Parser.Expected (p.pos, "expected pattern constructor arg thing: ) or ,"))
        end
    in
    loop p []

  and parseFieldDeclaration p =
    let mut = if Parser.optional p Token.Mutable then
      Asttypes.Mutable
    else
      Asttypes.Immutable
    in
    let name = match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.Expected (p.pos, "need lowercase type name"))
    in
    Parser.expect p Colon;
    (* TODO: parse poly type expr *)
    let typ = parseTypExpr p in
    Ast_helper.Type.field ~mut name typ

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
        | _ -> raise (Parser.Expected (p.pos, "Expected comma or rbrace"))
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
       Parsetree.Pcstr_tuple (parseConstructorTypeArgs p)
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
     | _ -> raise (Parser.Expected (p.pos, "expected constr name"))

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
    | _ -> raise (Parser.Expected (p.pos, "expected constr-decl or record-decl"))
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
      | _ -> raise (Parser.Expected (p.pos, "type param needs lident"))
      end
    | Underscore ->
      Parser.next p;
      (Ast_helper.Typ.any (), variance)
    | _ -> raise (Parser.Expected (p.pos, "invalid type param"))
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
    | _ -> raise (Parser.Expected (p.pos, "Expected lowercase ident"))
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
         | _ -> raise (Parser.Expected (p.pos, "value path"))
        in
        let typeConstr = Location.mknoloc (loop p (Longident.Lident uident)) in
        let typ = match p.Parser.token with
        | Lparen ->
          Parser.next p;
          Ast_helper.Typ.constr typeConstr (parseConstructorTypeArgs p)
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
    | _ -> raise (Parser.Expected (p.pos, "Expected Uident"))

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
    let typeConstrName = match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      (Location.mknoloc ident)
    | _ -> raise (Parser.Expected (p.pos, "Type constructor name should be lowercase"))
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
      Ast_helper.Type.mk ~priv ~kind ~params ~cstrs ?manifest typeConstrName

    and parseTypeDefinition p =
    Parser.expect p Token.Typ;
    let recFlag =
      if Parser.optional p Token.Nonrec
        then Asttypes.Nonrecursive
        else Asttypes.Recursive
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

  (* TODO: make sure we have at least one string *)
  and parsePrimitive p =
    let rec loop p prims =
      match p.Parser.token with
      | String s ->
        Parser.next p;
        loop p (s::prims)
      | _ ->
        List.rev prims
    in
    loop p []

  and parseExternalDef ~attrs p =
    Parser.expect p Token.External;
    let name = match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.Expected (p.pos, "external name should be lident"))
    in
    Parser.expect p Colon;
    let typExpr = parseTypExpr p in
    Parser.expect p Equal;
    (* TODO accept multiple strings *)
    let prim = parsePrimitive p in
    Ast_helper.Val.mk ~attrs ~prim name typExpr

  and parseConstrDeclOrName p =
    let name = match p.Parser.token with
    | Uident name ->
      Parser.next p;
      Location.mknoloc name
    | _ -> raise (Parser.Expected (p.pos, "Expected constructor name"))
    in
    let kind = match p.Parser.token with
    | Lparen | Lbrace ->
      let (args, res) = parseConstrDeclArgs p in
      Parsetree.Pext_decl (args, res)
    | Equal ->
      Parser.next p;
      let longident = parseModuleLongIdent p in
      Parsetree.Pext_rebind (Location.mknoloc longident)
    | _ ->
      Parsetree.Pext_decl (Pcstr_tuple [], None)
    in
    (name, kind)

  and parseExceptionDef ~attrs p =
    Parser.expect p Token.Exception;
    let (name, kind) = parseConstrDeclOrName p in
    Ast_helper.Te.constructor ~attrs name kind

  and parseStructure p =
    let rec parse p acc = match p.Parser.token with
      | Eof | Rbrace -> acc
      | _ -> parse p ((parseStructureItem p)::acc)
    in
    let structure = parse p [] in
    List.rev structure

  and parseStructureItem p =
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Open ->
      Ast_helper.Str.open_ (parseOpenDescription ~attrs p)
    | Let ->
      let (recFlag, letBindings) = parseLetBindings p in
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
    | Module -> parseMaybeRecModuleBinding p
    | _ ->
      Ast_helper.Str.eval ~attrs (parseExpr ~allowAttrs:false p)

  and parseIncludeStatement ~attrs p =
    Parser.expect p Token.Include;
    let modExpr = parseModuleExpr p in
    Ast_helper.Incl.mk ~attrs modExpr

  and parseModuleOperand p =
    match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Ast_helper.Mod.ident (Location.mknoloc (Longident.Lident ident))
    | Lbrace ->
      Parser.next p;
      let structure = Ast_helper.Mod.structure (parseStructure p) in
      Parser.expect p Rbrace;
      structure
    | Lparen ->
      Parser.next p;
      parseParenthesizedOrFunctorModuleExpr p
    | _ -> raise (Parser.Expected (p.pos, "Unsupport module expression"))

  and parseParenthesizedOrFunctorModuleExpr p =
    (* Lparen consumed *)
    match p.Parser.token with
    | Underscore -> (* functor arg name, parse functor *)
      parseFunctorModuleExpr p []
    | Rparen ->
      Parser.next p;
      Parser.expect p EqualGreater;
      let rhs = parseModuleExpr p in
      Ast_helper.Mod.functor_ (Location.mknoloc "*") None rhs
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
                p.pos,
                "A functor arg needs a module type"
              ))
            in
          let arg = (
            argName,
            Some moduleType
          ) in
          parseFunctorModuleExpr p [arg]
        | Rparen ->
          Parser.next p;
          begin match p.Parser.token with
          | EqualGreater ->
            Parser.next p;
            let rhs = parseModuleExpr p in
            let argName = match moduleExpression.pmod_desc with
            | Parsetree.Pmod_ident ({Location.txt} as lident) ->
              {lident with txt = Longident.last txt}
            | _ -> raise (Parser.Expected (
                p.pos,
                "A functor arg needs a module type"
              ))
            in
            Ast_helper.Mod.functor_ argName (Some moduleType) rhs
          | _ ->
            Ast_helper.Mod.constraint_ moduleExpression moduleType
          end
        | _ -> raise (Parser.Expected (p.pos, "Expected ) or ,"))
        end
      | Rparen ->
        Parser.next p;
        begin match p.Parser.token with
        | EqualGreater ->
          raise (Parser.Expected (
            p.pos,
            "A functor arg needs a module type"
          ))
        | _ ->
          moduleExpression
        end
      | _ -> raise (Parser.Expected (p.pos, "Expected , or rparen"))
      end

  and parseFunctorArgName p =
    match p.Parser.token with
    | Uident ident -> Parser.next p; Location.mknoloc ident
    | Underscore -> Parser.next p; Location.mknoloc "_"
    | _ -> raise (Parser.Expected (p.pos, "functor arg name should be Uident or _"))


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
        | _ -> raise (Parser.Expected (p.pos, "Expected ) or ,"))
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
        Parser.next p;
        loop p (parseModuleApplication p modExpr)
      | EqualGreater ->
        Parser.next p;
        let arg = match modExpr.Parsetree.pmod_desc with
        | Parsetree.Pmod_ident {Location.loc; txt} ->
          {Location.loc; txt = Longident.last txt}
        | _ -> raise (Parser.Expected (p.pos, "TODO"))
        in
        Ast_helper.Mod.functor_ arg None (parseModuleExpr p)
      | _ ->
        modExpr
    in loop p modExpr

  and parseModuleApplication p modExpr =
    (* left '(' consumed *)
    let arg = parseModuleExpr p in
    Parser.expect p Rparen;
    Ast_helper.Mod.apply modExpr arg

  (* definition	::=
    ∣	 module rec module-name :  module-type =  module-expr   { and module-name :  module-type =  module-expr }
    |  module module-name  { ( module-name :  module-type ) }  [ : module-type ]  =  module-expr *)
  and parseMaybeRecModuleBinding p =
    Parser.expect p Module;
    if Parser.optional p Token.Rec then
      Ast_helper.Str.rec_module (parseModuleBindings p)
    else
      Ast_helper.Str.module_ (parseModuleBinding p)

  and parseModuleBinding p =
    let name = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.Expected (p.pos, "Expected module name"))
    in
    let body = parseModuleBindingBody p in
    Ast_helper.Mb.mk name body

  and parseModuleBindingBody p =
    match p.Parser.token with
    | Equal ->
      Parser.next p;
      parseModuleExpr p
    | _ -> raise (Parser.Expected (p.pos, "Unexpected module body"))

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
    Ast_helper.Mty.ident (Location.mknoloc (parseModuleLongIdent p))

  (* Module types are the module-level equivalent of type expressions: they
   * specify the general shape and type properties of modules. *)
 and parseModuleType p =
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
      parseSpecification p
    | Module ->
      parseModuleTypeOf p
    | _ ->
      raise (Parser.Expected (p.pos, "need a module type"))
    in
    match p.Parser.token with
    | With ->
      let constraints = parseWithConstraints p in
      Ast_helper.Mty.with_ moduleType constraints
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
        Parsetree.Pwith_modsubst (
          Location.mknoloc modulePath,
          Location.mknoloc lident
        )
      | Equal ->
        Parser.next p;
        let lident = parseModuleLongIdent p in
        Parsetree.Pwith_module (
          Location.mknoloc modulePath,
          Location.mknoloc lident
        )
      | _ -> raise (Parser.Expected (p.pos, "Expected = or :="))
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
          Location.mknoloc typeConstr,
          Ast_helper.Type.mk
            ~params
            ~manifest:typExpr
            (Location.mknoloc (Longident.last typeConstr))
        )
      | Equal ->
        Parser.next p;
        let typExpr = parseTypExpr p in
        let typeConstraints = parseTypeConstraints p in
        Parsetree.Pwith_type (
          Location.mknoloc typeConstr,
          Ast_helper.Type.mk
            ~params
            ~manifest:typExpr
            ~cstrs:typeConstraints
            (Location.mknoloc (Longident.last typeConstr))
        )
      | _ -> raise (Parser.Expected (p.pos, "Expected = or := to complete typeconstr"))
      end
    | _ -> raise (Parser.Expected (p.pos, "Unsupported with mod constraint"))

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
      match p.Parser.token with
      | Semicolon ->
        Parser.next p;
        loop p (item::spec)
      | Rbrace ->
        List.rev (item::spec)
      | _ -> raise (Parser.Expected (p.pos, "semi or rbrace expected"))
    in
    Ast_helper.Mty.signature (loop p [])

  and parseSignatureItem p =
    match p.Parser.token with
    | Let ->
      parseSignLetDesc p
    | Typ ->
      let (recFlag, typeDecls) = parseTypeDefinition p in
      Ast_helper.Sig.type_ recFlag typeDecls
    | External ->
      Ast_helper.Sig.value (parseExternalDef ~attrs:[] p)
    | Exception ->
      Ast_helper.Sig.exception_ (parseExceptionDef ~attrs:[] p)
    | Open ->
      Ast_helper.Sig.open_ (parseOpenDescription ~attrs:[] p)
    | Include ->
      Parser.next p;
      let moduleType = parseModuleType p in
      let includeDescription = Ast_helper.Incl.mk moduleType in
      Ast_helper.Sig.include_ includeDescription
    | Module ->
      Parser.next p;
      begin match p.Parser.token with
      | Uident _ ->
        parseModuleDeclarationOrAlias p
      | Typ ->
        parseModuleTypeDeclaration p
      | _ -> raise (Parser.Expected (p.pos, "need type or uident"))
      end
    | _ -> raise (Parser.Expected (p.pos, "signature item"))

  and parseModuleDeclarationOrAlias p =
    let moduleName = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.Expected (p.pos, "Module name should start with uident"))
    in
    let body = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parseModuleType p
    | Equal ->
      Parser.next p;
      let lident = parseModuleLongIdent p in
      Ast_helper.Mty.alias (Location.mknoloc lident)
    | _ -> raise (Parser.Expected (p.pos, "Expected : or ="))
    in
    Ast_helper.Sig.module_ (Ast_helper.Md.mk moduleName body)

  and parseModuleTypeDeclaration p =
    Parser.expect p Typ;
    let moduleName = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.Expected (p.pos, "Module type name should be uident"))
    in
    let typ = match p.Parser.token with
    | Equal ->
      Parser.next p;
      Some (parseModuleType p)
    | _ -> None
    in
    let moduleDecl = Ast_helper.Mtd.mk ?typ moduleName in
    Ast_helper.Sig.modtype moduleDecl

  and parseSignLetDesc p =
    Parser.expect p Let;
    let name = match p.Parser.token with
    | Lident ident -> Parser.next p; Location.mknoloc ident
    | _ -> raise (Parser.Expected (p.pos, "name of value should be lowercase"))
    in
    Parser.expect p Colon;
    let typExpr = parseTypExpr p in
    let valueDesc = Ast_helper.Val.mk name typExpr in
    Ast_helper.Sig.value valueDesc

(*    attr-id	::=	lowercase-ident
 	∣	  capitalized-ident
 	∣	  attr-id .  attr-id   *)
  and parseAttributeId p =
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
      | _ -> raise (Parser.Expected (p.pos, "Expected uident, lident"))
    in
    let id = loop p "" in
    Location.mknoloc id

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
    | Lparen when p.pos.pos_cnum = cnumEndAttrId + 1 ->
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
    let cnumEndAttrId = p.Parser.pos.pos_cnum in
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

  let () =
    let p = Parser.make "@structure (@onBar bar + @onFoo foo)

   @attr(payload) x
    " "file.rjs" in
    (* let p = Parser.make "open Foo.bar" "file.rjs" in *)
    try
      let ast = parseStructure p in
      Pprintast.structure Format.std_formatter ast;
      Format.pp_print_flush Format.std_formatter ();
      print_newline();
      Printast.implementation Format.std_formatter ast;
      Format.pp_print_flush Format.std_formatter ();
      print_newline();
      print_newline()
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
