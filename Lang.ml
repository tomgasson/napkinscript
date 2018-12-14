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

  let lparen = 0x28
  let rparen = 0x29
  let lbracket = 0x5B
  let rbracket = 0x5D
  let lbrace = 0x7B
  let rbrace = 0x7D


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
      Upper.a <= ch && ch <= Lower.z

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
    | Rec
    | Underscore
    | SingleQuote
    | Equal
    | And
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
    | Rec -> "rec"
    | Underscore -> "_"
    | SingleQuote -> "'"
    | Equal -> "="
    | And -> "and"
    | Eof -> "eof"
    | Bar -> "bar"
    | As -> "as"
    | Lparen -> "("
    | Rparen -> ")"
    | Lbracket -> "["
    | Rbracket -> "]"
    | Lbrace -> "{"
    | Rbrace -> "}"
    | Colon -> ":"
    | Comma -> ","
    | Exception -> "exception"


  let keywordTable =
    let keywords = [|
      "open", Open;
      "let", Let;
      "rec", Rec;
      "and", And;
      "as", As;
      "exception", Exception;
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
    while ((CharacterCodes.isLetter lexbuf.ch) || (CharacterCodes.isDigit lexbuf.ch)) do
      next(lexbuf)
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

  let lexChar lexbuf =
    let startOff = lexbuf.offset in
    while not (CharacterCodes.singleQuote == lexbuf.ch) do
      next lexbuf
    done;
    next lexbuf; (* advance past ending ' *)
    Token.Char (Bytes.unsafe_get lexbuf.src startOff)

  let lexString lexbuf =
    let startOff = lexbuf.offset in
    while not (CharacterCodes.doubleQuote == lexbuf.ch) do
      next lexbuf
    done;
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
        lexChar lexbuf
      else if ch == CharacterCodes.bang then
        Token.Bang
      else if ch == CharacterCodes.semicolon then
        Token.Semicolon
      else if ch == CharacterCodes.underscore then
        Token.Underscore
      else if ch == CharacterCodes.equal then
        Token.Equal
      else if ch == CharacterCodes.bar then
        Token.Bar
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
        Token.Colon
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
      next p;
      if p.token = token then
        let () = next p in true
      else
        false

    exception Expected of (Lexing.position * string)

    let expect p token =
      if p.token = token then
        next p
      else
        raise (Expected (p.pos, (Token.toString token)))
  end

  (* Parses module identifiers:
       Foo
       Foo.Bar *)
  let rec parseModuleLongIdent p ident =
    Parser.next p;
    match p.token with
    | Dot ->
      Parser.next p;
      (match p.token with
      | Uident afterDotIdent ->
        Longident.Ldot (parseModuleLongIdent p ident, afterDotIdent)
      | _ ->
        raise (Parser.Expected (p.pos, "expected Uident")))
    | _ ->  Lident ident


  let parseOpen p =
    let override = if Parser.optional p Token.Bang then
      Asttypes.Override
    else
      Asttypes.Fresh
    in
    let startIdentLoc = p.pos in
    Lex.printPos startIdentLoc;
    let modident = match p.token with
    | Uident ident ->
      parseModuleLongIdent p ident
    | _ -> raise (Parser.Expected (p.pos, "expected Uident"))
    in
    let identLoc = mkLoc startIdentLoc p.pos in
    Parser.optional p Token.Semicolon |> ignore;
    Ast_helper.Str.open_ (
      Ast_helper.Opn.mk ~override (
        Location.mkloc modident identLoc
      )
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
    | Char c -> Pconst_char c
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
    | Int _ | String _ | Char _ ->
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
        let pat = parsePattern p in
        Parser.expect p Token.Rparen;
        pat
    | Underscore ->
        Parser.next p;
        Ast_helper.Pat.any ()
    | Lident ident ->
        Parser.next p;
        Ast_helper.Pat.var (Location.mkloc ident Location.none)
    (* | Exception -> *)
        (* Parser.next p; *)
        (* let pat = parsePattern p in *)
        (* Ast_helper.Pat.exception pat *)
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


  let parseExpr p =
    let c = parseConstant p in
    Ast_helper.Exp.constant c


  (* definition	::=	let [rec] let-binding  { and let-binding }   *)
  let parseLetBindings p =
    let recFlag = if Parser.optional p Token.Rec then
      Asttypes.Recursive
    else
      Asttypes.Nonrecursive
    in
    let pat = parsePattern p in
    Parser.expect p Token.Equal;
    let exp = parseExpr p in
    let vb = Ast_helper.Vb.mk pat exp in
    Ast_helper.Str.value recFlag [vb]

  let parseStructureItem p =
    match p.Parser.token with
    | Open -> parseOpen p
    | Let -> parseLetBindings p
    | _ -> raise (Parser.Expected (p.pos, "structure item"))

  let () =
    let p = Parser.make "let 'a' .. 'b' as x = 1" "file.rjs" in
    (* let p = Parser.make "open Foo.bar" "file.rjs" in *)
    let ast = parseStructureItem p in
    Pprintast.structure Format.std_formatter [ast];
    Format.pp_print_flush Format.std_formatter ();
    print_newline();
    Printast.implementation Format.std_formatter [ast];
    Format.pp_print_flush Format.std_formatter ();
    print_newline();
    print_newline()
end
