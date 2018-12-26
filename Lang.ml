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
    | Rec | Nonrec
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
    | Backslash
    | Forwardslash
    | Minus
    | Plus
    | GreaterThan
    | LessThan
    | Hash
    | Asterisk
    | Assert
    | Lazy
    | Tilde
    | Question
    | If | Else | For | In | To | Downto | While | Switch
    | When
    | EqualGreater
    | External
    | Typ
    | Private
    | Mutable
    | Constraint
    | Include
    | Module

  let precedence = function
    | Plus | Minus -> 4
    | Asterisk | Forwardslash -> 5
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
    | Rec -> "rec" | Nonrec -> "nonrec"
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
    | Minus -> "-"
    | Plus -> "+"
    | Backslash -> "\\"
    | Forwardslash -> "/"
    | Exception -> "exception"
    | Hash -> "#"
    | GreaterThan -> ">"
    | LessThan -> "<"
    | Asterisk -> "*"
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
    | EqualGreater -> "=>"
    | External -> "external"
    | Typ -> "type"
    | Private -> "private"
    | Constraint -> "constraint"
    | Mutable -> "mutable"
    | Include -> "include"
    | Module -> "module"

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

  (* TODO: escaping *)
  let lexChar lexbuf =
    let startOff = lexbuf.offset in
    if peek lexbuf == CharacterCodes.singleQuote then (
      next lexbuf; next lexbuf;
      Token.Char (Bytes.unsafe_get lexbuf.src startOff)
    ) else (
      Token.SingleQuote
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
      else if ch == CharacterCodes.equal then (
        if lexbuf.ch == CharacterCodes.greaterThan then (
          next lexbuf;
          Token.EqualGreater
        ) else (
          Token.Equal
        )
      ) else if ch == CharacterCodes.bar then
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
      else if ch == CharacterCodes.backslash then
        Token.Backslash
      else if ch == CharacterCodes.forwardslash then
        Token.Forwardslash
      else if ch == CharacterCodes.minus then
        Token.Minus
      else if ch == CharacterCodes.plus then
        Token.Plus
      else if ch == CharacterCodes.greaterThan then
        Token.GreaterThan
      else if ch == CharacterCodes.lessThan then
        Token.LessThan
      else if ch == CharacterCodes.hash then
        Token.Hash
      else if ch == CharacterCodes.asterisk then
        Token.Asterisk
      else if ch == CharacterCodes.tilde then
        Token.Tilde
      else if ch == CharacterCodes.question then
        Token.Question
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

  let parseOpen p =
    Parser.expect p Open;
    let override = if Parser.optional p Token.Bang then
      Asttypes.Override
    else
      Asttypes.Fresh
    in
    let startIdentLoc = p.pos in
    Lex.printPos startIdentLoc;
    let modident = match p.token with
    | Uident _ ->
      parseModuleLongIdent p
    | _ -> raise (Parser.Expected (p.pos, "expected Uident"))
    in
    let identLoc = mkLoc startIdentLoc p.pos in
    Parser.next p;
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
    | Lbracket ->
      Parser.next p;
      parseArrayPattern p
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

  let rec parseExpr p =
    parseBinaryExpr p 1

  and parseOperand p =
    let expr = match p.Parser.token with
      | Int _ | String _ | Char _ ->
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
        let expr = parseExpr p in
        Parser.expect p Rbrace;
        expr
      | Forwardslash ->
        Parser.next p;
        let expr = parseTuple p in
        expr
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
      | _ ->
        raise (Parser.Expected (p.pos, "unsupported expresion"))
    in
    expr

  and parsePrimaryExpr p =
    let e1 = parseOperand p in
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
    | Minus | Plus | Bang ->
      Parser.next p;
      parseUnaryExpr p
    | _ ->
      parsePrimaryExpr p

  and parseBinaryExpr p prec =
    let a = parseUnaryExpr p in
    let rec loop a =
      let token = p.Parser.token in
      let tokenPrec = Token.precedence token in
      if tokenPrec < prec then a
      else begin
        let startPos = p.pos in
        Parser.next p;
        let endPos = p.pos in
        let operator = Location.mkloc
          (Longident.Lident (Token.toString token)) (mkLoc startPos endPos)
        in
        let b = parseBinaryExpr p (tokenPrec + 1) in
        let expr = Ast_helper.Exp.apply
          (Ast_helper.Exp.ident operator)
          [Nolabel, a; Nolabel, b]
        in
        loop expr
      end
    in
    loop a

  and parseIfExpression p =
    (* If token already consumed *)
    let conditionExpr = parseExpr p in
    Parser.expect p Lbrace;
    let thenExpr = parseExpr p in
    Parser.expect p Rbrace;
    let elseExpr = match p.Parser.token with
    | Else ->
      Parser.next p;
      Parser.expect p Lbrace;
      let elseExpr = parseExpr p in
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
    let bodyExpr = parseExpr p in
    Parser.expect p Rbrace;
    Ast_helper.Exp.for_ pattern e1 e2 direction bodyExpr

  and parseWhileExpression p =
    (* While token consumed *)
    let expr1 = parseExpr p in
    Parser.expect p Lbrace;
    let expr2 = parseExpr p in
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
        let rhs = parseExpr p in
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

   (* ∣	 for value-name =  expr  ( to ∣  downto ) expr do  expr done  *)

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

  (* definition	::=	let [rec] let-binding  { and let-binding }   *)
  let parseLetBindings p =
    Parser.expect p Let;
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

  let parseFieldDeclaration p =
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

  let parseRecordDeclaration p =
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

  let parseConstrDeclArgs p =
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

  let parseTypeConstructorDeclaration p =
     match p.Parser.token with
     | Uident uident ->
       Parser.next p;
       let (args, res) = parseConstrDeclArgs p in
       Ast_helper.Type.constructor ?res ~args (Location.mknoloc uident)
     | _ -> raise (Parser.Expected (p.pos, "expected constr name"))

   (* [|] constr-decl  { | constr-decl }   *)
  let parseTypeConstructorDeclarations p =
    ignore (Parser.optional p Token.Bar);
    let firstConstrDecl = parseTypeConstructorDeclaration p in
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
  let parseTypeRepresentation p =
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

  let parseTypeParam p =
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

  let parseTypeParams p =
    let params = match p.Parser.token with
    | LessThan -> Parser.next p; [parseTypeParam p]
    | _ -> []
    in
    params

  let parseTypeConstraint p =
    Parser.expect p SingleQuote;
    begin match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      Parser.expect p Equal;
      let typ = parseTypExpr p in
      (Ast_helper.Typ.var ident, typ, Location.none)
    | _ -> raise (Parser.Expected (p.pos, "Expected lowercase ident"))
    end

  let parseTypeConstraints p =
    let rec loop p constraints =
      match p.Parser.token with
      | Constraint ->
        Parser.next p;
        let constraint_ = parseTypeConstraint p in
        (constraint_::constraints)
      | _ -> List.rev constraints
    in
    loop p []

  let parseTypeEquationAndRepresentation p =
    match p.Parser.token with
    | Equal ->
      Parser.next p;
      begin match p.Parser.token with
      (* TODO: Uident as module path *)
      (* start of type representation *)
      | Bar | Uident _ | Lbrace | Private | Dot ->
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



  (* typedef ::= typeconstr-name [type-params] type-information *)
  (* type-information ::= [type-equation] [type-representation] * {type-constraint} *)
  let parseTypeDef p =
    let typeConstrName = match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      (Location.mknoloc ident)
    | _ -> raise (Parser.Expected (p.pos, "Expected lident"))
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

  let parseTypeDefinition p =
    Parser.expect p Token.Typ;
    let recFlag =
      if Parser.optional p Token.Nonrec
        then Asttypes.Nonrecursive
        else Asttypes.Recursive
    in
    let typeDef = parseTypeDef p in
    Ast_helper.Str.type_ recFlag [typeDef]

  let parseExternalDef p =
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
    let externalDecl = match p.Parser.token with
    | String s -> Parser.next p; s
    | _ -> raise (Parser.Expected (p.pos, "external decl needs to be a string"))
    in
    Ast_helper.Str.primitive (Ast_helper.Val.mk ~prim:[externalDecl] name typExpr)

  let parseConstrDeclOrName p =
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

  let parseExceptionDef p =
    Parser.expect p Token.Exception;
    let (name, kind) = parseConstrDeclOrName p in
    let constructor =
      Ast_helper.Te.constructor name kind
    in
    Ast_helper.Str.exception_ constructor



 let rec parseStructure p =
    let rec parse p acc = match p.Parser.token with
      | Eof -> acc
      (* TODO is this sane? *)
      | Rbrace -> Parser.next p; acc
      | _ -> parse p ((parseStructureItem p)::acc)
    in
    let structure = parse p [] in
    List.rev structure

  and parseStructureItem p =
    match p.Parser.token with
    | Open -> parseOpen p
    | Let -> parseLetBindings p
    | Typ -> parseTypeDefinition p
    | External -> parseExternalDef p
    | Exception -> parseExceptionDef p
    | Include -> parseIncludeStatement p
    | Module -> parseMaybeRecModuleBinding p
    | _ -> raise (Parser.Expected (p.pos, "structure item"))

  and parseIncludeStatement p =
    Parser.expect p Token.Include;
    let modExpr = parseModuleExpr p in
    Ast_helper.Str.include_ (Ast_helper.Incl.mk modExpr)

  and parseModuleOperand p =
    match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Ast_helper.Mod.ident (Location.mknoloc (Longident.Lident ident))
    | Lbrace ->
      Parser.next p;
      Ast_helper.Mod.structure (parseStructure p)
    | Lparen ->
      Parser.next p;
      let modExpr = parseModuleExpr p in
      Parser.expect p Rparen;
      modExpr
    | _ -> raise (Parser.Expected (p.pos, "Unsupport module expression"))

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

  let () =
    let p = Parser.make "
    include (Foo(Bar)(Baz))

    module Bar = Foo => Lala

    include {
      let x = 1
    }
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
