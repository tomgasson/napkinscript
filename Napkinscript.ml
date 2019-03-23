(* Uncomment for release, to output 4.02 binary ast

open Migrate_parsetree

module To_402 = Convert(OCaml_406)(OCaml_402)

open Ast_406
*)

module IO: sig
  val readFile: string -> string
end = struct
  (* random chunk size: 2^15, TODO: why do we guess randomly? *)
  let chunkSize = 32768

  let readFile filename =
    let chan = open_in filename in
    let buffer = Buffer.create chunkSize in
    let chunk = Bytes.create chunkSize in
    let rec loop () =
      let len = input chan chunk 0 chunkSize in
      if len == 0 then (
        close_in chan;
        Buffer.contents buffer
      ) else (
        Buffer.add_subbytes buffer chunk 0 len;
        loop ()
      )
    in
    loop ()
end

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
    | Plus | PlusDot | PlusPlus | PlusEqual
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
    | At | AtAt
    | Percent | PercentPercent
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
    | Plus -> "+" | PlusDot -> "+." | PlusPlus -> "++" | PlusEqual -> "+="
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
    | At -> "@" | AtAt -> "@@"
    | Percent -> "%" | PercentPercent -> "%%"
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
      "with", With;
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
    | Land | Lor | Lxor | Lsl | Lsr | Asr | List | Export | With -> true
    | _ -> false

  let lookupKeyword str =
    try Hashtbl.find keywordTable str with
    | Not_found ->
      if CharacterCodes.isUpperCase (int_of_char str.[0]) then
        Uident str
      else Lident str
end

module Grammar = struct
  type t =
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
    | ExprBlock
    | ExprCall
    | ExprList
    | ExprArrayAccess
    | ExprArrayMutation
    | ExprIf
    | IfCondition | IfBranch | ElseBranch
    | TypeExpression
    | External
    | PatternMatching
    | PatternMatchCase
    | LetBinding
    | PatternList

    | TypeDef
    | TypeConstrName
    | TypeParams
    | TypeParam

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

  let toString = function
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
    | ExprBlock -> "a block with expressions"
    | ExprSetField -> "a record field mutation"
    | ExprCall -> "a function application"
    | ExprArrayAccess -> "an array access expression"
    | ExprArrayMutation -> "an array mutation"
    | LetBinding -> "a let binding"
    | TypeDef -> "a type definition"
    | TypeParams -> "type parameters"
    | TypeParam -> "a type parameter"
    | TypeConstrName -> "a type-constructor name"
    | TypeRepresentation -> "a type representation"
    | RecordDecl -> "a record declaration"
    | PatternMatchCase -> "a pattern match case"
    | ConstructorDeclaration -> "a constructor declaration"
    | ExprList -> "multiple expressions"
    | PatternList -> "multiple patterns"
    | ParameterList -> "parameters"
    | StringFieldDeclarations -> "string field declarations"
    | FieldDeclarations -> "field declarations"
    | TypExprList -> "list of types"
    | FunctorArgs -> "functor arguments"
    | ModExprList -> "list of module expressions"
    | TypeParameters -> "list of type parameters"
    | RecordRows -> "rows of a record"
    | RecordRowsStringKey -> "rows of a record with string keys"
    | ArgumentList -> "arguments"

  let isStructureItemStart = function
    | Token.Open
    | Let
    | Typ
    | External
    | Exception
    | Include
    | Module
    | AtAt
    | PercentPercent -> true
    | _ -> false

  let isExprStart = function
    | Token.True | False
    | Int _ | String _ | Float _ | Backtick
    | Uident _ | Lident _
    | Lparen
    | List | Lbracket | Lbrace | Forwardslash
    | LessThan
    | Minus | MinusDot | Plus | PlusDot
    | Percent -> true
    | _ -> false

  let isPatternStart = function
    | Token.Int _ | String _
    | Lparen | Lbracket | Lbrace | Forwardslash | List
    | Underscore
    | Lident _ | Uident _
    | Exception | Lazy | Percent
    | At -> true
    | _ -> false

  let isParameterStart = function
    | token when isPatternStart token -> true
    | Token.Tilde -> true
    | _ -> false

  (* TODO: overparse Uident ? *)
  let isStringFieldDeclStart = function
    | Token.String _ | At -> true
    | _ -> false

  (* TODO: overparse Uident ? *)
  let isFieldDeclStart = function
    | Token.At | Mutable | Lident _ -> true
    | _ -> false

  let isRecordDeclStart = function
    | Token.At
    | Mutable
    | Lident _ -> true
    | _ -> false

  let isTypExprStart = function
    | Token.At
    | SingleQuote
    | Underscore
    | Forwardslash
    | Lparen
    | Uident _ | Lident _ | List
    | Percent
    | Lbrace -> true
    | _ -> false

  let isTypeParameterStart = function
    | token when isTypExprStart token -> true
    | Token.Tilde -> true
    | _ -> false

  let isTypeParamStart = function
    | Token.Plus | Minus | SingleQuote | Underscore -> true
    | _ -> false

  let isFunctorArgStart = function
    | Token.At | Uident _ | Underscore
    | Percent
    | Lbrace
    | Lparen -> true
    | _ -> false

  let isModExprStart = function
    | Token.At | Percent
    | Uident _ | Lbrace | Lparen -> true
    | _ -> false

  let isRecordRowStart = function
    | Token.Uident _ | Lident _ -> true
    | _ -> false

  let isRecordRowStringKeyStart = function
    | Token.String _ -> true
    | _ -> false

  let isArgumentStart = function
    | t when isExprStart t -> true
    | Token.Tilde -> true
    | _ -> false

  let isListElement grammar token =
    match grammar with
    | ExprList -> isExprStart token
    | PatternList -> isPatternStart token
    | ParameterList -> isParameterStart token
    | StringFieldDeclarations -> isStringFieldDeclStart token
    | FieldDeclarations -> isFieldDeclStart token
    | RecordDecl -> isRecordDeclStart token
    | TypExprList -> isTypExprStart token
    | TypeParams -> isTypeParamStart token
    | FunctorArgs -> isFunctorArgStart token
    | ModExprList -> isModExprStart token
    | TypeParameters -> isTypeParameterStart token
    | RecordRows -> isRecordRowStart token
    | RecordRowsStringKey -> isRecordRowStringKeyStart token
    | ArgumentList -> isArgumentStart token
    | _ -> false
end

module Diagnostics: sig
  type t
  type category

  val unexpected: Token.t -> (Grammar.t * Lexing.position) list -> category
  val uident: category
  val lident: category
  val unclosedString: category
  val unclosedTemplate: category
  val unclosedComment: category
  val unknownUchar: category

  val make:
    filename: string
    -> startPos: Lexing.position
    -> len: int
    -> category
    -> t

  val print: t -> unit

end = struct
  type category =
    | Unexpected of (Token.t * ((Grammar.t * Lexing.position) list))
    | Expected of (Token.t * Lexing.position (* prev token end*) * Grammar.t option)
    | Message of string
    | Uident
    | Lident
    | UnclosedString
    | UnclosedTemplate
    | UnclosedComment
    | UnknownUchar
    | Unbalanced of Token.t

  let stringOfCategory = function
    | Unexpected _ -> "unexpected"
    | Expected _ -> "expected"
    | Message txt -> txt
    | Uident -> "uident"
    | Lident -> "lident"
    | UnclosedString -> "unclosed string"
    | UnclosedTemplate -> "unclosed template"
    | UnclosedComment -> "unclosed comment"
    | Unbalanced _ -> "unbalanced"
    | UnknownUchar -> "unknown rune"

  type t = {
    filename: string;
    startPos: Lexing.position;
    length: int; (* or endPos? *)
    category: category;
  }

  let make ~filename ~startPos ~len category = {
    filename;
    startPos;
    length = len;
    category
  }

  let print {category} =
    prerr_endline (stringOfCategory category)

  let unexpected token context =
    Unexpected(token, context)

  let uident = Uident
  let lident = Lident
  let unclosedString = UnclosedString
  let unclosedComment = UnclosedComment
  let unclosedTemplate = UnclosedTemplate
  let unknownUchar = UnknownUchar
end

module Scanner = struct
  type mode = Template | Tuple | Jsx | Diamond

  let string_of_mode = function
    | Template -> "template"
    | Tuple -> "tuple"
    | Jsx -> "jsx"
    | Diamond -> "diamond"


  type t = {
    filename: string;
    src: bytes;
    mutable err: t -> Diagnostics.category -> unit;
    mutable ch: int; (* current character *)
    mutable offset: int; (* character offset *)
    mutable rdOffset: int; (* reading offset (position after current character) *)
    mutable lineOffset: int; (* current line offset *)
    mutable lnum: int; (* current line number *)
    mutable mode: mode list;
  }

	let setDiamondMode scanner =
		scanner.mode <- Diamond::scanner.mode

  let setTemplateMode scanner =
    scanner.mode <- Template::scanner.mode

  let setTupleMode scanner =
    scanner.mode <- Tuple::scanner.mode

  let setJsxMode scanner =
    scanner.mode <- Jsx::scanner.mode

  let popMode scanner mode =
    match scanner.mode with
    | m::ms when m = mode ->
      scanner.mode <- ms
    | _ -> ()

  let inTupleMode scanner = match scanner.mode with
    | Tuple::_ -> true
    | _ -> false

  let inDiamondMode scanner = match scanner.mode with
    | Diamond::_ -> true
    | _ -> false

  let inJsxMode scanner = match scanner.mode with
    | Jsx::_ -> true
    | _ -> false

  let inTemplateMode scanner = match scanner.mode with
    | Template::_ -> true
    | _ -> false

  let position scanner = Lexing.{
    pos_fname = scanner.filename;
    (* line number *)
    pos_lnum = scanner.lnum;
    (* offset of the beginning of the line (number
       of characters between the beginning of the scanner and the beginning
       of the line) *)
    pos_bol = scanner.lineOffset;
    (* [pos_cnum] is the offset of the position (number of
       characters between the beginning of the scanner and the position). *)
    pos_cnum = scanner.offset;
  }

  (* TODO: refactor, won't work if char width > 1 char *)
  let computeEndPosition scanner = Lexing.{
    pos_fname = scanner.filename;
    (* line number *)
    pos_lnum =
      if scanner.offset < scanner.lineOffset then scanner.lnum - 1 else scanner.lnum;
    (* offset of the beginning of the line (number
       of characters between the beginning of the scanner and the beginning
       of the line) *)
    pos_bol = scanner.lineOffset; (* Unsound *)
    (* [pos_cnum] is the offset of the position (number of
       characters between the beginning of the scanner and the position). *)
    pos_cnum = scanner.offset;
  }

  let printPos p =
    print_endline ("cnum: " ^ (string_of_int p.Lexing.pos_cnum));
    print_endline ("lnum: " ^ (string_of_int p.Lexing.pos_lnum));
    print_endline ("bol: " ^ (string_of_int p.Lexing.pos_bol));
    print_endline ("beginning of line: " ^ (string_of_int p.Lexing.pos_bol));
    print_endline ("-------------------")

  let next scanner =
    if scanner.rdOffset < (Bytes.length scanner.src) then (
      scanner.offset <- scanner.rdOffset;
      let ch = Bytes.get scanner.src scanner.rdOffset in
      if ch = '\n' then begin
        scanner.lineOffset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1
      end;
      scanner.rdOffset <- scanner.rdOffset + 1;
      scanner.ch <- int_of_char ch
    ) else (
      scanner.offset <- Bytes.length scanner.src;
      scanner.ch <- -1
    )

  let peek scanner =
    if scanner.rdOffset < (Bytes.length scanner.src) then
      int_of_char (Bytes.unsafe_get scanner.src scanner.rdOffset)
    else
      -1

  let make b filename =
    let scanner = {
      filename;
      src = b;
      err = (fun _ _ -> ());
      ch = CharacterCodes.space;
      offset = 0;
      rdOffset = 0;
      lineOffset = 0;
      lnum = 1;
      mode = [];
    } in
    next scanner;
    scanner


  (* black magic, use sparingly! *)
  let lookahead scanner callback =
    let scannerCopy = {scanner with filename = scanner.filename} in
    callback scannerCopy

  let skipWhitespace scanner =
    while
      (scanner.ch == CharacterCodes.space) ||
      (scanner.ch == CharacterCodes.tab) ||
      (scanner.ch == CharacterCodes.newline)
    do next scanner
    done

  let scanIdentifier scanner =
    let startOff = scanner.offset in
    while (
      CharacterCodes.isLetter scanner.ch ||
      CharacterCodes.isDigit scanner.ch ||
      scanner.ch == CharacterCodes.underscore
    ) do
      next scanner
    done;
    let str = Bytes.sub_string scanner.src startOff (scanner.offset - startOff) in
    Token.lookupKeyword str

  (* float: (0…9) { 0…9∣ _ } [. { 0…9∣ _ }] [(e∣ E) [+∣ -] (0…9) { 0…9∣ _ }]   *)
  let scanNumber scanner =
    let startOff = scanner.offset in
    while CharacterCodes.isDigit scanner.ch do
      next scanner
    done;
    (* floats *)
    if CharacterCodes.dot == scanner.ch then (
      next scanner;
      while CharacterCodes.isDigit scanner.ch do
        next scanner
      done;
      let str = Bytes.sub_string scanner.src startOff (scanner.offset - startOff) in
      Token.Float str
    ) else (
      let str = Bytes.sub_string scanner.src startOff (scanner.offset - startOff) in
      Token.Int str
    )

  let scanString scanner =
    let buffer = Buffer.create 256 in

    let rec scan () =
      if scanner.ch == CharacterCodes.eol then
        scanner.err scanner Diagnostics.unclosedString
      else if scanner.ch == CharacterCodes.doubleQuote then (
        next scanner
      ) else if scanner.ch == CharacterCodes.backslash then (
        next scanner;
        let char_for_backslash = function
          | 110 -> '\010'
          | 114 -> '\013'
          | 98 -> '\008'
          | 116 -> '\009'
          | c   -> Char.chr c
        in
        Buffer.add_char buffer (char_for_backslash scanner.ch);
        next scanner;
        scan ()
      ) else if CharacterCodes.isLineBreak scanner.ch then (
        scanner.err scanner Diagnostics.unclosedString;
        next scanner
      ) else (
        Buffer.add_char buffer (Char.chr scanner.ch);
        next scanner;
        scan ()
      )
    in
    scan ();
    Token.String (Buffer.contents buffer)

  let scanSingleLineComment scanner =
    let startOff = scanner.offset in
    while not (CharacterCodes.isLineBreak scanner.ch) &&
      scanner.rdOffset < (Bytes.length scanner.src)
    do
      next scanner
    done;
    next scanner;
    Token.Comment (
      Bytes.sub_string scanner.src startOff (scanner.offset - 1 - startOff)
    )

  let scanMultiLineComment scanner =
    let startOff = scanner.offset in
    let rec scan () =
      if scanner.ch == CharacterCodes.asterisk &&
         peek scanner == CharacterCodes.forwardslash then (
        next scanner;
        next scanner
      ) else if scanner.ch == CharacterCodes.eol then (
        scanner.err scanner Diagnostics.unclosedComment
      ) else (
        next scanner;
        scan ()
      )
    in
    scan();
    Token.Comment (
      Bytes.sub_string scanner.src startOff (scanner.offset - 1 - startOff)
    )

  let scanTemplate scanner =
    let startOff = scanner.offset in

    let rec scan () =
      if scanner.ch == CharacterCodes.eol then (
        scanner.err scanner Diagnostics.unclosedTemplate;
        popMode scanner Template;
        Token.TemplateTail(
          Bytes.sub_string scanner.src startOff (scanner.offset - 2 - startOff)
        )
      )
      else if scanner.ch == CharacterCodes.backtick then (
        next scanner;
        let contents =
          Bytes.sub_string scanner.src startOff (scanner.offset - 1 - startOff)
        in
        popMode scanner Template;
        Token.TemplateTail contents
      ) else if scanner.ch == CharacterCodes.dollar &&
                peek scanner == CharacterCodes.lbrace
        then (
          next scanner; (* consume $ *)
          next scanner; (* consume { *)
          let contents =
            Bytes.sub_string scanner.src startOff (scanner.offset - 2 - startOff)
          in
          popMode scanner Template;
          Token.TemplatePart contents
      ) else (
        next scanner;
        scan()
      )
    in
    scan()

  let rec scan scanner =
    if not (inTemplateMode scanner) then skipWhitespace scanner;
    let startPos = position scanner in
    let ch = scanner.ch in
    let token = if inTemplateMode scanner then
      scanTemplate scanner
    else if ch == CharacterCodes.underscore then (
      let nextCh = peek scanner in
      if nextCh == CharacterCodes.underscore || CharacterCodes.isLetter nextCh then
        scanIdentifier scanner
      else (
        next scanner;
        Token.Underscore
      )
    ) else if CharacterCodes.isLetter ch then
      scanIdentifier scanner
    else if CharacterCodes.isDigit ch then
      scanNumber scanner
    else begin
      next scanner;
      if ch == CharacterCodes.dot then
        if scanner.ch == CharacterCodes.dot then (
          next scanner;
          if scanner.ch == CharacterCodes.dot then (
            next scanner;
            Token.DotDotDot
          ) else (
            Token.DotDot
          )
        ) else (
          Token.Dot
        )
      else if ch == CharacterCodes.doubleQuote then
        scanString scanner
      else if ch == CharacterCodes.singleQuote then
        Token.SingleQuote
      else if ch == CharacterCodes.bang then
        if scanner.ch == CharacterCodes.equal then (
          next scanner;
          if scanner.ch == CharacterCodes.equal then (
            next scanner;
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
        if scanner.ch == CharacterCodes.greaterThan then (
          next scanner;
          Token.EqualGreater
        ) else if scanner.ch == CharacterCodes.equal then (
          next scanner;
          if scanner.ch == CharacterCodes.equal then (
            next scanner;
            Token.EqualEqualEqual
          ) else (
            Token.EqualEqual
          )
        ) else (
          Token.Equal
        )
      ) else if ch == CharacterCodes.bar then
        if scanner.ch == CharacterCodes.bar then (
          next scanner;
          Token.Lor
        ) else if scanner.ch == CharacterCodes.greaterThan then (
          next scanner;
          Token.BarGreater
        ) else (
          Token.Bar
        )
      else if ch == CharacterCodes.ampersand then
        if scanner.ch == CharacterCodes.ampersand then (
          next scanner;
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
       if scanner.ch == CharacterCodes.equal then(
          next scanner;
          Token.ColonEqual
        ) else (
          Token.Colon
        )
      else if ch == CharacterCodes.backslash then
        Token.Backslash
      else if ch == CharacterCodes.forwardslash then
        if scanner.ch == CharacterCodes.forwardslash then (
          next scanner;
          scanSingleLineComment scanner
        ) else if (scanner.ch == CharacterCodes.asterisk) then (
          next scanner;
          scanMultiLineComment scanner
        ) else (
          if inTupleMode scanner then
            scanForwardSlashOrTupleEnding scanner
          else
          Token.Forwardslash
        )
      else if ch == CharacterCodes.minus then
        if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.MinusDot
        ) else if scanner.ch == CharacterCodes.greaterThan then (
          next scanner;
          Token.MinusGreater;
        ) else (
          Token.Minus
        )
      else if ch == CharacterCodes.plus then
        if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.PlusDot
        ) else if scanner.ch == CharacterCodes.plus then (
          next scanner;
          Token.PlusPlus
        ) else if scanner.ch == CharacterCodes.equal then (
          next scanner;
          Token.PlusEqual
        ) else (
          Token.Plus
        )
      else if ch == CharacterCodes.greaterThan then
        if scanner.ch == CharacterCodes.equal && not (inDiamondMode scanner) then (
          next scanner;
          Token.GreaterEqual
        ) else (
          Token.GreaterThan
        )
      else if ch == CharacterCodes.lessThan then
        (* Imagine the following: <div><
         * < indicates the start of a new jsx-element, the parser expects
         * the name of a new element after the <
         * Example: <div> <div
         * But what if we have a / here: example </ in  <div></div>
         * This signals a closing element. To simulate the two-token lookahead,
         * the </ is emitted as a single new token LessThanSlash *)
        if inJsxMode scanner then (
          skipWhitespace scanner;
          if scanner.ch == CharacterCodes.forwardslash then
            let () = next scanner in
            Token.LessThanSlash
          else
            Token.LessThan
        ) else if scanner.ch == CharacterCodes.equal then (
          next scanner;
          Token.LessEqual
        ) else (
          Token.LessThan
        )
      else if ch == CharacterCodes.hash then
        if scanner.ch == CharacterCodes.hash then(
          next scanner;
          Token.HashHash
        ) else if scanner.ch == CharacterCodes.equal then(
          next scanner;
          Token.HashEqual
        ) else (
          Token.Hash
        )
      else if ch == CharacterCodes.asterisk then
        if scanner.ch == CharacterCodes.asterisk then (
          next scanner;
          Token.Exponentiation;
        ) else (
          Token.Asterisk
        )
      else if ch == CharacterCodes.tilde then
        Token.Tilde
      else if ch == CharacterCodes.question then
        Token.Question
      else if ch == CharacterCodes.at then
        if scanner.ch == CharacterCodes.at then (
          next scanner;
          Token.AtAt
        ) else (
          Token.At
        )
    else if ch == CharacterCodes.percent then
      if scanner.ch == CharacterCodes.percent then (
        next scanner;
        Token.PercentPercent
      ) else (
        Token.Percent
      )
      else if ch == CharacterCodes.backtick  then
        Token.Backtick
      else if ch == -1 then
        Token.Eof
      else (
        (* if we arrive here, we're dealing with an unkown character,
         * report the error and continue scanning… *)
        scanner.err scanner Diagnostics.unknownUchar;
        let (_, _, token) = scan scanner in
        token
      )
    end
    in
    let endPos = computeEndPosition scanner in
    (startPos, endPos, token)

  and scanForwardSlashOrTupleEnding scanner =
    let cb scanner =
      let (_, _, token) = scan scanner in
      match token with
      | Lident _ ->
        next scanner;
        if scanner.ch != CharacterCodes.equal then
          Token.TupleEnding
        else
          Token.Forwardslash
      | GreaterThan
      | Int _ | Uident _ | Lparen | Minus | Plus
      | Lazy | If | For | While | Switch | At -> Token.Forwardslash
      | _ -> TupleEnding
    in
    let result = lookahead scanner cb in
    if result = TupleEnding then popMode scanner Tuple;
    result

  (* Imagine: <div> <Navbar /> <
   * is `<` the start of a jsx-child? <div …
   * or is it the start of a closing tag?  </div>
   * reconsiderLessThan peeks at the next token and
   * determines the correct token to disambiguate *)
  let reconsiderLessThan scanner =
    (* < consumed *)
    skipWhitespace scanner;
    if scanner.ch == CharacterCodes.forwardslash then
      let () = next scanner in
      Token.LessThanSlash
    else
      Token.LessThan

end




module Reporting = struct

  module TerminalDoc = struct
    type break =
      | IfNeed
      | Never
      | Always

    type document =
      | Nil
      | Group of break * document
      | Text of string
      | Indent of int * document
      | Append of document* document

    let group ?(break= IfNeed)  doc = Group (break, doc)
    let text txt = Text (txt)
    let indent i d = Indent (i, d)
    let append d1 d2 = Append (d1, d2)

    type stack =
      | Empty
      | Cons of document* stack

    let push stack doc = Cons (doc, stack)

    type mode =
      | Flat
      | Break


    let rec fits w stack =
      match stack with
      | _ when w < 0 -> false
      | Empty  -> true
      | Cons (doc,stack) ->
        begin match doc with
         | Nil  -> fits w stack
         | Text txt ->
           fits (w - (String.length txt)) stack
         | Append (d1,d2) ->
           let stack =
             let stack = push stack d1 in
             push stack d2
           in
           fits w stack
         | Group (_,d) ->
           fits w (push stack d)
         | Indent (i,d) ->
           fits (w - i) (push stack d)
         end

    let toString ~width (doc : document) =
      let buffer = Buffer.create 100 in
      let rec loop stack mode offset =
        match stack with
        | Empty  -> ()
        | Cons (doc, rest) ->
          begin match doc with
           | Nil -> loop rest mode offset
           | Text txt ->
             Buffer.add_string buffer txt;
             loop rest mode (offset + (String.length txt))
           | Indent (i,doc) ->
             let indentation = String.make i ' ' in
             Buffer.add_string buffer indentation;
             loop (push rest doc) mode (offset + i)
           | Append (doc1,doc2) ->
              let rest = push rest doc2 in
              let rest = push rest
                (match mode = Flat with
                | true  -> Nil
                | false  -> text "\n")
              in
              let rest = push rest doc1 in
              loop rest mode offset
           | Group (break,doc) ->
             let rest = push rest doc in
             begin match break with
             | Always  -> loop rest Break offset
             | Never  -> loop rest Flat offset
             | IfNeed  ->
               if fits (width - offset) rest
               then loop rest Flat offset
               else loop rest Break offset
             end
            end
      in
      loop (push Empty doc) Flat 0;
      Buffer.contents buffer



  end

  type color =
    | NoColor
    | Red

  type style = {
    underline: bool;
    color: color;
  }

  let emptyStyle = {
    underline = false;
    color = NoColor;
  }

  let highlight ~from ~len txt =
    let before = String.sub txt 0 from in
    let content =
      "\027[31m" ^ String.sub txt from len ^ "\027[0m"
    in
    let after =
      String.sub txt (from + len) (String.length txt - (from + len))
    in
    before ^ content ^ after

  let underline ~from ~len txt =
    let open TerminalDoc in
    let indent = String.make from ' ' in
    let underline = String.make len '^' in
    group ~break:Always
      (append (text txt) (text (indent ^ underline)))

  let applyStyle ~from ~len style txt =
    let open TerminalDoc in
        let colorizedText =
      if style.color <> NoColor then
        highlight ~from ~len txt
      else
        txt
    in
    underline ~from ~len colorizedText

  let parseContext stack =
    match stack with
    | ((Grammar.ExprOperand, _)::cs) ->
        begin match cs with
        | (ExprBinaryAfterOp _ as c, _)::cs ->
          Grammar.toString c
        | _ -> "a basic expression"
        end
    | ((c, _)::cs) ->
        Grammar.toString c
    | [] -> "your code"

  let rec drop n l =
    if n == 1 then l
    else drop (n - 1) (match l with | x::xs -> xs | _ -> l)

  let rec take n l =
    match l with
    | _ when n == 0 -> []
    | [] -> []
    | x::xs -> x::(take (n -1) xs)

  let renderCodeContext (src : string) startPos endPos =
    let open Lexing in
    let startCol = (startPos.pos_cnum - startPos.pos_bol) in
    let endCol = (endPos.pos_cnum - endPos.pos_bol) in
    let lines =
      String.split_on_char '\n' src
      |> drop startPos.pos_lnum
      |> take (endPos.pos_lnum - startPos.pos_lnum + 1)
    in
    match lines with
    | x::xs ->
      let d =
        let open TerminalDoc in
        let rowNr = highlight ~from:0 ~len:1 (string_of_int startPos.pos_lnum) in
        let len = if endCol >= 0 then
          endCol - startCol
        else
          1
        in
        group ~break:Never
          (append
            (append (text rowNr) (text " |"))
            (indent 2
              (text (highlight ~from:startCol ~len x))))
      in
      TerminalDoc.toString ~width:80 d
    | _ -> ""

  type problem =
    | Unexpected of Token.t
    | Expected of (Token.t * Lexing.position * Grammar.t option)
    | Message of string
    | Uident
    | Lident
    | Unbalanced of Token.t

  type parseError = Lexing.position * problem

end

module NapkinScript = struct
  let mkLoc startLoc endLoc = Location.{
    loc_start = startLoc;
    loc_end = endLoc;
    loc_ghost = false;
  }

  module Parser = struct
    type t = {
      mutable scanner: Scanner.t;
      mutable token: Token.t;
      mutable startPos: Lexing.position;
      mutable endPos: Lexing.position;
      mutable prevEndPos: Lexing.position;
      mutable breadcrumbs: (Grammar.t * Lexing.position) list;
      mutable errors: Reporting.parseError list;
      mutable diagnostics: Diagnostics.t list;
    }

    let err p error =
      let d = Diagnostics.make
        ~filename:p.scanner.filename
        ~startPos:p.startPos
        ~len:(p.endPos.pos_cnum - p.startPos.pos_cnum)
        error
      in
      p.diagnostics <- d::p.diagnostics

    let rec next p =
      let (startPos, endPos, token) = Scanner.scan p.scanner in
      p.prevEndPos <- p.endPos;
      p.token <- token;
      p.startPos <- startPos;
      p.endPos <- endPos;
      match p.token with
      | Comment _ -> next p
      | _ -> ()

    let make src filename =
      let scanner = Scanner.make (Bytes.of_string src) filename in
      let parserState = {
        scanner;
        token = Token.Eof;
        startPos = Lexing.dummy_pos;
        prevEndPos = Lexing.dummy_pos;
        endPos = Lexing.dummy_pos;
        breadcrumbs = [];
        errors = [];
        diagnostics = [];
      } in
      parserState.scanner.err <- (fun scanner error ->
        let diagnostic = Diagnostics.make
          ~filename:scanner.filename
          ~startPos:(Scanner.position scanner)
          ~len:(scanner.rdOffset - scanner.offset)
          error
        in
        parserState.diagnostics <- diagnostic::parserState.diagnostics
      );
      next parserState;
      parserState

    let restore p1 p2 =
      p1.scanner <- p2.scanner;
      p1.token <- p2.token;
      p1.startPos <- p2.startPos;
      p1.prevEndPos <- p2.prevEndPos;
      p1.endPos <- p2.endPos;
      p1.breadcrumbs <- p2.breadcrumbs;
      p1.errors <- p1.errors

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

    exception ParseError of Reporting.parseError

    (* ?circumstance indicates an optional circumstance
     * The current reasoning is to be able to say:
     *   "I'm expecting a ":", which signals the start of a type"
     * the "which signals the start of a type", can be deduced from the
     * circumstance (if there's one) *)
    let expectExn ?circumstance token p =
      if p.token = token then
        next p
      else
        raise (ParseError (p.startPos, Reporting.Expected (token, p.prevEndPos, circumstance)))

    let expect ?circumstance token p =
      if p.token = token then
        next p
      else
        let error = (p.startPos, Reporting.Expected (token, p.prevEndPos, circumstance)) in
        p.errors <- error::p.errors

    let lookahead p callback =
      (* is this copying correct? *)
      let scannerCopy = {p.scanner with filename = p.scanner.filename} in
      let parserStateCopy = {p with scanner = scannerCopy} in
      callback parserStateCopy
  end

  module Recover = struct
    let default_expr () =
      let id = Location.mknoloc "napkinscript.exprhole" in
      Ast_helper.Exp.mk (Pexp_extension (id, PStr []))

    let default_type () =
      let id = Location.mknoloc "napkinscript.typehole" in
      Ast_helper.Typ.extension (id, PStr [])

    let default_pattern () =
      let id = Location.mknoloc "napkinscript.patternhole" in
      Ast_helper.Pat.extension (id, PStr [])
      (* Ast_helper.Pat.any  () *)

    let default_module_expr () = Ast_helper.Mod.structure []
    let default_module_type () = Ast_helper.Mty.signature []


    let recoverAtomicExpr p token =
      (* Check grammar *)
      if not (Grammar.isStructureItemStart token) then Parser.next p;
      default_expr ()

    (* let recoverUident p = *)
      (* match p.Parser.token with *)
      (* | Lident lident -> *)
      (* | t when Token.isKeyword t -> *)
      (* | _ -> *)

    (* let recoverLident p = *)
      (* match p.Parser.token with *)
      (* | Lident lident -> *)
      (* | t when Token.isKeyword t -> *)
      (* | _ -> *)




  end

  let jsxAttr = (Location.mknoloc "JSX", Parsetree.PStr [])

  type typDefOrExt =
    | TypeDef of (Asttypes.rec_flag * Parsetree.type_declaration list)
    | TypeExt of Parsetree.type_extension

  type context =
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
      raise (Parser.ParseError (state.startPos, Reporting.Unbalanced closingToken))
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


  let isEs6ArrowFunctor p =
    Parser.lookahead p (fun state ->
      match state.Parser.token with
      (* | Uident _ | Underscore -> *)
        (* Parser.next state; *)
        (* begin match state.Parser.token with *)
        (* | EqualGreater -> true *)
        (* | _ -> false *)
        (* end *)
      | Lparen ->
        Parser.next state;
        begin match state.token with
        | Rparen ->
          Parser.next state;
          begin match state.token with
          | Colon | EqualGreater -> true
          | _ -> false
          end
        | _ ->
          goToClosing Rparen state;
          begin match state.Parser.token with
          | EqualGreater | Lbrace -> true
          | Colon -> true
          | _ -> false
          end
        end
      | _ -> false
    )

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


  (* {"foo": bar} -> Js.t({. foo: bar})
   * {.. "foo": bar} -> Js.t({.. foo: bar}) *)
  let makeBsObjType ?attrs ~loc ~closed rows =
    let obj = Ast_helper.Typ.object_ ~loc rows closed in
    let jsDotTCtor =
      Location.mkloc (Longident.Ldot (Longident.Lident "Js", "t")) loc
    in
    Ast_helper.Typ.constr ?attrs jsDotTCtor [obj]

  (* TODO: diagnostic reporting *)
  let lidentOfPath longident =
    match Longident.flatten longident |> List.rev with
    | [] -> ""
    | ident::_ -> ident

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
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Longident.Lident "_"
    in
    let ident = match p.Parser.token with
    | List -> Longident.Lident "list"
    | Lident ident -> Longident.Lident ident
    | Uident ident ->
      Parser.next p;
      Parser.expectExn Dot p;
      aux p (Lident ident)
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Longident.Lident "_"
    in
    Parser.next p;
    Location.mkloc ident (mkLoc startPos p.prevEndPos)

 let parseValuePathTail p startPos ident =
    let rec loop p path =
      match p.Parser.token with
      | Lident ident ->
        Parser.next p;
        Location.mkloc (Longident.Ldot(path, ident)) (mkLoc startPos p.prevEndPos)
      | Uident ident ->
        Parser.next p;
        Parser.expect Dot p;
        loop p (Longident.Ldot (path, ident))
      | t -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected t))
    in
    loop p ident

  let parseModuleLongIdentTail p startPos ident =
    let rec loop p acc =
      match p.Parser.token with
      | Uident ident ->
        Parser.next p;
        let endPos = p.prevEndPos in
        let lident = (Longident.Ldot (acc, ident)) in
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          loop p lident
        | _ -> Location.mkloc lident (mkLoc startPos endPos)
        end
      | _ ->
        Parser.err p Diagnostics.uident;
        Location.mkloc acc (mkLoc startPos p.prevEndPos)
    in
    loop p ident

  (* Parses module identifiers:
       Foo
       Foo.Bar *)
  let parseModuleLongIdent p =
    (* Parser.leaveBreadcrumb p Reporting.ModuleLongIdent; *)
    let startPos = p.Parser.startPos in
    let moduleIdent = match p.Parser.token with
    | Uident ident ->
      let lident = Longident.Lident ident in
      let endPos = p.endPos in
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        parseModuleLongIdentTail p startPos lident
      | _ -> Location.mkloc lident (mkLoc startPos endPos)
      end
    | _ ->
      Parser.err p Diagnostics.uident;
      Location.mkloc (Longident.Lident "_") (mkLoc startPos p.prevEndPos)
    in
    (* Parser.eatBreadcrumb p; *)
    moduleIdent

  let verifyJsxOpeningClosingName p nameExpr =
    let closing = match p.Parser.token with
    | Lident lident -> Parser.next p; Longident.Lident lident
    | Uident _ ->
      (parseModuleLongIdent p).txt
    | _ -> Longident.Lident ""
    in
    match nameExpr.Parsetree.pexp_desc with
    | Pexp_ident openingIdent ->
      let opening =
        let withoutCreateElement =
          Longident.flatten openingIdent.txt
          |> List.filter (fun s -> s <> "createElement")
        in
        match (Longident.unflatten withoutCreateElement) with
        | Some li -> li
        | None -> Longident.Lident ""
      in
      opening = closing
    | _ -> assert false

  let string_of_pexp_ident nameExpr =
    match nameExpr.Parsetree.pexp_desc with
    | Pexp_ident openingIdent ->
      Longident.flatten openingIdent.txt
      |> List.filter (fun s -> s <> "createElement")
      |> String.concat "."
    | _ -> ""

  (* open-def ::=
   *   | open module-path
   *   | open! module-path *)
  let parseOpenDescription ~attrs p =
    Parser.leaveBreadcrumb p Grammar.OpenDescription;
    let startPos = p.Parser.startPos in
    Parser.expect Open p;
    let override = if Parser.optional p Token.Bang then
      Asttypes.Override
    else
      Asttypes.Fresh
    in
    let modident = parseModuleLongIdent p in
    let loc = mkLoc startPos p.prevEndPos in
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
      raise (Parser.ParseError (p.startPos, Reporting.Message "I'm expecting a constant like: 1 or \"a string\""))
    in
    Parser.next p;
    constant

  let parseCommaDelimitedList p ~grammar ~closing ~f =
    let rec loop nodes =
      if Grammar.isListElement grammar p.Parser.token then (
        let node = f p in
        let hasComma = Parser.optional p Comma in
        if hasComma then
          if p.Parser.token = closing then
            List.rev (node::nodes)
          else
            loop (node::nodes)
        else (
          if p.token = closing || p.token = Eof then
            List.rev (node::nodes)
          else (
            Parser.expect Comma p;
            if p.token = Semicolon then Parser.next p;
            loop (node::nodes)
          )
        )
      ) else if p.token = closing || p.token = Eof then
        List.rev nodes
      else
        List.rev nodes
    in
    loop []

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
     (* ∣	 / pattern  { , pattern }+  /   *)
     (* ∣	 { field  [: typexpr]  [= pattern] { ; field  [: typexpr]  [= pattern] }  [; _ ] [ ; ] }   *)
     (* ∣	 [ pattern  { ; pattern }  [ ; ] ]   *)
     (* ∣	 pattern ::  pattern   *)
     (* ∣	 [| pattern  { ; pattern }  [ ; ] |]   *)
     (* ∣	 char-literal ..  char-literal *)
     (*	∣	 exception pattern  *)
  let rec parsePattern ?(alias=true) ?(or_=true) p =
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
        Parser.expect Token.Rparen p;
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
        parseConstructorPatternArgs p constr startPos attrs
      | _ ->
        Ast_helper.Pat.construct ~loc:constr.loc ~attrs constr None
      end
    (* is this safe in a let-context? let exception Foo
     * probably, local exceptions in seq expressions are
     * exception Foo *)
    | Exception ->
      Parser.next p;
      let pat = parsePattern ~alias:false ~or_:false p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Pat.exception_ ~loc ~attrs pat
    | Lazy ->
      Parser.next p;
      let pat = parsePattern ~alias:false ~or_:false p in
      let loc = mkLoc startPos pat.ppat_loc.loc_end in
      Ast_helper.Pat.lazy_ ~loc ~attrs pat
    | List ->
      parseListPattern ~attrs p
    | Percent ->
      let (loc, extension) = parseExtension p in
      Ast_helper.Pat.extension ~loc ~attrs extension
    | token ->
      Parser.err p(Diagnostics.unexpected token p.breadcrumbs);
      Recover.default_pattern()
    in
    let pat = if alias then parseAliasPattern ~attrs pat p else pat in
    if or_ then parseOrPattern pat p else pat

  (* alias ::= pattern as lident *)
  and parseAliasPattern ~attrs pattern p =
    match p.Parser.token with
    | As ->
      Parser.next p;
      begin match p.token with
      | Lident ident ->
        let identStart = p.startPos in
        Parser.next p;
        let loc = mkLoc identStart p.prevEndPos in
        Ast_helper.Pat.alias
          ~loc:({pattern.ppat_loc with loc_end = p.prevEndPos})
          ~attrs
           pattern
           (Location.mkloc ident loc)
      | _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
      end
    | _ -> pattern

  (* or ::= pattern | pattern *)
  and parseOrPattern pattern1  p =
    match p.Parser.token with
    | Bar ->
      Parser.next p;
      let pattern2 = parsePattern p in
      let loc = {
        pattern1.Parsetree.ppat_loc with loc_end = pattern2.ppat_loc.loc_end
      } in
      Ast_helper.Pat.or_ ~loc pattern1 pattern2
    | _ -> pattern1

  and parseConstrainedPattern p =
    let pat = parsePattern p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let typ = parseTypExpr p in
      let loc = mkLoc pat.ppat_loc.loc_start typ.Parsetree.ptyp_loc.loc_end in
      Ast_helper.Pat.constraint_ ~loc pat typ
    | _ -> pat

	(* field ::=
	 *   | longident
	 *   | longident : pattern
	 *   | longident as lident
   *
	 *  row ::=
	 *	 | field ,
	 *	 | field , _
	 *	 | field , _,
	 *)
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
		match p.token with
		| As ->
			Parser.next p;
			begin match p.token with
			| Lident ident ->
				let startPosIdent = p.startPos in
				Parser.next p;
				let locIdent = mkLoc startPosIdent p.prevEndPos in
				let aliasPattern =
					Ast_helper.Pat.alias
						~loc:(mkLoc startPos locIdent.loc_end)
						pattern
						(Location.mkloc ident locIdent)
				in
				(Location.mkloc label.txt (mkLoc startPos aliasPattern.ppat_loc.loc_end), aliasPattern)
			| _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
			end
		| _ ->
    (Location.mkloc label.txt (mkLoc startPos pattern.ppat_loc.loc_end), pattern)


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
      | token -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected token))
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
      parseCommaDelimitedList
        p ~grammar:Grammar.PatternList ~closing:Forwardslash ~f:parseConstrainedPattern
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
      parseCommaDelimitedList
        p ~grammar:Grammar.PatternList ~closing:Rbracket ~f:parseConstrainedPattern
    in
    Parser.expect Rbracket p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.array ~loc ~attrs patterns

  and parseConstructorPatternArgs p constr startPos attrs =
    let lparen = p.startPos in
    Parser.expectExn Lparen p;
    let args = match
      parseCommaDelimitedList
        p ~grammar:Grammar.PatternList ~closing:Rparen ~f:parseConstrainedPattern
    with
    | [pattern] -> Some pattern
    | patterns ->
        Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen p.prevEndPos) patterns)
    in
    Parser.expect Rparen p;
    Ast_helper.Pat.construct ~loc:(mkLoc startPos p.prevEndPos) ~attrs constr args

  and parseExpr ?(context=OrdinaryExpr) p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let expr = match p.token with
    | Assert ->
      Parser.next p;
      let expr = parseUnaryExpr p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.assert_ ~loc expr
    | Lazy ->
      Parser.next p;
      let expr = parseUnaryExpr p in
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
      else
        parseUnaryExpr p
    in
    let expr = {expr with Parsetree.pexp_attributes = expr.pexp_attributes @ attrs} in
    let expr = parseBinaryExpr ~a:expr p 1 in
    parseTernaryExpr expr p

  (* expr ? expr : expr *)
  and parseTernaryExpr leftOperand p =
    match p.Parser.token with
    | Question ->
      Parser.leaveBreadcrumb p Grammar.Ternary;
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

  and parseEs6ArrowExpression ?parameters p =
    let startPos = p.Parser.startPos in
    Parser.leaveBreadcrumb p Grammar.Es6ArrowExpr;
    let parameters = match parameters with
    | Some params -> params
    | None -> parseParameters p
    in
    let returnType = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parseTypExpr ~es6Arrow:false p)
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
    let endPos = p.prevEndPos in
    let arrowExpr = List.fold_right (fun (attrs, lbl, defaultExpr, pat, startPos) expr ->
      Ast_helper.Exp.fun_ ~loc:(mkLoc startPos endPos) ~attrs lbl defaultExpr pat expr
    ) parameters body
    in
    {arrowExpr with pexp_loc = {arrowExpr.pexp_loc with loc_start = startPos}}

	(*
	 * parameter ::=
	 *   | pattern
   *   | pattern : type
	 *   | ~ labelName
	 *   | ~ labelName as pattern
	 *   | ~ labelName as pattern : type
	 *   | ~ labelName = expr
	 *   | ~ labelName as pattern = expr
	 *   | ~ labelName as pattern : type = expr
	 *   | ~ labelName = ?
	 *   | ~ labelName as pattern = ?
	 *   | ~ labelName as pattern : type = ?
	 *
	 * labelName ::= lident
   *)
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
      | _ -> raise (Parser.ParseError (p.startPos, Reporting.Message "Function parameters labels should be lowercased like ~a "))
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
      | t -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected t))
      end
    | _ ->
      (Asttypes.Nolabel, parseConstrainedPattern p)
    in
    match p.Parser.token with
    | Equal ->
      Parser.next p;
			let lbl = match lbl with
			| Asttypes.Labelled lblName -> Asttypes.Optional lblName
			| Asttypes.Optional _ as lbl -> lbl
			| Asttypes.Nolabel -> Asttypes.Nolabel
			in
      begin match p.Parser.token with
      | Question ->
        Parser.next p;
        (attrs, lbl, None, pat, startPos)
      | _ ->
        let expr = parseExpr p in
        (attrs, lbl, Some expr, pat, startPos)
      end
    | _ ->
      (attrs, lbl, None, pat, startPos)

  and parseParameterList p =
    let parameters =
      parseCommaDelimitedList
        ~grammar:Grammar.ParameterList
        ~f:parseParameter
        ~closing:Rparen
        p
    in
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
        Ast_helper.Pat.var ~loc (Location.mkloc ident loc),
        startPos
      )]
    | Underscore ->
      Parser.next p;
      let loc = mkLoc startPos p.Parser.prevEndPos in
      [[], Asttypes.Nolabel, None, Ast_helper.Pat.any ~loc (), startPos]
    | Lparen ->
      Parser.next p;
      begin match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mkLoc startPos p.Parser.prevEndPos in
        let unitPattern = Ast_helper.Pat.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
        in
        [[], Asttypes.Nolabel, None, unitPattern, startPos]
      | _ -> parseParameterList p
      end
    | token -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected token))

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
    Parser.leaveBreadcrumb p Grammar.ExprOperand;
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
          Parser.expect Rparen p;
          {expr with pexp_loc = mkLoc startPos p.startPos}
        end
      | List ->
        parseListExpr p
      | Lbracket ->
        parseArrayExp p
      | Lbrace ->
        parseBracedOrRecordExpr p
      | Forwardslash ->
        parseTupleExpr p
      | LessThan ->
        parseJsx p
      | Percent ->
        let (loc, extension) = parseExtension p in
        Ast_helper.Exp.extension ~loc extension
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Recover.recoverAtomicExpr p token
    in
    Parser.eatBreadcrumb p;
    expr

  and parseBracketAccess p expr startPos =
    Parser.leaveBreadcrumb p Grammar.ExprArrayAccess;
    let lbracket = p.startPos in
    Parser.next p;
    match p.Parser.token with
    | String s ->
      Parser.next p;
      Parser.expect Rbracket p;
      let e = Ast_helper.Exp.apply
        (Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident "##")))
        [Nolabel, expr; Nolabel, (Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident s)))]
      in
      let e = parsePrimaryExpr ~operand:e p in
      begin match p.token with
      | Equal ->
        Parser.next p;
        let rhsExpr = parseExpr p in
        Ast_helper.Exp.apply
          (Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident "#=")))
          [Nolabel, e; Nolabel, rhsExpr]
      | _ -> e
      end
    | _ ->
      let accessExpr = parseConstrainedExpr p in
      Parser.expect Rbracket p;
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
        let e =
          Ast_helper.Exp.apply
            ~loc:(mkLoc startPos endPos)
            (Ast_helper.Exp.ident
              ~loc:arrayLoc
              (Location.mkloc (Longident.Ldot(Lident "Array", "get")) arrayLoc)
              )
            [Nolabel, expr; Nolabel, accessExpr]
        in
        Parser.eatBreadcrumb p;
        parsePrimaryExpr ~operand:e p
      end

  (* * A primary expression represents
   *  - atomic-expr
   *  - john.age
   *  - array[0]
   *  - applyFunctionTo(arg1, arg2)
   *
   *  The "operand" represents the expression that is operated on
   *)
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
        | Equal when noCall = false ->
          Parser.leaveBreadcrumb p Grammar.ExprSetField;
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
        parseBracketAccess p expr startPos
      | Lparen when noCall = false ->
        loop p (parseCallExpr p expr)
      | _ -> expr
    in
    let expr = loop p e1 in
    {expr with pexp_loc = mkLoc startPos p.prevEndPos}


  (* a unary expression is an expression with only one operand and
   * unary operator. Example:
   * -> -1
   * -> !condition
   * -> -. 1.6
   *)
  and parseUnaryExpr p =
    match p.Parser.token with
    | (Minus | MinusDot | Plus | PlusDot | Bang) as token ->
      Parser.leaveBreadcrumb p Grammar.ExprUnary;
      Parser.next p;
      let unaryExpr = makeUnaryExpr token (parseUnaryExpr p) in
      Parser.eatBreadcrumb p;
      unaryExpr
    | Band (* & *) ->
      Parser.leaveBreadcrumb p Grammar.ExprUnary;
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

  and parseAttributedUnaryExpr p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let expr = parseUnaryExpr p in
    let endPos = p.Parser.prevEndPos in
    {expr with
      pexp_attributes = expr.Parsetree.pexp_attributes @ attrs;
      pexp_loc = mkLoc startPos endPos}

  (* a binary expression is an expression that combines two expressions with an
   * operator. Examples:
   * -> a + b
   * -> f(x) |> g(y)
   *)
  and parseBinaryExpr ?a p prec =
    let a = match a with
    | Some e -> e
    | None -> parseAttributedUnaryExpr p
    in
    let rec loop a =
      let token = p.Parser.token in
      let tokenPrec = Token.precedence token in
      if tokenPrec < prec then a
      else begin
        Parser.leaveBreadcrumb p (Grammar.ExprBinaryAfterOp token);
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
        let expr = parseExprBlock p in
        Scanner.setTemplateMode p.scanner;
        Parser.expect Rbrace p;
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
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        acc
    in
    Scanner.setTemplateMode p.scanner;
    Parser.expectExn Backtick p;
    match p.Parser.token with
    | TemplateTail txt ->
      Parser.next p;
      Ast_helper.Exp.constant (Pconst_string(txt, None))
    | TemplatePart txt ->
      Parser.next p;
      let expr = parseExprBlock p in
      Scanner.setTemplateMode p.scanner;
      Parser.expect Rbrace p;
      let str = Ast_helper.Exp.constant (Pconst_string(txt, None)) in
      let next =
        if String.length txt > 0 then
          Ast_helper.Exp.apply hiddenOperator [Nolabel, str; Nolabel, expr]
        else
          expr
      in
      loop next p
   | token ->
     Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
     Ast_helper.Exp.constant (Pconst_string("", None))

  and parseLetBindingBody ~attrs p =
    Parser.leaveBreadcrumb p Grammar.LetBinding;
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
    Parser.expect Token.Equal p;
    let exp = parseExpr p in
    let loc = {pat.ppat_loc with
      loc_end = exp.Parsetree.pexp_loc.loc_end
    } in
    let vb = Ast_helper.Vb.mk ~loc ~attrs pat exp in
    Parser.eatBreadcrumb p;
    vb

  (* TODO: find a better way? Is it possible?
   * let a = 1
   * @attr
   * and b = 2
   *
   * The problem is that without semi we need a lookahead to determine
   * if the attr is on the letbinding or the start of a new thing
   *
   * let a = 1
   * @attr
   * let b = 1
   *
   * Here @attr should attach to something "new": `let b = 1`
   * The parser state is forked, which is quite expensive…
   *)
  and parseAttributesAndBinding p =
    Parser.lookahead p (fun state ->
      match state.Parser.token with
      | At ->
        let attrs = parseAttributes state in
        begin match state.Parser.token with
        | And ->
          Parser.restore p state;
          attrs
        | _ -> []
        end
      | _ -> []
    )

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
      let attrs = parseAttributesAndBinding p in
      match p.Parser.token with
      | And ->
        Parser.next p;
        ignore(Parser.optional p Let); (* overparse for fault tolerance *)
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
    let longident = match p.Parser.token with
    | Lident ident ->
      let identStart = p.startPos in
      let identEnd = p.endPos in
      Parser.next p;
      let loc = mkLoc identStart identEnd in
      Location.mkloc (Longident.Lident ident) loc
    | Uident _ ->
      let longident = parseModuleLongIdent p in
      Location.mkloc (Longident.Ldot (longident.txt, "createElement")) longident.loc
    | _ ->
      raise (Parser.ParseError (p.startPos,
      Reporting.Message "A jsx name should start with a lowercase or uppercase identifier, like: div in <div /> or Navbar in <Navbar />"))
    in
    Ast_helper.Exp.ident ~loc:longident.loc longident

  and parseJsxOpeningOrSelfClosingElement p =
    let jsxStartPos = p.Parser.startPos in
    let name = parseJsxName p in
    let jsxProps = parseJsxProps p in
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
      Scanner.setJsxMode p.scanner;
      Parser.next p;
      let (spread, children) = parseJsxChildren p in
      let childrenEndPos = p.Parser.startPos in
      let () = match p.token with
      | LessThanSlash -> Parser.next p
      | LessThan -> Parser.next p; Parser.expectExn Forwardslash p
      | _ -> Parser.expectExn LessThanSlash p
      in
      begin match p.Parser.token with
      | Lident _ | Uident _ when verifyJsxOpeningClosingName p name ->
        Parser.expectExn GreaterThan p;
        let loc = mkLoc childrenStartPos childrenEndPos in
        if spread then
          List.hd children
        else
          makeListExpression loc children None
      | _ ->
        let opening = "</" ^ (string_of_pexp_ident name) ^ ">" in
        let message = "Closing jsx name should be the same as the opening name. Did you mean " ^ opening ^ " ?" in
        raise (Parser.ParseError (p.startPos, Reporting.Message message))
      end
    | unknownToken -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected unknownToken))
    in
    let jsxEndPos = p.prevEndPos in
    let loc = mkLoc jsxStartPos jsxEndPos in
    Ast_helper.Exp.apply
      ~loc
      name
      (jsxProps @ [
        (Asttypes.Labelled "children", children);
        (Asttypes.Nolabel, Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None)
      ])

  (*
   *  jsx ::=
   *    | <> jsx-children </>
   *    | <element-name {jsx-prop} />
   *    | <element-name {jsx-prop}> jsx-children </element-name>
   *
   *  jsx-children ::= primary-expr*          * => 0 or more
   *)
  and parseJsx p =
    Parser.leaveBreadcrumb p Grammar.Jsx;
    Parser.expectExn LessThan p;
    let jsxExpr = match p.Parser.token with
    | Lident _ | Uident _ ->
      parseJsxOpeningOrSelfClosingElement p
    | GreaterThan -> (* fragment: <> foo </> *)
      parseJsxFragment p
    | _ ->
      parseJsxName p
    in
    {jsxExpr with pexp_attributes = [jsxAttr]}

  (*
   * jsx-fragment ::=
   *  | <> </>
   *  | <> jsx-children </>
   *)
  and parseJsxFragment p =
    let childrenStartPos = p.Parser.startPos in
    Scanner.setJsxMode p.scanner;
    Parser.expectExn GreaterThan p;
    let (_spread, children) = parseJsxChildren p in
    let childrenEndPos = p.Parser.startPos in
    Parser.expectExn LessThanSlash p;
    Parser.expectExn GreaterThan p;
    let loc = mkLoc childrenStartPos childrenEndPos in
    makeListExpression loc children None


  (*
   * jsx-prop ::=
   *   |  lident
   *   | ?lident
   *   |  lident =  jsx_expr
   *   |  lident = ?jsx_expr
   *)
  and parseJsxProp p =
    Parser.leaveBreadcrumb p Grammar.JsxAttribute;
    let optional = Parser.optional p Question in
    let name = match p.Parser.token with
    | Lident ident -> Parser.next p; ident
    | _ ->
      raise (Parser.ParseError (p.startPos, Reporting.Lident))
    in
    (* optional punning: <foo ?a /> *)
    if optional then
      (Asttypes.Optional name, Ast_helper.Exp.ident (Location.mknoloc
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

  and parseJsxProps p =
    let rec loop p props =
      match p.Parser.token with
      | Token.Eof | Forwardslash | GreaterThan -> List.rev props
      | _ ->
        let prop = parseJsxProp p in
        loop p (prop::props)
    in
    loop p []

  and parseJsxChildren p =
    let rec loop p children =
      match p.Parser.token  with
      | Token.Eof | LessThanSlash ->
        Scanner.popMode p.scanner Jsx;
        List.rev children
      | LessThan ->
        (* Imagine: <div> <Navbar /> <
         * is `<` the start of a jsx-child? <div …
         * or is it the start of a closing tag?  </div>
         * reconsiderLessThan peeks at the next token and
         * determines the correct token to disambiguate *)
        let token = Scanner.reconsiderLessThan p.scanner in
        if token = LessThan then
          let child = parsePrimaryExpr ~noCall:true p in
          loop p (child::children)
        else (* LessThanSlash *)
          let () = p.token <- token in
          let () = Scanner.popMode p.scanner Jsx in
          List.rev children
      | _ ->
        let child = parsePrimaryExpr ~noCall:true p in
        loop p (child::children)
    in
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      (true, [parsePrimaryExpr ~noCall:true p])
    | _ -> (false, loop p [])

  and parseBracedOrRecordExpr p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Lbrace p;
    let expr = match p.Parser.token with
    | DotDotDot ->
      (* beginning of record spread, parse record *)
      Parser.next p;
      let spreadExpr = parseConstrainedExpr p in
      Parser.expect Comma p;
      parseRecordExpr ~spread:(Some spreadExpr) [] p
    | String s ->
      let field =
        let loc = mkLoc p.startPos p.endPos in
        Parser.next p;
        Location.mkloc (Longident.Lident s) loc
      in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let fieldExpr = parseExpr p in
        Parser.optional p Comma |> ignore;
        parseRecordExprWithStringKeys (field, fieldExpr) p
      | _ ->
        let constant = Ast_helper.Exp.constant (Parsetree.Pconst_string(s, None)) in
        let a = parsePrimaryExpr ~operand:constant p in
        let e = parseBinaryExpr ~a p 1 in
        let e = parseTernaryExpr e p in
        begin match p.Parser.token with
        | Semicolon ->
          Parser.next p;
          parseExprBlock ~first:e p
        | Rbrace -> e
        | _ -> parseExprBlock ~first:e p
        end
      end
    | Uident _ | Lident _ ->
      let valueOrConstructor = parseValueOrConstructor p in
      begin match valueOrConstructor.pexp_desc with
      | Pexp_ident pathIdent ->
        let identEndPos = p.prevEndPos in
        begin match p.Parser.token with
        | Comma ->
          Parser.next p;
          parseRecordExpr [(pathIdent, Ast_helper.Exp.ident pathIdent)] p
        | Colon ->
          Parser.next p;
          let fieldExpr = parseExpr p in
          begin match p.token with
          | Rbrace ->
            Ast_helper.Exp.record [(pathIdent, fieldExpr)] None
          | _ ->
            Parser.expect Comma p;
            parseRecordExpr [(pathIdent, fieldExpr)] p
          end
        | Semicolon ->
          Parser.next p;
          parseExprBlock ~first:(Ast_helper.Exp.ident pathIdent) p
        | Rbrace ->
          Ast_helper.Exp.ident pathIdent
        | EqualGreater ->
            let loc = mkLoc startPos identEndPos in
            let ident = Location.mkloc (Longident.last pathIdent.txt) loc in
            let a = parseEs6ArrowExpression
              ~parameters:[
                ([], Asttypes.Nolabel, None, Ast_helper.Pat.var ident, startPos)
                ]
               p
            in
            let e = parseBinaryExpr ~a p 1 in
            let e = parseTernaryExpr e p in
            begin match p.Parser.token with
            | Semicolon ->
              Parser.next p;
              parseExprBlock ~first:e p
            | Rbrace -> e
            | _ -> parseExprBlock ~first:e p
            end
        | _ ->
          let a = parsePrimaryExpr ~operand:(Ast_helper.Exp.ident ~loc:pathIdent.loc pathIdent) p in
          let e = parseBinaryExpr ~a p 1 in
          let e = parseTernaryExpr e p in
          begin match p.Parser.token with
          | Semicolon ->
            Parser.next p;
            parseExprBlock ~first:e p
          | Rbrace -> e
          | _ -> parseExprBlock ~first:e p
          end
         end
      | _ ->
        let a = parsePrimaryExpr ~operand:valueOrConstructor p in
        let e = parseBinaryExpr ~a p 1 in
        let e = parseTernaryExpr e p in
        begin match p.Parser.token with
        | Semicolon ->
          Parser.next p;
          parseExprBlock ~first:e p
        | Rbrace -> e
        | _ -> parseExprBlock ~first:e p
        end
         end
    | _ ->
      parseExprBlock p
    in
    Parser.expectExn Rbrace p;
    {expr with pexp_loc = mkLoc startPos p.prevEndPos}

  and parseRecordRowWithStringKey p =
    let field = match p.Parser.token with
    | String s ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc (Longident.Lident s) loc
    | t -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected t))
    in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let fieldExpr = parseExpr p in
      (field, fieldExpr)
    | _ ->
      (field, Ast_helper.Exp.ident ~loc:field.loc field)

  and parseRecordRow p =
    let field = parseValuePath p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let fieldExpr = parseExpr p in
      (field, fieldExpr)
    | _ ->
      (field, Ast_helper.Exp.ident ~loc:field.loc  field)

  and parseRecordExprWithStringKeys firstRow p =
    let startPos = p.Parser.startPos in
    let rows = firstRow::(
      parseCommaDelimitedList ~grammar:Grammar.RecordRowsStringKey ~closing:Rbrace ~f:parseRecordRowWithStringKey p
    ) in
    let recordStrExpr =
      Ast_helper.Exp.record rows None
      |> Ast_helper.Str.eval
    in
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    Ast_helper.Exp.extension
      (Location.mkloc "bs.obj" loc, Parsetree.PStr [recordStrExpr])

  and parseRecordExpr ?(spread=None) rows p =
    let exprs =
      parseCommaDelimitedList ~grammar:Grammar.RecordRows ~closing:Rbrace ~f:parseRecordRow p
    in
    let rows = rows @ exprs in
    match rows with
    | [] ->
      raise (Parser.ParseError (p.prevEndPos, Reporting.Message "Record spread needs at least one field that's updated"))
    | rows ->
      Ast_helper.Exp.record rows spread

  and parseExprBlockItem p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Module ->
      Parser.next p;
      let name = match p.Parser.token with
      | Uident ident ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        Location.mkloc ident loc
      | _ -> raise (Parser.ParseError (p.startPos, Reporting.Uident))
      in
      let body = parseModuleBindingBody p in
      Parser.optional p Semicolon |> ignore;
      let expr = parseExprBlock p in
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in
      Ast_helper.Exp.letmodule ~loc name body expr
    | Exception ->
      let extensionConstructor = parseExceptionDef ~attrs:[] p in
      Parser.optional p Semicolon |> ignore;
      let blockExpr = parseExprBlock  p in
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in
      Ast_helper.Exp.letexception ~loc extensionConstructor blockExpr
    | Open ->
      let od = parseOpenDescription ~attrs:[] p in
      Parser.optional p Semicolon |> ignore;
      let blockExpr = parseExprBlock p in
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in
      Ast_helper.Exp.open_ ~loc od.popen_override od.popen_lid blockExpr
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
          parseExprBlock p
        | _ ->
          Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None
        end
      (* High danger, TODO check if we really can omit semi in these case*)
      | Bang | Band
      | True | False | Int _ | Float _ | String _ | Lident _ | Uident _
      | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
      | Lazy | If | For | While | Switch | Open | Module | Exception | Let
      | LessThan | Backtick ->
        parseExprBlock p
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
        let e2 = parseExprBlock p in
        Ast_helper.Exp.sequence e1 e2
      | _ -> e1
      end

  (* blockExpr ::= expr
   *            |  expr          ;
   *            |  expr          ; blockExpr
   *            |  module    ... ; blockExpr
   *            |  open      ... ; blockExpr
   *            |  exception ... ; blockExpr
   *            |  let       ...
   *            |  let       ... ;
   *            |  let       ... ; blockExpr
   *
   *  note: semi should be made optional
   *  a block of expression is always
   *)
  and parseExprBlock ?first p =
      Parser.leaveBreadcrumb p Grammar.ExprBlock;
      let item = match first with
      | Some e -> e
      | None -> parseExprBlockItem p
      in
      let blockExpr = match p.Parser.token with
      | Semicolon ->
        Parser.next p;
        begin match p.Parser.token with
        (* seq expr start *)
        | At | Minus | MinusDot | Plus | PlusDot | Bang | Band
        | True | False | Int _ | String _ | Lident _ | Uident _
        | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
        | Lazy | If | For | While | Switch | Open | Module | Exception | Let
        | LessThan | Backtick ->
          let next = parseExprBlockItem p in
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
            let next = parseExprBlockItem p in
            ignore(Parser.optional p Semicolon);
            Ast_helper.Exp.sequence item next
          | _ -> item
          end
      | _ ->
        item
      in
      Parser.eatBreadcrumb p;
      blockExpr

  and parseIfExpression p =
    Parser.leaveBreadcrumb p Grammar.ExprIf;
    let startPos = p.Parser.startPos in
    Parser.expectExn If p;
    Parser.leaveBreadcrumb p Grammar.IfCondition;
    (* doesn't make sense to try es6 arrow here? *)
    let conditionExpr = parseExpr ~context:WhenExpr p in
    Parser.eatBreadcrumb p;
    Parser.leaveBreadcrumb p IfBranch;
    Parser.expectExn Lbrace p;
    let thenExpr = parseExprBlock p in
    Parser.expectExn Rbrace p;
    Parser.eatBreadcrumb p;
    let elseExpr = match p.Parser.token with
    | Else ->
      Parser.leaveBreadcrumb p Grammar.ElseBranch;
      Parser.next p;
      Parser.expectExn  Lbrace p;
      let elseExpr = parseExprBlock p in
      Parser.expectExn Rbrace p;
      Parser.eatBreadcrumb p;
      Some elseExpr
    | _ ->
      None
    in
    let loc = mkLoc startPos p.prevEndPos in
    Parser.eatBreadcrumb p;
    Ast_helper.Exp.ifthenelse ~loc conditionExpr thenExpr elseExpr

  and parseForRest hasOpeningParen pattern startPos p =
    Parser.expectExn In p;
    let e1 = parseExpr p in
    let direction = match p.Parser.token with
    | To -> Asttypes.Upto
    | Downto -> Asttypes.Downto
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Asttypes.Upto
    in
    Parser.next p;
    let e2 = parseExpr p in
    if hasOpeningParen then Parser.expectExn Rparen p;
    Parser.expectExn Lbrace p;
    let bodyExpr = parseExprBlock p in
    Parser.expectExn Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.for_ ~loc pattern e1 e2 direction bodyExpr

  and parseForExpression p =
    let startPos = p.Parser.startPos in
    Parser.expectExn For p;
		match p.token with
		| Lparen ->
			Parser.next p;
			let lparen = p.prevEndPos in
			begin match p.token with
			| Rparen ->
				Parser.next p;
				let unitPattern =
					let loc = mkLoc lparen p.prevEndPos in
					let lid = Location.mkloc (Longident.Lident "()") loc in
					Ast_helper.Pat.construct lid None
				in
        parseForRest false (parseAliasPattern ~attrs:[] unitPattern p) startPos p
      | Let ->
        let (recFlag, letBindings) = parseLetBindings ~attrs:[] p in
        Parser.expect Semicolon p;
        let condition = parseExpr p in
        Parser.expect Semicolon p;
        let after = parseExpr p in
        Parser.expect Rparen p;
        Parser.expectExn Lbrace p;
        let block = parseExprBlock p in
        Parser.expectExn Rbrace p;
        let while_ = Ast_helper.Exp.while_ condition (
          Ast_helper.Exp.sequence block after
        ) in
        Ast_helper.Exp.let_ recFlag letBindings while_
			| _ ->
        parseForRest true (parsePattern p) startPos p
			end
		| _ ->
      parseForRest false (parsePattern p) startPos p


  and parseWhileExpression p =
    let startPos = p.Parser.startPos in
    Parser.expectExn While p;
    let expr1 = parseExpr ~context:WhenExpr p in
    Parser.expectExn Lbrace p;
    let expr2 = parseExprBlock p in
    Parser.expectExn Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.while_ ~loc expr1 expr2

  and parsePatternMatchCase p =
    Parser.leaveBreadcrumb p Grammar.PatternMatchCase;
    Parser.expectExn Bar p;
    let lhs = parsePattern p in
    let guard = match p.Parser.token with
    | When ->
      Parser.next p;
      Some (parseExpr ~context:WhenExpr p)
    | _ ->
      None
    in
    Parser.expect EqualGreater p;
    let rhs = parseExprBlock p in
    Parser.eatBreadcrumb p;
    Ast_helper.Exp.case lhs ?guard rhs

  and parsePatternMatching p =
    Parser.leaveBreadcrumb p Grammar.PatternMatching;
    (* '{' consumed *)
    let rec loop p cases =
      match p.Parser.token with
      | Rbrace ->
        List.rev cases
      | _ ->
        let case = parsePatternMatchCase p in
        loop p (case::cases)
    in
    let cases = loop p [] in
    Parser.eatBreadcrumb p;
    cases

  and parseSwitchExpression p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Switch p;
    let switchExpr = parseExpr ~context:WhenExpr p in
    Parser.expect Lbrace p;
    let cases = parsePatternMatching p in
    Parser.expect Rbrace p;
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
      | _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
      end
    | _ -> (Nolabel, parseConstrainedExpr p)

  and parseCallExpr p funExpr =
    Parser.expectExn Lparen p;
    let startPos = p.Parser.startPos in
    Parser.leaveBreadcrumb p Grammar.ExprCall;
    let args =
      parseCommaDelimitedList
        ~grammar:Grammar.ArgumentList
        ~closing:Rparen
        ~f:parseArgument p
    in
    Parser.expect Rparen p;
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
      | token ->
        Parser.next p;
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Recover.default_expr()
        (* Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident "_")) *)
          (* raise (Parser.ParseError (p.startPos, Reporting.Unexpected token)) *)
    in
    aux p []

  and parseConstructorArgs p =
    let lparen = p.Parser.startPos in
    Parser.expectExn Lparen p;
    let args =
      parseCommaDelimitedList
        ~grammar:Grammar.ExprList ~f:parseConstrainedExpr ~closing:Rparen p
    in
    Parser.expect Rparen p;
    match args with
    | [] ->
      let loc = mkLoc lparen p.prevEndPos in
      [Ast_helper.Exp.construct
        ~loc (Location.mkloc (Longident.Lident "()") loc) None]
    | args -> args

  and parseTupleExpr p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Forwardslash p;
    Scanner.setTupleMode p.scanner;
    let exprs =
      parseCommaDelimitedList
        p ~grammar:Grammar.ExprList ~closing:TupleEnding ~f:parseConstrainedExpr
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
      parseCommaDelimitedList
        p ~grammar:Grammar.ExprList ~closing:Rbracket ~f:parseConstrainedExpr
    in
    Parser.expect Rbracket p;
    Ast_helper.Exp.array ~loc:(mkLoc startPos p.prevEndPos) exprs

  (* TODO: check attributes in the case of poly type vars,
   * might be context dependend: parseFieldDeclaration (see ocaml) *)
  and parsePolyTypeExpr p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | SingleQuote ->
      let vars = parseTypeVarList p in
      begin match vars with
      | _v1::_v2::_ ->
        Parser.expectExn Dot p;
        let typ = parseTypExpr p in
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Typ.poly ~loc vars typ
      | [var] ->
        begin match p.Parser.token with
        | Dot ->
          Parser.next p;
          let typ = parseTypExpr p in
          let loc = mkLoc startPos p.prevEndPos in
          Ast_helper.Typ.poly ~loc vars typ
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
        | _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
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
      | _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
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
        let args = parseTypeConstructorArgs p in
        Ast_helper.Typ.constr ~loc:(mkLoc startPos p.prevEndPos) ~attrs constr args
      | Lparen ->
        raise (Parser.ParseError (p.startPos, Reporting.Message "Type constructor args require diamonds, like: Belt.Map.String.t<int>"))
      | _ ->
        Ast_helper.Typ.constr ~loc:constr.loc ~attrs constr []
      end
    | Percent ->
      let (loc, extension) = parseExtension p in
      Ast_helper.Typ.extension ~loc extension
    | Lbrace ->
      parseBsObjectType p
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.default_type()
    in
    typ

  and parseBsObjectType p =
    let startPos = p.Parser.startPos in
    Parser.expect Lbrace p;
    let objectType = match p.Parser.token with
    | DotDot ->
      Parser.next p;
      let closedFlag = Asttypes.Open in
      let fields =
        parseCommaDelimitedList
          ~grammar:Grammar.StringFieldDeclarations
          ~closing:Rbrace
          ~f:parseStringFieldDeclaration
          p
      in
      Parser.expect Rbrace p;
      let loc = mkLoc startPos p.prevEndPos in
      makeBsObjType ~loc ~closed:closedFlag fields
    | _ ->
      let closedFlag = Asttypes.Closed in
      let fields =
        parseCommaDelimitedList
          ~grammar:Grammar.StringFieldDeclarations
          ~closing:Rbrace
          ~f:parseStringFieldDeclaration
          p
      in
      Parser.expect Rbrace p;
      let loc = mkLoc startPos p.prevEndPos in
      makeBsObjType ~loc ~closed:closedFlag fields
    in
    objectType

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
      | _ ->
          raise (Parser.ParseError (p.startPos, Reporting.Lident))
      end
    | _ -> typ


  (* type_parameter ::=
    *  | type_expr
    *  | ~ident: type_expr
    *  | ~ident: type_expr=?
    *)
  and parseTypeParameter p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let name = match p.Parser.token with
      | Lident ident -> Parser.next p; ident
      | _ ->
        raise (Parser.ParseError (
          p.startPos,
          Reporting.Lident
        ))
      in
      Parser.expectExn Colon p;
      let typ = parseTypExpr p in
      begin match p.Parser.token with
      | Equal ->
        Parser.next p;
        Parser.expectExn Question p;
        (attrs, Asttypes.Optional name, typ, startPos)
      | _ ->
        (attrs, Asttypes.Labelled name, typ, startPos)
      end
    | _ ->
      (attrs, Asttypes.Nolabel, parseTypExpr p, startPos)

  (* (int, ~x:string, float) *)
  and parseTypeParameters p =
    Parser.expectExn Lparen p;
    let params =
      parseCommaDelimitedList ~grammar:Grammar.TypeParameters ~closing:Rparen ~f:parseTypeParameter p
    in
    Parser.expect Rparen p;
    params

  and parseEs6ArrowType p =
    let parameters = parseTypeParameters p in
    Parser.expectExn EqualGreater p;
    let returnType = parseTypExpr ~alias:false p in
    let endPos = p.prevEndPos in
    List.fold_right (fun (attrs, argLbl, typ, startPos) t ->
      Ast_helper.Typ.arrow ~loc:(mkLoc startPos endPos) ~attrs argLbl typ t
    ) parameters returnType

  (*
   * typexpr ::=
   *  | 'ident
   *  | _
   *  | (typexpr)
   *  | typexpr => typexpr            --> es6 arrow
   *  | (typexpr, typexpr) => typexpr --> es6 arrow
   *  | /typexpr, typexpr, typexpr/  --> tuple
   *  | typeconstr
   *  | typeconstr<typexpr>
   *  | typeconstr<typexpr, typexpr,>
   *  | typexpr as 'ident
   *  | %attr-id                      --> extension
   *  | %attr-id(payload)             --> extension
   *
   * typeconstr ::=
   *  | lident
   *  | uident.lident
   *  | uident.uident.lident     --> long module path
   *)
  and parseTypExpr ?(es6Arrow=true) ?(alias=true) p =
    Parser.leaveBreadcrumb p Grammar.TypeExpression;
    let attrs = parseAttributes p in
    let typ = if es6Arrow && isEs6ArrowType p then
      parseEs6ArrowType p
    else
      let typ = parseAtomicTypExpr ~attrs p in
      match p.Parser.token with
      | EqualGreater when es6Arrow == true ->
        Parser.next p;
        let returnType = parseTypExpr ~alias:false p in
        let loc = mkLoc typ.Parsetree.ptyp_loc.loc_start p.prevEndPos in
        Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ returnType
      | _ -> typ
    in
    let typ = if alias then parseTypeAlias p typ else typ in
    Parser.eatBreadcrumb p;
    typ

  and parseTupleType p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Forwardslash p;
    let types =
      parseCommaDelimitedList ~grammar:Grammar.TypExprList ~closing:Forwardslash ~f:parseTypExpr p
    in
    Parser.expect Forwardslash p;
    Ast_helper.Typ.tuple ~loc:(mkLoc startPos p.prevEndPos) types

  and parseTypeConstructorArgs p =
		Scanner.setDiamondMode p.scanner;
		Parser.expectExn LessThan p;
		let typeArgs =
      parseCommaDelimitedList ~grammar:Grammar.TypExprList ~closing:GreaterThan ~f:parseTypExpr p
		in
		Parser.expect GreaterThan p;
		Scanner.popMode p.scanner Diamond;
		typeArgs

  and parseConstructorTypeArgs p =
		Scanner.setDiamondMode p.Parser.scanner;
		Parser.expectExn LessThan p;
		let typeArgs =
      parseCommaDelimitedList ~grammar:Grammar.TypExprList ~closing:GreaterThan ~f:parseTypExpr p
		in
		Parser.expect GreaterThan p;
		Scanner.popMode p.scanner Diamond;
		typeArgs

  (* string-field-decl ::=
   *  | string: poly-typexpr
   *  | attributes string-field-decl *)
  and parseStringFieldDeclaration p =
    let attrs = parseAttributes p in
    let fieldName = match p.Parser.token with
    | String name ->
      let nameStartPos = p.startPos in
      let nameEndPos = p.endPos in
      Parser.next p;
      Location.mkloc name (mkLoc nameStartPos nameEndPos)
    | t -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected t))
    in
    Parser.expect Colon p;
    let typ = parsePolyTypeExpr p in
    Parsetree.Otag (fieldName, attrs, typ)

  (* field-decl	::=
   *  | [mutable] field-name : poly-typexpr
   *  | attributes field-decl *)
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
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
    in
    let typ = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parsePolyTypeExpr p
    | _ ->
      Ast_helper.Typ.constr {name with txt = Lident name.txt} []
    in
    let loc = mkLoc startPos typ.ptyp_loc.loc_end in
    Ast_helper.Type.field ~attrs ~loc ~mut name typ

  (* record-decl ::=
   *  | { field-decl }
   *  | { field-decl, field-decl }
   *  | { field-decl, field-decl, field-decl, }
   *)
  and parseRecordDeclaration p =
    Parser.leaveBreadcrumb p Grammar.RecordDecl;
    Parser.expectExn Lbrace p;
    let rows =
      parseCommaDelimitedList
        ~grammar:Grammar.RecordDecl
        ~closing:Rbrace
        ~f:parseFieldDeclaration
        p
    in
    Parser.expect Rbrace p;
    Parser.eatBreadcrumb p;
    rows

  (* constr-args ::=
   *  | (typexpr)
   *  | (typexpr, typexpr)
   *  | (typexpr, typexpr, typexpr,)
   *  | (record-decl)
   *
   * TODO: should we overparse inline-records in every position?
   * Give a good error message afterwards?
   *)
  and parseConstrDeclArgs p =
    let constrArgs = match p.Parser.token with
    | Lparen ->
      Parser.next p;
      (* TODO: this could use some cleanup/stratification *)
      begin match p.Parser.token with
      | Lbrace ->
        Parser.next p;
        let startPos = p.Parser.startPos in
        begin match p.Parser.token with
        | DotDot ->
          Parser.next p;
          let closedFlag = Asttypes.Open in
          let fields =
            parseCommaDelimitedList
              ~grammar:Grammar.StringFieldDeclarations
              ~closing:Rbrace
              ~f:parseStringFieldDeclaration
              p
          in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let typ = makeBsObjType ~loc ~closed:closedFlag fields in
          Parser.optional p Comma |> ignore;
          let moreArgs =
            parseCommaDelimitedList
            ~grammar:Grammar.TypExprList
            ~closing:Rparen
            ~f:parseTypExpr
            p
          in
          Parser.expect Rparen p;
          Parsetree.Pcstr_tuple (typ::moreArgs)
        | _ ->
          let attrs = parseAttributes p in
          begin match p.Parser.token with
          | String _  ->
            let closedFlag = Asttypes.Closed in
            let fields = match attrs with
            | [] ->
              parseCommaDelimitedList
                ~grammar:Grammar.StringFieldDeclarations
                ~closing:Rbrace
                ~f:parseStringFieldDeclaration
                p
            | attrs ->
              let first =
                let field = parseStringFieldDeclaration p in
                Parser.optional p Comma |> ignore;
                begin match field with
                | Parsetree.Otag (label, _, ct) -> Parsetree.Otag (label, attrs, ct)
                | Oinherit ct -> Oinherit ct
                end
              in
              first::(
                parseCommaDelimitedList
                  ~grammar:Grammar.StringFieldDeclarations
                  ~closing:Rbrace
                  ~f:parseStringFieldDeclaration
                  p
              ) in
              Parser.expect Rbrace p;
              let loc = mkLoc startPos p.prevEndPos in
              let typ = makeBsObjType ~loc ~closed:closedFlag fields in
              Parser.optional p Comma |> ignore;
              let moreArgs =
                parseCommaDelimitedList
                  ~grammar:Grammar.TypExprList
                  ~closing:Rparen
                  ~f:parseTypExpr p
              in
              Parser.expect Rparen p;
              Parsetree.Pcstr_tuple (typ::moreArgs)
            | _ ->
              let fields = match attrs with
              | [] ->
                parseCommaDelimitedList
                  ~grammar:Grammar.FieldDeclarations
                  ~closing:Rbrace
                  ~f:parseFieldDeclaration
                  p
              | attrs ->
                let first =
                  let field = parseFieldDeclaration p in
                  Parser.expect Comma p;
                  {field with Parsetree.pld_attributes = attrs}
                in
                first::(
                  parseCommaDelimitedList
                    ~grammar:Grammar.FieldDeclarations
                    ~closing:Rbrace
                    ~f:parseFieldDeclaration
                    p
                )
              in
              Parser.expect Rbrace p;
              Parser.optional p Comma |> ignore;
              Parser.expectExn Rparen p;
              Parsetree.Pcstr_record fields
            end
        end
        | _ ->
          let args =
            parseCommaDelimitedList
              ~grammar:Grammar.TypExprList
              ~closing:Rparen
              ~f:parseTypExpr
              p
          in
          Parser.expect Rparen p;
          Parsetree.Pcstr_tuple args
       end
    | _ -> Pcstr_tuple []
    in
    let res = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parseTypExpr p)
    | _ -> None
    in
    (constrArgs, res)

  (* constr-decl ::=
   *  | constr-name
   *  | attrs constr-name
   *  | constr-name const-args
   *  | attrs constr-name const-args *)
   and parseTypeConstructorDeclaration p =
     Parser.leaveBreadcrumb p Grammar.ConstructorDeclaration;
     let attrs = parseAttributes p in
     match p.Parser.token with
     | Uident uident ->
       Parser.next p;
       let (args, res) = parseConstrDeclArgs p in
       Parser.eatBreadcrumb p;
       Ast_helper.Type.constructor ~attrs ?res ~args (Location.mknoloc uident)
     | _ -> raise (Parser.ParseError (p.startPos, Reporting.Uident))

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
   *  ∣	 = [ | ] constr-decl  { | constr-decl }
   *  ∣	 = private [ | ] constr-decl  { | constr-decl }
   *  |  = |
   *  ∣	 = private |
   *  ∣	 = record-decl
   *  ∣	 = private record-decl
   *  |  = ..
   *)
  and parseTypeRepresentation p =
    Parser.leaveBreadcrumb p Grammar.TypeRepresentation;
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
    | unknownToken -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected unknownToken))
    in
    Parser.eatBreadcrumb p;
    (privateFlag, kind)

  (* type-param	::=
   *  | variance 'lident
   *  | variance _
   *
   * variance ::=
   *   | +
   *   | -
   *   | (* empty *)
   *)
  and parseTypeParam p =
    Parser.leaveBreadcrumb p Grammar.TypeParam;
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
        let startPos = p.startPos in
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        (Ast_helper.Typ.var ~loc ident, variance)
      | _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
      end
    | Underscore ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      (Ast_helper.Typ.any ~loc (), variance)
    | token -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected token))
    in
    Parser.eatBreadcrumb p;
    param

  (* type-params	::=
   *  | <type-param>
 	 *  ∣	<type-param, type-param>
 	 *  ∣	<type-param, type-param, type-param>
 	 *  ∣	<type-param, type-param, type-param,> *)
  and parseTypeParams p =
    match p.Parser.token with
    | LessThan ->
      Parser.leaveBreadcrumb p Grammar.TypeParams;
      Parser.next p;
      let params =
        parseCommaDelimitedList
          ~grammar:Grammar.TypeParams
          ~closing:GreaterThan
          ~f:parseTypeParam
          p
      in
      Parser.expect GreaterThan p;
      Parser.eatBreadcrumb p;
      params
    | Lparen when p.startPos.pos_lnum == p.prevEndPos.pos_lnum ->
      raise (Parser.ParseError (p.startPos, Reporting.Message
        "Type parameters start with diamonds, example: type foo<'a>"
      ))
    | _ -> []

  (* type-constraint	::=	constraint ' ident =  typexpr *)
  and parseTypeConstraint p =
    Parser.expectExn SingleQuote p;
    begin match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      Parser.expect Equal p;
      let typ = parseTypExpr p in
      (Ast_helper.Typ.var ident, typ, Location.none)
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
    end

  (* type-constraints ::=
   *  | (* empty *)
   *  | type-constraint
   *  | type-constraint type-constraint
   *  | type-constraint type-constraint type-constraint (* 0 or more *)
   *)
  and parseTypeConstraints p =
    let rec loop p constraints =
      match p.Parser.token with
      | Constraint ->
        Parser.next p;
        let constraint_ = parseTypeConstraint p in
        loop p (constraint_::constraints)
      | _ ->
        List.rev constraints
    in
    loop p []

  and parseTypeEquationOrConstrDecl p =
    let uidentStartPos = p.Parser.startPos in
    match p.Parser.token with
    | Uident uident ->
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        let typeConstr =
          parseValuePathTail p uidentStartPos (Longident.Lident uident)
        in
        let typ = parseTypeAlias p (match p.Parser.token with
        | LessThan ->
          let args = parseTypeConstructorArgs p in
          Ast_helper.Typ.constr typeConstr args
        | _ ->
          Ast_helper.Typ.constr typeConstr []
        ) in
        begin match p.token with
        | Equal ->
          Parser.next p;
          let (priv, kind) = parseTypeRepresentation p in
          (Some typ, priv, kind)
        | _ -> (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
        end
      | _ ->
        let uidentEndPos = p.endPos in
        let (args, res) = parseConstrDeclArgs p in
        let first = Some (
          let uidentLoc = mkLoc uidentStartPos uidentEndPos in
          Ast_helper.Type.constructor
            ~loc:(mkLoc uidentStartPos p.prevEndPos)
            ?res
            ~args
            (Location.mkloc uident uidentLoc)
        ) in
        (None, Asttypes.Public, Parsetree.Ptype_variant (parseTypeConstructorDeclarations p ?first))
      end
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Uident))

  and parseRecordOrBsObjectDecl p =
    let startPos = p.Parser.startPos in
    Parser.expect Lbrace p;
    match p.Parser.token with
    | DotDot ->
      Parser.next p;
      let closedFlag = Asttypes.Open in
      let fields =
        parseCommaDelimitedList
          ~grammar:Grammar.StringFieldDeclarations
          ~closing:Rbrace
          ~f:parseStringFieldDeclaration
          p
      in
      Parser.expectExn Rbrace p;
      let loc = mkLoc startPos p.prevEndPos in
      let typ =
        makeBsObjType ~loc ~closed:closedFlag fields
        |> parseTypeAlias p
      in
      (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
    | _ ->
      let attrs = parseAttributes p in
      begin match p.Parser.token with
      | String _  ->
        let closedFlag = Asttypes.Closed in
        let fields = match attrs with
        | [] ->
          parseCommaDelimitedList
            ~grammar:Grammar.StringFieldDeclarations
            ~closing:Rbrace
            ~f:parseStringFieldDeclaration
            p
        | attrs ->
          let first =
            let field = parseStringFieldDeclaration p in
            Parser.optional p Comma |> ignore;
            begin match field with
            | Parsetree.Otag (label, _, ct) -> Parsetree.Otag (label, attrs, ct)
            | Oinherit ct -> Oinherit ct
            end
          in
          first::(
            parseCommaDelimitedList
              ~grammar:Grammar.StringFieldDeclarations
              ~closing:Rbrace
              ~f:parseStringFieldDeclaration
              p
          )
          in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let typ =
            makeBsObjType ~loc ~closed:closedFlag fields
            |> parseTypeAlias p
          in
          (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
      | _ ->
        Parser.leaveBreadcrumb p Grammar.RecordDecl;
        let fields = match attrs with
        | [] ->
          parseCommaDelimitedList
            ~grammar:Grammar.FieldDeclarations
            ~closing:Rbrace
            ~f:parseFieldDeclaration
            p
        | attrs ->
          let first =
            let field = parseFieldDeclaration p in
            Parser.optional p Comma |> ignore;
            {field with Parsetree.pld_attributes = attrs}
          in
          first::(
            parseCommaDelimitedList
              ~grammar:Grammar.FieldDeclarations
              ~closing:Rbrace
              ~f:parseFieldDeclaration
              p
          )
        in
        Parser.expect Rbrace p;
        Parser.eatBreadcrumb p;
        (None, Asttypes.Public, Parsetree.Ptype_record fields)
      end

  and parseTypeEquationAndRepresentation p =
    match p.Parser.token with
    | Equal ->
      Parser.next p;
      begin match p.Parser.token with
      | Uident _ ->
        parseTypeEquationOrConstrDecl p
      | Lbrace ->
        parseRecordOrBsObjectDecl p
      | Bar | Private | DotDot ->
        let (priv, kind) = parseTypeRepresentation p in
        (None, priv, kind)
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

  (* type-definition	::=	type [rec] typedef  { and typedef }
   * typedef	::=	typeconstr-name [type-params] type-information
   * type-information	::=	[type-equation]  [type-representation]  { type-constraint }
   * type-equation	::=	= typexpr *)
  and parseTypeDef ?attrs p =
    Parser.leaveBreadcrumb p Grammar.TypeDef;
    let startPos = p.Parser.startPos in
    let attrs = match attrs with | Some attrs -> attrs | None -> parseAttributes p in
    Parser.leaveBreadcrumb p Grammar.TypeConstrName;
    let typeConstrName = match p.Parser.token with
    | Lident ident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      (Location.mkloc ident loc)
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
    in
    Parser.eatBreadcrumb p;
    Parser.leaveBreadcrumb p Grammar.TypeParams;
    let params = parseTypeParams p in
    Parser.eatBreadcrumb p;
    let typeDef =
      let (manifest, priv, kind) = parseTypeEquationAndRepresentation p in
      let cstrs = parseTypeConstraints p in
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in
      Ast_helper.Type.mk
        ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest typeConstrName
    in
    Parser.eatBreadcrumb p;
    typeDef

  and parseTypeExtension ~params ~attrs ~name p =
    Parser.expect PlusEqual p;
    let priv =
      if Parser.optional p Token.Private
      then Asttypes.Private
      else Asttypes.Public
    in
    Parser.optional p Bar |> ignore;
    let first =
      let (attrs, name, kind) = match p.Parser.token with
      | Bar ->
        Parser.next p;
        parseConstrDef ~parseAttrs:true p
      | _ ->
        parseConstrDef ~parseAttrs:true p
      in
      Ast_helper.Te.constructor ~attrs name kind
    in
    let rec loop p cs =
      match p.Parser.token with
      | Bar ->
        let startPos = p.Parser.startPos in
        Parser.next p;
        let (attrs, name, kind) = parseConstrDef ~parseAttrs:true p in
        let extConstr =
          Ast_helper.Te.constructor ~attrs ~loc:(mkLoc startPos p.prevEndPos) name kind
        in
        loop p (extConstr::cs)
      | _ ->
        List.rev cs
    in
    let constructors = loop p [first] in
    Ast_helper.Te.mk ~attrs ~params ~priv name constructors

  and parseTypeDefinitions ~attrs ~name ~params ~startPos p =
      let typeDef =
        let (manifest, priv, kind) = parseTypeEquationAndRepresentation p in
        let cstrs = parseTypeConstraints p in
        let endPos = p.prevEndPos in
        let loc = mkLoc startPos endPos in
        Ast_helper.Type.mk
          ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest
          {name with txt = lidentOfPath name.Location.txt}
      in
      let rec loop p defs =
        let attrs = parseAttributesAndBinding p in
        match p.Parser.token with
        | And ->
          Parser.next p;
          let typeDef = parseTypeDef ~attrs p in
          loop p (typeDef::defs)
        | _ ->
          List.rev defs
      in
      loop p [typeDef]

  (* TODO: decide if we really want type extensions (eg. type x += Blue)
   * It adds quite a bit of complexity that can be avoided,
   * implemented for now. Needed to get a feel for the complexities of
   * this territory of the grammar *)
  and parseTypeDefinitionOrExtension ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Token.Typ p;
    let recFlag =
      if Parser.optional p Token.Rec
        then Asttypes.Recursive
        else Asttypes.Nonrecursive
    in
    let name = parseValuePath p in
    let params = parseTypeParams p in
    match p.Parser.token with
    | PlusEqual ->
      TypeExt(parseTypeExtension ~params ~attrs ~name p)
    | _ ->
      let typeDefs = parseTypeDefinitions ~attrs ~name ~params ~startPos p in
      TypeDef(recFlag, typeDefs)

  and parsePrimitive p =
    let rec loop p prims =
      match p.Parser.token with
      | String s ->
        Parser.next p;
        loop p (s::prims)
      | _ ->
        begin match prims with
        | [] -> raise (Parser.ParseError (p.startPos, Reporting.Message "An external definition should have at least one primitive. Example: \"setTimeout\""))
        | prims -> List.rev prims
        end
    in
    loop p []

  (* external value-name : typexp = external-declaration *)
  and parseExternalDef ~attrs p =
    Parser.leaveBreadcrumb p Grammar.External;
    let startPos = p.Parser.startPos in
    Parser.expectExn Token.External p;
    let name = match p.Parser.token with
    | Lident ident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc ident loc
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
    in
    Parser.expectExn ~circumstance:(Grammar.TypeExpression) Colon p;
    let typExpr = parseTypExpr p in
    Parser.expect Equal p;
    let prim = parsePrimitive p in
    let loc = mkLoc startPos p.prevEndPos in
    let vb = Ast_helper.Val.mk ~loc ~attrs ~prim name typExpr in
    Parser.eatBreadcrumb p;
    vb

  (* constr-def ::=
   *  | constr-decl
   *  | constr-name = constr
   *
   *  constr-decl ::= constr-name constr-args
   *  constr-name ::= uident
   *  constr      ::= path-uident *)
  and parseConstrDef ~parseAttrs p =
    let attrs = if parseAttrs then parseAttributes p else [] in
    let name = match p.Parser.token with
    | Uident name ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc name loc
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Uident))
    in
    let kind = match p.Parser.token with
    | Lparen ->
      let (args, res) = parseConstrDeclArgs p in
      Parsetree.Pext_decl (args, res)
    | Equal ->
      Parser.next p;
      let longident = parseModuleLongIdent p in
      Parsetree.Pext_rebind longident
    | _ ->
      Parsetree.Pext_decl (Pcstr_tuple [], None)
    in
    (attrs, name, kind)

  (*
   * exception-definition	::=
   *  | exception constr-decl
   *  ∣	exception constr-name = constr
   *
   *  constr-name ::= uident
   *  constr ::= long_uident *)
  and parseExceptionDef ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Token.Exception p;
    let (_, name, kind) = parseConstrDef ~parseAttrs:false p in
    let loc = mkLoc startPos p.prevEndPos in
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
      begin match parseTypeDefinitionOrExtension ~attrs p with
      | TypeDef(recFlag, types) ->
        Ast_helper.Str.type_ recFlag types
      | TypeExt(ext) ->
        Ast_helper.Str.type_extension ext
      end
    | External ->
      Ast_helper.Str.primitive (parseExternalDef ~attrs p)
    | Exception ->
      Ast_helper.Str.exception_ (parseExceptionDef ~attrs p)
    | Include ->
      Ast_helper.Str.include_ (parseIncludeStatement ~attrs p)
    | Module -> parseModuleOrModuleTypeImpl ~attrs p
    | AtAt ->
      let (loc, attr) = parseStandaloneAttribute p in
      Ast_helper.Str.attribute ~loc attr
    | PercentPercent ->
      let (loc, extension) = parseExtension ~moduleLanguage: true p in
      Ast_helper.Str.extension ~attrs ~loc extension
    | _ ->
      Ast_helper.Str.eval ~attrs (parseExpr p)
    in
    let loc = mkLoc startPos p.prevEndPos in
    {item with pstr_loc = loc}

  (* include-statement ::= include module-expr *)
  and parseIncludeStatement ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Token.Include p;
    let modExpr = parseModuleExpr p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Incl.mk ~loc ~attrs modExpr

  and parseAtomicModuleExpr p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Uident ident ->
      let longident = parseModuleLongIdent p in
      Ast_helper.Mod.ident ~loc:longident.loc longident
    | Lbrace ->
      Parser.next p;
      let structure = Ast_helper.Mod.structure (parseStructure p) in
      Parser.expectExn Rbrace p;
      let endPos = p.prevEndPos in
      {structure with pmod_loc = mkLoc startPos endPos}
    | Lparen ->
      Parser.next p;
      let modExpr = parseConstrainedModExpr p in
      Parser.expect Rparen p;
      modExpr
    | Percent ->
      let (loc, extension) = parseExtension p in
      Ast_helper.Mod.extension ~loc extension
    | unknownToken -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected unknownToken))

  and parsePrimaryModExpr p =
    let startPos = p.Parser.startPos in
    let modExpr = parseAtomicModuleExpr p in
    let rec loop p modExpr =
      match p.Parser.token with
      | Lparen ->
        loop p (parseModuleApplication p modExpr)
      | _ -> modExpr
    in
    let modExpr = loop p modExpr in
    {modExpr with pmod_loc = mkLoc startPos p.prevEndPos}

  and parseFunctorArgName p =
    let startPos = p.Parser.startPos in
    let ident = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      ident
    | Underscore ->
      Parser.next p;
      "_"
    | _ -> raise (Parser.ParseError (p.startPos,
      Reporting.Message "a functor arg name should be module name or _"
      ))
    in
    Location.mkloc ident (mkLoc startPos p.prevEndPos)

  (*
   * functor-arg ::=
   *  | uident : modtype
   *  | _ : modtype
   *  | modtype           --> "punning" for _ : modtype
   *  | attributes functor-arg
   *)
  and parseFunctorArg p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      let uidentEndPos = p.prevEndPos in
      begin match p.Parser.token with
      | Colon ->
        Parser.next p;
        let moduleType = parseModuleType p in
        let loc = mkLoc startPos uidentEndPos in
        let argName = Location.mkloc ident loc in
        (attrs, argName, Some moduleType, startPos)
      | Dot ->
        Parser.next p;
        let moduleType =
          let moduleLongIdent =
            parseModuleLongIdentTail p startPos (Longident.Lident ident) in
          Ast_helper.Mty.ident ~loc:moduleLongIdent.loc moduleLongIdent
        in
        let argName = Location.mknoloc "_" in
        (attrs, argName, Some moduleType, startPos)
      | _ ->
        let loc = mkLoc startPos uidentEndPos in
        let modIdent = Location.mkloc (Longident.Lident ident) loc in
        let moduleType = Ast_helper.Mty.ident ~loc modIdent in
        let argName = Location.mknoloc "_" in
        (attrs, argName, Some moduleType, startPos)
      end
    | Underscore ->
      Parser.next p;
      let argName = Location.mkloc "_" (mkLoc startPos p.prevEndPos) in
      Parser.expectExn Colon p;
      let moduleType = parseModuleType p in
      (attrs, argName, Some moduleType, startPos)
    | _ ->
      let moduleType = parseModuleType p in
      let argName = Location.mknoloc "_" in
      (attrs, argName, Some moduleType, startPos)

  and parseFunctorArgs p =
    let startPos = p.Parser.startPos in
    Parser.expect Lparen p;
    let args =
      parseCommaDelimitedList
        ~grammar:Grammar.FunctorArgs
        ~closing:Rparen
        ~f:parseFunctorArg
        p
    in
    Parser.expect Rparen p;
    match args with
    | [] ->
      [[], Location.mkloc "*" (mkLoc startPos p.prevEndPos), None, startPos]
    | args -> args

  and parseFunctorModuleExpr p =
    let args = parseFunctorArgs p in
    let returnType = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parseModuleType ~es6Arrow:false p)
    | _ -> None
    in
    Parser.expectExn EqualGreater p;
    let rhsModuleExpr =
      let modExpr = parseModuleExpr p in
      match returnType with
      | Some modType ->
        Ast_helper.Mod.constraint_
          ~loc:(mkLoc modExpr.pmod_loc.loc_start modType.Parsetree.pmty_loc.loc_end)
          modExpr modType
      | None -> modExpr
    in
    let endPos = p.prevEndPos in
    List.fold_right (fun (attrs, name, moduleType, startPos) acc ->
      Ast_helper.Mod.functor_
        ~loc:(mkLoc startPos endPos)
        ~attrs
        name moduleType acc
    ) args rhsModuleExpr

  (* module-expr	::=
   *  | module-path
   *  ∣	{ structure-items }
   *  ∣	functorArgs =>  module-expr
   *  ∣	module-expr(module-expr)
   *  ∣	( module-expr )
   *  ∣	( module-expr : module-type )
   *  | extension
   *  | attributes module-expr *)
  and parseModuleExpr ?(attrs=[]) p =
    let attrs = parseAttributes p in
    let modExpr = if isEs6ArrowFunctor p then
        parseFunctorModuleExpr p
      else
        parsePrimaryModExpr p
    in
    {modExpr with pmod_attributes = modExpr.pmod_attributes @ attrs}

  and parseConstrainedModExpr p =
    let modExpr = parseModuleExpr p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let modType = parseModuleType p in
      let loc = mkLoc modExpr.pmod_loc.loc_start modType.pmty_loc.loc_end in
      Ast_helper.Mod.constraint_ ~loc modExpr modType
    | _ -> modExpr

  and parseModuleApplication p modExpr =
    let startPos = p.Parser.startPos in
    Parser.expectExn Lparen p;
    let args =
      parseCommaDelimitedList ~grammar:Grammar.ModExprList ~closing:Rparen ~f:parseConstrainedModExpr p
    in
    Parser.expectExn Rparen p;
    let args = match args with
    | [] ->
      let loc = mkLoc startPos p.prevEndPos in
      [Ast_helper.Mod.structure ~loc []]
    | args -> args
    in
    List.fold_left (fun modExpr arg ->
      Ast_helper.Mod.apply
        ~loc:(mkLoc modExpr.Parsetree.pmod_loc.loc_start arg.Parsetree.pmod_loc.loc_end)
        modExpr arg
    ) modExpr args

  and parseModuleOrModuleTypeImpl ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Module p;
    match p.Parser.token with
    | Typ -> parseModuleTypeImpl ~attrs startPos p
    | _ -> parseMaybeRecModuleBinding ~attrs p

  and parseModuleTypeImpl ~attrs startPos p =
    Parser.expectExn Typ p;
    let nameStart = p.Parser.startPos in
    let name = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      let loc = mkLoc nameStart p.prevEndPos in
      Location.mkloc ident loc
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Uident))
    in
    Parser.expectExn Equal p;
    let moduleType = parseModuleType p in
    let moduleTypeDeclaration =
      Ast_helper.Mtd.mk
        ~attrs
        ~loc:(mkLoc nameStart p.prevEndPos)
        ~typ:moduleType
        name
    in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Str.modtype ~loc moduleTypeDeclaration

  (* definition	::=
    ∣	 module rec module-name :  module-type =  module-expr   { and module-name
    :  module-type =  module-expr } *)
  and parseMaybeRecModuleBinding ~attrs p =
    if Parser.optional p Token.Rec then
      Ast_helper.Str.rec_module (parseModuleBindings ~attrs p)
    else
      Ast_helper.Str.module_ (parseModuleBinding ~attrs p)

  and parseModuleBinding ~attrs p =
    let startPos = p.Parser.startPos in
    let name = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      Location.mkloc ident loc
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Uident))
    in
    let body = parseModuleBindingBody p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Mb.mk ~attrs ~loc name body

  and parseModuleBindingBody p =
    (* TODO: make required with good error message when rec module binding *)
    let returnModType = match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parseModuleType p)
    | _ -> None
    in
    Parser.expect Equal p;
    let modExpr = parseModuleExpr p in
    match returnModType with
    | Some modType ->
      Ast_helper.Mod.constraint_
        ~loc:(mkLoc modExpr.pmod_loc.loc_start modType.Parsetree.pmty_loc.loc_end)
        modExpr modType
    | None -> modExpr


  (* module-name :  module-type =  module-expr
   * { and module-name :  module-type =  module-expr } *)
  and parseModuleBindings ~attrs p =
    let rec loop p acc =
      let attrs = parseAttributesAndBinding p in
      match p.Parser.token with
      | And ->
        Parser.next p;
        ignore(Parser.optional p Module); (* over-parse for fault-tolerance *)
        let modBinding = parseModuleBinding ~attrs p in
        loop p (modBinding::acc)
      | _ -> List.rev acc
    in
    let first = parseModuleBinding ~attrs p in
    loop p [first]

  and parseAtomicModuleType p =
    let startPos = p.Parser.startPos in
    let moduleType = match p.Parser.token with
    | Uident _ ->
      (* Ocaml allows module types to end with lowercase: module Foo : bar = { ... }
       * lets go with uppercase terminal for now *)
      let moduleLongIdent = parseModuleLongIdent p in
      Ast_helper.Mty.ident ~loc:moduleLongIdent.loc moduleLongIdent
    | Lparen ->
      Parser.next p;
      let mty = parseModuleType p in
      Parser.expectExn Rparen p;
      {mty with pmty_loc = mkLoc startPos p.prevEndPos}
    | Lbrace ->
      Parser.next p;
      let spec = parseSpecification p in
      Parser.expectExn Rbrace p;
      spec
    | Module -> (* TODO: check if this is still atomic when implementing first class modules*)
      parseModuleTypeOf p
    | Percent ->
      let (loc, extension) = parseExtension p in
      Ast_helper.Mty.extension ~loc extension
    | token -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected token))
    in
    let moduleTypeLoc = mkLoc startPos p.prevEndPos in
    {moduleType with pmty_loc = moduleTypeLoc}

  and parseFunctorModuleType p =
    let args = parseFunctorArgs p in
    Parser.expect EqualGreater p;
    let rhs = parseModuleType p in
    let endPos = p.prevEndPos in
    List.fold_right (fun (attrs, name, moduleType, startPos) acc ->
      Ast_helper.Mty.functor_
        ~loc:(mkLoc startPos endPos)
        ~attrs
        name moduleType acc
    ) args rhs

  (* Module types are the module-level equivalent of type expressions: they
   * specify the general shape and type properties of modules.
   *
   * module-type ::=
   *  | modtype-path
   *  | { signature }
   *  | ( module-type )               --> parenthesized module-type
   *  | functor-args => module-type   --> functor
   *  | module-type => module-type    --> functor
   *  | module type of module-expr
   *  | attributes module-type
   *  | module-type with-mod-constraints
   *  | extension
   *)
   and parseModuleType ?(es6Arrow=true) ?(with_=true) p =
    let attrs = parseAttributes p in
    let modty = if es6Arrow && isEs6ArrowFunctor p then
      parseFunctorModuleType p
    else
      let modty = parseAtomicModuleType p in
      match p.Parser.token with
      | EqualGreater when es6Arrow == true ->
        Parser.next p;
        let rhs = parseModuleType ~with_:false p in
        let str = Location.mknoloc "_" in
        let loc = mkLoc modty.pmty_loc.loc_start p.prevEndPos in
        Ast_helper.Mty.functor_ ~loc str (Some modty) rhs
      | _ -> modty
    in
    let moduleType = { modty with
      pmty_attributes = modty.pmty_attributes @ attrs
    } in
    if with_ then
      parseWithConstraints moduleType p
    else moduleType


  and parseWithConstraints moduleType p =
    match p.Parser.token with
    | With ->
      Parser.next p;
      let first = parseWithConstraint p in
      let rec loop p acc =
        match p.Parser.token with
        | And ->
          Parser.next p;
          loop p ((parseWithConstraint p)::acc)
        | _ ->
          List.rev acc
      in
      let constraints = loop p [first] in
      let loc = mkLoc moduleType.pmty_loc.loc_start p.prevEndPos in
      Ast_helper.Mty.with_ ~loc moduleType constraints
    | _ ->
      moduleType

  (* mod-constraint	::=
   *  |  type typeconstr<type-params> type-equation type-constraints?
   *  ∣	 type typeconstr-name<type-params> := typexpr
   *  ∣	 module module-path = extended-module-path
   *  ∣	 module module-path :=  extended-module-path *)
  and parseWithConstraint p =
    match p.Parser.token with
    | Module ->
      Parser.next p;
      let modulePath = parseModuleLongIdent p in
      begin match p.Parser.token with
      | ColonEqual ->
        Parser.next p;
        let lident = parseModuleLongIdent p in
        Parsetree.Pwith_modsubst (modulePath, lident)
      | Equal ->
        Parser.next p;
        let lident = parseModuleLongIdent p in
        Parsetree.Pwith_module (modulePath, lident)
      | t -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected t))
      end
    | Typ ->
      Parser.next p;
      let typeConstr = parseValuePath p in
      let params = parseTypeParams p in
      begin match p.Parser.token with
      | ColonEqual ->
        Parser.next p;
        let typExpr = parseTypExpr p in
        Parsetree.Pwith_typesubst (
          typeConstr,
          Ast_helper.Type.mk
            ~loc:typeConstr.loc
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
            ~loc:typeConstr.loc
            ~params
            ~manifest:typExpr
            ~cstrs:typeConstraints
            (Location.mkloc (Longident.last typeConstr.txt) typeConstr.loc)
        )
      | t -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected t))
      end
    | token -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected token))

  and parseModuleTypeOf p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Module p;
    Parser.expectExn Typ p;
    Parser.expectExn Of p;
    let moduleExpr = parseModuleExpr p in
    Ast_helper.Mty.typeof_ ~loc:(mkLoc startPos p.prevEndPos) moduleExpr

  and parseSpecification p =
    (* { consumed *)
    let rec loop p spec =
      let item = parseSignatureItem p in
      let () = match p.Parser.token with
      | Semicolon -> Parser.next p
      | Let | Typ | At | AtAt | PercentPercent | External | Exception | Open | Include | Module | Rbrace | Eof -> ()
      | _ -> Parser.expectExn Semicolon p
      in
      let spec = (item::spec) in
      match p.Parser.token with
      | Rbrace | Eof -> spec
      | _ -> loop p spec
    in
    Ast_helper.Mty.signature (List.rev (loop p []))

  and parseSignature p =
    let rec loop p spec =
      if p.Parser.token = Eof then
        spec
      else
        let item = parseSignatureItem p in
        let () = match p.Parser.token with
        | Semicolon -> Parser.next p
        | Let | Typ | At | AtAt | PercentPercent | External | Exception | Open | Include | Module | Rbrace | Eof -> ()
        | _ -> Parser.expectExn Semicolon p
        in
        let spec = (item::spec) in
        match p.Parser.token with
        | Rbrace | Eof -> spec
        | _ -> loop p spec
    in
    List.rev (loop p [])

  and parseSignatureItem p =
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Let ->
      parseSignLetDesc ~attrs p
    | Typ ->
      begin match parseTypeDefinitionOrExtension ~attrs p with
      | TypeDef(recFlag, types) ->
        Ast_helper.Sig.type_ recFlag types
      | TypeExt(ext) ->
        Ast_helper.Sig.type_extension ext
      end
    | External ->
      Ast_helper.Sig.value (parseExternalDef ~attrs p)
    | Exception ->
      Ast_helper.Sig.exception_ (parseExceptionDef ~attrs p)
    | Open ->
      Ast_helper.Sig.open_ (parseOpenDescription ~attrs p)
    | Include ->
      Parser.next p;
      let moduleType = parseModuleType p in
      let includeDescription = Ast_helper.Incl.mk ~attrs moduleType in
      Ast_helper.Sig.include_ includeDescription
    | Module ->
      Parser.next p;
      begin match p.Parser.token with
      | Uident _ ->
        parseModuleDeclarationOrAlias ~attrs p
      | Rec ->
        Ast_helper.Sig.rec_module (
          parseRecModuleSpec ~attrs p
        )
      | Typ ->
        parseModuleTypeDeclaration ~attrs p
      | _ -> raise (Parser.ParseError (p.startPos, Reporting.Uident))
      end
    | AtAt ->
      let (loc, attr) = parseStandaloneAttribute p in
      Ast_helper.Sig.attribute ~loc attr
    | PercentPercent ->
      let (loc, extension) = parseExtension ~moduleLanguage:true p in
      Ast_helper.Sig.extension ~attrs ~loc extension
    | token -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected token))

  (* module rec module-name :  module-type  { and module-name:  module-type } *)
  and parseRecModuleSpec ~attrs p =
    Parser.expect Rec p;
    let rec loop p spec =
      let attrs = parseAttributesAndBinding p in
      match p.Parser.token with
      | And ->
        (* TODO: give a good error message when with constraint, no parens
         * and ASet: (Set.S with type elt = A.t)
         * and BTree: (Btree.S with type elt = A.t)
         * Without parens, the `and` signals the start of another
         * `with-constraint`
         *)
        Parser.expect And p;
        let decl = parseRecModuleDeclaration ~attrs p in
        loop p (decl::spec)
      | _ ->
        List.rev spec
    in
    let first = parseRecModuleDeclaration ~attrs p in
    loop p [first]

  (* module-name : module-type *)
  and parseRecModuleDeclaration ~attrs p =
    let name = match p.Parser.token with
    | Uident modName ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc modName loc
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Uident))
    in
    Parser.expect Colon p;
    let modType = parseModuleType p in
    Ast_helper.Md.mk ~attrs name modType

  and parseModuleDeclarationOrAlias ~attrs p =
    let moduleName = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Uident))
    in
    let body = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parseModuleType p
    | Equal ->
      Parser.next p;
      let lident = parseModuleLongIdent p in
      Ast_helper.Mty.alias lident
    | t -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected t))
    in
    Ast_helper.Sig.module_ (Ast_helper.Md.mk ~attrs moduleName body)

  and parseModuleTypeDeclaration ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expectExn Typ p;
    (* We diverge from ocaml here by requiring uident instead of ident *)
    let moduleName = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Uident))
    in
    let typ = match p.Parser.token with
    | Equal ->
      Parser.next p;
      Some (parseModuleType p)
    | _ -> None
    in
    let moduleDecl = Ast_helper.Mtd.mk ~attrs ?typ moduleName in
    Ast_helper.Sig.modtype ~loc:(mkLoc startPos p.prevEndPos) moduleDecl

  and parseSignLetDesc ~attrs p =
    Parser.expectExn Let p;
    let name = match p.Parser.token with
    | Lident ident ->
      let nameStartPos = p.startPos in
      Parser.next p;
      Location.mkloc ident (mkLoc nameStartPos p.prevEndPos)
    | _ -> raise (Parser.ParseError (p.startPos, Reporting.Lident))
    in
    Parser.expect Colon p;
    let typExpr = parsePolyTypeExpr p in
    let valueDesc = Ast_helper.Val.mk ~attrs name typExpr in
    Ast_helper.Sig.value valueDesc

(*    attr-id	::=	lowercase-ident
 	∣	  capitalized-ident
 	∣	  attr-id .  attr-id   *)
  and parseAttributeId p =
    let startPos = p.Parser.startPos in
    let rec loop p acc =
      match p.Parser.token with
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
      | token -> raise (Parser.ParseError (p.startPos, Reporting.Unexpected token))
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

  (*
   * standalone-attribute ::=
   *  | @@ atribute-id
   *  | @@ attribute-id ( structure-item )
   *)
  and parseStandaloneAttribute p =
    let startPos = p.Parser.startPos in
    Parser.expectExn AtAt p;
    let attrId = parseAttributeId p in
    let cnumEndAttrId = p.Parser.prevEndPos.pos_cnum - 1 in
    let payload = parsePayload cnumEndAttrId p in
    let attribute = (attrId, payload) in
    let loc = mkLoc startPos p.prevEndPos in
    (loc, attribute)

  (* extension	::=	% attr-id  attr-payload
   *              | %% attr-id(
   *  expr	::=	 ...
   *    ∣	 extension
   *
   *  typexpr	::=	 ...
   *    ∣	 extension
   *
   *  pattern	::=	 ...
   *    ∣	 extension
   *
   *  module-expr	::=	 ...
   *    ∣	 extension
   *
   *  module-type	::=	 ...
   *    ∣	 extension
   *
   *  class-expr	::=	 ...
   *    ∣	 extension
   *
   *  class-type	::=	 ...
   *    ∣	 extension
   *
   *
   * item extension nodes usable in structures and signature
   *
   * item-extension ::= %% attr-id
   *                  | %% attr-id(structure-item)
   *
   *  attr-payload ::= structure-item
   *
   *  ~moduleLanguage represents whether we're on the module level or not
   *)
  and parseExtension ?(moduleLanguage=false) p =
    let startPos = p.Parser.startPos in
    if moduleLanguage then
      Parser.expectExn PercentPercent p
    else
      Parser.expectExn Percent p;
    let attrId = parseAttributeId p in
    let cnumEndAttrId = p.Parser.prevEndPos.pos_cnum - 1 in
    let payload = parsePayload cnumEndAttrId p in
    let loc = mkLoc startPos p.prevEndPos in
    (loc, (attrId, payload))

  let () =
    (* let startTime = Unix.gettimeofday () in *)
    let filename = Sys.argv.(1) in
    let src = IO.readFile filename in
    let p = Parser.make src filename in
    try
      let () =
        (* poor mans version of determining interface files
         * does it end with 'i' ? :D
         * TODO: rewrite (maybe?) *)
        let len = String.length filename in
        if len > 0 && String.get filename (len - 1) = 'i' then (
          let ast = parseSignature p in
          (* Uncomment for release *)
          (* let ast402 = To_402.copy_signature ast in *)
          (* Ast_io.to_channel stdout filename *)
            (* (Ast_io.Intf ((module OCaml_402), ast402)) *)

          Pprintast.signature Format.std_formatter ast
        ) else
          let ast = parseStructure p in
          (* Uncomment for release *)
          (* let ast402 = To_402.copy_structure ast in *)
          (* Ast_io.to_channel stdout filename *)
            (* (Ast_io.Impl ((module OCaml_402), ast402)) *)

          Pprintast.structure Format.std_formatter ast
      in
      Format.pp_print_flush Format.std_formatter ();
      print_newline();
      (* Printast.implementation Format.std_formatter ast; *)
      (* Format.pp_print_flush Format.std_formatter (); *)
      (* print_newline(); *)
      List.iter (fun (pos, problem) ->
        Printf.eprintf "Parse error: %s\n" (p.scanner.filename);
        Printf.eprintf "Line: %d, Column: %d\n" (pos.Lexing.pos_lnum) (pos.pos_cnum - pos.pos_bol + 1);
        Printf.eprintf "%s\n"
        (match problem with
        | Reporting.Expected (t,_, _) ->
          "Missing " ^ Token.toString t
        | _ -> "Todo: pretty print parse error")
      ) p.errors;

      (* List.iter Diagnostics.print p.diagnostics; *)
      (* let endTime = Unix.gettimeofday () in *)
      (* let diff = (endTime -. startTime) *. 1000. in *)
      (* Printf.eprintf "Execution time: %fms\n%!" diff; *)
      exit 0
    with
    | Parser.ParseError (pos, problem) ->
      Printf.eprintf "\nParse error: %s, line: %d, col: %d\n" (p.scanner.filename) (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol + 1);
      Printf.eprintf "Problem encountered while trying to parse %s.\n" (Reporting.parseContext p.breadcrumbs);
      let ctx = Reporting.renderCodeContext (Bytes.to_string p.scanner.src) pos p.endPos in
      Printf.eprintf "\n%s\n\n" ctx;
      Printf.eprintf "%s\n\n"
      (match problem with
      | Reporting.Uident ->
        begin match p.Parser.token with
        | Lident lident ->
          let guess = String.capitalize_ascii lident in
          "Did you mean `" ^ guess ^"` instead of `" ^ lident ^ "`?"
        | t when Token.isKeyword t ->
          let token = Token.toString t in
          "`" ^ token ^ "` is a reserved keyword. Try `" ^ token ^ "_` or `_" ^ token ^ "` instead"
        | _ ->
          "At this point, I'm looking for an uppercased identifier like `Belt` or `Array`"
        end
      | Reporting.Lident ->
        begin match p.Parser.token with
        | Uident uident ->
          let guess = String.uncapitalize_ascii uident in
          "Did you mean `" ^ guess ^"` instead of `" ^ uident ^ "`?"
        | t when Token.isKeyword t ->
          let token = Token.toString t in
          "`" ^ token ^ "` is a reserved keyword. Try `" ^ token ^ "_` or `_" ^ token ^ "` instead"
        | _ ->
          "I'm expecting an lowercased identifier like `name` or `age`"
        end
      | Message msg -> msg
      | Unexpected t ->
          let name = (Token.toString t) in
          begin match p.breadcrumbs with
          | (ExprOperand, _)::breadcrumbs ->
              begin match breadcrumbs, p.token with
              | (ExprBlock, _) :: _, Rbrace ->
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
          | (TypeParam, _)::_ ->
              begin match t with
              | Lident ident ->
                "Did you mean '" ^ ident ^"? A Type parameter starts with a quote."
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
      | Expected (t, _, circumstance) ->
          "I'm expecting a \"" ^ (Token.toString t) ^ "\" here."
          ^ (match circumstance with
          | Some c -> "It signals the start of " ^ (Grammar.toString c)
          | None -> "")
      | Unbalanced t ->
          "Closing \"" ^ (Token.toString t) ^ "\" seems to be missing."
      );
      exit 1
end
