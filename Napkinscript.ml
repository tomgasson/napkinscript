module Doc = struct
  type mode = Break | Flat

  type lineStyle =
    | Classic (* fits? -> replace with space *)
    | Soft (* fits? -> replaced with nothing *)
    | Hard (* always included, forces breaks in parents *)
    | Literal (* always included, no identation *)


  type t =
    | Nil
    | Text of string
    | Concat of t list
    | Indent of t
    | IfBreaks of t * t
    | LineSuffix of t
    | LineBreak of lineStyle
    | Group of (bool (* should break *) * t)
    (* | Cursor *)

  let nil = Nil
  let line = LineBreak Classic
  let hardLine = LineBreak Hard
  let softLine = LineBreak Soft
  let text s = Text s
  let concat l = Concat l
  let indent d = Indent d
  let ifBreaks t f = IfBreaks(t, f)
  let lineSuffix d = LineSuffix d
  let group d = Group(false, d)
  let breakableGroup ~forceBreak d = Group(forceBreak, d)
  (* let cursor = Cursor *)

  let propagateForcedBreaks doc =
    let rec walk doc = match doc with
    | Text _ | Nil | LineSuffix _ ->
      (false, doc)
    | LineBreak (Hard | Literal) ->
      (true, doc)
    | LineBreak (Classic | Soft) ->
      (false, doc)
    | Indent children ->
      let (childForcesBreak, newChildren) = walk children in
      (childForcesBreak, Indent newChildren)
    | IfBreaks (trueDoc, falseDoc) ->
      (false, doc)
    | Group(forceBreak, children) ->
      let (childForcesBreak, newChildren) = walk children in
      let shouldBreak = forceBreak || childForcesBreak in
      (shouldBreak, Group (shouldBreak, newChildren))
    | Concat children ->
      let (forceBreak, newChildren) = List.fold_left (fun (forceBreak, newChildren) child ->
        let (childForcesBreak, newChild) = walk child in
        (forceBreak || childForcesBreak, newChild::newChildren)
      ) (false, []) children
      in
      (forceBreak, Concat (List.rev newChildren))
    in
    let (_, processedDoc) = walk doc in
    processedDoc

  let join ~sep docs =
    let rec loop acc sep docs =
      match docs with
      | [] -> List.rev acc
      | [x] -> List.rev (x::acc)
      | x::xs -> loop (sep::x::acc) sep xs
    in
    Concat(loop [] sep docs)

  let rec fits w = function
    | _ when w < 0 -> false
    | [] -> true
    | (_ind, _mode, Text txt)::rest -> fits (w - String.length txt) rest
    | (ind, mode, Indent doc)::rest -> fits w ((ind + 2, mode, doc)::rest)
    | (_ind, Flat, LineBreak break)::rest ->
        if break = Hard || break = Literal then true
        else
          let w = if break = Classic then w - 1 else w in
          fits w rest
    | (_ind, _mode, Nil)::rest -> fits w rest
    | (_ind, Break, LineBreak break)::rest -> true
    | (ind, mode, Group(shouldBreak, doc))::rest ->
      let mode = if shouldBreak then Break else mode in
      fits w ((ind, mode, doc)::rest)
    | (ind, mode, IfBreaks(breakDoc, flatDoc))::rest ->
        if mode = Break then
          fits w ((ind, mode, breakDoc)::rest)
        else
          fits w ((ind, mode, flatDoc)::rest)
    | (ind, mode, Concat docs)::rest ->
      let ops = List.map (fun doc -> (ind, mode, doc)) docs in
      fits w (List.append ops rest)
    (* | (_ind, _mode, Cursor)::rest -> fits w rest *)
    | (_ind, _mode, LineSuffix _)::rest -> fits w rest

  let toString ~width doc =
    (* let doc = propagateForcedBreaks doc in *)
    let buffer = Buffer.create 1000 in

    let rec process ~pos lineSuffices stack =
      match stack with
      | ((ind, mode, doc) as cmd)::rest ->
        begin match doc with
        | Nil ->
          process ~pos lineSuffices rest
        | Text txt ->
          Buffer.add_string buffer txt;
          process ~pos:(String.length txt + pos) lineSuffices rest
        | LineSuffix doc ->
          process ~pos ((ind, mode, doc)::lineSuffices) rest
        | Concat docs ->
          let ops = List.map (fun doc -> (ind, mode, doc)) docs in
          process ~pos lineSuffices (List.append ops rest)
        | Indent doc ->
          process ~pos lineSuffices ((ind + 2, mode, doc)::rest)
        | IfBreaks(breakDoc, flatDoc) ->
          if mode = Break then
            process ~pos lineSuffices ((ind, mode, breakDoc)::rest)
          else
            process ~pos lineSuffices ((ind, mode, flatDoc)::rest)
        | LineBreak lineStyle  ->
          if mode = Break then (
            begin match lineSuffices with
            | [] ->
              Buffer.add_string buffer "\n";
              Buffer.add_string buffer (String.make ind ' ');
              process ~pos:ind [] rest
            | docs ->
              process ~pos:ind [] (List.concat [lineSuffices; cmd::rest])
            end
          ) else (

            begin match lineSuffices with
            | [] ->
              let pos = match lineStyle with
              | Classic -> Buffer.add_string buffer " "; pos + 1
              | Hard | Literal -> Buffer.add_string buffer "\n"; 0
              | Soft -> pos
              in
              process ~pos [] rest
            | docs ->
              process ~pos:ind [] (List.concat [lineSuffices; cmd::rest])
            end

          )
        (* | Cursor -> *)
          (* process ~pos rest *)
        | Group (shouldBreak, doc) ->
          if shouldBreak || not (fits (width - pos) ((ind, Flat, doc)::rest)) then
            process ~pos lineSuffices ((ind, Break, doc)::rest)
          else
            process ~pos lineSuffices ((ind, Flat, doc)::rest)
        end
      | [] -> ()
    in
    process ~pos:0 [] [0, Flat, doc];
    Buffer.contents buffer


  let debug t =
    let rec toDoc = function
      | Nil -> text "nil"
      | Text txt -> text ("text(" ^ txt ^ ")")
      | LineSuffix doc -> group(
          concat [
            text "linesuffix(";
            indent (
              concat [line; toDoc doc]
            );
            line;
            text ")"
          ]
        )
      | Concat docs -> group(
          concat [
            text "concat(";
            indent (
              concat [
                line;
                join ~sep:(concat [text ","; line])
                  (List.map toDoc docs) ;
              ]
            );
            line;
            text ")"
          ]
        )
      | Indent doc ->
          concat [
            text "indent(";
            softLine;
            toDoc doc;
            softLine;
            text ")";
          ]
      | IfBreaks (trueDoc, falseDoc) ->
        group(
          concat [
            text "ifBreaks(";
            indent (
              concat [
                line;
                toDoc trueDoc;
                concat [text ",";  line];
                toDoc falseDoc;
              ]
            );
            line;
            text ")"
          ]
        )
      | LineBreak break ->
        let breakTxt = match break with
          | Classic -> "Classic"
          | Soft -> "Soft"
          | Hard -> "Hard"
          | Literal -> "Literal"
        in
        text ("LineBreak(" ^ breakTxt ^ ")")
      | Group (shouldBreak, doc) ->
        group(
          concat [
            text "Group(";
            indent (
              concat [
                line;
                text ("shouldbreak: " ^ (string_of_bool shouldBreak));
                concat [text ",";  line];
                toDoc doc;
              ]
            );
            line;
            text ")"
          ]
        )
    in
    let doc = toDoc t in
    toString ~width:10 doc |> print_endline
end

module Time: sig
  type t

  val now: unit -> t

  val toUint64: t -> int64
  (* let of_uint64_ns ns = ns *)

  val nanosecond: t
  val microsecond: t
  val millisecond: t
  val second: t
  val minute: t
  val hour: t

  val zero: t

  val diff: t -> t -> t
  val add: t -> t -> t
  val print: t -> float
end = struct
  (* nanoseconds *)
  type t = int64

  let zero = 0L

  let toUint64 s = s

  let nanosecond = 1L
  let microsecond = Int64.mul 1000L nanosecond
  let millisecond = Int64.mul 1000L microsecond
  let second = Int64.mul 1000L millisecond
  let minute = Int64.mul 60L second
  let hour = Int64.mul 60L minute

  (* TODO: we could do this inside caml_absolute_time *)
  external init: unit -> unit = "caml_mach_initialize"
  let () = init()
  external now: unit -> t = "caml_mach_absolute_time"

  let diff t1 t2 = Int64.sub t2 t1
  let add t1 t2 = Int64.add t1 t2
  let print t =
    (Int64.to_float t) *. 1e-6
end

module Benchmark: sig
  type t

  val make: name:string -> ?time:Time.t -> f:(t -> unit) -> unit -> t
  val launch: t -> unit
  val report: t -> unit
end = struct
  type benchmarkResult = {
    n: int; (* number of iterations *)
    t: Time.t; (* total time taken *)
    bytes: float; (* bytes processed in one iteration *)
    memAllocs: float; (* total number of memory allocations in words*)
    memBytes: float; (* total number of bytes allocated *)
  }

  type t = {
    name: string;
    time: Time.t; (* how long should this benchmark run? *)
    mutable start: Time.t;
    mutable n: int; (* number of iterations *)
    mutable duration: Time.t;
    benchFunc: t -> unit;
    mutable timerOn: bool;
    (* mutable result: benchmarkResult; *)
	  (* The initial states *)
    mutable startAllocs: float;
    mutable startBytes: float;
    (* The net total of this test after being run. *)
    mutable netAllocs: float;
    mutable netBytes: float;
  }

  let report b =
    print_endline (Format.sprintf "Benchmark: %s" b.name);
    print_endline (Format.sprintf "Nbr of iterations: %d" b.n);
    print_endline (Format.sprintf "Benchmark ran during: %fms" (Time.print b.duration));
    print_endline (Format.sprintf "Avg time/op: %fms" ((Time.print b.duration) /. (float_of_int b.n)));
    print_endline (Format.sprintf "Allocs/op: %d" (int_of_float (b.netAllocs /.  (float_of_int b.n))));
    print_endline (Format.sprintf "B/op: %d" (int_of_float (b.netBytes /. (float_of_int b.n))));
    (* return (float64(r.Bytes) * float64(r.N) / 1e6) / r.T.Seconds() *)


    print_newline();
    ()

  let make ~name ?(time=Time.second) ~f () = {
    name;
    time;
    start = Time.zero;
    n = 0;
    benchFunc = f;
    duration = Time.zero;
    timerOn = false;
    startAllocs = 0.;
    startBytes = 0.;
    netAllocs = 0.;
    netBytes = 0.;
  }

  (* total amount of memory allocated by the program since it started in words *)
  let mallocs () =
    let stats = Gc.quick_stat() in
    stats.minor_words +. stats.major_words -. stats.promoted_words

  let startTimer b =
    if not b.timerOn then (
      let allocatedWords = mallocs() in
      b.startAllocs <- allocatedWords;
      b.startBytes <- allocatedWords *. 8.;
      b.start <- Time.now();
      b.timerOn <- true
    )

  let stopTimer b =
    if b.timerOn then (
      let allocatedWords = mallocs() in
      let diff = (Time.diff b.start (Time.now())) in
      b.duration <- Time.add b.duration diff;
      b.netAllocs <- b.netAllocs +. (allocatedWords -. b.startAllocs);
      b.netBytes <- b.netBytes +. (allocatedWords *. 8. -. b.startBytes);
      b.timerOn <- false
    )

  let resetTimer b =
    if b.timerOn then (
      let allocatedWords = mallocs() in
      b.startAllocs <- allocatedWords;
      b.netAllocs <- allocatedWords *. 8.;
    b.start <- Time.now();
    );
    b.netAllocs <- 0.;
    b.netBytes <- 0.

  let runIteration b n =
    Gc.full_major();
    b.n <- n;
    resetTimer b;
    startTimer b;
    b.benchFunc b;
    stopTimer b

  let launch b =
    let d = b.time in
    let n = ref 0 in
    while b.duration < d && !n < 1000000000 do
      n := !n + 1;
      runIteration b !n
    done
end

module Profile: sig
  val record : name:string -> (unit -> 'a) -> 'a
  val print: unit -> unit
end = struct
  let state = Hashtbl.create 2

  let record ~name f =
    let startTime = Time.now() in
    let result = f() in
    let endTime = Time.now() in

    Hashtbl.add state name (Time.diff startTime endTime);
    result

  let print () =
    let report = Hashtbl.fold (fun k v acc ->
      let line = Printf.sprintf "%s: %fms\n" k (Time.print v) in
      acc ^ line
    ) state "\n\n"
    in
    print_endline report
end

module IO: sig
  val readFile: string -> string
  val writeFile: string -> string -> unit
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

  let writeFile filename txt =
    let chan = open_out_bin filename in
    output_string chan txt;
    close_out chan
end

module LocationState = struct
  (* stores diamonds location based on the location of the first list item *)
  let diamondTbl = Hashtbl.create 10

  let bracesTbl = Hashtbl.create 10
end

module CharacterCodes = struct
  let eof = -1

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


module Comment: sig
  type t

  val toString: t -> string
  val toAttribute: t -> Parsetree.attribute

  val loc: t -> Location.t

  val makeSingleLineComment: loc:Location.t -> string -> t
  val makeMultiLineComment: loc:Location.t -> string -> t
end = struct
  type style =
    | SingleLine
    | MultiLine

  let styleToString s = match s with
    | SingleLine -> "SingleLine"
    | MultiLine -> "MultiLine"

  type t = {
    txt: string;
    style: style;
    loc: Location.t;
  }

  let loc t = t.loc

  let toString t =
    Format.sprintf
      "(txt: %s\nstyle: %s\nlines: %d-%d)"
      t.txt
      (styleToString t.style)
      t.loc.loc_start.pos_lnum
      t.loc.loc_end.pos_lnum

  let toAttribute t =
    let expr =
      Ast_helper.Exp.constant (Parsetree.Pconst_string (t.txt, None))
    in
    let id = Location.mkloc
      (match t.style with
       | SingleLine -> "napkinscript.singleLineComment"
       | MultiLine -> "napkinscript.multiLineComment")
      t.loc
    in
    (id, Parsetree.PStr [Ast_helper.Str.eval expr])

  let makeSingleLineComment ~loc txt = {
    txt;
    loc;
    style = SingleLine
  }

  let makeMultiLineComment ~loc txt = {
    txt;
    loc;
    style = MultiLine
  }
end

module AstUtils = struct
  type attachment =
    | Before
    | Inside
    | SomewhereElse

  let classify loc1 loc2 =
    let open Location in
    if loc1.loc_end.pos_lnum < loc2.loc_start.pos_lnum then Before
    else if
      (loc1.loc_start.pos_lnum == loc2.loc_start.pos_lnum && loc1.loc_start.pos_cnum <= loc2.loc_end.pos_cnum) ||
        (loc1.loc_start.pos_cnum >= loc2.loc_start.pos_cnum && loc1.loc_end.pos_cnum <= loc2.loc_end.pos_cnum) ||
        (loc1.loc_start.pos_lnum == loc2.loc_end.pos_lnum)
    then
      Inside
    else
      SomewhereElse

  (* the name of this type is dubious *)
  type tieBreakResult = First | Second

  (* helps determine whether a comment attaches to a `loc1` or a `loc2`
   * `loc2` is the location of first item of a list
   * Example:
   * | Foo /* after constructor name */(/* before first constructor arg */string, int, float)
   * The (string, int, float) doesn't have a location in the ast, the only way to check
   * where the comment "probably" attaches is to compare the location with the first
   * item of the list… *)
  let breakTieWithFirstListItem commentLoc loc1 loc2 =
    let open Location in
    if commentLoc.loc_start.pos_cnum >= loc2.loc_end.pos_cnum then
      Second
    else if loc1.loc_end.pos_lnum == loc2.loc_start.pos_lnum then (
      let distanceToEndLoc1 =
        commentLoc.loc_start.pos_cnum - loc1.loc_end.pos_cnum in
      let distanceToStartLoc2 =
        loc2.loc_start.pos_cnum - commentLoc.loc_end.pos_cnum in
      if distanceToEndLoc1 < distanceToStartLoc2 then First
      else Second
    ) else
      Second

  (* Tries to answer the following question:
   *  ~ Does the comment attach more to loc1 or more to loc2 ? *)
  let breakTieBetween commentLoc loc1 loc2 =
    let open Location in
    let distanceToEndLoc1 = commentLoc.loc_start.pos_cnum - (loc1.loc_end.pos_cnum - 1) in
    let distanceToStartLoc2 = loc2.loc_start.pos_cnum -
      (commentLoc.loc_end.pos_cnum - 1) in
    if distanceToEndLoc1 < distanceToStartLoc2 ||
      distanceToEndLoc1 == distanceToStartLoc2
    then
      First
    else
      Second

  (*
   * Linear scan of attributes, inserts comment into the correct spot
   * Sort order: ascending location
   *)
  let insertIntoAttributes comment attrs =
    let commentLoc = Comment.loc comment in
    let commentAttr = Comment.toAttribute comment in
    let rec find acc attrs = match (attrs: Parsetree.attributes) with
    | [] -> List.rev (commentAttr::acc)
    | ((id, _) as attr)::attrs ->
        if commentLoc.loc_end.pos_cnum <= id.loc.loc_start.pos_cnum then (
          (List.rev acc) @ (commentAttr::attr::attrs)
        ) else (
          find (attr::acc) attrs
        )
    in
    find [] attrs

  let rec scanStructure comment (s : Parsetree.structure) =
    let rec scan comment acc nodes =
      match nodes with
      | [] ->
        let commentNode =
          Ast_helper.Str.attribute ~loc:(Comment.loc comment) (Comment.toAttribute comment)
        in
        List.rev (commentNode::acc)
      | node::nodes ->
        let commentLoc = Comment.loc comment in
        match classify commentLoc  node.Parsetree.pstr_loc with
        | Before ->
          let commentNode =
            Ast_helper.Str.attribute ~loc:commentLoc (Comment.toAttribute comment)
          in
          List.rev (node::commentNode::acc) @ nodes
        | Inside ->
          let node = scanStructureItem comment node in
          List.rev (node::acc) @ nodes
        | SomewhereElse ->
          scan comment (node::acc) nodes
    in
    scan comment [] s

  and scanStructureItem comment (si : Parsetree.structure_item) =
      match si.pstr_desc with
      | Pstr_value (recFlag, valueBindings) ->
        {si with pstr_desc = Pstr_value (recFlag, scanValueBindings comment valueBindings)}
      | Pstr_type (recFlag, typeDeclarations) ->
        {si with pstr_desc =
        Pstr_type (recFlag, scanTypeDeclarations comment typeDeclarations)}
      | Pstr_primitive valueDescription ->
        {si with pstr_desc = Pstr_primitive (
          scanValueDescription comment valueDescription)}
      | _ -> si

  and scanValueDescription comment (vd : Parsetree.value_description) =
    let commentLoc = Comment.loc comment in
    if commentLoc.loc_end.pos_cnum <= vd.pval_name.loc.loc_start.pos_cnum ||
      commentLoc.loc_start.pos_cnum >= vd.pval_loc.loc_end.pos_cnum
    then
      { vd with pval_attributes = insertIntoAttributes  comment vd.pval_attributes }
    else
      begin match breakTieBetween commentLoc vd.pval_name.loc vd.pval_type.ptyp_loc with
      | First ->
        { vd with pval_attributes = insertIntoAttributes  comment vd.pval_attributes }
      | Second -> { vd with pval_type = { vd.pval_type with
          ptyp_attributes = insertIntoAttributes
            comment
            vd.pval_type.ptyp_attributes
          }
        }
      end

  and scanTypeDeclarations comment (tds : Parsetree.type_declaration list) =
    let rec scan comment acc nodes =
      match nodes with
      | [] -> List.rev acc
      | node::nodes ->
        let commentLoc = Comment.loc comment in
        match classify commentLoc node.Parsetree.ptype_loc with
        | SomewhereElse ->
          scan comment (node::acc) nodes
        | Before ->
          let attr = Comment.toAttribute comment in
          let node = { node with
            Parsetree.ptype_attributes = attr::node.ptype_attributes
          } in
          List.rev (node::acc) @ nodes
        | Inside ->
          let node = scanTypeDeclaration comment node in
          List.rev (node::acc) @ nodes
    in
    scan comment [] tds

  and scanTypeDeclaration comment (td : Parsetree.type_declaration) =
    let open Location in
    let commentLoc = Comment.loc comment in
    match (td.ptype_kind, td.ptype_params, td.ptype_manifest) with
      (* type t = .. *)
    | (Ptype_open, [], _) ->
      {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
    | (Ptype_open, hdParam::tailParams, _) ->
      let (hdTyp, _) = hdParam in
      let paramsLoc = Hashtbl.find LocationState.diamondTbl hdTyp.Parsetree.ptyp_loc in
      if commentLoc.loc_end.pos_cnum <= paramsLoc.loc_start.pos_cnum ||
         commentLoc.loc_start.pos_cnum >= paramsLoc.loc_end.pos_cnum then
        (* type /* a */ t /* b */<'a, 'b> = ..*)
        (* type t <'a, 'b> /* here */  = .. // another one *)
        {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
      else
        { td with ptype_params = scanTypeParams comment td.ptype_params }

      (* type /* a */ t /* b */<'a, 'b>  *)
    | (Ptype_abstract, hdParam::tailParams, None) ->
      let (hdTyp, _) = hdParam in
      let paramsLoc = Hashtbl.find LocationState.diamondTbl hdTyp.Parsetree.ptyp_loc in
      if commentLoc.loc_end.pos_cnum <= paramsLoc.loc_start.pos_cnum ||
         commentLoc.loc_start.pos_cnum >= paramsLoc.loc_end.pos_cnum then
        (* type /* a */ t /* b */<'a, 'b> *)
        {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
      else
        (* type t<'a /* inside */, 'b> *)
        { td with ptype_params = scanTypeParams comment td.ptype_params }
    | (Ptype_abstract, hdParam::tailParams, Some manifest) ->
      let (hdTyp, _) = hdParam in
      let paramsLoc = Hashtbl.find LocationState.diamondTbl hdTyp.Parsetree.ptyp_loc in
      if commentLoc.loc_end.pos_cnum <= paramsLoc.loc_start.pos_cnum then
        (* type /* a */ t /* b */<'a> = myType *)
        {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
      else
        begin match breakTieBetween commentLoc paramsLoc manifest.ptyp_loc with
        | First ->
          (* type t<'a> /* here */ = myType *)
          {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
        | Second ->
          (* type t<'a> = /* here */ myType *)
          {td with ptype_manifest = Some ({ manifest with
            ptyp_attributes = insertIntoAttributes comment manifest.ptyp_attributes
          })}
        end
      (* type /* c1*/ t /* c2 */ *)
    | (Ptype_abstract, [], None) ->
      {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
      (* type /* c1 */ t /* c2 */ = /* c3 */ string /* c4 */  *)
    | (Ptype_abstract, [], Some typ) ->
      (* type /* c1 */ t = string *)
      if commentLoc.loc_end.pos_cnum < td.ptype_name.loc.loc_start.pos_cnum then
        {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
      (* type t = string /* c4 */ *)
      else if commentLoc.loc_start.pos_cnum > typ.ptyp_loc.loc_end.pos_cnum then
        {td with ptype_manifest = Some {typ with
          ptyp_attributes = insertIntoAttributes comment typ.ptyp_attributes }}
      else
        (* two cases left:
         *  - trailing comment on the type name: type t /* comment */ = string
         *  - leading comment on the type manifest: type t = /* comment */ string
         *)
        let distanceToEndOfTypeName =
          commentLoc.loc_start.pos_cnum  - td.ptype_name.loc.loc_end.pos_cnum
        in
        let distanceToStartOfManifest =
          typ.ptyp_loc.loc_start.pos_cnum - commentLoc.loc_end.pos_cnum
        in
        (* if the distance from the end of the comment to the start of the type manifest
         * is smaller, than the start of the comment to the end of the type name,
         * we're dealing with a comment attached to the manifest *)
        if distanceToStartOfManifest < distanceToEndOfTypeName then
          (* type t = /* comment */ string *)
          {td with ptype_manifest = Some {typ with
            ptyp_attributes = insertIntoAttributes comment typ.ptyp_attributes }}
        else
          (* type t /* comment */ = string *)
          {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }

    (* type user = {name: string, age: int} *)
    | (Ptype_record labelDeclarations, [], None) ->
      (* type /* comment */ user = {name: string, age: int} *)
      if commentLoc.loc_end.pos_cnum < td.ptype_name.loc.loc_start.pos_cnum then
        {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
      else if commentLoc.loc_start.pos_cnum > td.ptype_loc.loc_end.pos_cnum then
        (*
         * type user = {
         *  name: string,
         *  age: int
         * } /* comment */
         *)
        {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
      else
        (* problematic: ast doesn't contain location info for the leading { in  { }
         * it's impossible to deduce the difference between:
         *   - type t = /* before brace */ {name: string}
         *   - type t =  { /* after brace */ name: string}
         *
         *  we could drop brace location info into attributes as a solution… *)
        begin match labelDeclarations with
        (* type t = { }  --> won't typecheck *)
        | [] ->
          { td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
        | ld::_ as lds ->
          (* two cases:
           * - it's a trailing comment on the type name: type t /* comment */ = {name: string}
           * - it's part of the record declaration: type t = {/* comment */ name: string}
           *   or line below:
           *     type t = {
           *       /* comment */
           *       name: string
           *     }
           *)
          if (
            let distanceToEndOfTypeName =
              commentLoc.loc_start.pos_cnum  - td.ptype_name.loc.loc_end.pos_cnum
            in
            let distanceToLDName =
              ld.pld_name.loc.loc_start.pos_cnum - commentLoc.loc_end.pos_cnum in
            distanceToEndOfTypeName < distanceToLDName
          ) then
            { td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
          else
            {td with ptype_kind = Ptype_record(scanLabelDeclarations comment lds)}
        end
    (* type user = User.t = {name: string, age: int} *)
    | (Ptype_record labelDeclarations, [], Some manifest) ->
      (* type /* comment */ user = User.t = {name: string, age: int} *)
      if commentLoc.loc_end.pos_cnum < td.ptype_name.loc.loc_start.pos_cnum then
        {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
      else if commentLoc.loc_start.pos_cnum > td.ptype_loc.loc_end.pos_cnum then
        (*
         * type user = User.t = {
         *  name: string,
         *  age: int
         * } /* comment */
         *)
        {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
      (* type user /* loc1 */ = /* loc2*/ User.t = {name:string, age: int } *)
      else if commentLoc.loc_start.pos_cnum >= td.ptype_name.loc.loc_end.pos_cnum &&
        commentLoc.loc_end.pos_cnum <= manifest.ptyp_loc.loc_start.pos_cnum
      then (
        (* two cases:
         *  - type t /* after type name */ = User.t = {name: string, age: int}
         *  - type t = /* before manifest */ User.t = {name: string, age: int}
         *)
        let distanceToStartOfName =
          commentLoc.loc_start.pos_cnum - td.ptype_name.loc.loc_end.pos_cnum in
        let distanceToStartOfManifest =
          manifest.ptyp_loc.loc_start.pos_cnum - commentLoc.loc_end.pos_cnum in
        if distanceToStartOfName < distanceToStartOfManifest then
          {td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
        else
          {td with ptype_manifest = Some {manifest with
            ptyp_attributes = insertIntoAttributes comment manifest.ptyp_attributes}}
      )
      else
        begin match labelDeclarations with
        (* type t = { }  --> won't typecheck *)
        | [] ->
          { td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }
        | ld::_ as lds ->
          (* type t = User.t /* after manifest */ = {name: string}
           * type t = User.t = {/* leading on label name */ name: string} *)
          if (
            let distanceToEndOfManifest =
              commentLoc.loc_start.pos_cnum  - manifest.ptyp_loc.loc_end.pos_cnum
            in
            let distanceToLDName =
              ld.pld_name.loc.loc_start.pos_cnum - commentLoc.loc_end.pos_cnum in
            distanceToEndOfManifest < distanceToLDName
          ) then
            { td with ptype_manifest = Some {manifest with
            ptyp_attributes = insertIntoAttributes comment manifest.ptyp_attributes}}
          else
            {td with ptype_kind = Ptype_record(scanLabelDeclarations comment lds)}
        end
    (* type color = Red | Blue | Green
     * type color =
     *   | Red
     *   | Blue
     *   | Green
     *)
    | (Ptype_variant constructorDeclarations, [], None) ->
        begin match constructorDeclarations with
        | [] ->
          { td with ptype_attributes = insertIntoAttributes comment td.ptype_attributes }

        | cds ->
          {td with ptype_kind = Ptype_variant(scanConstructorDeclarations comment cds)}
        end
    | _ -> td

  and scanTypExpr comment (typ : Parsetree.core_type) =
    {typ with
      ptyp_attributes = insertIntoAttributes comment typ.ptyp_attributes}

  and scanTypeParams comment (params : (Parsetree.core_type * Asttypes.variance) list) =
    scanList
      comment
      (fun (typ, _) -> typ.Parsetree.ptyp_loc)
      (fun comment (typ, variance) -> (
        Parsetree.{typ with
         ptyp_attributes = insertIntoAttributes comment typ.ptyp_attributes},
         variance))
      (fun comment (typ, variance) -> (
        Parsetree.{typ with
         ptyp_attributes = insertIntoAttributes comment typ.ptyp_attributes},
         variance))
      params

  and scanTypeExpressions comment (typExprs: Parsetree.core_type list) =
    scanList
      comment
      (fun n -> n.Parsetree.ptyp_loc)
      scanTypExpr
      scanTypExpr
      typExprs


  and scanLabelDeclarations comment (lds: Parsetree.label_declaration list) =
    scanList
      comment
      (fun n -> n.Parsetree.pld_loc)
      scanLabelDeclaration
      (fun comment n -> Parsetree.{n with
        pld_attributes = insertIntoAttributes comment n.pld_attributes
      })
      lds

  and scanConstructorDeclarations comment (cds : Parsetree.constructor_declaration list) =
    scanList
      comment
      (fun n -> n.Parsetree.pcd_loc)
      scanConstructorDeclaration
      (fun comment n -> Parsetree.{n with
        pcd_attributes = insertIntoAttributes comment n.pcd_attributes
      })
      cds

  and scanList: 'a. Comment.t -> ('a -> Location.t)
    -> (Comment.t -> 'a -> 'a) -> (Comment.t -> 'a -> 'a) -> 'a list -> 'a list
    = fun comment getNodeLoc scanNode attachToNode lst ->
    (* let rec scan comment acc lst = *)
      (* match lst with *)
      (* | [] -> [] *)
      (* | [node] -> *)
        (* let node = scanNode comment node in *)
        (* List.rev (node::acc) *)
      (* | node::nodes -> *)
        (* let commentLoc = Comment.loc comment in *)
        (* match classify commentLoc (getNodeLoc node) with *)
        (* | SomewhereElse -> *)
          (* scan comment (node::acc) nodes *)
        (* | Before | Inside -> *)
          (* let node = scanNode comment node in *)
          (* List.concat [List.rev (node::acc); nodes] *)
    (* in *)
    (* scan comment [] lst *)

    let open Location in
    let commentLoc = Comment.loc comment in
    let rec scan comment acc prev lst =
      match lst with
      | [] -> []
      | [node] ->
        let nodeLoc = getNodeLoc node in
        if (commentLoc.loc_start.pos_cnum >= nodeLoc.loc_start.pos_cnum &&
            commentLoc.loc_end.pos_cnum <= nodeLoc.loc_end.pos_cnum) ||
            commentLoc.loc_start.pos_cnum >= nodeLoc.loc_end.pos_cnum then
          let node = scanNode comment node in
          List.rev (node::prev::acc)
        else begin match breakTieBetween commentLoc (getNodeLoc prev) nodeLoc with
        | First ->
          let prevNode = scanNode comment prev in
          List.rev (node::prevNode::acc)
        | Second ->
          let node = scanNode comment node in
          List.rev (node::prev::acc)
        end
      | node::nodes ->
        let nodeLoc = getNodeLoc node in
        if commentLoc.loc_end.pos_lnum < nodeLoc.loc_start.pos_lnum ||
            (commentLoc.loc_start.pos_cnum >= nodeLoc.loc_start.pos_cnum &&
            commentLoc.loc_end.pos_cnum <= nodeLoc.loc_end.pos_cnum)
        then
            List.rev (node::prev::acc)
        else
          let prevLoc = getNodeLoc prev in
          if prevLoc.loc_start.pos_lnum == nodeLoc.loc_end.pos_lnum then
          begin match breakTieBetween commentLoc (getNodeLoc prev) nodeLoc with
          | First ->
            let prevNode = scanNode comment prev in
            List.rev (node::prevNode::acc)
          | Second ->
            scan comment (prev::acc) node nodes
          end
          else
            scan comment (prev::acc) node nodes

    in
    match lst with
    | [] -> []
    | [node] ->
      let node = scanNode comment node in
      [node]
    | node::nodes ->
      let nodeLoc = getNodeLoc node in
      if commentLoc.loc_end.pos_lnum < nodeLoc.loc_start.pos_lnum ||
          (commentLoc.loc_start.pos_cnum >= nodeLoc.loc_start.pos_cnum &&
          commentLoc.loc_end.pos_cnum <= nodeLoc.loc_end.pos_cnum)
      then
        let node = scanNode comment node in
        node::nodes
      else
        scan comment [] node nodes


  and scanConstructorDeclaration comment (cd : Parsetree.constructor_declaration) =
    let commentLoc = Comment.loc comment in
    if commentLoc.loc_end.pos_cnum <= cd.pcd_name.loc.loc_start.pos_cnum then
      (* leading: | /* comment */ Foo *)
      {cd with pcd_attributes = insertIntoAttributes comment cd.pcd_attributes}
    else if commentLoc.loc_start.pos_cnum >= cd.pcd_loc.loc_end.pos_cnum then
      (* trailing: | Foo(a, b, c): typ /* comment */*)
      {cd with pcd_attributes = insertIntoAttributes comment cd.pcd_attributes}
    else begin
      match (cd.pcd_name.loc, cd.pcd_args, cd.pcd_res) with
      | (nameLoc, Pcstr_tuple([]), None) ->
        (* | /* before */ Blue*)
        {cd with pcd_attributes = insertIntoAttributes comment cd.pcd_attributes}
      | (nameLoc, Pcstr_tuple([]), Some gadt) ->
        if commentLoc.loc_end.pos_cnum <= gadt.ptyp_loc.loc_start.pos_cnum then
          (* | /* here */ Blue /* or here */: color *)
          {cd with pcd_attributes = insertIntoAttributes comment cd.pcd_attributes}
        else
          (* | Blue: /* here */ color *)
          {cd with pcd_res = Some {gadt with
            ptyp_attributes =
              insertIntoAttributes comment gadt.ptyp_attributes
          }}
      | (nameLoc, Pcstr_tuple(firstType::rest as constructors), None) ->
        begin match (breakTieWithFirstListItem commentLoc nameLoc firstType.ptyp_loc) with
        | First ->
          (* | Blue /* after pcd_name */(string, int)*)
          {cd with pcd_attributes = insertIntoAttributes comment cd.pcd_attributes}
        | Second ->
          (* | Blue(/* in list */ string /* another */, /* comment */int)*)
          {cd with pcd_args = Pcstr_tuple(
            scanTypeExpressions comment constructors
          )}
        end
      | (nameLoc, Pcstr_record(ld::_ as lds), None) ->
        begin match (breakTieWithFirstListItem commentLoc nameLoc ld.pld_loc) with
        | First ->
          (* | Color /* after pcd_name */({r: float, g: float, b: float})*)
          {cd with pcd_attributes = insertIntoAttributes comment cd.pcd_attributes}
        | Second ->
          (* | Color({/* comment */ r: /* somewhere inside */ float, /* here */ g: float, b: float})*)
          {cd with pcd_args = Pcstr_record(scanLabelDeclarations comment lds)}
        end
      | _ -> cd
    end

  and scanLabelDeclaration comment (ld : Parsetree.label_declaration) =
    let commentLoc = Comment.loc comment in
    if commentLoc.loc_end.pos_cnum <= ld.pld_name.loc.loc_start.pos_cnum then
      (* /* comment */ name: string
       * or
       * /* comment */
       * name: string *)
      {ld with pld_attributes = insertIntoAttributes comment ld.pld_attributes}
    else if commentLoc.loc_start.pos_cnum >= ld.pld_loc.loc_end.pos_cnum then
      (* three cases, the parsetree has no notion of commas
       *  name: string, /* comment */
       *  name: string /* comment */,
       *  name: string, // comment
       *
       *)
      {ld with pld_type = { ld.pld_type with ptyp_attributes =
        insertIntoAttributes comment ld.pld_type.ptyp_attributes}}
    else
      (* two cases:
       * - trailing on the pld_name:  `name /* comment */: string`
       * - leading on the pld_type: `name: /* string */ *)
      let distanceToEndOfName =
        commentLoc.loc_start.pos_cnum - ld.pld_name.loc.loc_end.pos_cnum in
      let distanceToStartOfType =
        ld.pld_type.ptyp_loc.loc_start.pos_cnum - commentLoc.loc_end.pos_cnum in
      if distanceToEndOfName < distanceToStartOfType then
        (* name /* comment */: string  *)
        {ld with pld_attributes = insertIntoAttributes comment ld.pld_attributes}
      else
        (* name: string /* comment */ *)
        let ()  = print_endline "we are fucked" in
        {ld with pld_type =
          {ld.pld_type with ptyp_attributes =
            insertIntoAttributes comment ld.pld_type.ptyp_attributes
          }
        }

  and scanValueBindings comment (vbs : Parsetree.value_binding list) =
    let rec scan comment acc nodes =
      match nodes with
      | [] -> List.rev acc
      | node::nodes ->
        let commentLoc = Comment.loc comment in
        match classify commentLoc node.Parsetree.pvb_loc with
        | SomewhereElse ->
          scan comment (node::acc) nodes
        | Before ->
          let attr = Comment.toAttribute comment in
          let node = { node with
            Parsetree.pvb_attributes = attr::node.pvb_attributes;
          } in
          List.rev (node::acc) @ nodes
        | Inside ->
          let node = scanValueBinding comment node in
          List.rev (node::acc) @ nodes
    in
    scan comment [] vbs

  and scanValueBinding comment (vb : Parsetree.value_binding) =
    let open Parsetree in
    let commentLoc = Comment.loc comment in
    (* @attr let x = 1 -> before "let" *)
    if commentLoc.loc_end.pos_cnum < vb.pvb_loc.loc_start.pos_cnum then
      {vb with pvb_attributes = insertIntoAttributes comment vb.pvb_attributes}
    (* let /* comment */ x = 1 -> before pattern "x" *)
    else if commentLoc.loc_end.pos_cnum < vb.pvb_pat.ppat_loc.loc_start.pos_cnum then
      {vb with pvb_pat =
        {vb.pvb_pat with ppat_attributes =
            insertIntoAttributes comment vb.pvb_pat.ppat_attributes
        }
      }
    (* let x = foo /* comment */ -> after expr "foo" *)
    else if commentLoc.loc_start.pos_cnum > vb.pvb_expr.pexp_loc.loc_end.pos_cnum then
      {vb with pvb_expr =
        {vb.pvb_expr with pexp_attributes =
          insertIntoAttributes comment vb.pvb_expr.pexp_attributes
        }
      }
    (* two cases left:
     *  - trailing comment on the pattern: let x /* comment */ = foo
     *  - leading comment on the expr: let x = /* comment */ foo
     *)
    else
      let distanceToEndOfPattern =
        commentLoc.loc_start.pos_cnum - vb.pvb_pat.ppat_loc.loc_end.pos_cnum
      in
      let distanceToStartOfExpression =
        vb.pvb_expr.pexp_loc.loc_start.pos_cnum - commentLoc.loc_start.pos_cnum
      in
      (* if the start of the comment is closer to the end of the pattern, then
       * it's a comment on the pattern *)
      if distanceToEndOfPattern < distanceToStartOfExpression then
        {vb with pvb_pat =
          {vb.pvb_pat with ppat_attributes =
              insertIntoAttributes comment vb.pvb_pat.ppat_attributes}}
      else (* we're dealing with a leading comment on the expression *)
        {vb with pvb_expr =
          {vb.pvb_expr with pexp_attributes =
            insertIntoAttributes comment vb.pvb_expr.pexp_attributes}}

  let mergeCommentsIntoStructure (comments : Comment.t list) (structure : Parsetree.structure) =
    List.fold_left (fun ast comment ->
      scanStructure comment ast
    ) structure comments

  let labelDeclarationsShouldForceBreak (lds : Parsetree.label_declaration list) =
    let open Location in
    let rec loop prevLoc (lds : Parsetree.label_declaration list) = match lds with
      | [] -> false
      | ld::lds ->
        if ld.pld_loc.loc_end.pos_lnum > prevLoc.loc_start.pos_lnum then
          false
        else
          loop ld.pld_loc lds
    in
    match lds with
    | [] -> false
    | ld::lds ->
      loop ld.pld_loc lds
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
    | Comment of Comment.t
    | List
    | TemplateTail of string
    | TemplatePart of string
    | Backtick
    | BarGreater
    | Try | Catch

  let precedence = function
    | HashEqual | ColonEqual -> 1
    | Lor -> 2
    | Land -> 3
    | Equal | EqualEqual | EqualEqualEqual | LessThan | GreaterThan
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
    | Comment c -> "Comment(" ^ (Comment.toString c) ^ ")"
    | List -> "list"
    | TemplatePart text -> text ^ "${"
    | TemplateTail text -> "TemplateTail(" ^ text ^ ")"
    | Backtick -> "`"
    | BarGreater -> "|>"
    | Try -> "try" | Catch -> "catch"

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
      "with", With;
      "try", Try;
      "catch", Catch;
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
    | Land | Lor | Lxor | Lsl | Lsr | Asr | List | With
    | Try | Catch -> true
    | _ -> false

  let lookupKeyword str =
    try Hashtbl.find keywordTable str with
    | Not_found ->
      if CharacterCodes.isUpperCase (int_of_char str.[0]) then
        Uident str
      else Lident str
end

exception InfiniteLoop of Lexing.position * Token.t

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
    | PatternOcamlList
    | PatternRecord

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
    | Signature
    | Structure
    | Attribute
    | TypeConstraint
    | Primitive
    | AtomicTypExpr
    | ListExpr

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
    | PatternOcamlList -> "a list pattern"
    | PatternRecord -> "a record pattern"
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
    | Signature -> "signature"
    | Structure -> "structure"
    | Attribute -> "an attribute"
    | TypeConstraint -> "constraints on a type"
    | Primitive -> "an external primitive"
    | AtomicTypExpr -> "a type"
    | ListExpr -> "an ocaml list expr"

  let isSignatureItemStart = function
    | Token.At
    | Let
    | Typ
    | External
    | Exception
    | Open
    | Include
    | Module
    | AtAt
    | PercentPercent -> true
    | _ -> false

  let isAtomicPatternStart = function
    | Token.Int _ | String _
    | Lparen | Lbracket | Lbrace | Forwardslash
    | Underscore
    | Lident _ | Uident _ | List
    | Exception | Lazy
    | Percent -> true
    | _ -> false

  let isAtomicExprStart = function
    | Token.True | False
    | Int _ | String _ | Float _
    | Backtick
    | Uident _ | Lident _
    | Lparen
    | List
    | Lbracket
    | Lbrace
    | Forwardslash
    | LessThan
    | Percent -> true
    | _ -> false

  let isAtomicTypExprStart = function
    | Token.SingleQuote | Underscore
    | Forwardslash | Lparen | Lbrace
    | Uident _ | Lident _ | List
    | Percent -> true
    | _ -> false

  let isExprStart = function
    | Token.True | False
    | Int _ | String _ | Float _ | Backtick
    | Uident _ | Lident _
    | Lparen | List | Lbracket | Lbrace | Forwardslash
    | LessThan
    | Minus | MinusDot | Plus | PlusDot | Bang | Band
    | Percent | At
    | If | Switch | While | For | Assert | Lazy | Try -> true
    | _ -> false

  let isJsxAttributeStart = function
    | Token.Lident _ | Question -> true
    | _ -> false

 let isStructureItemStart = function
    | Token.Open
    | Let
    | Typ
    | External
    | Exception
    | Include
    | Module
    | AtAt
    | PercentPercent
    | At -> true
    | t when isExprStart t -> true
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
    | Token.Typ | Tilde | Dot -> true
    | token when isPatternStart token -> true
    | _ -> false

  (* TODO: overparse Uident ? *)
  let isStringFieldDeclStart = function
    | Token.String _ | At -> true
    | _ -> false

  (* TODO: overparse Uident ? *)
  let isFieldDeclStart = function
    | Token.At | Mutable | Lident _  -> true
    (* recovery, TODO: this is not ideal… *)
    | Uident _ -> true
    | t when Token.isKeyword t -> true
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
    | Token.Tilde | Dot -> true
    | token when isTypExprStart token -> true
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
    (* TODO *)
    | t when Token.isKeyword t -> true
    | _ -> false

  let isRecordRowStringKeyStart = function
    | Token.String _ -> true
    | _ -> false

  let isArgumentStart = function
    | Token.Tilde | Dot | Underscore -> true
    | t when isExprStart t -> true
    | _ -> false

  let isPatternMatchStart = function
    | Token.Bar -> true
    | t when isPatternStart t -> true
    | _ -> false

  let isPatternOcamlListStart = function
    | Token.DotDotDot -> true
    | t when isPatternStart t -> true
    | _ -> false

  let isPatternRecordItemStart = function
    | Token.DotDotDot | Uident _ | Lident _ | Underscore -> true
    | _ -> false

  let isAttributeStart = function
    | Token.At -> true
    | _ -> false

  let isListElement grammar token =
    match grammar with
    | ExprList -> isExprStart token
    | ListExpr -> token = DotDotDot || isExprStart token
    | PatternList -> isPatternStart token
    | ParameterList -> isParameterStart token
    | StringFieldDeclarations -> isStringFieldDeclStart token
    | FieldDeclarations -> isFieldDeclStart token
    | RecordDecl -> isRecordDeclStart token
    | TypExprList -> isTypExprStart token || token = Token.LessThan
    | TypeParams -> isTypeParamStart token
    | FunctorArgs -> isFunctorArgStart token
    | ModExprList -> isModExprStart token
    | TypeParameters -> isTypeParameterStart token
    | RecordRows -> isRecordRowStart token
    | RecordRowsStringKey -> isRecordRowStringKeyStart token
    | ArgumentList -> isArgumentStart token
    | Signature -> isSignatureItemStart token
    | Structure -> isStructureItemStart token
    | PatternMatching -> isPatternMatchStart token
    | PatternOcamlList -> isPatternOcamlListStart token
    | PatternRecord -> isPatternRecordItemStart token
    | Attribute -> isAttributeStart token
    | TypeConstraint -> token = Constraint
    | ConstructorDeclaration -> token = Bar
    | Primitive -> begin match token with Token.String _ -> true | _ -> false end
    | JsxAttribute -> isJsxAttributeStart token
    | _ -> false

  let isListTerminator grammar token =
    token = Token.Eof ||
    (match grammar with
    | ExprList  ->
        token = Token.Rparen || token = Forwardslash || token = Rbracket
    | ListExpr ->
        token = Token.Rparen
    | ArgumentList -> token = Token.Rparen
    | TypExprList ->
        token = Rparen || token = Forwardslash || token = GreaterThan
        || token = Equal
    | ModExprList ->
        token = Rparen
    | PatternList | PatternOcamlList | PatternRecord ->
        token = Forwardslash || token = Rbracket || token = Rparen
        || token = EqualGreater (* pattern matching =>*)
        || token = In (* for expressions *)
        || token = Equal (* let {x} = foo *)
    | ExprBlock -> token = Rbrace
    | Structure -> token = Rbrace
    | TypeParams -> token = Rparen
    | ParameterList -> token = EqualGreater || token = Lbrace
    | Attribute -> token <> At
    | TypeConstraint -> token <> Constraint
    | ConstructorDeclaration -> token <> Bar
    | Primitive -> isStructureItemStart token || token = Semicolon
    | JsxAttribute -> token = Forwardslash || token = GreaterThan
    | _ -> false
    )

  let isPartOfList grammar token =
    isListElement grammar token || isListTerminator grammar token
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
    let nil = Nil

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
    if from < 0 || (String.length txt) == 0 || (from >= String.length txt) then txt else
    let before = String.sub txt 0 from in
    let content =
      "\027[31m" ^ (String.sub txt from len) ^ "\027[0m"
    in
    let after = String.sub txt (from + len) (String.length txt - (from + len)) in
    before ^ content ^ after

  let underline ~from ~len txt =
    let open TerminalDoc in
    let indent = String.make from ' ' in
    let underline = String.make len '^' in
    let line = highlight ~from:0 ~len underline in
    group ~break:Always
      (append (text txt) (text (indent ^ line)))

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

  (* TODO: cleanup *)
  let renderCodeContext ~missing (src : string) startPos endPos =
    let open Lexing in
    let startCol = (startPos.pos_cnum - startPos.pos_bol) in
    let endCol = endPos.pos_cnum - startPos.pos_cnum + startCol in
    let startLine = max 1 (startPos.pos_lnum - 2) in (* 2 lines before *)
    let lines =  String.split_on_char '\n' src in
    let endLine =
      let len = List.length lines in
      min len (startPos.pos_lnum + 3) (* 2 lines after *)
    in
    let lines =
      lines
      |> drop startLine
      |> take (endLine - startLine)
      |> Array.of_list
    in

    let renderLine x ix =
      let x = if ix = startPos.pos_lnum then
          begin match missing with
          | Some len -> x ^ (String.make 10 ' ')
          | None -> x
          end
        else
          x
      in

      let open TerminalDoc in
      let rowNr =
        let txt = string_of_int ix in
        let len = String.length txt in
        if ix = startPos.pos_lnum then
          highlight ~from:0 ~len txt
        else txt
      in
      let len =
        let len = if endCol >= 0 then
          endCol - startCol
        else
          1
        in
        if (startCol + len) > String.length x then String.length x - startCol - 1 else len
      in
      let line =
        if ix = startPos.pos_lnum then
          begin match missing with
          | Some len ->
            underline
              ~from:(
              startCol + String.length (String.length (string_of_int ix) |> string_of_int) + 5
              ) ~len x
          | None ->
              let len = if startCol + len > String.length x then
                (String.length x) - startCol
              else
                len
              in
            text (highlight ~from:startCol ~len x)
          end
        else text x
      in
      group ~break:Never
        (append
          (append (text rowNr) (text " │"))
          (indent 2 line))
    in

    let reportDoc = ref TerminalDoc.nil in

    let linesLen = Array.length lines in
    for i = 0 to (linesLen - 1) do
      let line = Array.get lines i in
      reportDoc :=
        let open TerminalDoc in
        let ix = startLine + i in
        group ~break:Always (append !reportDoc (renderLine line ix))
    done;

    TerminalDoc.toString ~width:80 !reportDoc

  type problem =
    | Unexpected of Token.t
    | Expected of (Token.t * Lexing.position * Grammar.t option)
    | Message of string
    | Uident
    | Lident
    | Unbalanced of Token.t

  type parseError = Lexing.position * problem
end

module Diagnostics: sig
  type t
  type category

  val unexpected: Token.t -> (Grammar.t * Lexing.position) list -> category
  val expected:  ?grammar:Grammar.t -> Lexing.position -> Token.t -> category
  val uident: Token.t -> category
  val lident: Token.t -> category
  val unclosedString: category
  val unclosedTemplate: category
  val unclosedComment: category
  val unknownUchar: int -> category
  val message: string -> category
  val unbalanced: Token.t -> category

  val make:
    filename: string
    -> startPos: Lexing.position
    -> endPos: Lexing.position
    -> category
    -> t

  val makeReport: t list -> string -> string


end = struct
  type category =
    | Unexpected of (Token.t * ((Grammar.t * Lexing.position) list))
    | Expected of (Grammar.t option * Lexing.position (* prev token end*) * Token.t)
    | Message of string
    | Uident of Token.t
    | Lident of Token.t
    | UnclosedString
    | UnclosedTemplate
    | UnclosedComment
    | UnknownUchar of int
    | Unbalanced of Token.t

  let stringOfCategory = function
    | Unexpected _ -> "unexpected"
    | Expected _ -> "expected"
    | Message txt -> txt
    | Uident _ -> "uident"
    | Lident _ -> "lident"
    | UnclosedString -> "unclosed string"
    | UnclosedTemplate -> "unclosed template"
    | UnclosedComment -> "unclosed comment"
    | Unbalanced _ -> "unbalanced"
    | UnknownUchar _ -> "unknown rune"

  type t = {
    filename: string;
    startPos: Lexing.position;
    endPos: Lexing.position;
    category: category;
  }

  let defaultUnexpected token =
    "I'm not sure what to parse here when looking at \"" ^ (Token.toString token) ^ "\"."

  let toString t src =
    let open Lexing in
    let  startchar = t.startPos.pos_cnum - t.startPos.pos_bol in
    let endchar = t.endPos.pos_cnum - t.startPos.pos_cnum + startchar in
    let locationInfo =
      Printf.sprintf (* ReasonLanguageServer requires the following format *)
        "File \"%s\", line %d, characters %d-%d:"
        t.filename
        t.startPos.pos_lnum
        startchar
        endchar
    in
    let code =
      let missing = match t.category with
      | Expected (_, _, t) ->
        Some (String.length (Token.toString t))
      | _ -> None
      in
      Reporting.renderCodeContext ~missing src t.startPos t.endPos
    in
    let explanation = match t.category with
    | Uident currentToken ->
      begin match currentToken with
      | Lident lident ->
        let guess = String.capitalize_ascii lident in
        "Did you mean `" ^ guess ^"` instead of `" ^ lident ^ "`?"
      | t when Token.isKeyword t ->
        let token = Token.toString t in
        "`" ^ token ^ "` is a reserved keyword. Try `" ^ token ^ "_` or `_" ^ token ^ "` instead"
      | _ ->
        "At this point, I'm looking for an uppercased identifier like `Belt` or `Array`"
      end
    | Lident currentToken ->
      begin match currentToken with
      | Uident uident ->
        let guess = String.uncapitalize_ascii uident in
        "Did you mean `" ^ guess ^"` instead of `" ^ uident ^ "`?"
      | t when Token.isKeyword t ->
        let token = Token.toString t in
        "`" ^ token ^ "` is a reserved keyword. Try `" ^ token ^ "_` or `_" ^ token ^ "` instead"
      | _ ->
        "I'm expecting an lowercased identifier like `name` or `age`"
      end
    | Message txt -> txt
    | UnclosedString ->
      "This string is missing a double quote at the end"
    | UnclosedTemplate ->
      "Did you forget to close this template expression with a backtick?"
    | UnclosedComment ->
      "This comment seems to be missing a closing `*/`"
    | UnknownUchar uchar ->
      begin match uchar with
      | 94 (* ^ *) ->
        "Hmm, not sure what I should do here with this character.\nIf you're trying to deref an expression, use `foo.contents` instead."
      | _ ->
        "Hmm, I have no idea what this character means…"
      end
    | Unbalanced t ->
      "Closing \"" ^ (Token.toString t) ^ "\" seems to be missing."
    | Expected (context, _, t) ->
      let hint = match context with
      | Some grammar -> "It signals the start of " ^ (Grammar.toString grammar)
      | None -> ""
      in
      "Did you forget a `" ^ (Token.toString t) ^ "` here? " ^ hint
    | Unexpected (t, breadcrumbs) ->
      let name = (Token.toString t) in
      begin match breadcrumbs with
      | (AtomicTypExpr, _)::breadcrumbs ->
          begin match breadcrumbs, t with
          | (StringFieldDeclarations, _) :: _, (String _ | At | Rbrace | Comma | Eof) ->
              "I'm missing a type here"
          | _, t when Grammar.isStructureItemStart t || t = Eof ->
              "Missing a type here"
          | _ ->
            defaultUnexpected t
          end
      | (ExprOperand, _)::breadcrumbs ->
          begin match breadcrumbs, t with
          | (ExprBlock, _) :: _, Rbrace ->
            "It seems that this expression block is empty"
          | (ExprBlock, _) :: _, Bar -> (* Pattern matching *)
            "Looks like there might be an expression missing here"
          | (ExprSetField, _) :: _, _ ->
            "It seems that this record field mutation misses an expression"
          | (ExprArrayMutation, _) :: _, _ ->
            "Seems that an expression is missing, with what do I mutate the array?"
          | ((ExprBinaryAfterOp _ | ExprUnary), _) ::_, _ ->
            "Did you forget to write an expression here?"
          | (Grammar.LetBinding, _)::_, _ ->
            "This let-binding misses an expression"
          | _::_, Rbracket ->
            "Missing expression"
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
          name ^ " is a reserved keyword, Try `" ^ name ^ "_` or `_" ^ name ^ "` instead"
        else
        "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
      end
    in
    Printf.sprintf "%s\n\n%s\n\n%s\n\n" locationInfo code explanation

  let make ~filename ~startPos ~endPos category = {
    filename;
    startPos;
    endPos;
    category
  }

  let makeReport diagnostics src =
    List.fold_left (fun report diagnostic ->
      report ^ (toString diagnostic src) ^ "\n"
    ) "\n" (List.rev diagnostics)

  let print {category} =
    prerr_endline (stringOfCategory category)

  let unexpected token context =
    Unexpected(token, context)

  let expected ?grammar pos token =
    Expected(grammar, pos, token)

  let uident currentToken = Uident currentToken
  let lident currentToken = Lident currentToken
  let unclosedString = UnclosedString
  let unclosedComment = UnclosedComment
  let unclosedTemplate = UnclosedTemplate
  let unknownUchar code = UnknownUchar code
  let message txt = Message txt
  let unbalanced token = Unbalanced token
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
    mutable err:
      startPos: Lexing.position
      -> endPos: Lexing.position
      -> Diagnostics.category
      -> unit;
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
      err = (fun ~startPos:_ ~endPos:_ _ -> ());
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
    let err = scanner.err in
    let ch = scanner.ch in
    let offset = scanner.offset in
    let rdOffset = scanner.rdOffset in
    let lineOffset = scanner.lineOffset in
    let lnum = scanner.lnum in
    let mode = scanner.mode in
    let res = callback scanner in
    scanner.err <- err;
    scanner.ch <- ch;
    scanner.offset <- offset;
    scanner.rdOffset <- rdOffset;
    scanner.lineOffset <- lineOffset;
    scanner.lnum <- lnum;
    scanner.mode <- mode;
    res


  let skipWhitespace scanner =
    let rec scan () =
      if scanner.ch == CharacterCodes.space || scanner.ch == CharacterCodes.tab then (
        next scanner;
        scan()
      ) else if CharacterCodes.isLineBreak scanner.ch then (
        scanner.lineOffset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
        next scanner;
        scan()
      ) else (
        ()
      )
    in
    scan()

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

    let startPos = position scanner in
    let rec scan () =
      if scanner.ch == CharacterCodes.eof then
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedString
      else if scanner.ch == CharacterCodes.doubleQuote then (
        next scanner;
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
        scanner.lineOffset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedString;
        next scanner
      ) else (
        Buffer.add_char buffer (Char.chr scanner.ch);
        next scanner;
        scan ()
      )
    in
    scan ();
    Token.String (Buffer.contents buffer)

  let scanSingleLineComment ~startPos scanner =
    while not (CharacterCodes.isLineBreak scanner.ch) &&
      scanner.rdOffset < (Bytes.length scanner.src)
    do
      next scanner
    done;
    let endPos = position scanner in
    if CharacterCodes.isLineBreak scanner.ch then (
      scanner.lineOffset <- scanner.offset + 1;
      scanner.lnum <- scanner.lnum + 1;
    );
    next scanner;
    Token.Comment (
      Comment.makeSingleLineComment
        ~loc:(Location.{loc_start = startPos; loc_end = endPos; loc_ghost = false})
        (let startOff = startPos.pos_cnum + 2 in
          Bytes.sub_string scanner.src startOff (scanner.offset - 1 - startOff))
    )

  let scanMultiLineComment ~startPos scanner =
    let rec scan () =
      if scanner.ch == CharacterCodes.asterisk &&
         peek scanner == CharacterCodes.forwardslash then (
        next scanner;
        next scanner
      ) else if scanner.ch == CharacterCodes.eof then (
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedComment
      ) else (
        if CharacterCodes.isLineBreak scanner.ch then (
          scanner.lineOffset <- scanner.offset + 1;
          scanner.lnum <- scanner.lnum + 1;
        );
        next scanner;
        scan ()
      )
    in
    scan();
    Token.Comment (
      Comment.makeMultiLineComment
        ~loc:(Location.{loc_start = startPos; loc_end = (position scanner); loc_ghost = false})
        (let startOff = startPos.pos_cnum + 2 in
          Bytes.sub_string scanner.src startOff (scanner.offset - 2 - startOff))
    )

  let scanTemplate scanner =
    let startOff = scanner.offset in
    let startPos = position scanner in

    let rec scan () =
      if scanner.ch == CharacterCodes.eof then (
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedTemplate;
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
        if CharacterCodes.isLineBreak scanner.ch then (
          scanner.lineOffset <- scanner.offset + 1;
          scanner.lnum <- scanner.lnum + 1;
        );
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
          scanSingleLineComment ~startPos scanner
        ) else if (scanner.ch == CharacterCodes.asterisk) then (
          next scanner;
          scanMultiLineComment ~startPos scanner
        ) else if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.ForwardslashDot
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
        ) else if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.AsteriskDot
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
        let endPos = position scanner in
        scanner.err ~startPos ~endPos (Diagnostics.unknownUchar ch);
        let (_, _, token) = scan scanner in
        token
      )
    end in
    let endPos = position scanner in
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

module Parser = struct
  type directive = DirDisabled | DirIfTrue | DirIfFalse

  type directiveNode =
    | DirBool of bool
    | DirString of string
    | DirInt of int
    | DirFloat of float
    | DirBinary of (Token.t * directiveNode * directiveNode)
    | DirError


  type t = {
    mutable scanner: Scanner.t;
    mutable token: Token.t;
    mutable startPos: Lexing.position;
    mutable endPos: Lexing.position;
    mutable prevEndPos: Lexing.position;
    mutable breadcrumbs: (Grammar.t * Lexing.position) list;
    mutable errors: Reporting.parseError list;
    mutable diagnostics: Diagnostics.t list;
    mutable directive: directive;
    mutable comments: Comment.t list;
  }

  let err ?startPos ?endPos p error =
    let d = Diagnostics.make
      ~filename:p.scanner.filename
      ~startPos:(match startPos with | Some pos -> pos | None -> p.startPos)
      ~endPos:(match endPos with | Some pos -> pos | None -> p.endPos)
      error
    in
    p.diagnostics <- d::p.diagnostics

  let debugBreadcrumbs bcs =
    print_endline "current breadcrumbs:";
    List.iter (fun (grammar, _) ->
      print_endline (Grammar.toString grammar)) bcs;
    print_endline "================="


  let dropLastDiagnostic p =
    match p.diagnostics with
    | _::ds -> p.diagnostics <- ds
    | [] -> ()

   let isDirectiveOp = function
   | Token.LessThan
   | GreaterThan
   | GreaterEqual
   | LessEqual
   | EqualEqual
   | EqualEqualEqual
   | BangEqual
   | BangEqualEqual -> true
   | _ -> false

  let rec advance p =
    let (startPos, endPos, token) = Scanner.scan p.scanner in
    match token with
    | Comment c ->
        p.comments <- c::p.comments;
        advance p
    | _ ->
      p.token <- token;
      p.prevEndPos <- p.endPos;
      p.startPos <- startPos;
      p.endPos <- endPos

  let rec skipTokens p cont =
    advance p;
    if p.token = Eof then
      err p (Diagnostics.message "Missing #endif")
    else if p.token = Hash then (
      advance p;
      match p.token with
      | Lident "endif" ->
        p.directive <- DirDisabled;
        cont p
      | Lident "elseif" ->
        advance p;
        let v = parseDirectiveExpr p in
        interpretDirective p v cont
      | Else ->
        if p.directive = DirIfTrue then skipTokens p cont else cont p
      | _ -> skipTokens p cont
    )
    else
      skipTokens p cont

  and parseDirectiveOperand p =
    match p.token with
    | Uident uident ->
        advance p;
        DirString (
        match Sys.getenv_opt uident with
        | Some v -> v
        | None -> ""
      )
    | True -> advance p; DirBool true
    | False -> advance p; DirBool false
    | Int n -> advance p; DirInt (int_of_string n)
    | Float n -> advance p; DirFloat (float_of_string n)
    | String s -> advance p; DirString s
    | Lparen -> advance p; parseDirectiveExpr p
    | _ -> DirError

  and parseDirectiveBinaryExpr p prec =
    let rec loop a =
      let token = p.token in
      if isDirectiveOp token then
        let tokenPrec = Token.precedence token in
        let () = advance p in
        if tokenPrec < prec then a
        else (
          let b = parseDirectiveBinaryExpr p (tokenPrec + 1) in
          let expr = DirBinary (token, a, b) in
          loop expr
        )
      else
        a
    in
    let operand = parseDirectiveOperand p in
    loop operand

 and eval exp =
   let f_of_op = function
   | Token.LessThan -> (<)
   | GreaterThan -> (>)
   | GreaterEqual -> (>=)
   | LessEqual -> (<=)
   | EqualEqual -> (=)
   | EqualEqualEqual -> (==)
   | BangEqual -> (<>)
   | BangEqualEqual -> (!=)
   | _ -> assert false
   in
   match exp with
    | DirBool b -> b
    | DirInt n -> n <> 0
    | DirFloat n -> n <> 0.0
    | DirBinary (op, DirString a, DirString b) ->
      (f_of_op op) a b
    | DirBinary (op, DirInt a, DirInt b) ->
      (f_of_op op) a b
    | DirBinary (op, DirFloat a, DirFloat b) ->
      (f_of_op op) a b
    | _ -> false

  and parseDirectiveExpr p =
    parseDirectiveBinaryExpr p 1

  and interpretDirective p v cont =
    if eval v then (
      p.directive <- DirIfTrue
    ) else (
      p.directive <- DirIfFalse;
      skipTokens p cont
    )

  let rec next p =
    advance p;
    match p.token with
    | Hash ->
      advance p;
      begin match (p.token, p.directive) with
      | If, DirDisabled ->
        advance p;
        let v = parseDirectiveExpr p in
        interpretDirective p v next
      | (Lident "elseif" | Else), DirIfTrue ->
        advance p;
        let rec skip p =
          let token = p.token in
          if token = Eof then
            ()
          else (
            advance p;
            if token = Hash then (
              match p.token with
              | Lident "endif" ->
                p.directive <- DirDisabled;
                next p
              | _ -> skip p
            ) else (
              skip p
            )
          )
        in
        skip p
      | Else, DirIfFalse ->
        advance p
      | Lident "endif", (DirIfTrue | DirIfFalse) ->
        p.directive <- DirDisabled;
        next p
      | _ -> ()
      end
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
      directive = DirDisabled;
      comments = [];
    } in
    parserState.scanner.err <- (fun ~startPos ~endPos error ->
      let diagnostic = Diagnostics.make
        ~filename
        ~startPos
        ~endPos
        error
      in
      parserState.diagnostics <- diagnostic::parserState.diagnostics
    );
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

  (* TODO: should we bail if there's too much stuff going wrong? *)
  exception Exit

  let expect ?grammar token p =
    if p.token = token then
      next p
    else
      let error = Diagnostics.expected ?grammar p.prevEndPos token in
      err ~startPos:p.prevEndPos p error

  (* Don't use immutable copies here, it trashes certain heuristics
   * in the ocaml compiler, resulting in massive slowdowns of the parser *)
  let lookahead p callback =
    let err = p.scanner.err in
    let ch = p.scanner.ch in
    let offset = p.scanner.offset in
    let rdOffset = p.scanner.rdOffset in
    let lineOffset = p.scanner.lineOffset in
    let lnum = p.scanner.lnum in
    let mode = p.scanner.mode in
    let token = p.token in
    let startPos = p.startPos in
    let endPos = p.endPos in
    let prevEndPos = p.prevEndPos in
    let breadcrumbs = p.breadcrumbs in
    let errors = p.errors in
    let diagnostics = p.diagnostics in
    let directive = p.directive in
    let comments = p.comments in

    let res = callback p in

    p.scanner.err <- err;
    p.scanner.ch <- ch;
    p.scanner.offset <- offset;
    p.scanner.rdOffset <- rdOffset;
    p.scanner.lineOffset <- lineOffset;
    p.scanner.lnum <- lnum;
    p.scanner.mode <- mode;
    p.token <- token;
    p.startPos <- startPos;
    p.endPos <- endPos;
    p.prevEndPos <- prevEndPos;
    p.breadcrumbs <- breadcrumbs;
    p.errors <- errors;
    p.diagnostics <- diagnostics;
    p.directive <- directive;
    p.comments <- comments;

    res
end


module NapkinScript = struct
  let mkLoc startLoc endLoc = Location.{
    loc_start = startLoc;
    loc_end = endLoc;
    loc_ghost = false;
  }


  module Recover = struct
    type action =
      | Retry
      | Abort

    let defaultStructureItem () =
      let id = Location.mknoloc "napkinscript.strItemHole" in
      Ast_helper.Str.extension (id, PStr [])

    let defaultSignatureItem () =
      let id = Location.mknoloc "napkinscript.SigItemHole" in
      Ast_helper.Sig.extension (id, PStr [])

    let defaultExpr () =
      let id = Location.mknoloc "napkinscript.exprhole" in
      Ast_helper.Exp.mk (Pexp_extension (id, PStr []))

    let defaultType () =
      let id = Location.mknoloc "napkinscript.typehole" in
      Ast_helper.Typ.extension (id, PStr [])

    let defaultPattern () =
      let id = Location.mknoloc "napkinscript.patternhole" in
      Ast_helper.Pat.extension (id, PStr [])
      (* Ast_helper.Pat.any  () *)

    let defaultModuleExpr () = Ast_helper.Mod.structure []
    let defaultModuleType () = Ast_helper.Mty.signature []

    (* let recoverUident p = *)
      (* match p.Parser.token with *)
      (* | Lident lident -> *)
      (* | t when Token.isKeyword t -> *)
      (* | _ -> *)

    let recoverEqualGreater p =
      Parser.expect EqualGreater p;
      match p.Parser.token with
      | MinusGreater -> Parser.next p
      | _ -> ()

    let shouldAbortListParse p =
      let rec check breadcrumbs i =
        if i > 100 then
          raise (InfiniteLoop (p.Parser.startPos, p.token))
        else
        match breadcrumbs with
        | [] -> false
        | (grammar, _)::rest ->
          if Grammar.isPartOfList grammar p.Parser.token then
            true
          else
            check rest (i + 1)
      in
      check p.breadcrumbs 0

    let recoverLident p =
      let counter = ref(0) in
      if Token.isKeyword p.Parser.token
         && p.Parser.prevEndPos.pos_lnum == p.startPos.pos_lnum
      then (
        Parser.err p (Diagnostics.lident p.Parser.token);
        Parser.next p;
        Abort
      ) else (
        while not (shouldAbortListParse p) && !counter < 100 do
          let () = counter := !counter + 1 in
          Parser.next p
        done;
        if !counter > 100 then
          raise (InfiniteLoop (p.startPos, p.token))
        else (
          match p.Parser.token with
          | Lident _ -> Retry
          | _ -> Abort
        )
      )

    let skipTokensAndMaybeRetry p ~isStartOfGrammar =
      if Token.isKeyword p.Parser.token
         && p.Parser.prevEndPos.pos_lnum == p.startPos.pos_lnum
      then (
        Parser.next p;
        Abort
      ) else (
        while not (shouldAbortListParse p) do
          Parser.next p
        done;
        if isStartOfGrammar p.Parser.token then
          Retry
        else
          Abort
      )
  end

  module ErrorMessages = struct
    let listPatternSpread = "List pattern matches only supports one `...` spread, at the end.
Explanation: a list spread at the tail is efficient, but a spread in the middle would create new list(s); out of performance concern, our pattern matching currently guarantees to never create new intermediate data."

    let recordPatternSpread = "Record's `...` spread is not supported in pattern matches.
Explanation: you can't collect a subset of a record's field into its own record, since a record needs an explicit declaration and that subset wouldn't have one.
Solution: you need to pull out each field you want explicitly."

  let recordPatternUnderscore = "Record patterns only supports one `_`, at the end."
  end


  let jsxAttr = (Location.mknoloc "JSX", Parsetree.PStr [])
  let uncurryAttr = (Location.mknoloc "bs", Parsetree.PStr [])

  type typDefOrExt =
    | TypeDef of (Asttypes.rec_flag * Parsetree.type_declaration list)
    | TypeExt of Parsetree.type_extension

  type labelledParameter =
    | TermParameter of
        (bool * Parsetree.attributes * Asttypes.arg_label * Parsetree.expression option *
        Parsetree.pattern * Lexing.position)
    | TypeParameter of (string Location.loc list * Lexing.position)

  type recordPatternItem =
    | PatUnderscore
    | PatField of (Ast_helper.lid * Parsetree.pattern)

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
      () (* TODO: how do report errors here? *)
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
          | EqualGreater | Lbrace ->
              true
          | Colon when not inTernary -> true
          | _ ->
            Parser.next state;
            (* error recovery, peek at the next token,
             * (elements, providerId] => {
             *  in the example above, we have an unbalanced ] here
             *)
            begin match state.Parser.token with
            | EqualGreater -> true
            | _ -> false
            end
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
      | Tilde -> true
      | _ -> false
    )

  let buildLongident words = match List.rev words with
    | [] -> assert false
    | hd::tl -> List.fold_left (fun p s -> Longident.Ldot (p, s)) (Lident hd) tl

  let makeInfixOperator p token startPos endPos =
    let stringifiedToken =
      if token = Token.MinusGreater then "|."
      else if token = Token.PlusPlus then "^"
      else if token = Token.BangEqual then "<>"
      else if token = Token.BangEqualEqual then "!="
      else if token = Token.Equal then (
        (* TODO: could have a totally different meaning like x->fooSet(y)*)
        Parser.err ~startPos ~endPos p (
          Diagnostics.message "Did you mean `==` here?"
        );
        "="
      ) else if token = Token.EqualEqual then "="
      else if token = Token.EqualEqualEqual then "=="
      else Token.toString token
    in
    let loc = mkLoc startPos endPos in
    let operator = Location.mkloc
      (Longident.Lident stringifiedToken) loc
    in
    Ast_helper.Exp.ident ~loc operator

  let negateString s =
    if String.length s > 0 && s.[0] = '-'
    then String.sub s 1 (String.length s - 1)
    else "-" ^ s

  let makeUnaryExpr startPos tokenEnd token operand =
    match token, operand.Parsetree.pexp_desc with
    | (Token.Plus | PlusDot), Pexp_constant((Pconst_integer _ | Pconst_float _)) ->
      operand
    | (Minus | MinusDot), Pexp_constant(Pconst_integer (n,m)) ->
      {operand with pexp_desc = Pexp_constant(Pconst_integer (negateString n,m))}
    | (Minus | MinusDot), Pexp_constant(Pconst_float (n,m)) ->
      {operand with pexp_desc = Pexp_constant(Pconst_float (negateString n,m))}
    | (Token.Plus | PlusDot | Minus | MinusDot ), _ ->
      let tokenLoc = mkLoc startPos tokenEnd in
      let operator = "~" ^ Token.toString token in
      Ast_helper.Exp.apply
        ~loc:(mkLoc startPos operand.Parsetree.pexp_loc.loc_end)
        (Ast_helper.Exp.ident ~loc:tokenLoc
          (Location.mkloc (Longident.Lident operator) tokenLoc))
        [Nolabel, operand]
    | Token.Bang, _ ->
      let tokenLoc = mkLoc startPos tokenEnd in
      Ast_helper.Exp.apply
        ~loc:(mkLoc startPos operand.Parsetree.pexp_loc.loc_end)
        (Ast_helper.Exp.ident ~loc:tokenLoc
          (Location.mkloc (Longident.Lident "not") tokenLoc))
        [Nolabel, operand]
    | Token.Band, _ ->
      let tokenLoc = mkLoc startPos tokenEnd in
      let operator =
        Ast_helper.Exp.ident ~loc:tokenLoc
          (Location.mkloc (Longident.Lident "!") tokenLoc)
      in
      Ast_helper.Exp.apply
        ~loc:(mkLoc startPos operand.Parsetree.pexp_loc.loc_end)
        operator
        [Nolabel, operand]
    | _ ->
      operand

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
    Ast_helper.Typ.constr ~loc ?attrs jsDotTCtor [obj]

  (* TODO: diagnostic reporting *)
  let lidentOfPath longident =
    match Longident.flatten longident |> List.rev with
    | [] -> ""
    | ident::_ -> ident

  let makeNewtypes ~loc newtypes exp =
    List.fold_right (fun newtype exp ->
      Ast_helper.Exp.mk ~loc (Pexp_newtype (newtype, exp))
    ) newtypes exp

  (* locally abstract types syntax sugar
   * Transforms
   *  let f: type t u v. = (foo : list</t, u, v/>) => ...
   * into
   *  let f = (type t u v. foo : list</t, u, v/>) => ...
   *)
  let wrap_type_annotation ~loc newtypes core_type body =
    let exp = makeNewtypes ~loc newtypes
      (Ast_helper.Exp.constraint_ ~loc body core_type)
    in
    let typ = Ast_helper.Typ.poly ~loc newtypes
      (Ast_helper.Typ.varify_constructors newtypes core_type)
    in
    (exp, typ)

  (**
    * process the occurrence of _ in the arguments of a function application
    * replace _ with a new variable, currently __x, in the arguments
    * return a wrapping function that wraps ((__x) => ...) around an expression
    * e.g. foo(_, 3) becomes (__x) => foo(__x, 3)
    *)
  let processUnderscoreApplication args =
    let open Parsetree in
    let exp_question = ref None in
    let hidden_var = "__x" in
    let check_arg ((lab, exp) as arg) =
      match exp.pexp_desc with
      | Pexp_ident ({ txt = Lident "_"} as id) ->
        let new_id = Location.mkloc (Longident.Lident hidden_var) id.loc in
        let new_exp = Ast_helper.Exp.mk (Pexp_ident new_id) ~loc:exp.pexp_loc in
        exp_question := Some new_exp;
        (lab, new_exp)
      | _ ->
        arg
    in
    let args = List.map check_arg args in
    let wrap exp_apply =
      match !exp_question with
      | Some {pexp_loc=loc} ->
        let pattern = Ast_helper.Pat.mk (Ppat_var (Location.mkloc hidden_var loc)) ~loc in
        Ast_helper.Exp.mk (Pexp_fun (Nolabel, None, pattern, exp_apply)) ~loc
      | None ->
        exp_apply
    in
    (args, wrap)

  let rec parseLident p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      (ident, loc)
    | t ->
      begin match Recover.recoverLident p with
      | Retry ->
        parseLident p
      | Abort ->
        ("_", mkLoc startPos p.prevEndPos)
      end

  (* Ldot (Ldot (Lident "Foo", "Bar"), "baz") *)
  let parseValuePath p =
    let startPos = p.Parser.startPos in
    let rec aux p path =
      match p.Parser.token with
      | List -> Longident.Ldot(path, "list")
      | Lident ident -> Longident.Ldot(path, ident)
      | Uident uident ->
        Parser.next p;
        Parser.expect Dot p;
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
      Parser.expect Dot p;
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
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Location.mknoloc path
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
      | t ->
        Parser.err p (Diagnostics.uident t);
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
    | t ->
      Parser.err p (Diagnostics.uident t);
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
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Pconst_string("", None)
    in
    Parser.next p;
    constant

  let parseCommaDelimitedList p ~grammar ~closing ~f =
    Parser.leaveBreadcrumb p grammar;
    let rec loop nodes =
      if Grammar.isListElement grammar p.Parser.token then (
        let node = f p in
        if Parser.optional p Comma then
          loop (node::nodes)
        else if p.token = closing || p.token = Eof then
          List.rev (node::nodes)
        else (
          Parser.expect Comma p;
          if p.token = Semicolon then Parser.next p;
          loop (node::nodes)
        )
      ) else if p.token = Eof then (
        List.rev nodes
      ) else if p.token = closing then (
        List.rev nodes
      ) else (
        if Recover.shouldAbortListParse p then (
          (* TODO: is it ok to just randomly drop the expect , ???*)
          Parser.dropLastDiagnostic p; (* we don't expect a `,` *)
          List.rev nodes
        ) else (
          (* TODO: is it ok to just randomly drop the expect , ???*)
          Parser.dropLastDiagnostic p; (* we don't expect a `,` *)
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        )
      )
    in
    let nodes = loop [] in
    Parser.eatBreadcrumb p;
    nodes

  let parseCommaDelimitedReversedList p ~grammar ~closing ~f =
    Parser.leaveBreadcrumb p grammar;
    let rec loop nodes =
      if Grammar.isListElement grammar p.Parser.token then (
        let node = f p in
        if Parser.optional p Comma then
          loop (node::nodes)
        else if p.token = closing || p.token = Eof then
          node::nodes
        else (
          Parser.expect Comma p;
          if p.token = Semicolon then Parser.next p;
          loop (node::nodes)
        )
      ) else if p.token = Eof then (
        nodes
      ) else if p.token = closing then (
        nodes
      )  else (
        if Recover.shouldAbortListParse p then (
          Parser.dropLastDiagnostic p; (* we don't expect a `,` *)
          nodes
        ) else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        )
      )
    in
    let nodes = loop [] in
    Parser.eatBreadcrumb p;
    nodes

  let parseDelimitedList p ~grammar ~closing ~f =
    Parser.leaveBreadcrumb p grammar;
    let rec loop nodes =
      if p.Parser.token = Token.Eof || p.token = closing then
        List.rev nodes
      else if Grammar.isListElement grammar p.token then
        let node = f p in
        loop (node::nodes)
      else (* trouble *) (
        if Recover.shouldAbortListParse p then
          List.rev nodes
        else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        )
      )
    in
    let nodes = loop [] in
    Parser.eatBreadcrumb p;
    nodes

  let parseList p ~grammar ~f =
    Parser.leaveBreadcrumb p grammar;
    let rec loop nodes =
      if p.Parser.token = Token.Eof then
        List.rev nodes
      else if Grammar.isListElement grammar p.token then
        let node = f p in
        loop (node::nodes)
      else (* trouble *) (
        if Recover.shouldAbortListParse p then
          List.rev nodes
        else (
          Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
          Parser.next p;
          loop nodes
        )
      )
    in
    let nodes = loop [] in
    Parser.eatBreadcrumb p;
    nodes

  (* let-binding	::=	pattern =  expr   *)
     (* ∣	 value-name  { parameter }  [: typexpr]  [:> typexpr] =  expr   *)
     (* ∣	 value-name :  poly-typexpr =  expr   *)

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
    | Int _ | String _ | Float _ ->
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
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      begin match Recover.skipTokensAndMaybeRetry p ~isStartOfGrammar:Grammar.isAtomicPatternStart with
      | Abort ->
        Recover.defaultPattern()
      | Retry ->
        parsePattern p
      end
    in
    let pat = if alias then parseAliasPattern ~attrs pat p else pat in
    if or_ then parseOrPattern pat p else pat

  (* alias ::= pattern as lident *)
  and parseAliasPattern ~attrs pattern p =
    match p.Parser.token with
    | As ->
      Parser.next p;
      let (name, loc) = parseLident p in
      let name = Location.mkloc name loc in
      Ast_helper.Pat.alias
        ~loc:({pattern.ppat_loc with loc_end = p.prevEndPos})
        ~attrs
         pattern
         name
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
      let (name, loc) = parseLident p in
      let name = Location.mkloc name loc in
      let aliasPattern = Ast_helper.Pat.alias
        ~loc:(mkLoc startPos p.prevEndPos)
        pattern
        name
      in
      (Location.mkloc label.txt (mkLoc startPos aliasPattern.ppat_loc.loc_end), aliasPattern)
		| _ ->
    (Location.mkloc label.txt (mkLoc startPos pattern.ppat_loc.loc_end), pattern)

   (* TODO: there are better representations than PatField|Underscore ? *)
  and parseRecordPatternItem p =
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      (true, PatField (parseRecordPatternField p))
    | Uident _ | Lident _ ->
      (false, PatField (parseRecordPatternField p))
    | Underscore ->
      Parser.next p;
      (false, PatUnderscore)
    | _ ->
      (false, PatField (parseRecordPatternField p))

  and parseRecordPattern ~attrs p =
    let startPos = p.startPos in
    Parser.expect Lbrace p;
    let rawFields =
      parseCommaDelimitedReversedList p
       ~grammar:PatternRecord
       ~closing:Rbrace
       ~f:parseRecordPatternItem
    in
    Parser.expect Rbrace p;
    let (fields, closedFlag) =
      let (rawFields, flag) = match rawFields with
      | (_hasSpread, PatUnderscore)::rest ->
        (rest, Asttypes.Open)
      | rawFields ->
        (rawFields, Asttypes.Closed)
      in
      List.fold_left (fun (fields, flag) curr ->
        let (hasSpread, field) = curr in
        match field with
        | PatField field ->
          if hasSpread then (
            let (_, pattern) = field in
            Parser.err ~startPos:pattern.Parsetree.ppat_loc.loc_start p (Diagnostics.message ErrorMessages.recordPatternSpread)
          );
          (field::fields, flag)
        | PatUnderscore ->
          (fields, flag)
      ) ([], flag) rawFields
    in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.record ~loc ~attrs fields closedFlag

  and parseTuplePattern ~attrs p =
    let startPos = p.startPos in
    Parser.expect Forwardslash p;
    let patterns =
      parseCommaDelimitedList p
        ~grammar:Grammar.PatternList
        ~closing:Forwardslash
        ~f:parseConstrainedPattern
    in
    Parser.expect Forwardslash p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.tuple ~loc ~attrs patterns

  and parsePatternListItem p =
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      (true, parseConstrainedPattern p)
    | _ ->
      (false, parseConstrainedPattern p)

  and parseListPattern ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect List p;
    Parser.expect Lparen p;
    let listPatterns =
      parseCommaDelimitedReversedList p
        ~grammar:Grammar.PatternOcamlList
        ~closing:Rparen
        ~f:parsePatternListItem
    in
    Parser.expect Rparen p;
    let endPos = p.prevEndPos in
    let loc = mkLoc startPos endPos in
    let filterSpread (hasSpread, pattern) =
      if hasSpread then (
        Parser.err ~startPos:pattern.Parsetree.ppat_loc.loc_start p (Diagnostics.message ErrorMessages.listPatternSpread);
        pattern
      ) else
        pattern
    in
    match listPatterns with
    | (true, pattern)::patterns ->
      let patterns = patterns |> List.map filterSpread |> List.rev in
      makeListPattern loc patterns (Some pattern)
    | patterns ->
      let patterns = patterns |> List.map filterSpread |> List.rev in
      makeListPattern loc patterns None

  and parseArrayPattern ~attrs p =
    let startPos = p.startPos in
    Parser.expect Lbracket p;
    let patterns =
      parseCommaDelimitedList
        p ~grammar:Grammar.PatternList ~closing:Rbracket ~f:parseConstrainedPattern
    in
    Parser.expect Rbracket p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.array ~loc ~attrs patterns

  and parseConstructorPatternArgs p constr startPos attrs =
    let lparen = p.startPos in
    Parser.expect Lparen p;
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
    let expr = parseOperandExpr ~context p in
    let expr = parseBinaryExpr ~context ~a:expr p 1 in
    parseTernaryExpr expr p

  (* expr ? expr : expr *)
  and parseTernaryExpr leftOperand p =
    match p.Parser.token with
    | Question ->
      Parser.leaveBreadcrumb p Grammar.Ternary;
      Parser.next p;
      let trueBranch = parseExpr ~context:TernaryTrueBranchExpr p in
      Parser.expect Colon p;
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
    Parser.expect EqualGreater p;
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
    let arrowExpr =
      List.fold_right (fun parameter expr ->
        match parameter with
        | TermParameter (uncurried, attrs, lbl, defaultExpr, pat, startPos) ->
          let attrs = if uncurried then uncurryAttr::attrs else attrs in
          Ast_helper.Exp.fun_ ~loc:(mkLoc startPos endPos) ~attrs lbl defaultExpr pat expr
        | TypeParameter (newtypes, startPos) ->
          makeNewtypes ~loc:(mkLoc startPos endPos) newtypes expr
      ) parameters body
    in
    {arrowExpr with pexp_loc = {arrowExpr.pexp_loc with loc_start = startPos}}

	(*
   * uncurried_parameter ::=
   *   | . parameter
   *
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
    if p.Parser.token = Typ then (
      Parser.next p;
      let lidents = parseLidentList p in
      TypeParameter (lidents, startPos)
    ) else (
    (* TODO: this is a shift reduce conflict, we reduce here :)
     * UPDATE: not sure if we should do this in the non-labelled arg case
     * let f = ( @attr x ) => x + 1; -> on pattern x or on Pexp_fun? *)
    let uncurried = Parser.optional p Token.Dot in
    let attrs = parseAttributes p in
    let (lbl, pat) = match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let (lblName, _loc) = parseLident p in
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
      | t ->
        Parser.err p (Diagnostics.unexpected t p.breadcrumbs);
        let loc = mkLoc startPos p.prevEndPos in
        (
          Asttypes.Labelled lblName,
          Ast_helper.Pat.var ~loc (Location.mkloc lblName loc)
        )
      end
    | _ ->
      (Asttypes.Nolabel, parseConstrainedPattern p)
    in
    let parameter = match p.Parser.token with
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
        (uncurried, attrs, lbl, None, pat, startPos)
      | _ ->
        let expr = parseExpr p in
        (uncurried, attrs, lbl, Some expr, pat, startPos)
      end
    | _ ->
      (uncurried, attrs, lbl, None, pat, startPos)
    in
    TermParameter parameter
    )

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
      [TermParameter(
        false,
        [],
        Asttypes.Nolabel,
        None,
        Ast_helper.Pat.var ~loc (Location.mkloc ident loc),
        startPos
      )]
    | Underscore ->
      Parser.next p;
      let loc = mkLoc startPos p.Parser.prevEndPos in
      [TermParameter (false, [], Asttypes.Nolabel, None, Ast_helper.Pat.any ~loc (), startPos)]
    | Lparen ->
      Parser.next p;
      begin match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mkLoc startPos p.Parser.prevEndPos in
        let unitPattern = Ast_helper.Pat.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
        in
        [TermParameter (false, [], Asttypes.Nolabel, None, unitPattern, startPos)]
      | _ -> parseParameterList p
      end
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      []

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
        let errPos = p.prevEndPos in
        begin match Recover.skipTokensAndMaybeRetry p ~isStartOfGrammar:Grammar.isAtomicExprStart with
        | Abort ->
          Parser.err ~startPos:errPos p (Diagnostics.unexpected token p.breadcrumbs);
          Recover.defaultExpr ()
        | Retry -> parseAtomicExpr p
        end
    in
    Parser.eatBreadcrumb p;
    expr

  and parseBracketAccess p expr startPos =
    Parser.leaveBreadcrumb p Grammar.ExprArrayAccess;
    let lbracket = p.startPos in
    Parser.next p;
    let stringStart = p.startPos in
    match p.Parser.token with
    | String s ->
      Parser.next p;
      let stringEnd = p.prevEndPos in
      Parser.expect Rbracket p;
      let rbracket = p.prevEndPos in
      let e =
        let identLoc = mkLoc stringStart stringEnd in
        let loc = mkLoc lbracket rbracket in
        Ast_helper.Exp.apply ~loc
        (Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident "##") loc))
        [Nolabel, expr; Nolabel, (Ast_helper.Exp.ident ~loc:identLoc (Location.mkloc (Longident.Lident s) identLoc))]
      in
      let e = parsePrimaryExpr ~operand:e p in
      let equalStart = p.startPos in
      begin match p.token with
      | Equal ->
        Parser.next p;
        let equalEnd = p.prevEndPos in
        let rhsExpr = parseExpr p in
        let loc = mkLoc startPos rhsExpr.pexp_loc.loc_end in
        let operatorLoc = mkLoc equalStart equalEnd in
        Ast_helper.Exp.apply ~loc
          (Ast_helper.Exp.ident ~loc:operatorLoc (Location.mkloc (Longident.Lident "#=") operatorLoc))
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
      | Lbracket when noCall = false && p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
        parseBracketAccess p expr startPos
      | Lparen when noCall = false && p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
        loop p (parseCallExpr p expr)
      | _ -> expr
    in
    let expr = loop p e1 in
    {expr with pexp_loc = mkLoc startPos p.prevEndPos}


  (* a unary expression is an expression with only one operand and
   * unary operator. Examples:
   *   -1
   *   !condition
   *   -. 1.6
   *)
  and parseUnaryExpr p =
    let startPos = p.Parser.startPos in
    match p.Parser.token with
    | (Minus | MinusDot | Plus | PlusDot | Bang | Band) as token ->
      Parser.leaveBreadcrumb p Grammar.ExprUnary;
      let tokenEnd = p.endPos in
      Parser.next p;
      let operand = parseUnaryExpr p in
      let unaryExpr = makeUnaryExpr startPos tokenEnd token operand  in
      Parser.eatBreadcrumb p;
      unaryExpr
    | _ ->
      parsePrimaryExpr p

  (* Represents an "operand" in a binary expression.
   * If you have `a + b`, `a` and `b` both represent
   * the operands of the binary expression with opeartor `+` *)
  and parseOperandExpr ?(context=OrdinaryExpr) p =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributes p in
    let expr = match p.Parser.token with
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
    | Try ->
      parseTryExpression p
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
    let endPos = p.Parser.prevEndPos in
    {expr with
      pexp_attributes = expr.Parsetree.pexp_attributes @ attrs;
      pexp_loc = mkLoc startPos endPos}

  (* a binary expression is an expression that combines two expressions with an
   * operator. Examples:
   *    a + b
   *    f(x) |> g(y)
   *)
  and parseBinaryExpr ?(context=OrdinaryExpr) ?a p prec =
    let a = match a with
    | Some e -> e
    | None -> parseOperandExpr ~context p
    in
    let rec loop a =
      let token = p.Parser.token in
      let tokenPrec =
        (* Disambiguate division VS start of a tuple:
         *  foo() / 1
         *  VS
         *  foo()
         *  /1, 2/
         * The newline indicates the difference between the two.
         * Branching here has a performance impact.
         * TODO: totally different tuple syntax *)
        if token = Token.Forwardslash && p.startPos.pos_lnum > p.prevEndPos.pos_lnum then
          -1
        else
          Token.precedence token
      in
      if tokenPrec < prec then a
      else begin
        Parser.leaveBreadcrumb p (Grammar.ExprBinaryAfterOp token);
        let startPos = p.startPos in
        Parser.next p;
        let endPos = p.prevEndPos in
        let b = parseBinaryExpr ~context p (tokenPrec + 1) in
        let loc = mkLoc a.Parsetree.pexp_loc.loc_start b.pexp_loc.loc_end in
        let expr = Ast_helper.Exp.apply
          ~loc
          (makeInfixOperator p token startPos endPos)
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
          let str = Ast_helper.Exp.constant (Pconst_string(txt, Some "j")) in
          Ast_helper.Exp.apply hiddenOperator
            [Nolabel, acc; Nolabel, str]
        else
          acc
      | TemplatePart txt ->
        Parser.next p;
        let expr = parseExprBlock p in
        Scanner.setTemplateMode p.scanner;
        Parser.expect Rbrace p;
        let str = Ast_helper.Exp.constant (Pconst_string(txt, Some "j")) in
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
    Parser.expect Backtick p;
    match p.Parser.token with
    | TemplateTail txt ->
      Parser.next p;
      Ast_helper.Exp.constant (Pconst_string(txt, Some "j"))
    | TemplatePart txt ->
      Parser.next p;
      let expr = parseExprBlock p in
      Scanner.setTemplateMode p.scanner;
      Parser.expect Rbrace p;
      let str = Ast_helper.Exp.constant (Pconst_string(txt, Some "j")) in
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

  and parseLetBindingBody ~startPos ~attrs p =
    Parser.leaveBreadcrumb p Grammar.LetBinding;
    let startPos = p.Parser.startPos in
    let pat, exp =
      let pat = parsePattern p in
      match p.Parser.token with
      | Colon ->
        Parser.next p;
        begin match p.token with
        | Typ -> (* locally abstract types *)
          Parser.next p;
          let newtypes = parseLidentList p in
          Parser.expect Dot p;
          let typ = parseTypExpr p in
          Parser.expect Equal p;
          let expr = parseExpr p in
          let loc = mkLoc startPos p.prevEndPos in
          let exp, poly = wrap_type_annotation ~loc newtypes typ expr in
          let pat = Ast_helper.Pat.constraint_ ~loc pat poly in
          (pat, exp)
        | _ ->
          let polyType = parsePolyTypeExpr p in
          let loc = {pat.ppat_loc with loc_end = polyType.Parsetree.ptyp_loc.loc_end} in
          let pat = Ast_helper.Pat.constraint_ ~loc pat polyType in
          Parser.expect Token.Equal p;
          let exp = parseExpr p in
          (pat, exp)
        end
      | _ ->
        Parser.expect Token.Equal p;
        let exp = parseExpr p in
        (pat, exp)
    in
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
   * The parser state is copied, which is quite expensive…
   *)
  and parseAttributesAndBinding (p : Parser.t) =
    let err = p.scanner.err in
    let ch = p.scanner.ch in
    let offset = p.scanner.offset in
    let rdOffset = p.scanner.rdOffset in
    let lineOffset = p.scanner.lineOffset in
    let lnum = p.scanner.lnum in
    let mode = p.scanner.mode in
    let token = p.token in
    let startPos = p.startPos in
    let endPos = p.endPos in
    let prevEndPos = p.prevEndPos in
    let breadcrumbs = p.breadcrumbs in
    let errors = p.errors in
    let diagnostics = p.diagnostics in
    let directive = p.directive in
    let comments = p.comments in

    match p.Parser.token with
    | At ->
      let attrs = parseAttributes p in
      begin match p.Parser.token with
      | And ->
        attrs
      | _ ->
        p.scanner.err <- err;
        p.scanner.ch <- ch;
        p.scanner.offset <- offset;
        p.scanner.rdOffset <- rdOffset;
        p.scanner.lineOffset <- lineOffset;
        p.scanner.lnum <- lnum;
        p.scanner.mode <- mode;
        p.token <- token;
        p.startPos <- startPos;
        p.endPos <- endPos;
        p.prevEndPos <- prevEndPos;
        p.breadcrumbs <- breadcrumbs;
        p.errors <- errors;
        p.diagnostics <- diagnostics;
        p.directive <- directive;
        p.comments <- comments;
        []
      end
    | _ -> []

  (* definition	::=	let [rec] let-binding  { and let-binding }   *)
  and parseLetBindings ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Let p;
    let recFlag = if Parser.optional p Token.Rec then
      Asttypes.Recursive
    else
      Asttypes.Nonrecursive
    in
    let first = parseLetBindingBody ~startPos ~attrs p in

    let rec loop p bindings =
      let attrs = parseAttributesAndBinding p in
      let startPos = p.Parser.startPos in
      match p.Parser.token with
      | And ->
        Parser.next p;
        ignore(Parser.optional p Let); (* overparse for fault tolerance *)
        let letBinding = parseLetBindingBody ~startPos ~attrs p in
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
      let msg = "A jsx name should start with a lowercase or uppercase identifier, like: div in <div /> or Navbar in <Navbar />"
      in
      Parser.err p (Diagnostics.message msg);
      Location.mknoloc (Longident.Lident "_")
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
      Parser.expect GreaterThan p;
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
      | LessThan -> Parser.next p; Parser.expect Forwardslash p
      | _ -> Parser.expect LessThanSlash p
      in
      begin match p.Parser.token with
      | Lident _ | Uident _ when verifyJsxOpeningClosingName p name ->
        Parser.expect GreaterThan p;
        let loc = mkLoc childrenStartPos childrenEndPos in
        if spread then
          List.hd children
        else
          makeListExpression loc children None
      | _ ->
        let opening = "</" ^ (string_of_pexp_ident name) ^ ">" in
        let msg = "Closing jsx name should be the same as the opening name. Did you mean " ^ opening ^ " ?" in
        Parser.err p (Diagnostics.message msg);
        Parser.expect GreaterThan p;
        let loc = mkLoc childrenStartPos childrenEndPos in
        if spread then
          List.hd children
        else
          makeListExpression loc children None
      end
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      makeListExpression Location.none [] None
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
    Parser.expect LessThan p;
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
    Parser.expect GreaterThan p;
    let (_spread, children) = parseJsxChildren p in
    let childrenEndPos = p.Parser.startPos in
    Parser.expect LessThanSlash p;
    Parser.expect GreaterThan p;
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
    let (name, _loc) = parseLident p in
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
    parseList
      ~grammar:Grammar.JsxAttribute
      ~f:parseJsxProp
      p

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
    Parser.expect Lbrace p;
    let expr = match p.Parser.token with
    | Rbrace ->
      Parser.err  p (Diagnostics.unexpected Rbrace p.breadcrumbs);
      let loc = mkLoc p.prevEndPos p.endPos in
      Ast_helper.Exp.construct
        ~loc (Location.mkloc (Longident.Lident "()") loc) None
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
        let constant = Ast_helper.Exp.constant ~loc:field.loc (Parsetree.Pconst_string(s, None)) in
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
          parseRecordExpr [(pathIdent, valueOrConstructor)] p
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
        (* error case *)
        | Lident _ ->
          if p.prevEndPos.pos_lnum < p.startPos.pos_lnum then (
            Parser.expect Comma p;
            parseRecordExpr [(pathIdent, valueOrConstructor)] p
          ) else (
            Parser.expect Colon p;
            parseRecordExpr [(pathIdent, valueOrConstructor)] p
          )
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
                TermParameter (false, [], Asttypes.Nolabel, None, Ast_helper.Pat.var ident, startPos)
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
    Parser.expect Rbrace p;
    {expr with pexp_loc = mkLoc startPos p.prevEndPos}

  and parseRecordRowWithStringKey p =
    let field = match p.Parser.token with
    | String s ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc (Longident.Lident s) loc
    | t ->
      Parser.err p (Diagnostics.unexpected t p.breadcrumbs);
      Location.mknoloc (Longident.Lident "_")
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
    let rows = firstRow::(
      parseCommaDelimitedList ~grammar:Grammar.RecordRowsStringKey ~closing:Rbrace ~f:parseRecordRowWithStringKey p
    ) in
    let loc = mkLoc ((fst firstRow).loc.loc_start) p.endPos in
    let recordStrExpr = Ast_helper.Str.eval ~loc (
      Ast_helper.Exp.record ~loc rows None
    ) in
    Ast_helper.Exp.extension ~loc
      (Location.mkloc "bs.obj" loc, Parsetree.PStr [recordStrExpr])

  and parseRecordExpr ?(spread=None) rows p =
    let exprs =
      parseCommaDelimitedList ~grammar:Grammar.RecordRows ~closing:Rbrace ~f:parseRecordRow p
    in
    let rows = rows @ exprs in
    let () = match rows with
    | [] ->
      let msg = "Record spread needs at least one field that's updated" in
      Parser.err p (Diagnostics.message msg);
    | rows -> ()
    in
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
      | t ->
        Parser.err p (Diagnostics.uident t);
        Location.mknoloc "_"
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
        | LessThan | Backtick | Percent | Try ->
          parseExprBlock p
        | _ ->
          let loc = mkLoc p.startPos p.endPos in
          Ast_helper.Exp.construct ~loc
            (Location.mkloc (Longident.Lident "()") loc) None
        end
      (* High danger, TODO check if we really can omit semi in these case*)
      | Bang | Band
      | True | False | Int _ | Float _ | String _ | Lident _ | Uident _
      | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
      | Lazy | If | For | While | Switch | Open | Module | Exception | Let
      | LessThan | Backtick | Percent | Try ->
        parseExprBlock p
      | _ ->
        let loc = mkLoc p.startPos p.endPos in
        Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
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
      | LessThan | Backtick | Percent | Try ->
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
        | At | Percent | Minus | MinusDot | Plus | PlusDot | Bang | Band
        | True | False | Int _ | String _ | Lident _ | Uident _
        | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
        | Lazy | If | For | While | Switch | Open | Module | Exception | Let
        | LessThan | Backtick | Try ->
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
          | LessThan | Backtick | Percent | Try -> true
          | _ -> false
          end
        ->
          begin match p.Parser.token with
          (* seq expr start *)
          | At | Minus | MinusDot | Plus | PlusDot | Bang | Band
          | True | False | Int _ | String _ | Lident _ | Uident _
          | Lparen | List | Lbracket | Lbrace | Forwardslash | Assert
          | Lazy | If | For | While | Switch | Open | Module | Exception | Let
          | LessThan | Backtick | Percent | Try ->
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

  and parseTryExpression p =
    let startPos = p.Parser.startPos in
    Parser.expect Try p;
    let expr = parseExpr ~context:WhenExpr p in
    Parser.expect Catch p;
    Parser.expect Lbrace p;
    let cases = parsePatternMatching p in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.try_ ~loc expr cases

  and parseIfExpression p =
    Parser.leaveBreadcrumb p Grammar.ExprIf;
    let startPos = p.Parser.startPos in
    Parser.expect If p;
    Parser.leaveBreadcrumb p Grammar.IfCondition;
    (* doesn't make sense to try es6 arrow here? *)
    let conditionExpr = parseExpr ~context:WhenExpr p in
    Parser.eatBreadcrumb p;
    Parser.leaveBreadcrumb p IfBranch;
    Parser.expect Lbrace p;
    let thenExpr = parseExprBlock p in
    Parser.expect Rbrace p;
    Parser.eatBreadcrumb p;
    let elseExpr = match p.Parser.token with
    | Else ->
      Parser.leaveBreadcrumb p Grammar.ElseBranch;
      Parser.next p;
      let elseExpr = match p.token with
      | If ->
        parseIfExpression p
      | _ ->
        Parser.expect  Lbrace p;
        let blockExpr = parseExprBlock p in
        Parser.expect Rbrace p;
        blockExpr
      in
      Parser.eatBreadcrumb p;
      Some elseExpr
    | _ ->
      None
    in
    let loc = mkLoc startPos p.prevEndPos in
    Parser.eatBreadcrumb p;
    Ast_helper.Exp.ifthenelse ~loc conditionExpr thenExpr elseExpr

  and parseForRest hasOpeningParen pattern startPos p =
    Parser.expect In p;
    let e1 = parseExpr p in
    let direction = match p.Parser.token with
    | To -> Asttypes.Upto
    | Downto -> Asttypes.Downto
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Asttypes.Upto
    in
    Parser.next p;
    let e2 = parseExpr ~context:WhenExpr p in
    if hasOpeningParen then Parser.expect Rparen p;
    Parser.expect Lbrace p;
    let bodyExpr = parseExprBlock p in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.for_ ~loc pattern e1 e2 direction bodyExpr

  and parseForExpression p =
    let startPos = p.Parser.startPos in
    Parser.expect For p;
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
        Parser.expect Lbrace p;
        let block = parseExprBlock p in
        Parser.expect Rbrace p;
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
    Parser.expect While p;
    let expr1 = parseExpr ~context:WhenExpr p in
    Parser.expect Lbrace p;
    let expr2 = parseExprBlock p in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.while_ ~loc expr1 expr2

  and parsePatternMatchCase p =
    Parser.leaveBreadcrumb p Grammar.PatternMatchCase;
    Parser.expect Bar p;
    let lhs = parsePattern p in
    let guard = match p.Parser.token with
    | When ->
      Parser.next p;
      Some (parseExpr ~context:WhenExpr p)
    | _ ->
      None
    in
    let () = match p.token with
    | EqualGreater -> Parser.next p
    | _ -> Recover.recoverEqualGreater p
    in
    let rhs = parseExprBlock p in
    Parser.eatBreadcrumb p;
    Ast_helper.Exp.case lhs ?guard rhs

  and parsePatternMatching p =
    Parser.leaveBreadcrumb p Grammar.PatternMatching;
    let cases =
      parseDelimitedList
        ~grammar:Grammar.PatternMatching
        ~closing:Rbrace
        ~f:parsePatternMatchCase
        p
    in
    cases

  and parseSwitchExpression p =
    let startPos = p.Parser.startPos in
    Parser.expect Switch p;
    let switchExpr = parseExpr ~context:WhenExpr p in
    Parser.expect Lbrace p;
    let cases = parsePatternMatching p in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.match_ ~loc switchExpr cases

  (*
   * argument ::=
   *   | _                            (* syntax sugar *)
   *   | expr
   *   | expr : type
   *   | ~ label-name
   *   | ~ label-name
   *   | ~ label-name ?
   *   | ~ label-name =   expr
   *   | ~ label-name =   _           (* syntax sugar *)
   *   | ~ label-name =   expr : type
   *   | ~ label-name = ? expr
   *   | ~ label-name = ? _           (* syntax sugar *)
   *   | ~ label-name = ? expr : type
   *
   *  uncurried_argument ::=
   *   | . argument
   *)
  and parseArgument p =
    let uncurried = Parser.optional p Dot in
    match p.Parser.token with
    (* foo(_), do not confuse with foo(_ => x), TODO: performance *)
    | Underscore when not (isEs6ArrowExpression ~inTernary:false p) ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      let exp = Ast_helper.Exp.ident ~loc (
        Location.mkloc (Longident.Lident "_") loc
      ) in
      (uncurried, Asttypes.Nolabel, exp)
    | Tilde ->
      Parser.next p;
      (* TODO: nesting of pattern matches not intuitive for error recovery *)
      begin match p.Parser.token with
      | Lident ident ->
        let startPos = p.startPos in
        Parser.next p;
        let endPos = p.prevEndPos in
        let loc = mkLoc startPos endPos in
        let identExpr = Ast_helper.Exp.ident ~loc (
          Location.mkloc (Longident.Lident ident) loc
        ) in
        begin match p.Parser.token with
        | Question ->
          Parser.next p;
          (uncurried, Asttypes.Optional ident, identExpr)
        | Equal ->
          Parser.next p;
          let label = match p.Parser.token with
          | Question ->
            Parser.next p;
            Asttypes.Optional ident
          | _ ->
            Labelled ident
          in
          let expr = match p.Parser.token with
          | Underscore ->
            let loc = mkLoc p.startPos p.endPos in
            Parser.next p;
            Ast_helper.Exp.ident ~loc (
              Location.mkloc (Longident.Lident "_") loc
            )
          | _ -> parseConstrainedExpr p
          in
          (uncurried, label, expr)
        | _ ->
          (uncurried, Labelled ident, identExpr)
        end
      | t ->
        Parser.err p (Diagnostics.lident t);
        (uncurried, Nolabel, Recover.defaultExpr ())
      end
    | _ -> (uncurried, Nolabel, parseConstrainedExpr p)

  and parseCallExpr p funExpr =
    Parser.expect Lparen p;
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
      [ false,
        Asttypes.Nolabel,
        Ast_helper.Exp.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
      ]
    | args -> args
    in
    let loc = {funExpr.pexp_loc with loc_end = p.prevEndPos } in

    let rec group grp acc = function
    | (uncurried, lbl, expr)::xs ->
        let (_u, grp) = grp in
        if uncurried == true then
          group (true, [lbl, expr]) ((_u, (List.rev grp))::acc) xs
        else
          group (_u, ((lbl, expr)::grp)) acc xs
    | [] ->
        let (_u, grp) = grp in
        List.rev ((_u, (List.rev grp))::acc)
    in
    let args = match args with
    | (u, lbl, expr)::args -> group (u, [lbl, expr]) [] args
    | [] -> []
    in
    let apply = List.fold_left (fun callBody group ->
      let (uncurried, args) = group in
      let (args, wrap) = processUnderscoreApplication args in
      let exp = if uncurried then
        let attrs = [uncurryAttr] in
        Ast_helper.Exp.apply ~loc ~attrs callBody args
      else
        Ast_helper.Exp.apply ~loc callBody args
      in
      wrap exp
    ) funExpr args
    in
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
        | Lparen when p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
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
        Recover.defaultExpr()
    in
    aux p []

  and parseConstructorArgs p =
    let lparen = p.Parser.startPos in
    Parser.expect Lparen p;
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
    Parser.expect Forwardslash p;
    Scanner.setTupleMode p.scanner;
    let exprs =
      parseCommaDelimitedList
        p ~grammar:Grammar.ExprList ~closing:TupleEnding ~f:parseConstrainedExpr
    in
    Parser.expect TupleEnding p;
    Ast_helper.Exp.tuple ~loc:(mkLoc startPos p.prevEndPos) exprs

  and parseListExprItem p =
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      let expr = parseConstrainedExpr p in
      (true, expr)
    | _ ->
      (false, parseConstrainedExpr p)

  and parseListExpr p =
    let startPos = p.Parser.startPos in
    Parser.expect List p;
    Parser.expect Lparen p;
    let listExprs =
      parseCommaDelimitedReversedList
      p ~grammar:Grammar.ListExpr ~closing:Rparen ~f:parseListExprItem
    in
    Parser.expect Rparen p;
    let loc = mkLoc startPos p.prevEndPos in
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
    Parser.expect Lbracket p;
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
        Parser.expect Dot p;
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
        | EqualGreater ->
          Parser.next p;
          let typ = Ast_helper.Typ.var ~loc:var.loc var.txt in
          let returnType = parseTypExpr ~alias:false p in
          let loc = mkLoc typ.Parsetree.ptyp_loc.loc_start p.prevEndPos in
          Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ returnType
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
      match p.Parser.token with
      | SingleQuote ->
        Parser.next p;
        let (lident, loc) = parseLident p in
        let var = Location.mkloc lident loc in
        loop p (var::vars)
      | _ ->
        List.rev vars
    in
    loop p []

  and parseLidentList p =
    let rec loop p ls =
      match p.Parser.token with
      | Lident lident ->
        Parser.next p;
        let loc = mkLoc p.startPos p.endPos in
        loop p ((Location.mkloc lident loc )::ls)
      | _ ->
        List.rev ls
    in
    loop p []

  and parseAtomicTypExpr ~attrs p =
    Parser.leaveBreadcrumb p Grammar.AtomicTypExpr;
    let startPos = p.Parser.startPos in
    let typ = match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      let (ident, loc) = parseLident p in
      Ast_helper.Typ.var ~loc ~attrs ident
    | Underscore ->
      let endPos = p.endPos in
      Parser.next p;
      Ast_helper.Typ.any ~loc:(mkLoc startPos endPos) ~attrs ()
    | Forwardslash ->
      parseTupleType p
    | Lparen ->
      Parser.next p;
      begin match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        let unitConstr = Location.mkloc (Longident.Lident "unit") loc in
        Ast_helper.Typ.constr ~attrs unitConstr []
      | _ ->
        let t = parseTypExpr p in
        Parser.expect Rparen p;
        {t with ptyp_loc = mkLoc startPos p.prevEndPos}
      end
    | Uident _ | Lident _ | List ->
      let constr = parseValuePath p in
      let args =  parseTypeConstructorArgs p in
      Ast_helper.Typ.constr ~loc:(mkLoc startPos p.prevEndPos) ~attrs constr args
    | Percent ->
      let (loc, extension) = parseExtension p in
      Ast_helper.Typ.extension ~loc extension
    | Lbrace ->
      parseBsObjectType p
    | token ->
      begin match Recover.skipTokensAndMaybeRetry p ~isStartOfGrammar:Grammar.isAtomicTypExprStart with
      | Retry ->
        parseAtomicTypExpr ~attrs p
      | Abort ->
        Parser.err ~startPos:p.prevEndPos p (Diagnostics.unexpected token p.breadcrumbs);
        Recover.defaultType()
      end
    in
    Parser.eatBreadcrumb p;
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
      Parser.expect SingleQuote p;
      let (ident, _loc) = parseLident p in
      (* TODO: how do we parse attributes here? *)
      Ast_helper.Typ.alias ~loc:(mkLoc typ.Parsetree.ptyp_loc.loc_start p.prevEndPos) typ ident
    | _ -> typ


  (* type_parameter ::=
    *  | type_expr
    *  | ~ident: type_expr
    *  | ~ident: type_expr=?
    *
    * uncurried_type_parameter ::=
    *  | . type_parameter
    *)
  and parseTypeParameter p =
    let startPos = p.Parser.startPos in
    let uncurried = Parser.optional p Dot in
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let (name, _loc) = parseLident p in
      Parser.expect Colon p;
      let typ = parseTypExpr p in
      begin match p.Parser.token with
      | Equal ->
        Parser.next p;
        Parser.expect Question p;
        (uncurried, attrs, Asttypes.Optional name, typ, startPos)
      | _ ->
        (uncurried, attrs, Asttypes.Labelled name, typ, startPos)
      end
    | _ ->
      (uncurried, attrs, Asttypes.Nolabel, parseTypExpr p, startPos)

  (* (int, ~x:string, float) *)
  and parseTypeParameters p =
    let startPos = p.Parser.startPos in
    Parser.expect Lparen p;
    match p.Parser.token with
    | Rparen ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      let unitConstr = Location.mkloc (Longident.Lident "unit") loc in
      let typ = Ast_helper.Typ.constr unitConstr [] in
      [(false, [], Asttypes.Nolabel, typ, startPos)]
    | _ ->
      let params =
        parseCommaDelimitedList ~grammar:Grammar.TypeParameters ~closing:Rparen ~f:parseTypeParameter p
      in
      Parser.expect Rparen p;
      params

  and parseEs6ArrowType ~attrs p =
    match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let (name, _loc) = parseLident p in
      Parser.expect Colon p;
      let typ = parseTypExpr ~alias:false ~es6Arrow:false p in
      let arg = match p.Parser.token with
      | Equal ->
        Parser.next p;
        Parser.expect Question p;
        Asttypes.Optional name
      | _ ->
        Asttypes.Labelled name
      in
      Parser.expect EqualGreater p;
      let returnType = parseTypExpr ~alias:false p in
      Ast_helper.Typ.arrow arg typ returnType
    | _ ->
      let parameters = parseTypeParameters p in
      Parser.expect EqualGreater p;
      let returnType = parseTypExpr ~alias:false p in
      let endPos = p.prevEndPos in
      let typ = List.fold_right (fun (uncurried, attrs, argLbl, typ, startPos) t ->
        let attrs = if uncurried then uncurryAttr::attrs else attrs in
        Ast_helper.Typ.arrow ~loc:(mkLoc startPos endPos) ~attrs argLbl typ t
      ) parameters returnType
      in
      {typ with ptyp_attributes = typ.ptyp_attributes @ attrs}

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
    (* Parser.leaveBreadcrumb p Grammar.TypeExpression; *)
    let attrs = parseAttributes p in
    let typ = if es6Arrow && isEs6ArrowType p then
      parseEs6ArrowType ~attrs p
    else
      let typ = parseAtomicTypExpr ~attrs p in
      match p.Parser.token with
      | (EqualGreater | MinusGreater) as token when es6Arrow == true ->
        (* error recovery *)
        if token = MinusGreater then (
          Parser.expect EqualGreater p;
        );
        Parser.next p;
        let returnType = parseTypExpr ~alias:false p in
        let loc = mkLoc typ.Parsetree.ptyp_loc.loc_start p.prevEndPos in
        Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ returnType
      | _ -> typ
    in
    let typ = if alias then parseTypeAlias p typ else typ in
    (* Parser.eatBreadcrumb p; *)
    typ

  and parseTupleType p =
    let startPos = p.Parser.startPos in
    Parser.expect Forwardslash p;
    let types =
      parseCommaDelimitedList ~grammar:Grammar.TypExprList ~closing:Forwardslash ~f:parseTypExpr p
    in
    Parser.expect Forwardslash p;
    Ast_helper.Typ.tuple ~loc:(mkLoc startPos p.prevEndPos) types

  (* be more robust: option(<node<int>>) option<<node<int>> *)
  and parseTypeConstructorArg p =
    if p.Parser.token = Token.LessThan then Parser.next p;
    let typ = parseTypExpr p in
    typ

  (* Js.Nullable.value<'a> *)
  and parseTypeConstructorArgs p =
    let opening = p.Parser.token in
    match opening with
    | LessThan | Lparen ->
      if p.token = Lparen then (
        let msg = "Type parameters need to be wrapped in angle brackets, not parentheses, like so: \"Belt.Map.String.t<int>\"" in
        Parser.err p (Diagnostics.message msg)
      );
      Scanner.setDiamondMode p.scanner;
      Parser.next p;
      let typeArgs =
        (* TODO: change Grammar.TypExprList to TypArgList!!! *)
        parseCommaDelimitedList ~grammar:Grammar.TypExprList ~closing:GreaterThan ~f:parseTypeConstructorArg p
      in
      let () = match p.token with
      | Rparen when opening = Token.Lparen ->
        Parser.next p
      | _ ->
        Parser.expect GreaterThan p
      in
      Scanner.popMode p.scanner Diamond;
      typeArgs
    | _ -> []

  and parseConstructorTypeArgs p =
		Scanner.setDiamondMode p.Parser.scanner;
		Parser.expect LessThan p;
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
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Location.mknoloc "_"
    in
    Parser.expect ~grammar:Grammar.TypeExpression Colon p;
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
    let (lident, loc) = parseLident p in
    let name = Location.mkloc lident loc in
    let typ = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parsePolyTypeExpr p
    | _ ->
      Ast_helper.Typ.constr ~loc:name.loc {name with txt = Lident name.txt} []
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
    let startPos = p.Parser.startPos in
    Parser.expect Lbrace p;
    let rows =
      parseCommaDelimitedList
        ~grammar:Grammar.RecordDecl
        ~closing:Rbrace
        ~f:parseFieldDeclaration
        p
    in
    Parser.expect Rbrace p;
    let () = match rows with
    | ld::_ ->
      let bracesLocation = mkLoc startPos p.prevEndPos in
      Hashtbl.add LocationState.bracesTbl ld.pld_loc bracesLocation
    | _ -> ()
    in
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
                Parser.leaveBreadcrumb p Grammar.StringFieldDeclarations;
                let field = parseStringFieldDeclaration p in
                (* parse comma after first *)
                let () = match p.Parser.token with
                | Rbrace | Eof -> ()
                | Comma -> Parser.next p
                | _ -> Parser.expect Comma p
                in
                Parser.eatBreadcrumb p;
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
              Parser.expect Rparen p;
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
      let startPos = p.startPos in
      Parser.next p;
      let typ = parseTypExpr p in
      Some ({typ with ptyp_loc = mkLoc startPos p.prevEndPos})
    | _ -> None
    in
    (constrArgs, res)

  (* constr-decl ::=
   *  | constr-name
   *  | attrs constr-name
   *  | constr-name const-args
   *  | attrs constr-name const-args *)
   and parseTypeConstructorDeclarationWithBar p =
    Parser.expect Bar p;
    let startPos = p.Parser.prevEndPos in
    parseTypeConstructorDeclaration ~startPos p

   and parseTypeConstructorDeclaration ~startPos p =
     Parser.leaveBreadcrumb p Grammar.ConstructorDeclaration;
     let attrs = parseAttributes p in
     match p.Parser.token with
     | Uident uident ->
       let uidentLoc = mkLoc p.startPos p.endPos in
       Parser.next p;
       let (args, res) = parseConstrDeclArgs p in
       Parser.eatBreadcrumb p;
       let loc = mkLoc startPos p.prevEndPos in
       Ast_helper.Type.constructor ~loc ~attrs ?res ~args (Location.mkloc uident uidentLoc)
     | t ->
      Parser.err p (Diagnostics.uident t);
      Ast_helper.Type.constructor (Location.mknoloc "_")

   (* [|] constr-decl  { | constr-decl }   *)
   and parseTypeConstructorDeclarations ?first p =
    let firstConstrDecl = match first with
    | None ->
      let startPos = p.Parser.startPos in
      ignore (Parser.optional p Token.Bar);
      parseTypeConstructorDeclaration ~startPos p
    | Some firstConstrDecl ->
      firstConstrDecl
    in
    firstConstrDecl::(
      parseList
        ~grammar:Grammar.ConstructorDeclaration
        ~f:parseTypeConstructorDeclarationWithBar
        p
    )

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
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      (* TODO: I have no idea if this is even remotely a good idea *)
      Parsetree.Ptype_variant []
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
      let (ident, loc) = parseLident p in
      (Ast_helper.Typ.var ~loc ident, variance)
    | Underscore ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      (Ast_helper.Typ.any ~loc (), variance)
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      (Ast_helper.Typ.any (), variance)
    in
    Parser.eatBreadcrumb p;
    param

  (* type-params	::=
   *  | <type-param>
 	 *  ∣	<type-param, type-param>
 	 *  ∣	<type-param, type-param, type-param>
 	 *  ∣	<type-param, type-param, type-param,>
   *
   *  TODO: when we have pretty-printer show an error
   *  with the actual code corrected. *)
  and parseTypeParams p =
    let opening = p.Parser.token in
    let startPos = p.Parser.startPos in
    match opening with
    | LessThan | Lparen when p.startPos.pos_lnum == p.prevEndPos.pos_lnum ->
      Parser.leaveBreadcrumb p Grammar.TypeParams;
      if (p.token = Lparen) then (
        let msg = "Type params require diamonds, example: type node<'a>" in
        Parser.err p (Diagnostics.message msg)
      );
      Parser.next p;
      let params =
        parseCommaDelimitedList
          ~grammar:Grammar.TypeParams
          ~closing:GreaterThan
          ~f:parseTypeParam
          p
      in
      let () = match p.token with
      | Rparen when opening = Token.Lparen ->
        Parser.next p
      | _ ->
        Parser.expect GreaterThan p
      in
      let () =
        match params with
        | (typ, _)::_ ->
          let diamondLoc = mkLoc startPos p.prevEndPos in
          Hashtbl.add LocationState.diamondTbl typ.ptyp_loc diamondLoc
        | _ -> ()
      in
      Parser.eatBreadcrumb p;
      params
    | _ -> []

  (* type-constraint	::=	constraint ' ident =  typexpr *)
  and parseTypeConstraint p =
    Parser.expect Constraint p;
    Parser.expect SingleQuote p;
    begin match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      Parser.expect Equal p;
      let typ = parseTypExpr p in
      (Ast_helper.Typ.var ident, typ, Location.none)
    | t ->
      Parser.err p (Diagnostics.lident t);
      (Ast_helper.Typ.any (), parseTypExpr p, Location.none)
    end

  (* type-constraints ::=
   *  | (* empty *)
   *  | type-constraint
   *  | type-constraint type-constraint
   *  | type-constraint type-constraint type-constraint (* 0 or more *)
   *)
  and parseTypeConstraints p =
    parseList
      ~grammar:Grammar.TypeConstraint
      ~f:parseTypeConstraint
      p

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
        let loc = mkLoc uidentStartPos p.prevEndPos in
        let typ = parseTypeAlias p (
          Ast_helper.Typ.constr ~loc typeConstr (parseTypeConstructorArgs p)
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
    | t ->
      Parser.err p (Diagnostics.uident t);
      (* TODO: is this a good idea? *)
      (None, Asttypes.Public, Parsetree.Ptype_abstract)

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
      Parser.expect Rbrace p;
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
            Parser.leaveBreadcrumb p Grammar.StringFieldDeclarations;
            let field = parseStringFieldDeclaration p in
            (* parse comma after first *)
            let () = match p.Parser.token with
            | Rbrace | Eof -> ()
            | Comma -> Parser.next p
            | _ -> Parser.expect Comma p
            in
            Parser.eatBreadcrumb p;
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
            makeBsObjType ~loc ~closed:closedFlag fields |> parseTypeAlias p
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
        let () = match fields with
        | ld::_ ->
          let bracesLoc = mkLoc startPos p.prevEndPos in
          Hashtbl.add LocationState.bracesTbl ld.pld_loc bracesLoc
        | _ -> ()
        in
        Parser.eatBreadcrumb p;
        (None, Asttypes.Public, Parsetree.Ptype_record fields)
      end

  and parsePrivateEqOrRepr p =
    Parser.expect Private p;
    match p.Parser.token with
    | Lbrace ->
      let (manifest, _, kind) = parseRecordOrBsObjectDecl p in
      (manifest, Asttypes.Private, kind)
    | Uident _ ->
      let (manifest, _, kind) = parseTypeEquationOrConstrDecl p in
      (manifest, Asttypes.Private, kind)
    | Bar | DotDot ->
      let (_, kind) = parseTypeRepresentation p in
      (None, Asttypes.Private, kind)
    | t when Grammar.isTypExprStart t ->
      (Some (parseTypExpr p), Asttypes.Private, Parsetree.Ptype_abstract)
    | _ ->
      let (_, kind) = parseTypeRepresentation p in
      (None, Asttypes.Private, kind)

  and parseTypeEquationAndRepresentation p =
    match p.Parser.token with
    | Equal | Bar as token ->
      if token = Bar then Parser.expect Equal p;
      Parser.next p;
      begin match p.Parser.token with
      | Uident _ ->
        parseTypeEquationOrConstrDecl p
      | Lbrace ->
        parseRecordOrBsObjectDecl p
      | Private ->
        parsePrivateEqOrRepr p
      | Bar | DotDot ->
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
  and parseTypeDef ?attrs ~startPos p =
    Parser.leaveBreadcrumb p Grammar.TypeDef;
    let attrs = match attrs with | Some attrs -> attrs | None -> parseAttributes p in
    Parser.leaveBreadcrumb p Grammar.TypeConstrName;
    let (name, loc) = parseLident p in
    let typeConstrName = Location.mkloc name loc in
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
          let startPos = p.Parser.startPos in
          Parser.next p;
          let typeDef = parseTypeDef ~attrs ~startPos p in
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
    Parser.expect Token.Typ p;
    let recFlag = match p.token with
      | Rec -> Parser.next p; Asttypes.Recursive
      | Lident "nonrec" ->
        Parser.next p;
        Asttypes.Nonrecursive
      | _ -> Asttypes.Nonrecursive
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
    match p.Parser.token with
    | String s -> Parser.next p; s
    | _ -> ""

  and parsePrimitives p =
    match (parseList ~grammar:Grammar.Primitive ~f:parsePrimitive p) with
    | [] ->
      let msg = "An external definition should have at least one primitive. Example: \"setTimeout\"" in
      Parser.err p (Diagnostics.message msg);
      []
    | primitives -> primitives

  (* external value-name : typexp = external-declaration *)
  and parseExternalDef ~attrs p =
    Parser.leaveBreadcrumb p Grammar.External;
    let startPos = p.Parser.startPos in
    Parser.expect Token.External p;
    let (name, loc) = parseLident p in
    let name = Location.mkloc name loc in
    Parser.expect ~grammar:(Grammar.TypeExpression) Colon p;
    let typExpr = parseTypExpr p in
    Parser.expect Equal p;
    let prim = parsePrimitives p in
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
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
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
    Parser.expect Token.Exception p;
    let (_, name, kind) = parseConstrDef ~parseAttrs:false p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Te.constructor ~loc ~attrs name kind

  and parseStructure p : Parsetree.structure =
    parseList p ~grammar:Grammar.Structure ~f:parseStructureItem

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
      let exp = parseExpr p in
      begin match exp.pexp_desc with
      | Pexp_apply _ ->
          let fakeUnitPat =
            let unitLid = Location.mknoloc (Longident.Lident "()") in
            Ast_helper.Pat.construct unitLid None
          in
          let vb = Ast_helper.Vb.mk ~attrs fakeUnitPat exp in
          Ast_helper.Str.value Asttypes.Nonrecursive [vb]
       | _ ->
         Ast_helper.Str.eval ~attrs exp
      end
    in
    Parser.optional p Semicolon |> ignore;
    let loc = mkLoc startPos p.prevEndPos in
    {item with pstr_loc = loc}

  (* include-statement ::= include module-expr *)
  and parseIncludeStatement ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Token.Include p;
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
      let structure = Ast_helper.Mod.structure (
        parseDelimitedList
          ~grammar:Grammar.Structure
          ~closing:Rbrace
          ~f:parseStructureItem
          p
      ) in
      Parser.expect Rbrace p;
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
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.defaultModuleExpr()

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
    | _ ->
      let msg = "a functor arg name should be module name or _" in
      Parser.err p (Diagnostics.message msg);
      "_"
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
      Parser.expect Colon p;
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
    Parser.expect EqualGreater p;
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
    Parser.expect Lparen p;
    let args =
      parseCommaDelimitedList ~grammar:Grammar.ModExprList ~closing:Rparen ~f:parseConstrainedModExpr p
    in
    Parser.expect Rparen p;
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
    Parser.expect Module p;
    match p.Parser.token with
    | Typ -> parseModuleTypeImpl ~attrs startPos p
    | _ -> parseMaybeRecModuleBinding ~attrs p

  and parseModuleTypeImpl ~attrs startPos p =
    Parser.expect Typ p;
    let nameStart = p.Parser.startPos in
    let name = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      let loc = mkLoc nameStart p.prevEndPos in
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    Parser.expect Equal p;
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
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
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
      Parser.expect Rparen p;
      {mty with pmty_loc = mkLoc startPos p.prevEndPos}
    | Lbrace -> parseSpecification p
    | Module -> (* TODO: check if this is still atomic when implementing first class modules*)
      parseModuleTypeOf p
    | Percent ->
      let (loc, extension) = parseExtension p in
      Ast_helper.Mty.extension ~loc extension
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.defaultModuleType()
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
   *  ∣	 module module-path :=  extended-module-path
   *
   *  TODO: split this up into multiple functions, better errors *)
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
      | token ->
        (* TODO: revisit *)
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        let lident = parseModuleLongIdent p in
        Parsetree.Pwith_modsubst (modulePath, lident)
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
      | token ->
        (* TODO: revisit *)
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
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
      end
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      raise Exit

  and parseModuleTypeOf p =
    let startPos = p.Parser.startPos in
    Parser.expect Module p;
    Parser.expect Typ p;
    Parser.expect Of p;
    let moduleExpr = parseModuleExpr p in
    Ast_helper.Mty.typeof_ ~loc:(mkLoc startPos p.prevEndPos) moduleExpr

  and parseSpecification p =
    Parser.expect Lbrace p;
    let spec =
      parseDelimitedList ~grammar:Grammar.Signature ~closing:Rbrace ~f:parseSignatureItem p
    in
    Parser.expect Rbrace p;
    Ast_helper.Mty.signature spec

  and parseSignature p =
    parseList ~grammar:Grammar.Signature ~f:parseSignatureItem p

  and parseSignatureItem p =
    let attrs = parseAttributes p in
    let item = match p.Parser.token with
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
      | t ->
        Parser.err p (Diagnostics.uident t);
        parseModuleDeclarationOrAlias ~attrs p
      end
    | AtAt ->
      let (loc, attr) = parseStandaloneAttribute p in
      Ast_helper.Sig.attribute ~loc attr
    | PercentPercent ->
      let (loc, extension) = parseExtension ~moduleLanguage:true p in
      Ast_helper.Sig.extension ~attrs ~loc extension
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.defaultSignatureItem()
    in
    Parser.optional p Semicolon |> ignore;
    item

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
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    Parser.expect Colon p;
    let modType = parseModuleType p in
    Ast_helper.Md.mk ~attrs name modType

  and parseModuleDeclarationOrAlias ~attrs p =
    let moduleName = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
    in
    let body = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parseModuleType p
    | Equal ->
      Parser.next p;
      let lident = parseModuleLongIdent p in
      Ast_helper.Mty.alias lident
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.defaultModuleType()
    in
    Ast_helper.Sig.module_ (Ast_helper.Md.mk ~attrs moduleName body)

  and parseModuleTypeDeclaration ~attrs p =
    let startPos = p.Parser.startPos in
    Parser.expect Typ p;
    (* We diverge from ocaml here by requiring uident instead of ident *)
    let moduleName = match p.Parser.token with
    | Uident ident ->
      Parser.next p;
      Location.mknoloc ident
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
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
    Parser.expect Let p;
    let (name, loc) = parseLident p in
    let name = Location.mkloc name loc in
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
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        acc
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
  and parsePayload p =
    let structure = match p.Parser.token with
    | Lparen when p.startPos.pos_cnum = p.prevEndPos.pos_cnum  ->
      Parser.next p;
      let item = parseStructureItem p in
      Parser.expect Rparen p;
      [item]
    | _ -> []
    in
    Parsetree.PStr structure

  (* type attribute = string loc * payload *)
  and parseAttribute p =
    Parser.expect At p;
    let attrId = parseAttributeId p in
    let payload = parsePayload p in
    (attrId, payload)

  and parseAttributes p =
    parseList p
      ~grammar:Grammar.Attribute
      ~f:parseAttribute

  (*
   * standalone-attribute ::=
   *  | @@ atribute-id
   *  | @@ attribute-id ( structure-item )
   *)
  and parseStandaloneAttribute p =
    let startPos = p.Parser.startPos in
    Parser.expect AtAt p;
    let attrId = parseAttributeId p in
    let payload = parsePayload p in
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
      Parser.expect PercentPercent p
    else
      Parser.expect Percent p;
    let attrId = parseAttributeId p in
    let payload = parsePayload p in
    let loc = mkLoc startPos p.prevEndPos in
    (loc, (attrId, payload))
end

module Printer = struct
  (* TODO: should this go inside a ast utility module? *)
  let rec collectPatternsFromListConstruct acc pattern =
    let open Parsetree in
    match pattern.ppat_desc with
    | Ppat_construct(
        {txt = Longident.Lident "::"},
        Some {ppat_desc=Ppat_tuple (pat::rest::[])}
      ) ->
      collectPatternsFromListConstruct (pat::acc) rest
    | _ -> List.rev acc, pattern


  type attributeTemplate = {
    commentsAbove: Parsetree.attributes;
    attributesAbove: Parsetree.attributes;
    leadingAttributes: Parsetree.attributes;
    commentsInside: Parsetree.attributes;
    trailingComments: Parsetree.attributes;
    commentsBelow: Parsetree.attributes;
  }

  (* TODO: attributes inside *)
  let classifyAttributes (attrs: Parsetree.attributes) nodeLoc =
    let open Location in

    let rec parseCommentsAbove acc (attrs : Parsetree.attributes) =
      match (attrs: Parsetree.attributes) with
      | [] -> (List.rev acc, [])
      | ({txt = "napkinscript.multiLineComment" | "napkinscript.singleLineComment"; loc}, _)::_ when
        loc.loc_start.pos_cnum >= nodeLoc.loc_end.pos_cnum ->
        (List.rev acc, attrs)
      | [(({txt = "napkinscript.multiLineComment" | "napkinscript.singleLineComment"; loc}, _) as attr)] ->
        (List.rev (attr::acc), [])
      | (({txt = "napkinscript.multiLineComment" |
          "napkinscript.singleLineComment";loc}, _) as attr)::(({txt = txt2;loc =loc2}, _) as attr2)::rest ->
        if loc.loc_end.pos_lnum < loc2.loc_start.pos_lnum then
          parseCommentsAbove (attr::acc) (attr2::rest)
        else
          (List.rev acc, rest)
      | rest ->
        (List.rev acc, rest)
    in

    let rec loop template (attrs : Parsetree.attributes) =
      match (attrs : Parsetree.attributes) with
      | (({txt; loc}, _) as attr)::rest ->
        if (txt = "napkinscript.multiLineComment" || txt = "napkinscript.singleLineComment") &&
          loc.loc_end.pos_lnum < nodeLoc.loc_start.pos_lnum
        then
          loop { template with commentsAbove = attr::template.commentsAbove }
            rest
        else if (txt = "napkinscript.multiLineComment" || txt = "napkinscript.singleLineComment") &&
          loc.loc_start.pos_cnum > nodeLoc.loc_start.pos_cnum &&
          loc.loc_end.pos_cnum < nodeLoc.loc_end.pos_cnum
        then
          loop { template with commentsInside = attr::template.commentsInside }
            rest
        else if loc.loc_end.pos_lnum < nodeLoc.loc_start.pos_lnum then
          loop { template with attributesAbove = attr::template.attributesAbove }
            rest
        else if loc.loc_end.pos_cnum <= nodeLoc.loc_start.pos_cnum then
          loop { template with leadingAttributes = attr::template.leadingAttributes }
            rest
        else if loc.loc_start.pos_lnum == nodeLoc.loc_end.pos_lnum then
          loop { template with trailingComments = attr::template.trailingComments }
            rest
        else
          loop { template with commentsBelow = attr::template.commentsBelow }
            rest
      | [] -> template
    in
    let (commentsAbove, attrs) = parseCommentsAbove [] attrs in
    let template = loop {
      commentsAbove = commentsAbove;
      attributesAbove = [];
      leadingAttributes = [];
      commentsInside = [];
      trailingComments = [];
      commentsBelow = [];
    } attrs in
    (* if we have both above & leading attributes, put them all above *)
    let {attributesAbove; leadingAttributes} = template in
    let hasAttributeAbove =
      let attrs = List.filter (fun (attr : Parsetree.attribute) ->
        let {txt} = fst attr in
        txt <> "napkinscript.multiLineComment" && txt <> "napkinscript.multiLineComment"
      ) attributesAbove in
      match attrs with
      | _::_ -> true
      | [] -> false
    in
    if hasAttributeAbove then
      (* Move all leading attributes up until the first leading comment to the
       * attributes above the node *)
      let rec split acc attrs =
        match (attrs : Parsetree.attributes) with
        | (({txt="napkinscript.multiLineComment" | "napkinscript.singleLineComment"; loc}, _) as attr)::rest ->
          split (attr::acc) rest
        | _ -> (acc, attrs)
      in
      let (leadingComments, newAttrsAbove) = split [] leadingAttributes in
      {
        commentsAbove = List.rev template.commentsAbove;
        attributesAbove = List.concat [List.rev template.attributesAbove; List.rev newAttrsAbove];
        commentsInside = List.rev template.commentsInside;
        leadingAttributes = leadingComments;
        trailingComments = List.rev template.trailingComments;
        commentsBelow = List.rev template.commentsBelow;
      }
    else {
      commentsAbove = List.rev template.commentsAbove;
      attributesAbove = List.rev template.attributesAbove;
      leadingAttributes = List.rev template.leadingAttributes;
      commentsInside = List.rev template.commentsInside;
      trailingComments = List.rev template.trailingComments;
      commentsBelow = List.rev template.commentsBelow;
    }

  type attributePartition = {
    commentsBefore: Parsetree.attribute list;
    commentsAfter: Parsetree.attribute list;
    attrs: Parsetree.attribute list;
  }

  (* TODO: perf of this function *)
  let partitionAttributes (attrs : Parsetree.attributes) nodeLoc =
    let rec partition attrs part =
      let open Location in
      match (attrs : Parsetree.attributes) with
      | (({txt="napkinscript.multiLineComment" | "napkinscript.singleLineComment"; loc}, _) as attr)::rest ->
        let part = if loc.loc_end.pos_lnum < nodeLoc.loc_start.pos_lnum then
          { part with commentsBefore = attr::part.commentsBefore }
          else if loc.loc_start.pos_cnum >= nodeLoc.loc_end.pos_cnum then
          { part with commentsAfter = attr::part.commentsAfter }
        else
          { part with attrs = attr::part.attrs }
        in
        partition rest part
      | attr::rest ->
          partition  rest {part with attrs = attr::part.attrs }
      | [] ->
        {
          commentsBefore = List.rev part.commentsBefore;
          commentsAfter = List.rev part.commentsAfter;
          attrs = List.rev part.attrs;
        }
    in
    partition attrs {commentsBefore = []; commentsAfter = []; attrs = []}

  (* `// comment
   *  @bs.val /* c1 */ @bs.unsafe /* c2 */
   *  @attr /* c3 */ and /* c4 */ cs /* c5 */ = /* c6 */ 2 // c7
   *
   * commentsAbove: // comment
   * attrs: @bs.val /* c1 */ @bs.unsafe /* c2 */ @attr /* c3 */
   * commentsInside: /* c4 */ /* c5 */ /* c6 */ // c7
   *)
  type bindingAttributeTemplate = {
    commentsAbove: Parsetree.attribute list;
    attrs: Parsetree.attribute list;
    commentsInside: Parsetree.attribute list;
  }

  let partitionBindingAttributes (attrs : Parsetree.attributes) nodeLoc =
    let open Location in
    let rec parseCommentsAbove acc (attrs : Parsetree.attributes) =
      match attrs with
      | (({txt="napkinscript.multiLineComment" | "napkinscript.singleLineComment"; loc}, _) as attr)::attrs         when loc.loc_end.pos_lnum < nodeLoc.loc_start.pos_lnum ->
          parseCommentsAbove (attr::acc) attrs
      | attrs ->
        (List.rev acc, attrs)
    in
    let rec parseAttributes acc (attrs : Parsetree.attributes) =
      match attrs with
      | (({loc}, _) as attr)::attrs when loc.loc_end.pos_cnum < nodeLoc.loc_start.pos_cnum ->
        parseAttributes (attr::acc) attrs
      | _ ->
        (List.rev acc, attrs)
    in
    let (commentsAbove, rest) = parseCommentsAbove [] attrs in
    let (attrs, rest) = parseAttributes [] rest in
    {
      commentsAbove;
      attrs;
      commentsInside = rest
    }

  (* Example: type t = /* c1 */ @attr /* c2 */ foo /*c3 */
   * attrsBefore: [/* c1 */, @attr, /* c2 */]
   * attrsAfter: /* c3 */ *)
  type beforeAfterAttributeTemplate = {
    attrsBefore: Parsetree.attributes;
    attrsAfter: Parsetree.attributes;
  }

  let splitBeforeAfterLocation (attrs : Parsetree.attributes) nodeLoc =
    let rec parse acc (attrs : Parsetree.attributes) =
      let open Location in
      match (attrs : Parsetree.attributes) with
      | [] -> (List.rev acc, [])
      | attr::attrs ->
          let id = fst attr in
          if id.loc.loc_end.pos_cnum <= nodeLoc.loc_start.pos_cnum then
            parse (attr::acc) attrs
          else
            (List.rev acc, attr::attrs)
   in
   let (before, after) = parse [] attrs in
   {
     attrsBefore = before;
     attrsAfter = after;
   }


  let interleaveWhitespace ~getLoc ~render ~sep nodes =
    let open Location in
    let rec loop i prevLoc docs nodes =
      match nodes with
      | [] -> List.rev docs
      | [node] ->
        let currLoc = getLoc node in
        let renderedNode = render node i in
        let docs =
          if currLoc.loc_start.pos_lnum - prevLoc.loc_end.pos_lnum > 1 then
            renderedNode::Doc.hardLine::docs
          else
            renderedNode::docs
        in
        List.rev docs
      | node::nodes ->
        let currLoc = getLoc node in
        let renderedNode = render node i in
        let docs =
          if currLoc.loc_start.pos_lnum - prevLoc.loc_end.pos_lnum > 1 then
            sep::renderedNode::Doc.hardLine::docs
          else
            sep::renderedNode::docs
        in
        loop (i + 1) currLoc docs nodes
    in
    match nodes with
    | [] -> Doc.nil
    | [node] -> Doc.concat([render node 0])
    | node::nodes ->
      let docs = [sep; render node 0] in
      Doc.group(Doc.concat(loop 1 (getLoc node) docs nodes))


  let printLongident l = match l with
    | Longident.Lident lident -> Doc.text lident
    | Longident.Ldot (lident, txt) as l ->
      let txts = Longident.flatten l in
      Doc.join ~sep:(Doc.text ".") (List.map Doc.text txts)
    | _ -> failwith "unsupported ident"

  let printConstant c = match c with
    | Parsetree.Pconst_integer (s, _) -> Doc.text s
    | Pconst_string (s, _) -> Doc.text ("\"" ^ s ^ "\"")
    | Pconst_float (s, _) -> Doc.text s
    | Pconst_char c -> Doc.text ("'" ^ (Char.escaped c) ^ "'")

  (* TODO: simplify this thing *)
  let rec renderSequenceWhitespace: 'a.
    getNodeLoc:('a -> Location.t) -> getSpan:('a -> Location.t)
    -> getAttrs:('a -> Parsetree.attributes)
    -> render:('a -> Parsetree.attributes -> int -> Doc.t)
    -> sep: Doc.t
    -> forceBreak: bool
    -> 'a list
    -> Doc.t
    = fun ~getNodeLoc ~getSpan ~getAttrs ~render ~sep ~forceBreak nodes ->
    let open Location in

    let rec loop i prevSpan docs nodes =
      match nodes with
      | [] -> Doc.nil
      | [node] ->
        let nodeLoc = getNodeLoc node in
        let currSpan = getSpan node in
        let attributeTemplate = classifyAttributes (getAttrs node) nodeLoc in
        let renderedNode =
          let doc = render node attributeTemplate.commentsInside i in
          Doc.concat [doc; Doc.ifBreaks sep Doc.nil]
        in
        let renderedNodeWithAttrs = attachAttributeTemplate renderedNode attributeTemplate in
        let whitespace =
          if currSpan.loc_start.pos_lnum - prevSpan.loc_end.pos_lnum > 1 then
            Doc.hardLine
          else Doc.nil
        in
        Doc.concat (List.rev (renderedNodeWithAttrs::whitespace::Doc.line::docs))
      | node::nodes ->
        let nodeLoc = getNodeLoc node in
        let currSpan = getSpan node in
        let attributeTemplate = classifyAttributes (getAttrs node) nodeLoc in
        let renderedNode =
          let doc = render node attributeTemplate.commentsInside i in
          Doc.concat [doc; sep]
        in
        let renderedNodeWithAttrs = attachAttributeTemplate renderedNode attributeTemplate in
        let whitespace =
          if currSpan.loc_start.pos_lnum - prevSpan.loc_end.pos_lnum > 1 then
            Doc.hardLine
          else Doc.nil
        in
        let docs = renderedNodeWithAttrs::whitespace::Doc.line::docs in
        loop (i + 1) currSpan docs nodes
    in
    match nodes with
    | [] -> Doc.nil
    | [node] ->
        let nodeLoc = getNodeLoc node in
        let attributeTemplate = classifyAttributes (getAttrs node) nodeLoc in
        let renderedNode = render node attributeTemplate.commentsInside 0 in
        attachAttributeTemplate renderedNode attributeTemplate
    | node::nodes ->
        let nodeLoc = getNodeLoc node in
        let currSpan = getSpan node in
        let attributeTemplate = classifyAttributes (getAttrs node) nodeLoc in
        let renderedNode = render node attributeTemplate.commentsInside 0 in
        let firstDoc = attachAttributeTemplate renderedNode attributeTemplate in
        Doc.breakableGroup ~forceBreak (loop 1 currSpan [firstDoc] nodes)

  and printStructure (structure : Parsetree.structure) =
    renderSequenceWhitespace
      ~getNodeLoc:(fun si -> si.Parsetree.pstr_loc)
      ~getSpan:(fun si -> si.Parsetree.pstr_loc)
      ~getAttrs:(fun (_: Parsetree.structure_item) -> [])
      ~render:printStructureItem
      ~forceBreak:true
      ~sep:Doc.nil
      structure

  and printStructureItem (si: Parsetree.structure_item) _comments _i =
    match si.pstr_desc with
    | Pstr_value(rec_flag, valueBindings) ->
      let recFlag = match rec_flag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
      in
      renderSequenceWhitespace
        ~getNodeLoc:(fun vb -> vb.Parsetree.pvb_loc)
        ~getSpan:(fun vb -> {vb.Parsetree.pvb_loc with
            loc_start = match vb.pvb_attributes with
            | (id, _):: _ -> id.loc.loc_start
            | _ -> vb.pvb_loc.loc_start
         })
        ~getAttrs:(fun vb -> vb.Parsetree.pvb_attributes)
        (* allocating a closure here…*)
        ~render:(printValueBinding ~recFlag)
        ~sep:Doc.nil
        ~forceBreak:(si.pstr_loc.loc_end.pos_lnum > si.pstr_loc.loc_start.pos_lnum)
        valueBindings

    | Pstr_type(recFlag, typeDeclarations) ->
      let renderedRecFlag = match recFlag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
      in
      renderSequenceWhitespace
        ~getNodeLoc:(fun td -> td.Parsetree.ptype_loc)
        ~getSpan:(fun td -> {td.Parsetree.ptype_loc with
           loc_start = match td.ptype_attributes with
           | (id, _)::_ -> id.loc.loc_start
           | _ -> td.ptype_loc.loc_start
        })
        ~getAttrs:(fun (td : Parsetree.type_declaration) -> td.ptype_attributes)
        (* allocating a closure here…*)
        ~render:(printTypeDeclaration ~recFlag:renderedRecFlag)
        ~sep:Doc.nil
        ~forceBreak:(si.pstr_loc.loc_end.pos_lnum > si.pstr_loc.loc_start.pos_lnum)
        typeDeclarations

    | Pstr_primitive valueDescription ->
      printValueDescription valueDescription

    | Pstr_attribute (
        ({txt = "napkinscript.singleLineComment"} | {txt =
          "napkinscript.multiLineComment"}) as id,
        PStr [{pstr_desc =
          Parsetree.Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string
          (commentTxt, _))}, _)}]
      ) ->
        let c = if id.txt = "napkinscript.singleLineComment" then
            "//" ^ commentTxt
          else
            "/*" ^ commentTxt ^ "*/"
        in
        Doc.text c
    | _ -> failwith "unsupported"

  (*
   * type value_description = {
   *   pval_name : string Asttypes.loc;
   *   pval_type : Parsetree.core_type;
   *   pval_prim : string list;
   *   pval_attributes : Parsetree.attributes;
   *   pval_loc : Location.t;
   * }
   *)
  and printValueDescription valueDescription =
    let template = classifyAttributes
      valueDescription.pval_attributes
      valueDescription.pval_loc
    in
    let valName =
      let name = Doc.text valueDescription.pval_name.txt in
      attachCommentsBeforeAfter
        name
        template.commentsInside
        valueDescription.pval_name.loc
    in
    let renderedExternal = Doc.group ( Doc.concat [
      Doc.text "external ";
      valName;
      Doc.text ": ";
      printTypExpr valueDescription.pval_type;
      Doc.text " =";
      Doc.indent(Doc.concat [
        Doc.line;
          Doc.join
            ~sep:(Doc.text " ")
            (List.map (fun s -> Doc.concat [
              Doc.text "\"";
              Doc.text s;
              Doc.text "\"";
            ])
            valueDescription.pval_prim);
      ])
    ]) in
    attachAttributeTemplate renderedExternal template


  and printCommentAttribute (attr : Parsetree.attribute) = match attr with
    | ({txt = "napkinscript.singleLineComment"} | {txt = "napkinscript.multiLineComment"}) as id,
        PStr [{pstr_desc =
          Parsetree.Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string
          (commentTxt, _))}, _)}]
        ->
        if id.txt = "napkinscript.singleLineComment" then
          Doc.text ("//" ^ commentTxt)
          (* Doc.concat [Doc.text ("//" ^ commentTxt); Doc.hardLine] *)
        else
          Doc.text ("/*" ^ commentTxt ^ "*/")
    | _ -> Doc.nil

  and attachCommentsBeforeNode (comments: Parsetree.attribute list) nodeLoc =
    let open Location in
    let rec loop (prev : Parsetree.attribute) docs nodes =
      match nodes with
      | [] ->
        let prevLoc = (fst prev).loc in
        let docs =
          let lineDiff = nodeLoc.loc_start.pos_lnum - prevLoc.loc_end.pos_lnum in
          match prev with
          | ({txt="napkinscript.singleLineComment"}, _) ->
              if lineDiff > 1 then
                Doc.hardLine::docs
              else
                docs
          | ({txt="napkinscript.multiLineComment"}, _) ->
              if lineDiff == 0 then
                docs
              else if lineDiff == 1 then
                Doc.hardLine::docs
              else (* lineDiff > 1 *)
                Doc.hardLine::Doc.hardLine::docs
          | _ ->
            docs
        in
        List.rev docs
      | (((id, payload) as attr) : Parsetree.attribute)::nodes ->
        let prevLoc = (fst prev).loc in
        let currLoc = id.loc in
        let renderedNode = printCommentAttribute attr in
        let docs =
          let lineDiff = currLoc.loc_start.pos_lnum - prevLoc.loc_end.pos_lnum in
          if lineDiff == 0 then
            renderedNode::docs
          else (
            match prev with
            | ({txt="napkinscript.singleLineComment"}, _) ->
                if lineDiff > 1 then
                  renderedNode::Doc.hardLine::docs
                else
                  renderedNode::docs
            | ({txt="napkinscript.multiLineComment"}, _) ->
                if lineDiff == 0 then
                  renderedNode::docs
                else if lineDiff == 1 then
                  renderedNode::Doc.hardLine::docs
                else (* if lineDiff > 1 then *)
                  renderedNode::Doc.hardLine::Doc.hardLine::docs
            | _ ->
              renderedNode::docs
          )
        in
        loop attr docs nodes
    in
    match comments with
    | [] -> Doc.nil
    | node::nodes ->
        let docs = [printCommentAttribute node] in
        Doc.concat (loop node docs nodes)

  (*
   * type_declaration = {
   *    ptype_name: string loc;
   *    ptype_params: (core_type * variance) list;
   *          (* ('a1,...'an) t; None represents  _*)
   *    ptype_cstrs: (core_type * core_type * Location.t) list;
   *          (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
   *    ptype_kind: type_kind;
   *    ptype_private: private_flag;   (* = private ... *)
   *    ptype_manifest: core_type option;  (* = T *)
   *    ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
   *    ptype_loc: Location.t;
   * }
   *
   *
   *  type t                     (abstract, no manifest)
   *  type t = T0                (abstract, manifest=T0)
   *  type t = C of T | ...      (variant,  no manifest)
   *  type t = T0 = C of T | ... (variant,  manifest=T0)
   *  type t = {l: T; ...}       (record,   no manifest)
   *  type t = T0 = {l : T; ...} (record,   manifest=T0)
   *  type t = ..                (open,     no manifest)
   *
   *
   * and type_kind =
   *  | Ptype_abstract
   *  | Ptype_variant of constructor_declaration list
   *        (* Invariant: non-empty list *)
   *  | Ptype_record of label_declaration list
   *        (* Invariant: non-empty list *)
   *  | Ptype_open
   *)
  and printTypeDeclaration ~recFlag (td : Parsetree.type_declaration) comments i =
    let prefix = if i > 0 then
      Doc.text "and "
    else
      Doc.concat [Doc.text "type "; recFlag]
    in
    let (commentsBeforeParams, commentsAfterParams) = match td.ptype_params with
    | [] -> (comments, [])
    | (typ, _)::_ ->
      let paramsLoc = Hashtbl.find LocationState.diamondTbl typ.ptyp_loc in
      let {attrsBefore; attrsAfter} = splitBeforeAfterLocation comments paramsLoc in
      (attrsBefore, attrsAfter)
    in
    let typeName =
      let name = Doc.text td.ptype_name.txt in
      let nameWithComments = attachCommentsBeforeAfter name commentsBeforeParams td.ptype_name.loc in
      let nameWithParams = Doc.concat [
        nameWithComments;
        match td.ptype_params with
        | [] -> Doc.nil
        | params -> printTypeDeclParams params
      ] in
      attachCommentsAfter nameWithParams commentsAfterParams
    in
    let doc = match (td.ptype_kind, td.ptype_manifest) with
    (* type t *)
    | (Ptype_abstract, None) ->
        Doc.concat [prefix; typeName]

      (* type t = myType *)
    | (Ptype_abstract, Some typ) ->
        Doc.concat [
          prefix;
          typeName;
          Doc.text " = ";
          printTypExpr typ
        ]
      (* type t = .. *)
    | (Ptype_open, _) ->
      Doc.concat [prefix; typeName; Doc.text " = .."]
    | (Ptype_variant constrDeclarations, manifest) ->
      let overMultipleLines = td.ptype_loc.loc_end.pos_lnum > td.ptype_loc.loc_start.pos_lnum in
      let renderedManifest = match manifest with
      | Some typ ->
        Doc.concat [
          Doc.line;
          Doc.indent (
            Doc.concat [Doc.text "= "; printTypExpr typ]
          )
        ]
      | None -> Doc.nil
      in
      Doc.concat [
        prefix;
        typeName;
        renderedManifest;
        Doc.breakableGroup ~forceBreak:overMultipleLines (
          Doc.concat [
            Doc.text " = ";
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                renderSequenceWhitespace
                  ~getNodeLoc:(fun cd -> cd.Parsetree.pcd_loc)
                  ~getSpan:(fun cd -> {cd.Parsetree.pcd_loc with
                     loc_start = match cd.pcd_attributes with
                     | (id, _)::_ -> id.loc.loc_start
                     | _ -> cd.pcd_loc.loc_start
                  })
                  ~getAttrs:(fun cd -> cd.Parsetree.pcd_attributes)
                  ~render:printConstructorDeclaration
                  ~sep:Doc.nil
                  ~forceBreak:overMultipleLines
                  constrDeclarations;
              ]
            )
          ])
      ]

   (* type t = {l: T; ...}       (record,   no manifest)
    * type t = T0 = {l : T; ...} (record,   manifest=T0) *)
    | (Ptype_record labelDeclarations, manifest) ->
      (* hmm, is this the right heuristic… *)
      let forceBreak = td.ptype_loc.loc_start.pos_lnum < td.ptype_loc.loc_end.pos_lnum  in
      let renderedManifest = match manifest with
      | Some typExpr -> Doc.concat [
          Doc.text " = ";
          printTypExpr typExpr;
        ]
      | None -> Doc.nil
      in
      Doc.breakableGroup ~forceBreak (
        Doc.concat [
          prefix;
          typeName;
          renderedManifest;
          Doc.text " = ";
          Doc.text "{";
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              renderSequence
                ~getNodeLoc:(fun n -> n.Parsetree.pld_loc)
                ~render:printLabelDeclaration
                ~sep:(Doc.text ",")
                ~getAttrs:(fun n -> n.Parsetree.pld_attributes)
                labelDeclarations
            ]
          );
          Doc.softLine;
          Doc.text "}";
        ]
      )
    in
    Doc.group doc

  (* (core_type * variance) list *)
  and printTypeDeclParams (typeParams : (Parsetree.core_type * Asttypes.variance) list) =
    Doc.group (
    Doc.concat [
      Doc.text "<";
      Doc.indent (
        Doc.concat [
          Doc.softLine;
          renderSequence
            ~getNodeLoc:(fun (typ, _) -> typ.Parsetree.ptyp_loc)
            ~render:(fun (typ, variance) _ _ ->
              let renderedVariance = match variance with
              | Asttypes.Covariant -> Doc.text "+"
              | Contravariant -> Doc.text "-"
              | _ -> Doc.nil
              in
              Doc.concat [renderedVariance; printTypExpr typ]
            )
            ~sep:(Doc.text ",")
            ~getAttrs:(fun (typ, _)-> typ.Parsetree.ptyp_attributes)
            typeParams
        ]
      );
      Doc.softLine;
      Doc.text ">";
    ])

  and renderSequence: 'a. getNodeLoc:('a -> Location.t)
    -> render:('a -> Parsetree.attributes -> int ->Doc.t)
    -> sep:Doc.t
    -> getAttrs:('a -> Parsetree.attributes)
    -> 'a list
    -> Doc.t
  = fun ~getNodeLoc ~render ~sep ~getAttrs nodes ->
    let open Location in

    let rec loop i docs nodes =
      match nodes with
      | [] -> Doc.nil
      | [node] ->
        let nodeLoc = getNodeLoc node in
        let attributeTemplate = classifyAttributes (getAttrs node) nodeLoc in
        let renderedNode =
          let doc = render node attributeTemplate.commentsInside i in
          Doc.concat [doc; Doc.ifBreaks sep Doc.nil]
        in
        let renderedNodeWithAttrs = attachAttributeTemplate renderedNode attributeTemplate in
        Doc.concat (List.rev (renderedNodeWithAttrs::docs))
      | node::nodes ->
        let nodeLoc = getNodeLoc node in
        let attributeTemplate = classifyAttributes (getAttrs node) nodeLoc in
        let renderedNode =
          let doc = render node attributeTemplate.commentsInside i in
          Doc.concat [doc; sep]
        in
        let renderedNodeWithAttrs = attachAttributeTemplate renderedNode attributeTemplate in
        let doc = Doc.concat [renderedNodeWithAttrs; Doc.line] in
        loop (i + 1) (doc::docs) nodes
    in
    loop 0 [] nodes

    (*
     * {
     *  pcd_name: string loc;
     *  pcd_args: constructor_arguments;
     *  pcd_res: core_type option;
     *  pcd_loc: Location.t;
     *  pcd_attributes: attributes; (* C of ... [@id1] [@id2] *)
     * }
     *)
  and printConstructorDeclaration (cd : Parsetree.constructor_declaration) comments i =
    let constrName =
      let name = Doc.text cd.pcd_name.txt in
      attachCommentsBeforeAfter name comments cd.pcd_name.loc
    in
    let bar = if i > 0 then Doc.text "| " else
      Doc.ifBreaks (Doc.text "| ") Doc.nil
    in
    let renderedConstrDecl = match cd.pcd_args with
    | Pcstr_tuple [] -> constrName
    | constructorArgs -> Doc.group(
        Doc.indent(
          Doc.concat([
            constrName;
            printConstructorArguments constructorArgs;
          ])
        )
      )
    in
    let gadt = match cd.pcd_res with
    | Some(typExpr) ->
      Doc.concat [
        Doc.text ": ";
        printTypExpr typExpr
      ]
    | None -> Doc.nil
    in
    Doc.concat [bar; renderedConstrDecl; gadt]

  (*
   * constructor_arguments =
   *   | Pcstr_tuple of core_type list
   *   | Pcstr_record of label_declaration list
   *)
  and printConstructorArguments (constrArgs : Parsetree.constructor_arguments) =
    match constrArgs with
    | Pcstr_tuple types ->
      Doc.breakableGroup ~forceBreak:true (
        Doc.concat([
          Doc.text "(";
          Doc.softLine;
          renderSequence
            ~getNodeLoc:(fun n -> n.Parsetree.ptyp_loc)
            ~render:(fun t _comments _i -> printTypExpr t)
            ~sep:(Doc.text ",")
            ~getAttrs:(fun n -> n.Parsetree.ptyp_attributes)
            types;
          Doc.softLine;
          Doc.text ")";
        ])
      );
    | Pcstr_record labelDeclarations ->
      Doc.breakableGroup ~forceBreak:true (
      Doc.concat [
        Doc.text "({";
        Doc.indent (
          Doc.concat [
            Doc.softLine;
            renderSequence
              ~getNodeLoc:(fun n -> n.Parsetree.pld_loc)
              ~render:printLabelDeclaration
              ~sep:(Doc.text ",")
              ~getAttrs:(fun n -> n.Parsetree.pld_attributes)
              labelDeclarations
          ]
        );
        Doc.softLine;
        Doc.text "})";
      ])

    and printCommentLayer (attrs : Parsetree.attributes) =
      let open Location in
      let rec process acc prev (attrs : Parsetree.attributes) =
        match attrs with
        | [] -> List.rev acc
        | attr::attrs ->
          let currentLoc = (fst attr).loc in
          let renderedNode = printCommentAttribute attr in
          match prev with
          | ({txt = "napkinscript.singleLineComment"; loc = prevLoc}, _) ->
              let lineDiff = currentLoc.loc_start.pos_lnum - prevLoc.loc_end.pos_lnum in
              if lineDiff > 1 then
                process (renderedNode::Doc.hardLine::Doc.hardLine::acc) attr attrs
              else if lineDiff == 1 then
                process (renderedNode::Doc.hardLine::acc) attr attrs
              else
                process (renderedNode::(Doc.text " ")::acc) attr attrs
          | ({txt = "napkinscript.multiLineComment"; loc = prevLoc}, _) ->
              let lineDiff = currentLoc.loc_start.pos_lnum - prevLoc.loc_end.pos_lnum in
              let acc = if lineDiff > 1 then
                  Doc.hardLine::Doc.hardLine::acc
                else if lineDiff == 1 then
                  Doc.hardLine::acc
                else
                  (Doc.text " ")::acc
              in
              process (renderedNode::acc) attr attrs
          | _ -> assert false
      in
      match attrs with
      | [] -> Doc.nil
      | attr::attrs -> Doc.concat (
          process [printCommentAttribute attr] attr attrs
        )

    (*
     * {
     *  pld_name: string loc;
     *  pld_mutable: mutable_flag;
     *  pld_type: core_type;
     *  pld_loc: Location.t;
     *  pld_attributes: attributes; (* l : T [@id1] [@id2] *)
     * }
     *)
  and printLabelDeclaration (lblDecl : Parsetree.label_declaration) comments i =
    let mutableFlag = match lblDecl.pld_mutable with
    | Mutable -> Doc.text "mutable "
    | Immutable -> Doc.nil
    in
    let lblName =
      let name = Doc.text lblDecl.pld_name.txt in
      attachCommentsBeforeAfter name comments lblDecl.pld_name.loc
    in
    Doc.group (Doc.concat [
      mutableFlag;
      lblName;
      Doc.text ":";
      Doc.indent (
        Doc.concat [
          Doc.line;
          printTypExpr lblDecl.pld_type
        ]
      )
    ])
    (* in *)
    (* attachAttributeTemplate renderedNode template *)

  and attachCommentsBeforeAfter doc comments loc =
    let {attrsBefore; attrsAfter} = splitBeforeAfterLocation comments loc in
    let doc = match attrsBefore with
    | [] -> doc
    | attrs -> Doc.concat [
        Doc.join ~sep:(Doc.text " ") (List.map printAttribute attrsBefore);
        Doc.text " ";
        doc;
      ]
    in
    match attrsAfter with
    | [] -> doc
    | attrs -> Doc.concat [
        doc;
        Doc.text " ";
        Doc.join ~sep:(Doc.text " ") (List.map printAttribute attrsAfter);
        ]

  and attachCommentsAfter doc comments =
    match comments with
    | [] -> doc
    | attrs -> Doc.concat [
        doc;
        Doc.text " ";
        Doc.join ~sep:(Doc.text " ") (List.map printAttribute comments);
      ]

  and attachAttributeTemplate doc template =
    let doc = match template.leadingAttributes with
    | [] -> doc
    | attrs -> doc
      (* Doc.concat [ *)
        (* Doc.join ~sep:Doc.line (List.map printAttribute attrs); *)
        (* Doc.line; *)
        (* doc; *)
      (* ] *)
    in
    let doc = match template.trailingComments with
    | [] -> doc
    | attrs ->
        Doc.concat [
        doc;
        Doc.text " ";
        Doc.lineSuffix(
          Doc.join ~sep:(Doc.text " ") (List.map printCommentAttribute attrs)
        )
      ]
    in
    let doc = match template.attributesAbove with
    | [] -> doc
    | attrs -> doc
      (* Doc.concat [ *)
        (* Doc.group(Doc.join ~sep:Doc.line (List.map printAttribute attrs)); *)
        (* Doc.line; *)
        (* doc *)
      (* ] *)
    in
    let doc = match template.commentsAbove with
    | [] -> doc
    | attrs -> doc
      (* Doc.concat [printCommentLayer template.commentsAbove; Doc.hardLine; doc] *)
    in
    let doc = match template.commentsBelow with
    | [] -> doc
    | attrs ->
      Doc.concat [doc; Doc.hardLine; printCommentLayer template.commentsBelow]
    in
    doc

  and printTypExpr (typExpr : Parsetree.core_type) =
    let renderedType = printTypExprWithoutAttributes typExpr in
    match typExpr.ptyp_attributes with
    | [] -> renderedType
    | attrs ->
      let {attrsBefore; attrsAfter} =
        splitBeforeAfterLocation attrs typExpr.ptyp_loc
      in
      let renderedType = match attrsBefore with
      | [] -> renderedType
      | attrs ->
        Doc.concat [
          Doc.join ~sep:(Doc.text " ") (List.map printAttribute attrs);
          Doc.text " ";
          renderedType
        ]
      in
      begin match attrsAfter with
      | [] -> renderedType
      | attrs ->
        Doc.concat [
          renderedType;
          Doc.text " ";
          Doc.join ~sep:(Doc.text " ") (List.map printAttribute attrs);
        ]
      end

  and printTypExprWithoutAttributes (typExpr : Parsetree.core_type) =
    let renderedType = match typExpr.ptyp_desc with
    | Ptyp_any -> Doc.text "_"
    | Ptyp_var var -> Doc.text ("'" ^ var)
    | Ptyp_extension(extension) ->
      printExtension extension
    | Ptyp_alias(typ, alias) ->
      Doc.concat [printTypExpr typ; Doc.text " as "; Doc.text alias]
    | Ptyp_constr(longidentLoc, constrArgs) ->
        let constrName = printLongident longidentLoc.txt in
        begin match constrArgs with
        | [] -> constrName
        | args -> Doc.group(
          Doc.concat([
            constrName;
            Doc.text "<";
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                renderSequence
                  ~getNodeLoc:(fun n -> n.Parsetree.ptyp_loc)
                  ~render:(fun n _ _ -> printTypExprWithoutAttributes n)
                  ~sep:(Doc.text ",")
                  ~getAttrs:(fun n -> n.Parsetree.ptyp_attributes)
                  args;
              ]
            );
            Doc.softLine;
            Doc.text ">";
          ])
        )
        end
    | Ptyp_arrow _ ->
      let rec parseArrowParsetree acc typ = match (typ : Parsetree.core_type) with
      | {ptyp_desc = Ptyp_arrow (lbl, typ1, typ2)} ->
        let arg = (lbl, typ1) in
        parseArrowParsetree (arg::acc) typ2
      | typ ->
        (acc, typ)
      in
      let (args, returnType) = parseArrowParsetree [] typExpr in
      let renderedArgs = match args with
      | [] -> Doc.nil
      | [(_, n)] ->  printTypExprWithoutAttributes n
      | args -> Doc.concat [
        Doc.text "(";
        Doc.indent (
          Doc.concat [
            Doc.softLine;
            renderSequence
              ~getNodeLoc:(fun (_, n) -> n.Parsetree.ptyp_loc)
              ~render:(fun (_, n) _ _ -> printTypExprWithoutAttributes n)
              ~sep:(Doc.text ",")
              ~getAttrs:(fun (_, n) -> n.Parsetree.ptyp_attributes)
              args;
          ]
        );
        Doc.softLine;
        Doc.text ")";
      ]
      in
      Doc.concat [
        renderedArgs;
        Doc.text " => ";
        printTypExpr returnType;
      ]

    | Ptyp_tuple types ->
      Doc.group(
        Doc.concat([
          Doc.text "/";
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              renderSequence
                ~getNodeLoc:(fun n -> n.Parsetree.ptyp_loc)
                ~render:(fun n _ _ -> printTypExprWithoutAttributes n)
                ~sep:(Doc.text ",")
                ~getAttrs:(fun n -> n.Parsetree.ptyp_attributes)
                types;
            ])
          );
          Doc.ifBreaks (Doc.text ",") Doc.nil;
          Doc.softLine;
          Doc.text "/";
        ])
      )
    | _ -> failwith "unimplemented"
    in
    renderedType


  (*
   * {
   *   pvb_pat: pattern;
   *   pvb_expr: expression;
   *   pvb_attributes: attributes;
   *   pvb_loc: Location.t;
   * }
   *)
  and printValueBinding ~recFlag vb _comments i =
    let prefix = if i > 0 then Doc.text "and " else
      Doc.concat [Doc.text "let "; recFlag]
    in
    Doc.concat [
      prefix;
      printPattern vb.Parsetree.pvb_pat;
      Doc.text " = ";
      printExpression vb.pvb_expr
    ]
   (* let { *)
      (* commentsBefore; commentsAfter; attrs; *)
    (* } = partitionAttributes vb.pvb_attributes vb.pvb_loc in *)
    (* let comments = attachCommentsBeforeNode commentsBefore vb.pvb_loc in *)
    (* Doc.concat [ *)
      (* comments; *)
      (* Doc.group (Doc.join ~sep:Doc.line (List.map printAttribute attrs)); *)
      (* begin match attrs with | [] -> Doc.nil | _ -> Doc.line end; *)

    (* ] *)

  and printAttribute (attr : Parsetree.attribute) =
    match attr with
    | ({txt = "napkinscript.singleLineComment"} | {txt = "napkinscript.multiLineComment"}) as id,
        PStr [{pstr_desc =
          Parsetree.Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string
          (commentTxt, _))}, _)}]
        ->
        if id.txt = "napkinscript.singleLineComment" then
          Doc.concat [Doc.text ("//" ^ commentTxt); Doc.hardLine]
        else
          Doc.text ("/*" ^ commentTxt ^ "*/")
    | (id, _) -> Doc.text ("@" ^ id.txt)


  and printExtension (stringLoc, _payload) =
    Doc.text ("%" ^ stringLoc.Location.txt)

  and printPattern (p : Parsetree.pattern) =
    let renderedPattern = match p.ppat_desc with
    | Ppat_any -> Doc.text "_"
    | Ppat_var stringLoc ->
      Doc.text (stringLoc.txt)
    | Ppat_constant c -> printConstant c
    | Ppat_tuple patterns ->
      Doc.group(
        Doc.concat([
          Doc.text "/";
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map printPattern patterns)
            ])
          );
          Doc.ifBreaks (Doc.text ",") Doc.nil;
          Doc.softLine;
          Doc.text "/";
        ])
      )
    | Ppat_array patterns ->
      Doc.group(
        Doc.concat([
          Doc.text "[";
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map printPattern patterns)
            ])
          );
          Doc.ifBreaks (Doc.text ",") Doc.nil;
          Doc.softLine;
          Doc.text "]";
        ])
      )
    | Ppat_construct({txt = Longident.Lident "[]"}, _) ->
        Doc.text "list()"
    | Ppat_construct({txt = Longident.Lident "::"}, _) ->
      let (patterns, tail) = collectPatternsFromListConstruct [] p in
      Doc.group(
        Doc.concat([
          Doc.text "list(";
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map printPattern patterns);
              begin match tail.Parsetree.ppat_desc with
              | Ppat_construct({txt = Longident.Lident "[]"}, _) -> Doc.nil
              | _ -> Doc.concat([Doc.text ","; Doc.line; Doc.text "..."; printPattern tail])
              end;
            ])
          );
          Doc.ifBreaks (Doc.text ",") Doc.nil;
          Doc.softLine;
          Doc.text ")";
        ])
      )
    | Ppat_construct(constrName, constructorArgs) ->
      let constrName = printLongident constrName.txt in
      begin match constructorArgs with
      | None -> constrName
      | Some(args) ->
        let args = match args.ppat_desc with
        | Ppat_construct({txt = Longident.Lident "()"}, None) -> [Doc.nil]
        | Ppat_tuple(patterns) -> List.map printPattern patterns
        | _ -> [printPattern args]
        in
        Doc.group(
          Doc.concat([
            constrName;
            Doc.text "(";
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  args
              ]
            );
            Doc.ifBreaks (Doc.text ",") Doc.nil;
            Doc.softLine;
            Doc.text ")";
          ])
        )
      end
    | Ppat_record(rows, openFlag) ->
        Doc.group(
          Doc.concat([
            Doc.text "{";
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map printPatternRecordRow rows);
                begin match openFlag with
                | Open -> Doc.concat [Doc.text ","; Doc.line; Doc.text "_"]
                | Closed -> Doc.nil
                end;
              ]
            );
            Doc.ifBreaks (Doc.text ",") Doc.nil;
            Doc.softLine;
            Doc.text "}";
          ])
        )

    | Ppat_exception p ->
        let needsParens = match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
        in
        let pat =
          let p = printPattern p in
          if needsParens then
            Doc.concat [Doc.text "("; p; Doc.text ")"]
          else
            p
        in
        Doc.group (
          Doc.concat [ Doc.text "exception"; Doc.line; pat ]
        )
    | Ppat_or (p1, p2) ->
      let p1 =
        let p = printPattern p1 in
        match p1.ppat_desc with
        | Ppat_or (_, _) -> Doc.concat [Doc.text "("; p; Doc.text ")"]
        | _ -> p
      in
      let p2 =
        let p = printPattern p2 in
        match p2.ppat_desc with
        | Ppat_or (_, _) -> Doc.concat [Doc.text "("; p; Doc.text ")"]
        | _ -> p
      in
      Doc.group(
        Doc.concat([p1; Doc.line; Doc.text "| "; p2])
      )
    | Ppat_extension ext ->
      printExtension ext
    | Ppat_lazy p ->
      let needsParens = match p.ppat_desc with
      | Ppat_or (_, _) | Ppat_alias (_, _) -> true
      | _ -> false
      in
      let pat =
        let p = printPattern p in
        if needsParens then
          Doc.concat [Doc.text "("; p; Doc.text ")"]
        else
          p
      in
      Doc.concat [Doc.text "lazy "; pat]
    | Ppat_alias (p, aliasLoc) ->
      let needsParens = match p.ppat_desc with
      | Ppat_or (_, _) | Ppat_alias (_, _) -> true
      | _ -> false
      in
      let renderedPattern =
        let p = printPattern p in
        if needsParens then
          Doc.concat [Doc.text "("; p; Doc.text ")"]
        else
          p
      in
      Doc.concat([
        renderedPattern;
        Doc.text " as ";
        Doc.text aliasLoc.txt
      ])
    | _ -> failwith "unsupported pattern"
    in
    match p.ppat_attributes with
    | [] -> renderedPattern
    | attrs ->
      let {
        commentsBefore; commentsAfter; attrs;
      } = partitionAttributes attrs p.ppat_loc in
      let comments = attachCommentsBeforeNode commentsBefore p.ppat_loc in
      Doc.concat [
        comments;
        Doc.group (Doc.join ~sep:Doc.line (List.map printAttribute attrs));
        Doc.line;
        renderedPattern;
        Doc.group (Doc.join ~sep:Doc.line (List.map printAttribute commentsAfter));
      ]

  and printPatternRecordRow row =
    match row with
    (* punned {x}*)
    | ({Location.txt=Longident.Lident ident},
       {Parsetree.ppat_desc=Ppat_var {txt;_}}) when ident = txt ->
        Doc.text ident
    | (longident, pattern) ->
        Doc.concat([
          printLongident longident.txt;
          Doc.text ": ";
          Doc.indent(
            Doc.concat [
              Doc.softLine;
              printPattern pattern;
            ]
          )
        ])

  and printExpression (e : Parsetree.expression) =
    match e.pexp_desc with
      (* 1 or "string" or 3.14 *)
    | Parsetree.Pexp_constant c -> printConstant c
     (* foo *)
    | Pexp_ident longidentLoc ->
      printLongident longidentLoc.txt

      (* expr.longident *)
    | Pexp_field (expr, longidentLoc) ->
      Doc.concat [
        printExpression expr;
        Doc.text ".";
        printLongident longidentLoc.txt
      ]

      (* expr.longident = expr2 *)
    | Pexp_setfield (expr1, longidentLoc, expr2) ->
      Doc.concat [
        printExpression expr1;
        Doc.text ".";
        printLongident longidentLoc.txt;
        Doc.text " = ";
        printExpression expr2;
      ]

      (* [expr1, expr2, expr3] *)
    | Pexp_array exprs ->
      Doc.group(
        Doc.concat [
          Doc.text "[";
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
               (List.map printExpression exprs)
            ]
          );
          Doc.line;
          Doc.text "]";
        ]
      )

      (* if expr1 { expr2 } else { optionalExpr3 } *)
    | Pexp_ifthenelse (expr1, expr2, _optionalExpr3) ->
        Doc.group (
          Doc.concat [
            Doc.text "if ";
            Doc.group (
              Doc.indent (
                printExpression expr1
              )
            );
            Doc.text " {";
            Doc.indent (
              Doc.concat [
                Doc.hardLine;
                printExpression expr2;
              ]
            );
            Doc.hardLine;
            Doc.text "}";
          ]
        )

      (* switch switchExpr { cases } *)
    | Pexp_match (switchExpr, cases) ->
      Doc.concat [
        Doc.text "switch ";
        printExpression switchExpr;
        Doc.concat [
          Doc.text " {";
          Doc.hardLine;
          interleaveWhitespace
            ~getLoc:(fun case -> case.Parsetree.pc_lhs.ppat_loc)
            ~render:(fun case i -> printCase case)
            ~sep:Doc.hardLine
            cases;
          Doc.hardLine;
          Doc.text "}";
        ]
      ]
    | _ -> failwith "unsupported expression"


   (* { *)
     (* pc_lhs: pattern; *)
     (* pc_guard: expression option; *)
     (* pc_rhs: expression; *)
    (* } *)
  and printCase (case : Parsetree.case) =
    Doc.concat [
      Doc.text "| ";
      printPattern case.pc_lhs;
      Doc.text " => ";
      Doc.softLine;
      printExpression case.pc_rhs;
    ]

  let printStructure (s: Parsetree.structure) =
    let doc = printStructure s in
    Doc.debug doc;
    let renderedDoc = Doc.toString ~width:80 doc in
    print_endline renderedDoc;
    print_newline()

end

(* command line flags *)
module Clflags: sig
  val ancient: bool ref
  val recover: bool ref
  val profile: bool ref
  val print: string ref
  val files: string list ref

  val parse: unit -> unit
end = struct
  let ancient = ref false
  let recover = ref false
  let profile = ref false
  let setRecover () = recover := true

  let files = ref []
  let addFilename filename = files := filename::(!files)

  let print = ref ""

  let usage = "Usage: napkinscript <options> <file>\nOptions are:"

  let spec = [
    ("-recover", Arg.Unit (fun () -> recover := true), "Emit partial ast");
    ("-print", Arg.String (fun txt -> print := txt), "Print either binary, ocaml or ast");
    ("-ancient", Arg.Unit (fun () -> ancient := true), "Output 4.02.3 binary ast");
    ("-profile", Arg.Unit (fun () -> profile := true), "Enable performance profiling");
  ]

  let parse () = Arg.parse spec addFilename usage
end

module Driver: sig
  val processFile: recover: bool -> target: string -> string -> unit
end = struct
  type 'a file_kind =
    | Structure: Parsetree.structure file_kind
    | Signature: Parsetree.signature file_kind

  let parse (type a) (kind : a file_kind) p : a =
    match kind with
    | Structure -> NapkinScript.parseStructure p
    | Signature -> NapkinScript.parseSignature p

  let parseFile kind filename =
    let src = IO.readFile filename in
    let p = Parser.make src filename in
    let ast = parse kind p in
    let report = match p.diagnostics with
    | [] -> None
    | diagnostics ->
      Some(
        Diagnostics.makeReport p.diagnostics (Bytes.to_string p.scanner.src)
      )
    in
    (ast, report, p.comments)

  let parseImplementation filename =
    parseFile Structure filename

  let parseInterface filename =
    parseFile Signature filename

  let process parseFn printFn recover filename =
    let (ast, report, comments) =
      Profile.record ~name:"parser" (fun () -> parseFn filename)
    in
    match report with
    | Some report when recover = true ->
      printFn ast comments;
      prerr_string report;
    | Some report ->
      prerr_string report;
      exit 1
    | None ->
      printFn ast comments

  type action =
    | ProcessImplementation
    | ProcessInterface

  let printImplementation ~target filename ast comments =
    match target with
    | "ml" | "ocaml" ->
        Pprintast.structure Format.std_formatter ast
    | "ns" | "napkinscript" ->
        Printer.printStructure (
          AstUtils.mergeCommentsIntoStructure comments ast
        )
    | "ast" -> Printast.implementation Format.std_formatter ast
    | _ -> (* default binary *)
      if !Clflags.ancient then (
        let open Migrate_parsetree in
        let module Convert = Convert(OCaml_406)(OCaml_402) in
        let ast402 = Convert.copy_structure ast in
        Ast_io.to_channel stdout filename (Ast_io.Impl ((module OCaml_402), ast402))
      ) else (
				output_string stdout Config.ast_impl_magic_number;
				output_value stdout filename;
				output_value stdout ast
      )

  let printInterface ~target filename ast comments =
    match target with
    | "ml" | "ocaml" -> Pprintast.signature Format.std_formatter ast
    (* | "ns" | "napkinscript" -> PPrin *)
    | "ast" -> Printast.interface Format.std_formatter ast
    | _ -> (* default binary *)
      if !Clflags.ancient then (
        let open Migrate_parsetree in
        let module Convert = Convert(OCaml_406)(OCaml_402) in
        let ast402 = Convert.copy_signature ast in
        Ast_io.to_channel stdout filename (Ast_io.Intf ((module OCaml_402), ast402))
      ) else (
				output_string stdout Config.ast_intf_magic_number;
				output_value stdout filename;
				output_value stdout ast
      )

  let processFile ~recover ~target filename =
    try
      let len = String.length filename in
      let action =
        if len > 0 && String.get filename (len - 1) = 'i' then ProcessInterface
        else ProcessImplementation
      in
      match action with
      | ProcessImplementation ->
        process parseImplementation (printImplementation ~target filename) recover filename
      | ProcessInterface ->
        process parseInterface (printInterface ~target filename) recover filename
    with
    | InfiniteLoop(pos, _token) ->
      let locationInfo =
        Printf.sprintf (* ReasonLanguageServer requires the following format *)
          "File \"%s\", line %d, characters %d-%d:"
          filename
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol)
          (pos.pos_cnum - pos.pos_bol)
      in
      let msg =
        Format.sprintf
          "%s\n\nPossible infinite loop detected\n\n"
          locationInfo
      in
      prerr_string msg;
      exit 1
    | _ -> exit 1
end

(* let () = *)
  (* let filename = "RedBlackTree.ml" in *)
  (* let src = IO.readFile filename in *)
  (* let benchmark = Benchmark.make ~name:"RedBlackTree Napkinscript parser" ~f:(fun _ -> *)
    (* let p = Parser.make src filename in *)
    (* let _ast = NapkinScript.parseStructure p in *)
    (* () *)
  (* ) () *)
  (* in *)
  (* let () = Benchmark.launch benchmark in *)
  (* Benchmark.report benchmark; *)
  (* print_newline(); *)
  (* print_newline() *)


(* let () = *)
  (* let filename = "RedBlackTreePureOcaml.ml" in *)
  (* let src = IO.readFile filename in *)
  (* let benchmark = Benchmark.make ~name:"RedBlackTree Ocaml 4.06 parser" ~f:(fun _ -> *)
    (* let lexbuf = Lexing.from_string src in *)
    (* Location.init lexbuf filename; *)
    (* let _ast = Parse.implementation lexbuf in *)
    (* () *)
  (* ) () *)
  (* in *)
  (* let () = Benchmark.launch benchmark in *)
  (* Benchmark.report benchmark; *)
  (* print_newline(); *)
  (* print_newline() *)


(* let () = *)
  (* let filename = "RedBlackTree.re" in *)
  (* let src = IO.readFile filename in *)
  (* let benchmark = Benchmark.make ~name:"RedBlackTree reason parser" ~f:(fun _ -> *)
    (* let lexbuf = Lexing.from_string src in *)
    (* Location.init lexbuf filename; *)
    (* let (_ast, _comments) = Refmt_main3.Reason_toolchain.RE.implementation_with_comments lexbuf in *)
    (* () *)
  (* ) () *)
  (* in *)
  (* let () = Benchmark.launch benchmark in *)
  (* Benchmark.report benchmark; *)
  (* print_newline(); *)
  (* print_newline() *)

let () =
  Clflags.parse ();
  List.iter (fun filename ->
    Driver.processFile ~recover:!Clflags.recover ~target:!Clflags.print filename
  ) !Clflags.files;
  if !Clflags.profile then Profile.print();
  exit 0
