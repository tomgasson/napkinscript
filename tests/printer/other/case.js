let printExprFunParameters = (~uncurried, parameters) =>
  switch parameters {
  | list(/[], Asttypes.Nolabel, None, {Parsetree.ppat_desc: Ppat_any}/)
    when !uncurried =>
    Doc.text("_")
  | list(/
      [],
      Asttypes.Nolabel,
      None,
      {Parsetree.ppat_desc: Ppat_var(stringLoc)}
    /) when !uncurried =>
    Doc.text(stringLoc.txt)
  | list(/
      [],
      Nolabel,
      None,
      {ppat_desc: Ppat_construct({txt: Longident.Lident("()")}, None)}
    /) when !uncurried =>
    Doc.text("()")
  | parameters =>
    let lparen = if uncurried {
      Doc.text("(. ")
    } else {
      Doc.lparen
    }
    let shouldHug = ParsetreeViewer.parametersShouldHug(parameters)
    let printedParamaters = Doc.concat(list(
      if shouldHug {
        Doc.nil
      } else {
        Doc.softLine
      },
      Doc.join(
        ~sep=Doc.concat(list(Doc.comma, Doc.line)),
        List.map(printExpFunParameter, parameters),
      ),
    ))
    Doc.group(
      Doc.concat(list(
        lparen,
        if shouldHug {
          printedParamaters
        } else {
          Doc.indent(printedParamaters)
        },
        if shouldHug {
          Doc.nil
        } else {
          Doc.concat(list(Doc.trailingComma, Doc.softLine))
        },
        Doc.rparen,
      )),
    )
  }
