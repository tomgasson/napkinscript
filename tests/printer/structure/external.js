[@bs.val] external null: reactElement = "null";

external string: string => reactElement = "%identity";

external array: array<reactElement> => reactElement = "%identity";

external refToJsObj: reactRef => Js.t<{..}> = "%identity";
