#if BACKEND == "native"
  Js.log("native backend")
#endif

#if BACKEND == "native"
  Js.log("native backend")
#elseif BACKEND == "byte"
  Js.log("byte backend")
#endif

#if BACKEND != "native"
  Js.log("native backend")
#elseif BACKEND == "byte"
  Js.log("byte backend")
#endif

#if BACKEND != "native"
  Js.log("native backend")
#elseif BACKEND == "native"
  Js.log("== native")
#endif

#if BACKEND != "native"
  Js.log("native backend")
#else
  Js.log("byte backend")
#endif

#if BACKEND != "native"
  Js.log("native backend")
#elseif BACKEND == "js"
  Js.log("== js")
#else
  Js.log("byte backend")
#endif

#if BACKEND != "native"
  Js.log("not native backend")
#elseif BACKEND == "js"
  Js.log("== js")
#elseif BACKEND == "native"
  Js.log("it is native backend")
#endif
