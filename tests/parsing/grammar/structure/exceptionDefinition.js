exception ExitEarly
exception ExitEarly(int)
exception ExitEarly({x: int})
exception ExitEarly({. "jsExit": int})
exception ExitEarly({. "jsExit": int},)
exception ExitEarly({. "jsExit": int}, {. "code": int})
exception ExitEarly({. "jsExit": int}, int, {. "code": int},)
exception ExitJsStyle({.})
exception ExitJsStyle({..})

@onConstructor
exception ExitEarly
@onConstructor
exception ExitEarly(int)

exception Exit = Terminate 
exception Exit = Lib.Terminate 
exception Exit = Ns.Lib.Terminate 

@onConstructor
exception Exit = Terminate 
@onConstructor
exception Exit = Lib.Terminate 
