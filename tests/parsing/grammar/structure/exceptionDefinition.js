exception ExitEarly
exception ExitEarly(int)
exception ExitEarly{x: int} // overparse
exception ExitEarly({x: int})

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
