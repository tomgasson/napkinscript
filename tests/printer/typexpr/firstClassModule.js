type t = module(S)
type t = module(Hashmap with type key = string)
type t = module(Hashmap with type key = string and type value = int)
type t = module(Hashmap with type key = string and type value = int and type superLongThingHere = definitelyLineBreak)

type t = constr<module(S)>

external foo: module(S) = "primitive"

let x: module(S) = y


let x: module(Hashmap with type key = string and type value = int and type superLongThingHere = definitelyLineBreak) = y

type toValueLikeInstance = t<'a> => module(RxValueLikeInstance.S with type a = 'a)
type t<'a> = module(Test with type a = 'a)
type t = ref<module(Console)>

let devices: Hastbl.t<string, module(DEVICE)> = xyz


// attributes
type t = @attr module(S)
external foo: @attr module(S) = "primitive"
let devices: @attr Hastbl.t<string, module(DEVICE)> = xyz
