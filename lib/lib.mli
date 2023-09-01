open Specs

val success : 't -> 't result
val failure : _ result
val terminal : string -> string -> recognizer
val epsilon : recognizer
val seq : recognizer list -> recognizer
val rule : string -> recognizer list -> recognizer
val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
