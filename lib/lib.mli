type 't mresult  = 't option

val success: 't -> 't option
val failure: _ option

type recognizer = int -> int mresult

val terminal: string -> string -> recognizer