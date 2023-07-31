type 't mresult  = 't option

let success t = Some(t)
let failure = None

type recognizer = int -> int mresult

let terminal input t : recognizer = 
  fun i -> if String.starts_with ~prefix:t (String.sub input i  (String.length input)) then success (i + String.length t) else failure
