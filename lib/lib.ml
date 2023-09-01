let success t = Some t
let failure = None

let terminal input t i =
  if i + String.length t <= String.length input
     && String.starts_with ~prefix:t (String.sub input i (String.length t))
  then success (i + String.length t)
  else failure

let epsilon i = success i

let reduce f l =
  let cut = function [] -> [] | _ :: xs -> xs in
  let rec folder acc = function [] -> acc | n :: ns -> folder (f acc n) ns in
  folder (List.hd l) (cut l)

let seq rs =
  let open Preface.Option.Monad.Infix in
  reduce (fun cur r i -> cur i >>= r) rs

let rule _nt alts =
  let ( <|> ) o a = match o with Some x -> Some x | None -> a in
  reduce (fun cur alt i -> cur i <|> alt i) alts

let fix f =
  let rec p () = f (fun t -> p () t) in
  p ()