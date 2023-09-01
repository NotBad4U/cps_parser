let success t = Some t
let failure = None

let terminal input t i =
  if i + String.length t <= String.length input
     && String.starts_with ~prefix:t (String.sub input i (String.length t))
  then success (i + String.length t)
  else failure

let epsilon i = success i
let cut = function [] -> [] | _ :: xs -> xs

let reduce f l =
  let rec folder acc l2 =
    match l2 with [] -> acc | n :: ns -> folder (f acc n) ns
  in
  folder (List.hd l) (cut l)
(* curieux ... on n'applique pas la tête*)

let flatmap o f =
  match o with
  | Some x ->
    print_newline ();
    print_int x;
    f x
  | None -> None

let seq rs = reduce (fun cur r i -> flatmap (cur i) r) rs
let ( <?> ) o a = match o with Some x -> Some x | None -> a
let rule _nt alts = reduce (fun cur alt i -> cur i <?> alt i) alts

let fix f =
  let rec p () = f (fun t -> p () t) in
  p ()
