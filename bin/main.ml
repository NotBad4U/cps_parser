let (|>) v f = f v 
let (<|) f v = f v
let (@@) f v = f v

type 't mresult  = 't option

let success t = Some(t)
let failure = None

type recognizer = int -> int mresult

let terminal input t : recognizer = 
  fun i -> if String.starts_with ~prefix:t (String.sub input i  (String.length input)) then success (i + String.length t) else failure

let _epsilon : recognizer = fun i -> success(i)

let cut = function
  | [] -> [] 
  | _::xs -> xs

let reduce f l =
  let rec folder acc l2  = 
    match l2 with
    | n::ns -> folder (f acc n ) ns
    | [] -> acc
  in
    folder (List.hd l) (cut l) 

(* let ($) f x = f x *)

(* let reduce2 f l =  List.fold_left f (List.hd l) (List.rev $ cut l) *)


(* final def flatMap[B](f: Int => Option[B]): Option[B] *)
let flatmap o (f: int -> int mresult) = 
match o with
| Some x ->  print_newline () ; print_int x ; f x
| None -> None

let seq (rs: recognizer list) : recognizer = reduce (fun cur r -> fun i -> flatmap (cur i) r) rs

let (<?>) o a : int mresult = match o with
| Some x -> Some x
| None -> a

let rule (_nt: string) alts : recognizer = 
  reduce (fun cur alt -> fun i -> (cur i) <?> (alt i) ) alts


(* def fix[A,B](f: (A=>B)=>(A=>B)): A =>B = { lazy val p: A=>B = f(t => p(t)); p } *)

type ('a, 'b) fix = (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

module S : sig
  val fix1 : ('a, ' b) fix
  val fix2 : ('a, 'b) fix
end = struct
  (* Naive implementation just using delayed recursion*)
  let rec fix1 f k = f (fix1 f) k

  (* Using ISO ADT and isomorphism *)
  type ('a, 'b) w2 = W2 of (('a, 'b) w2 -> 'a -> 'b)

  let extract (W2 x) = x

  let fix2 f =
    let a g = g (W2 g) and b x k = f (extract x x) k in
    a b
end

let s_rule input = S.fix1 (fun s ->  rule "S" [
    seq [terminal input "a" ; s ; terminal input "b"]; (* S ::=  a S b *)
    seq [terminal input "a" ; s ] ;                    (*     | a S    *)
    terminal input "s"                                 (*     | S      *)
  ])


let () =  match (s_rule "aaaas" 0) with
          | Some _ -> print_endline "PARSED"
          | None -> print_endline "NOT PARSED"


