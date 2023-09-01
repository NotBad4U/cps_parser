open Cps_parser.Lib

let s_rule input =
  fix (fun s ->
      rule "S"
        [
          (* S ::=  a S b *)
          seq [ terminal input "a"; s; terminal input "b" ]
        ; (*     | a S    *)
          seq [ terminal input "a"; s ]
        ; (*     | s      *)
          terminal input "s"
        ] )

let test input () =
  let result = s_rule input 0 in
  let expected = Some (String.length input) in
  Alcotest.(check (option int)) "s" result expected

let test_case s = Alcotest.(test_case s `Quick (test s))

let cases =
  ( "S ::= aSb|aS|s"
  , [ test_case "s"
  ; test_case "as"
  ; test_case "asb"
  ; test_case "aaas"
  ; test_case "aaasb" ] )
