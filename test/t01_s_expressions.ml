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

let recognize_010 () =
  let result = s_rule "s" 0 in
  let expected = Some 1 in
  Alcotest.(check (option int)) "s" result expected

let recognize_020 () =
  let result = s_rule "as" 0 in
  let expected = Some 2 in
  Alcotest.(check (option int)) "as" result expected

let recognize_030 () =
  let result = s_rule "asb" 0 in
  let expected = Some 3 in
  Alcotest.(check (option int)) "asb" result expected

let cases =
  Alcotest.
    ( "S ::= aSb|aS|s"
    , [
        test_case "s" `Quick recognize_010
      ; test_case "as" `Quick recognize_020
      ; test_case "asb" `Quick recognize_030
      ] )
