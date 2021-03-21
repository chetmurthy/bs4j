open OUnit2
open OUnitTest
open Pa_ppx_testutils
open Jsontoken

let warning s = Fmt.(pf stderr "%s\n%!" s)

let matches ~pattern text =
  match Str.search_forward (Str.regexp pattern) text 0 with
    _ -> true
  | exception Not_found -> false

let assert_raises_exn_pattern pattern f =
  Testutil.assert_raises_exn_pred
    (function
        Failure msg when matches ~pattern msg -> true
      | Invalid_argument msg when matches ~pattern msg -> true
      | _ -> false
    )
    f

type token = Jsontoken.token =
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COLON
  | COMMA
  | DASH
  | NUMBER of string
  | STRING of string
  | RAWSTRING of string
  | YAMLSTRING of string
  | INDENT
  | DEDENT
  | NEWLINE (* internal token *)
  | EOF  [@@deriving show,eq]
type toks = token list [@@deriving show,eq]

let printer = show_toks

let tokens_of_string s =
  List.map fst (lex_string jsontoken s)

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        assert_equal ~printer
          [RAWSTRING "a"; EOF]
          (tokens_of_string {|a|})
      )
  ; "2" >:: (fun ctxt ->
        assert_equal ~printer
          [RAWSTRING "a"; COLON; INDENT;
           RAWSTRING "null"; DEDENT; EOF]
          (tokens_of_string "\na:\n  null")
      )
  ]

let tests = "all" >::: [
    simple
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
