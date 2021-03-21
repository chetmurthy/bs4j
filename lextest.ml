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
  | INDENT of int * int
  | DEDENT of int * int
  | NEWLINE (* internal token *)
  | EOF  [@@deriving show,eq]
type toks = token list [@@deriving show,eq]

let printer = show_toks

let tokens_of_string s =
  List.map fst (lex_string jsontoken s)

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        assert_equal ~printer
          [YAMLSTRING "a"; EOF]
          (tokens_of_string {|a|})
      )
  ; "2" >:: (fun ctxt ->
        assert_equal ~printer
          [YAMLSTRING "a"; COLON; INDENT(0,2);
           YAMLSTRING "null"; DEDENT(0,2); EOF]
          (tokens_of_string "\na:\n  null")
      )
  ; "flow" >:: (fun ctxt ->
        assert_equal ~printer
          [LBRACKET; (STRING "\"a\""); COMMA;
           (STRING "\"b\""); RBRACKET; EOF]
          (tokens_of_string {|["a", "b"]|})
      )
  ; "flow" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; LBRACKET;
           (STRING "\"a\""); COMMA; (STRING "\"b\"");
           RBRACKET; EOF]
          (tokens_of_string {|
a:
 ["a", "b"]
|})
      )
  ; "indents" >:: (fun ctxt ->
        assert_equal ~printer
          [YAMLSTRING "a"; COLON; INDENT (0, 1);
           YAMLSTRING "b"; COLON; INDENT (1, 3);
           YAMLSTRING " c"; DEDENT (1, 3);
           DEDENT (0, 1); EOF]
          (tokens_of_string {|
a:
 b: c
|})
      )
  ; "rawstring" >:: (fun ctxt ->
        assert_equal ~printer
          [INDENT(0,4); RAWSTRING {|R"a(foo)a"|}; DEDENT(0,4); EOF]
          (tokens_of_string {|
R"a(foo)a"
|})
      )
  ; "rawstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          [INDENT(0,5); RAWSTRING {|R"(foo)"|}; DEDENT(0,5); EOF]
          (tokens_of_string {|
  R"(foo)"
|})
      )
  ]

let tests = "all" >::: [
    simple
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
