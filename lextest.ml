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
  | DASHDASHDASH
  | DOTDOTDOT
  | BAR
  | GT
  | PLUS
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

let lexing = "lexing" >::: [
    "simple" >:: (fun ctxt ->
        assert_equal ~printer
          [INDENT (0, 0); YAMLSTRING "a";
           DEDENT (0, 0); EOF]
          (tokens_of_string {|a|})
      )
  ; "2" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0)); (YAMLSTRING "a"); COLON;
           (INDENT (0, 2)); (YAMLSTRING "null");
           (DEDENT (0, 2)); (DEDENT (0, 0)); EOF]
          (tokens_of_string "\na:\n  null")
      )
  ; "3" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0));
           (YAMLSTRING "a"); COLON; (INDENT (0, 3)); (YAMLSTRING "b"); (DEDENT (0, 3));
           (YAMLSTRING "c"); COLON; (INDENT (0, 3)); (YAMLSTRING "d"); (DEDENT (0, 3));
           (DEDENT (0, 0)); EOF]
          (tokens_of_string {|
a: b
c: d
|})
      )
  ; "flow" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0));
           LBRACKET; (STRING "\"a\""); COMMA;
           (STRING "\"b\""); RBRACKET;
           (DEDENT (0, 0)); EOF]
          (tokens_of_string {|["a", "b"]|})
      )
  ; "flow-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0));
           (YAMLSTRING "a"); COLON; LBRACKET;
           (STRING "\"a\""); COMMA; (STRING "\"b\"");
           RBRACKET; (DEDENT (0, 0)); EOF]
          (tokens_of_string {|
a:
 ["a", "b"]
|})
      )
  ; "indents" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0));
           YAMLSTRING "a"; COLON; INDENT (0, 1);
           YAMLSTRING "b"; COLON; INDENT (1, 4);
           YAMLSTRING "c"; DEDENT (1, 4);
           DEDENT (0, 1);
           (DEDENT (0, 0)); EOF]
          (tokens_of_string {|
a:
 b: c
|})
      )
  ; "indents-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0));
           YAMLSTRING "a"; COLON; INDENT (0, 1);
           YAMLSTRING "b"; COLON; INDENT (1, 4);
           YAMLSTRING "c"; DEDENT (1, 4);
           YAMLSTRING "d"; COLON; INDENT (1, 4);
           YAMLSTRING "e"; DEDENT (1, 4);
           DEDENT (0, 1);
           (DEDENT (0, 0)); EOF]
          (tokens_of_string {|
a:
 b: c
 d: e
|})
      )
  ; "indents-3" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0));
           YAMLSTRING "a"; COLON; INDENT (0, 1);
           DASH; INDENT (1, 3); YAMLSTRING "b";
           DEDENT (1, 3); DASH; INDENT (1, 3);
           YAMLSTRING "d"; DEDENT (1, 3);
           DEDENT (0, 1);
           (DEDENT (0, 0)); EOF]
          (tokens_of_string {|
a:
 - b
 - d
|})
      )
  ; "rawstring" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0));
           INDENT(0,4); RAWSTRING {|R"a(foo)a"|}; DEDENT(0,4);
           DEDENT (0, 0); EOF]
          (tokens_of_string {|
R"a(foo)a"
|})
      )
  ; "rawstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0));
           INDENT(0,5); RAWSTRING {|R"(foo)"|}; DEDENT(0,5);
           DEDENT (0, 0);  EOF]
          (tokens_of_string {|
  R"(foo)"
|})
      )
  ; "strings-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0)); (YAMLSTRING "a"); COLON;
           (INDENT (0, 2)); (YAMLSTRING "b");
           (YAMLSTRING "c"); (DEDENT (0, 2));
           (DEDENT (0, 0)); EOF]
          (tokens_of_string {|
a:
  b
  c
|})
      )
  ; "empty-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0)); (DEDENT (0, 0)); EOF]
          (tokens_of_string {||})
      )
  ; "empty-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0)); DASHDASHDASH; DOTDOTDOT;
           (DEDENT (0, 0)); EOF]
          (tokens_of_string {|
---
...
|})
      )
  ; "comment-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 0)); (YAMLSTRING "a");
           (DEDENT (0, 0)); EOF]
          (tokens_of_string {|
# foo
a
|})
      )
  ]

open Jsonparse

let printer = show_value_
let parse1 = parse_string parse_json_eoi
let parsing = "parsing" >::: [
    "simple" >:: (fun ctxt ->
        assert_equal ~printer
          (`String"a")
          (parse1 {|a|})
      )
  ; "2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `Null)]))
          (parse1 "\na:\n  null")
      )

  ; "3" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `String ("b")); ("c", `String ("d"))]))
          (parse1 {|
a: b
c: d
|})
      )
  ; "flow" >:: (fun ctxt ->
        assert_equal ~printer
          (`A ([`String ({|"a"|}); `String ({|"b"|})]))
          (parse1 {|["a", "b"]|})
      )
  ; "flow-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `A ([`String ({|"a"|}); `String ({|"b"|})]))]))
          (parse1 {|
a:
 ["a", "b"]
|})
      )
  ; "indents" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `O ([("b", `String ("c"))]))]))
          (parse1 {|
a:
 b: c
|})
      )
  ; "indents-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `O ([("b", `String ("c")); ("d", `String ("e"))]))]))
          (parse1 {|
a:
 b: c
 d: e
|})
      )
  ; "indents-3" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `A ([`String ("b"); `String ("d")]))]))
          (parse1 {|
a:
 - b
 - d
|})
      )
  ; "rawstring" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ({|R"a(foo)a"|}))
          (parse1 {|
R"a(foo)a"
|})
      )
  ; "rawstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ({|R"(foo)"|}))
          (parse1 {|
  R"(foo)"
|})
      )
  ; "strings-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `String ("b c"))]))
          (parse1 {|
a:
  b
  c
|})
      )
  ; "empty-1" >:: (fun ctxt ->
        assert_equal ~printer
          (`Null)
          (parse1 {||})
      )
  ; "comment-1" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a"))
          (parse1 {|
# foo
a
|})
    )
  ; "2.1" >:: (fun ctxt ->
        assert_equal ~printer
          (`A (
              [`String ("Mark McGwire")
              ; `String ("Sammy Sosa")
              ; `String ("Ken Griffey")]
            ))
          (parse1 {|- Mark McGwire
- Sammy Sosa
- Ken Griffey|})
      )
  ]

let tests = "all" >::: [
    lexing ; parsing
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
