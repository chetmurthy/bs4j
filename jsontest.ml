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
          [YAMLSTRING "a";
           EOF]
          (tokens_of_string {|a|})
      )
  ; "2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON;
           (INDENT (0, 2)); (YAMLSTRING "null");
           (DEDENT (0, 2)); EOF]
          (tokens_of_string "\na:\n  null")
      )
  ; "3" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; (INDENT (0, 3)); (YAMLSTRING "b"); (DEDENT (0, 3));
           (YAMLSTRING "c"); COLON; (INDENT (0, 3)); (YAMLSTRING "d"); (DEDENT (0, 3));
           EOF]
          (tokens_of_string {|
a: b
c: d
|})
      )
  ; "flow" >:: (fun ctxt ->
        assert_equal ~printer
          [LBRACKET; (STRING "\"a\""); COMMA;
           (STRING "\"b\""); RBRACKET;
           EOF]
          (tokens_of_string {|["a", "b"]|})
      )
  ; "flow-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; LBRACKET;
           (STRING "\"a\""); COMMA; (STRING "\"b\"");
           RBRACKET; EOF]
          (tokens_of_string {|
a:
 ["a", "b"]
|})
      )
  ; "flow-3" >:: (fun ctxt ->
        assert_equal ~printer
          [LBRACE; (YAMLSTRING "hr");
           COLON; (NUMBER "63"); RBRACE;
           EOF]
          (tokens_of_string {|
{ hr: 63
}
|})
      )
  ; "indents" >:: (fun ctxt ->
        assert_equal ~printer
          [YAMLSTRING "a"; COLON; INDENT (0, 1);
           YAMLSTRING "b"; COLON; INDENT (1, 4);
           YAMLSTRING "c"; DEDENT (1, 4);
           DEDENT (0, 1);
           EOF]
          (tokens_of_string {|
a:
 b: c
|})
      )
  ; "indents-2" >:: (fun ctxt ->
        assert_equal ~printer
          [YAMLSTRING "a"; COLON; INDENT (0, 1);
           YAMLSTRING "b"; COLON; INDENT (1, 4);
           YAMLSTRING "c"; DEDENT (1, 4);
           YAMLSTRING "d"; COLON; INDENT (1, 4);
           YAMLSTRING "e"; DEDENT (1, 4);
           DEDENT (0, 1);
           EOF]
          (tokens_of_string {|
a:
 b: c
 d: e
|})
      )
  ; "indents-3" >:: (fun ctxt ->
        assert_equal ~printer
          [YAMLSTRING "a"; COLON; INDENT (0, 1);
           DASH; INDENT (1, 3); YAMLSTRING "b";
           DEDENT (1, 3); DASH; INDENT (1, 3);
           YAMLSTRING "d"; DEDENT (1, 3);
           DEDENT (0, 1);
           EOF]
          (tokens_of_string {|
a:
 - b
 - d
|})
      )
  ; "rawstring" >:: (fun ctxt ->
        assert_equal ~printer
          [INDENT(0,4); RAWSTRING {|R"a(foo)a"|}; DEDENT(0,4);
           EOF]
          (tokens_of_string {|
R"a(foo)a"
|})
      )
  ; "rawstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          [INDENT(0,5); RAWSTRING {|R"(foo)"|}; DEDENT(0,5);
           EOF]
          (tokens_of_string {|
  R"(foo)"
|})
      )
  ; "strings-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON;
           (INDENT (0, 2)); (YAMLSTRING "b");
           (YAMLSTRING "c"); (DEDENT (0, 2));
           EOF]
          (tokens_of_string {|
a:
  b
  c
|})
      )
  ; "empty-1" >:: (fun ctxt ->
        assert_equal ~printer
          [EOF]
          (tokens_of_string {||})
      )
  ; "empty-2" >:: (fun ctxt ->
        assert_equal ~printer
          [DASHDASHDASH; DOTDOTDOT;
           EOF]
          (tokens_of_string {|
---
...
|})
      )
  ; "comment-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a");
           EOF]
          (tokens_of_string {|
# foo
a
|})
      )
  ; "float-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "hr"); COLON;
           (INDENT (0, 5)); (NUMBER "65");
           (DEDENT (0, 5)); EOF]
          (tokens_of_string {|
hr:  65    # Home runs
|})
      )
  ]

open Jsonparse

let printer = show_value_
let docs_printer = show_value_list
let cmp = equal_value_
let docs_cmp = equal_value_list

let of_string_exn = parse_string parse_doc_eoi
let docs_of_string_exn = parse_string parse_docs_eoi

let parsing = "parsing" >::: [
    "simple" >:: (fun ctxt ->
        assert_equal ~printer
          (`String"a")
          (of_string_exn {|a|})
      )
  ; "2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `Null)]))
          (of_string_exn "\na:\n  null")
      )

  ; "3" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `String ("b")); ("c", `String ("d"))]))
          (of_string_exn {|
a: b
c: d
|})
      )
  ; "flow" >:: (fun ctxt ->
        assert_equal ~printer
          (`A ([`String ("a"); `String ("b")]))
          (of_string_exn {|["a", "b"]|})
      )
  ; "flow-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `A ([`String ("a"); `String ("b")]))]))
          (of_string_exn {|
a:
 ["a", "b"]
|})
      )
  ; "indents" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `O ([("b", `String ("c"))]))]))
          (of_string_exn {|
a:
 b: c
|})
      )
  ; "indents-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `O ([("b", `String ("c")); ("d", `String ("e"))]))]))
          (of_string_exn {|
a:
 b: c
 d: e
|})
      )
  ; "indents-3" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `A ([`String ("b"); `String ("d")]))]))
          (of_string_exn {|
a:
 - b
 - d
|})
      )
  ; "rawstring" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("foo"))
          (of_string_exn {|
R"a(foo)a"
|})
      )
  ; "rawstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("foo"))
          (of_string_exn {|
  R"(foo)"
|})
      )
  ; "strings-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `String ("b c"))]))
          (of_string_exn {|
a:
  b
  c
|})
      )
  ; "comment-1" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a"))
          (of_string_exn {|
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
          (of_string_exn {|- Mark McGwire
- Sammy Sosa
- Ken Griffey|})
      )
  ; "float-1" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("hr", `Float (65.)); ("avg", `Float (0.288))]))
          (of_string_exn {|
hr:  65    # Home runs
avg: 0.288
|})
      )
  ]

let preview = "preview" >::: [
    "2.1" >:: (fun ctxt ->
        assert_equal ~printer
          (`A (
              [`String ("Mark McGwire")
              ; `String ("Sammy Sosa")
              ; `String ("Ken Griffey")]
            ))
        (of_string_exn {|- Mark McGwire
- Sammy Sosa
- Ken Griffey|})
      )
  ; "2.2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("hr", `Float (65.)); ("avg", `Float (0.278)); ("rbi", `Float (147.))]))
        (of_string_exn {|hr:  65    # Home runs
avg: 0.278 # Batting average
rbi: 147   # Runs Batted In|})
      )
  ; "2.3" >:: (fun ctxt ->
        assert_equal ~printer
          (`O (
              [("american",
                `A (
                  [`String ("Boston Red Sox"); `String ("Detroit Tigers");
                   `String ("New York Yankees")]
                ));
               ("national",
                `A (
                  [`String ("New York Mets"); `String ("Chicago Cubs");
                   `String ("Atlanta Braves")]
                ))
              ]
            ))
        (of_string_exn {|american:
  - Boston Red Sox
  - Detroit Tigers
  - New York Yankees
national:
  - New York Mets
  - Chicago Cubs
  - Atlanta Braves|})
      )
  ; "2.4" >:: (fun ctxt ->
        assert_equal ~printer
          (`A (
              [`O (
                  [("name", `String ("Mark McGwire")); ("hr", `Float (65.));
                   ("avg", `Float (0.278))]
                );
               `O (
                 [("name", `String ("Sammy Sosa")); ("hr", `Float (63.));
                  ("avg", `Float (0.288))]
               )
              ]
            ))
        (of_string_exn {|-
  name: Mark McGwire
  hr:   65
  avg:  0.278
-
  name: Sammy Sosa
  hr:   63
  avg:  0.288|})
      )
  ; "2.5" >:: (fun ctxt ->
        assert_equal ~printer
          (`A (
              [`A ([`String ("name"); `String ("hr"); `String ("avg")]);
               `A ([`String ("Mark McGwire"); `Float (65.); `Float (0.278)]);
               `A ([`String ("Sammy Sosa"); `Float (63.); `Float (0.288)])]
            ))
        (of_string_exn {|- [name        , hr, avg  ]
- [Mark McGwire, 65, 0.278]
- [Sammy Sosa  , 63, 0.288]

|})
      )
  ; "2.6" >:: (fun ctxt ->
        assert_equal ~printer
          (`O (
              [("Mark McGwire", `O ([("hr", `Float (65.)); ("avg", `Float (0.278))]));
               ("Sammy Sosa", `O ([("hr", `Float (63.)); ("avg", `Float (0.288))]))]
            ))
        (of_string_exn {|Mark McGwire: {hr: 65, avg: 0.278}
Sammy Sosa: {
    hr: 63,
    avg: 0.288
  }|})
      )
  ; "2.7" >:: (fun ctxt ->
        assert_equal ~printer:docs_printer
          [`A (
              [`String ("Mark McGwire"); `String ("Sammy Sosa"); `String ("Ken Griffey")
              ]
            );
           `A ([`String ("Chicago Cubs"); `String ("St Louis Cardinals")])]
        (docs_of_string_exn {|# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals|})
      )
  ; "2.8" >:: (fun ctxt ->
        assert_equal ~printer:docs_printer
          [`O (
              [("time", `String ("20:03:20")); ("player", `String ("Sammy Sosa"));
               ("action", `String ("strike (miss)"))]
            );
           `O (
             [("time", `String ("20:03:47")); ("player", `String ("Sammy Sosa"));
              ("action", `String ("grand slam"))]
           )
          ]
        (docs_of_string_exn {|---
time: "20:03:20"
player: Sammy Sosa
action: strike (miss)
...
---
time: "20:03:47"
player: Sammy Sosa
action: grand slam
...|})
      )
  ; "2.9" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("hr", `A ([`String ("Mark McGwire"); `String ("Sammy Sosa")]));
             ("rbi", `A ([`String ("Sammy Sosa"); `String ("Ken Griffey")]))]
          ))
        (of_string_exn {|---
hr: # 1998 hr ranking
  - Mark McGwire
  - Sammy Sosa
rbi:
  # 1998 rbi ranking
  - Sammy Sosa
  - Ken Griffey|})
      )
  ; "2.12" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`O ([("item", `String ("Super Hoop")); ("quantity", `Float (1.))]);
             `O ([("item", `String ("Basketball")); ("quantity", `Float (4.))]);
             `O ([("item", `String ("Big Shoes")); ("quantity", `Float (1.))])]
          ))
        (of_string_exn {|---
# Products purchased
- item    : Super Hoop
  quantity: 1
- item    : Basketball
  quantity: 4
- item    : Big Shoes
  quantity: 1
|})
      )
  ; "2.13" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("\\//||\\/||\n\
                   // ||  ||__"))
        (of_string_exn {|# ASCII Art
---
  R"(\//||\/||
     // ||  ||__)"|})
      )
  ]

let preview2 = "preview2" >::: [
    "mt" >:: (fun ctxt ->
        ()
      )
  ; "2.14" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("Mark McGwire's year was crippled by a knee injury."))
        (of_string_exn {|--- >
  Mark McGwire's
  year was crippled
  by a knee injury.|})
      )
  ; "2.15" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (
            "Sammy Sosa completed another fine season with great stats.\n\n\
            \  63 Home Runs\n\
            \  0.288 Batting Average\n\n\
             What a year!"
          ))
        (of_string_exn {|>
 Sammy Sosa completed another
 fine season with great stats.

   63 Home Runs
   0.288 Batting Average

 What a year!|})
      )
  ; "2.16" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("name", `String ("Mark McGwire"));
             ("accomplishment",
              `String ("Mark set a major league home run record in 1998.\n"));
             ("stats", `String ("65 Home Runs\n0.278 Batting Average\n"))]
          ))
        (of_string_exn {|name: Mark McGwire
accomplishment: >
  Mark set a major league
  home run record in 1998.
stats: |
  65 Home Runs
  0.278 Batting Average
|})
      )
  ; "2.17" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("unicode", `String ("Sosa did fine.\226\152\186"));
             ("control", `String ("\b1998\t1999\t2000\n"));
             ("hex esc", `String ("\r\n is \r\n"));
             ("single", `String ("\"Howdy!\" he cried."));
             ("quoted", `String (" # Not a 'comment'."));
             ("tie-fighter", `String ("|\\-*-/|"))]
          ))
        (of_string_exn {|unicode: "Sosa did fine.\u263A"
control: "\b1998\t1999\t2000\n"
hex esc: "\x0d\x0a is \r\n"

single: '"Howdy!" he cried.'
quoted: ' # Not a ''comment''.'
tie-fighter: '|\-*-/|'|})
      )
  ; "2.18" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("plain", `String ("This unquoted scalar spans many lines."));
             ("quoted", `String ("So does this quoted scalar.\n"))]
          ))
        (of_string_exn {|plain:
  This unquoted scalar
  spans many lines.

quoted: "So does this
  quoted scalar.\n"
|})
      )
  ; "2.19" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("canonical", `Float (12345.)); ("decimal", `Float (12345.));
             ("octal", `String ("0o14")); ("hexadecimal", `Float (12.))]
          ))
        (of_string_exn {|canonical: 12345
decimal: +12345
octal: 0o14
hexadecimal: 0xC
|})
      )
  ; "2.20" >:: (fun ctxt ->
      assert_equal ~printer
        ~cmp
        (`O (
            [("canonical", `Float (1230.15)); ("exponential", `Float (1230.15));
             ("fixed", `Float (1230.15));
             ("negative infinity", `Float (neg_infinity));
             ("not a number", `Float (nan))]
          ))
        (of_string_exn {|canonical: 1.23015e+3
exponential: 12.3015e+02
fixed: 1230.15
negative infinity: -.inf
not a number: .NaN|})
      )
  ; "2.21" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("null", `Null); ("booleans", `A ([`Bool (true); `Bool (false)]));
             ("string", `String ("012345"))]
          ))
        (of_string_exn {|null:
booleans: [ true, false ]
string: '012345'|})
      )
  ; "2.22" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("canonical", `String ("2001-12-15T02:59:43.1Z"));
             ("iso8601", `String ("2001-12-14t21:59:43.10-05:00"));
             ("spaced", `String ("2001-12-14 21:59:43.10 -5"));
             ("date", `String ("2002-12-14"))]
          ))
        (of_string_exn {|canonical: 2001-12-15T02:59:43.1Z
iso8601: 2001-12-14t21:59:43.10-05:00
spaced: 2001-12-14 21:59:43.10 -5
date: 2002-12-14|})
      )
  ; "2.23" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("not-date", `String ("2002-04-28"));
             ("picture",
              `String (
                "R0lGODlhDAAMAIQAAP//9/X\n17unp5WZmZgAAAOfn515eXv\nPz7Y6OjuDg4J+fn5OTk6enp\n56enmleECcgggoBADs=\n"
              ));
             ("application specific tag",
              `String (
                "The semantics of the tag\nabove may be different for\ndifferent documents.\n"
              ))
            ]
          ))
        (of_string_exn {|---
not-date: !!str 2002-04-28

picture: !!binary |
 R0lGODlhDAAMAIQAAP//9/X
 17unp5WZmZgAAAOfn515eXv
 Pz7Y6OjuDg4J+fn5OTk6enp
 56enmleECcgggoBADs=

application specific tag: !something |
 The semantics of the tag
 above may be different for
 different documents.
|})
      )
  ; "2.24" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "Anchors are not supported when serialising to JSON"
        (fun () -> of_string_exn {|%TAG ! tag:clarkevans.com,2002:
--- !shape
  # Use the ! handle for presenting
  # tag:clarkevans.com,2002:circle
- !circle
  center: &ORIGIN {x: 73, y: 129}
  radius: 7
- !line
  start: *ORIGIN
  finish: { x: 89, y: 102 }
- !label
  start: *ORIGIN
  color: 0xFFEEBB
  text: Pretty vector drawing.|})
      )
  ; "2.25" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("Mark McGwire", `Null); ("Sammy Sosa", `Null); ("Ken Griff", `Null)]))
        (of_string_exn {|# Sets are represented as a
# Mapping where each key is
# associated with a null value
--- !!set
? Mark McGwire
? Sammy Sosa
? Ken Griff|})
      )
  ; "2.26" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`O ([("Mark McGwire", `Float (65.))]);
             `O ([("Sammy Sosa", `Float (63.))]); `O ([("Ken Griffy", `Float (58.))])]
          ))
        (of_string_exn {|# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
--- !!omap
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58|})
      )
  ; "2.27" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "Anchors are not supported when serialising to JSON"
        (fun () -> of_string_exn {|--- !<tag:clarkevans.com,2002:invoice>
invoice: 34843
date   : 2001-01-23
bill-to: &id001
    given  : Chris
    family : Dumars
    address:
        lines: |
            458 Walkman Dr.
            Suite #292
        city    : Royal Oak
        state   : MI
        postal  : 48046
ship-to: *id001
product:
    - sku         : BL394D
      quantity    : 4
      description : Basketball
      price       : 450.00
    - sku         : BL4438H
      quantity    : 1
      description : Super Hoop
      price       : 2392.00
tax  : 251.42
total: 4443.52
comments:
    Late afternoon is best.
    Backup contact is Nancy
    Billsmer @ 338-4338.|})
      )
  ; "2.28" >:: (fun ctxt ->
      warning "example 2.28 has multiple docs: this isn't implemented right" ;
      assert_equal ~printer
        (`O (
            [("Time", `String ("2001-11-23 15:01:42 -5")); ("User", `String ("ed"));
             ("Warning", `String ("This is an error message for the log file"))]
          ))
        (of_string_exn {|---
Time: 2001-11-23 15:01:42 -5
User: ed
Warning:
  This is an error message
  for the log file
---
Time: 2001-11-23 15:02:31 -5
User: ed
Warning:
  A slightly different error
  message.
---
Date: 2001-11-23 15:03:17 -5
User: ed
Fatal:
  Unknown variable "bar"
Stack:
  - file: TopClass.py
    line: 23
    code: |
      x = MoreObject("345\n")
  - file: MoreClass.py
    line: 58
    code: |-
      foo = bar
|})
      )
  ]


let tests = "all" >::: [
    lexing
  ; parsing
  ; preview
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
