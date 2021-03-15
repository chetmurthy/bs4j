open OUnit2
open OUnitTest
open Yaml
open Pa_ppx_testutils

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

type value =
  [ `A of value list
  | `Bool of bool
  | `Float of float
  | `Null
  | `O of (string * value) list
  | `String of string ] [@@deriving show,eq]

let yaml_res_pp pps x = Rresult.R.(pp ~error:pp_msg) ~ok:pp_value pps x

let printer x = Fmt.(str "%a" yaml_res_pp x)

let examples = "examples" >::: [
    "empty" >:: (fun ctxt ->
        ()
      )
  ; "2.1" >:: (fun ctxt ->
        assert_equal ~printer
          (Ok(`A (
               [`String ("Mark McGwire")
               ; `String ("Sammy Sosa")
               ; `String ("Ken Griffey")]
             )))
        (of_string {|- Mark McGwire
- Sammy Sosa
- Ken Griffey|})
      )
  ; "2.2" >:: (fun ctxt ->
        assert_equal ~printer
          (Ok(`O ([("hr", `Float (65.)); ("avg", `Float (0.278)); ("rbi", `Float (147.))])))
        (of_string {|hr:  65    # Home runs
avg: 0.278 # Batting average
rbi: 147   # Runs Batted In|})
      )
  ; "2.3" >:: (fun ctxt ->
        assert_equal ~printer
          (Ok(`O (
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
             )))
        (of_string {|american:
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
          (Ok(`A (
               [`O (
                   [("name", `String ("Mark McGwire")); ("hr", `Float (65.));
                    ("avg", `Float (0.278))]
                 );
                `O (
                  [("name", `String ("Sammy Sosa")); ("hr", `Float (63.));
                   ("avg", `Float (0.288))]
                )
               ]
             )))
        (of_string {|-
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
          (Ok(`A (
               [`A ([`String ("name"); `String ("hr"); `String ("avg")]);
                `A ([`String ("Mark McGwire"); `Float (65.); `Float (0.278)]);
                `A ([`String ("Sammy Sosa"); `Float (63.); `Float (0.288)])]
             )))
        (of_string {|- [name        , hr, avg  ]
- [Mark McGwire, 65, 0.278]
- [Sammy Sosa  , 63, 0.288]

|})
      )
  ; "2.6" >:: (fun ctxt ->
        assert_equal ~printer
          (Ok(`O (
               [("Mark McGwire", `O ([("hr", `Float (65.)); ("avg", `Float (0.278))]));
                ("Sammy Sosa", `O ([("hr", `Float (63.)); ("avg", `Float (0.288))]))]
             )))
        (of_string {|Mark McGwire: {hr: 65, avg: 0.278}
Sammy Sosa: {
    hr: 63,
    avg: 0.288
  }|})
      )
  ; "2.7" >:: (fun ctxt ->
      warning "example 2.7 has multiple docs: this isn't implemented right" ;
        assert_equal ~printer
          (Ok(`A (
               [`String ("Mark McGwire"); `String ("Sammy Sosa"); `String ("Ken Griffey")]
             )))
        (of_string {|# Ranking of 1998 home runs
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
      warning "example 2.8 has multiple docs: this isn't implemented right" ;
        assert_equal ~printer
          (Ok(`O (
               [("time", `String ("20:03:20")); ("player", `String ("Sammy Sosa"));
                ("action", `String ("strike (miss)"))]
             )))
        (of_string {|---
time: 20:03:20
player: Sammy Sosa
action: strike (miss)
...
---
time: 20:03:47
player: Sammy Sosa
action: grand slam
...|})
      )
  ; "2.9" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`O (
  [("hr", `A ([`String ("Mark McGwire"); `String ("Sammy Sosa")]));
    ("rbi", `A ([`String ("Sammy Sosa"); `String ("Ken Griffey")]))]
  )))
        (of_string {|---
hr: # 1998 hr ranking
  - Mark McGwire
  - Sammy Sosa
rbi:
  # 1998 rbi ranking
  - Sammy Sosa
  - Ken Griffey|})
      )
  ; "2.10" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "Anchors are not supported when serialising to JSON"
        (fun () -> of_string_exn {|---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey|})
      )
  ; "2.11" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "only string keys are supported"
        (fun () -> of_string_exn {|? - Detroit Tigers
  - Chicago cubs
:
  - 2001-07-23

? [ New York Yankees,
    Atlanta Braves ]
: [ 2001-07-02, 2001-08-12,
    2001-08-14 ]|})
      )
  ; "2.12" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok (`A (
            [`O ([("item", `String ("Super Hoop")); ("quantity", `Float (1.))]);
             `O ([("item", `String ("Basketball")); ("quantity", `Float (4.))]);
             `O ([("item", `String ("Big Shoes")); ("quantity", `Float (1.))])]
          )))
        (of_string {|---
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
          (Ok(`String ("\\//||\\/||\n\
                        // ||  ||__")))
        (of_string {|# ASCII Art
--- |
  \//||\/||
  // ||  ||__|})
      )
  ; "2.14" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`String ("Mark McGwire's year was crippled by a knee injury.")))
        (of_string {|--- >
  Mark McGwire's
  year was crippled
  by a knee injury.|})
      )
  ; "2.15" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`String (
  "Sammy Sosa completed another fine season with great stats.\n\n\
  \  63 Home Runs\n\
  \  0.288 Batting Average\n\n\
   What a year!"
  )))
        (of_string {|>
 Sammy Sosa completed another
 fine season with great stats.

   63 Home Runs
   0.288 Batting Average

 What a year!|})
      )
  ; "2.16" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("name", `String ("Mark McGwire"));
              ("accomplishment",
               `String ("Mark set a major league home run record in 1998.\n"));
              ("stats", `String ("65 Home Runs\n0.278 Batting Average\n"))]
           )))
        (of_string {|name: Mark McGwire
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
        (Ok(`O (
             [("unicode", `String ("Sosa did fine.\226\152\186"));
              ("control", `String ("\b1998\t1999\t2000\n"));
              ("hex esc", `String ("\r\n is \r\n"));
              ("single", `String ("\"Howdy!\" he cried."));
              ("quoted", `String (" # Not a 'comment'."));
              ("tie-fighter", `String ("|\\-*-/|"))]
           )))
        (of_string {|unicode: "Sosa did fine.\u263A"
control: "\b1998\t1999\t2000\n"
hex esc: "\x0d\x0a is \r\n"

single: '"Howdy!" he cried.'
quoted: ' # Not a ''comment''.'
tie-fighter: '|\-*-/|'|})
      )
  ; "2.18" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("plain", `String ("This unquoted scalar spans many lines."));
              ("quoted", `String ("So does this quoted scalar.\n"))]
           )))
        (of_string {|plain:
  This unquoted scalar
  spans many lines.

quoted: "So does this
  quoted scalar.\n"
|})
      )
  ; "2.19" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("canonical", `Float (12345.)); ("decimal", `Float (12345.));
              ("octal", `String ("0o14")); ("hexadecimal", `Float (12.))]
           )))
        (of_string {|canonical: 12345
decimal: +12345
octal: 0o14
hexadecimal: 0xC
|})
      )
  ; "2.20" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("canonical", `Float (1230.15)); ("exponential", `Float (1230.15));
              ("fixed", `Float (1230.15));
              ("negative infinity", `Float (neg_infinity));
              ("not a number", `Float (nan))]
           )))
        (of_string {|canonical: 1.23015e+3
exponential: 12.3015e+02
fixed: 1230.15
negative infinity: -.inf
not a number: .NaN|})
      )
  ; "busted-2.20" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("not a number", `Float (nan))]
           )))
        (of_string {|not a number: .NaN|})
      )
  ; "2.21" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("null", `Null); ("booleans", `A ([`Bool (true); `Bool (false)]));
              ("string", `String ("012345"))]
           )))
        (of_string {|null:
booleans: [ true, false ]
string: '012345'|})
      )
  ; "2.22" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("canonical", `String ("2001-12-15T02:59:43.1Z"));
              ("iso8601", `String ("2001-12-14t21:59:43.10-05:00"));
              ("spaced", `String ("2001-12-14 21:59:43.10 -5"));
              ("date", `String ("2002-12-14"))]
           )))
        (of_string {|canonical: 2001-12-15T02:59:43.1Z
iso8601: 2001-12-14t21:59:43.10-05:00
spaced: 2001-12-14 21:59:43.10 -5
date: 2002-12-14|})
      )
  ; "2.23" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
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
           )))
        (of_string {|---
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
          (Ok(`O ([("Mark McGwire", `Null); ("Sammy Sosa", `Null); ("Ken Griff", `Null)])))
        (of_string {|# Sets are represented as a
# Mapping where each key is
# associated with a null value
--- !!set
? Mark McGwire
? Sammy Sosa
? Ken Griff|})
      )
  ; "2.26" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`A (
             [`O ([("Mark McGwire", `Float (65.))]);
              `O ([("Sammy Sosa", `Float (63.))]); `O ([("Ken Griffy", `Float (58.))])]
           )))
        (of_string {|# Ordered maps are represented as
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
        (Ok(`O (
             [("Time", `String ("2001-11-23 15:01:42 -5")); ("User", `String ("ed"));
              ("Warning", `String ("This is an error message for the log file"))]
           )))
        (of_string {|---
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
  ; "5.3" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("sequence", `A ([`String ("one"); `String ("two")]));
              ("mapping", `O ([("sky", `String ("blue")); ("sea", `String ("green"))]))
             ]
           )))
        (of_string {|sequence:
  - one
  - two
mapping:
  ? sky
  : blue
  sea : green|})
      )
  ; "5.4" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("sequence", `A ([`String ("one"); `String ("two")]));
              ("mapping", `O ([("sky", `String ("blue")); ("sea", `String ("green"))]))
             ]
           )))
        (of_string {|
sequence: [ one, two, ]
mapping: { sky: blue, sea: green }|})
      )
  ; "5.5" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {|# Comment only.|})
      )
  ; "5.6" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "Anchors are not supported when serialising to JSON"
        (fun () -> of_string_exn {|anchored: !local &anchor value
alias: *anchor|})
      )
  ; "5.7" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("literal", `String ("some\ntext\n")); ("folded", `String ("some text"))])))
        (of_string {|literal: |
  some
  text
folded: >
  some
  text|})
      )
  ; "5.8" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O ([("single", `String ("text")); ("double", `String ("text"))])))
        (of_string {|
single: 'text'
double: "text"|})
      )
  ; "5.9" >:: (fun ctxt ->
      warning "example 5.9 won't work b/c ocaml-yaml is 1.1-compat" ;
      assert_raises_exn_pattern
        "incompatible YAML document"
        (fun () -> of_string_exn {|%YAML 1.2
--- text|})
      )
  ; "5.10" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "found character that cannot start any token"
        (fun () -> of_string_exn {|commercial-at: @text
grave-accent: `text|})
      )
  ; "5.11" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`String ("Line break (no glyph)\nLine break (glyphed)\n")))
        (of_string {||
  Line break (no glyph)
  Line break (glyphed)
|})
      )
  ; "5.12" >:: (fun ctxt ->
      assert_equal ~printer
        (Ok(`O (
             [("quoted", `String ("Quoted \t"));
              ("block", `String ("void main() {\n\
                                  \tprintf(\"Hello, world!\\n\");\n\
                                  }"))]
           )))
        (of_string {|# Tabs and spaces
quoted: "Quoted 	"
block:	|
  void main() {
  	printf("Hello, world!\n");
  }|})
      )
  ; "5.13" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`String ("Fun with \x5C \x22 \x07 \x08 \x1B \x0C \x0A \x0D \x09 \x0B \x00 \x20 \xA0 \x85 \u2028 \u2029 A A A")))
        (of_string {|"Fun with \\
\" \a \b \e \f \
\n \r \t \v \0 \
\  \_ \N \L \P \
\x41 \u0041 \U00000041"|})
      )
  ; "5.14" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "unknown escape character"
        (fun () -> of_string_exn {|Bad escapes:
  "\c
  \xq-"|})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )

  ]


let tests = "all" >::: [examples]

if not !Sys.interactive then
  run_test_tt_main tests
;;
