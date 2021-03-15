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
  | `Float of (float [@equal fun x y -> 0 = compare x y])
  | `Null
  | `O of (string * value) list
  | `String of string ] [@@deriving show,eq]

let printer x = Fmt.(str "%a" pp_value x)

let cmp = equal_value

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
      warning "example 2.7 has multiple docs: this isn't implemented right" ;
        assert_equal ~printer
          (`A (
              [`String ("Mark McGwire"); `String ("Sammy Sosa"); `String ("Ken Griffey")]
            ))
        (of_string_exn {|# Ranking of 1998 home runs
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
          (`O (
              [("time", `String ("20:03:20")); ("player", `String ("Sammy Sosa"));
               ("action", `String ("strike (miss)"))]
            ))
        (of_string_exn {|---
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
--- |
  \//||\/||
  // ||  ||__|})
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


let characters = "characters" >::: [
    "5.3" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("sequence", `A ([`String ("one"); `String ("two")]));
             ("mapping", `O ([("sky", `String ("blue")); ("sea", `String ("green"))]))
            ]
          ))
        (of_string_exn {|sequence:
  - one
  - two
mapping:
  ? sky
  : blue
  sea : green|})
      )
  ; "5.4" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("sequence", `A ([`String ("one"); `String ("two")]));
             ("mapping", `O ([("sky", `String ("blue")); ("sea", `String ("green"))]))
            ]
          ))
        (of_string_exn {|
sequence: [ one, two, ]
mapping: { sky: blue, sea: green }|})
      )
  ; "5.5" >:: (fun ctxt ->
      assert_equal ~printer
        (`Null)
        (of_string_exn {|# Comment only.|})
      )
  ; "5.6" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "Anchors are not supported when serialising to JSON"
        (fun () -> of_string_exn {|anchored: !local &anchor value
alias: *anchor|})
      )
  ; "5.7" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("literal", `String ("some\ntext\n")); ("folded", `String ("some text"))]))
        (of_string_exn {|literal: |
  some
  text
folded: >
  some
  text|})
      )
  ; "5.8" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("single", `String ("text")); ("double", `String ("text"))]))
        (of_string_exn {|
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
        (`String ("Line break (no glyph)\nLine break (glyphed)\n"))
        (of_string_exn {||
  Line break (no glyph)
  Line break (glyphed)
|})
      )
  ; "5.12" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("quoted", `String ("Quoted \t"));
             ("block", `String ("void main() {\n\
                                 \tprintf(\"Hello, world!\\n\");\n\
                                 }"))]
          ))
        (of_string_exn {|# Tabs and spaces
quoted: "Quoted 	"
block:	|
  void main() {
  	printf("Hello, world!\n");
  }|})
      )
  ; "5.13" >:: (fun ctxt ->
      warning "example 5.13 won't work b/c ocaml-yaml doesn't handle in-string NULLs right" ;
      assert_equal ~printer
        (`String ("Fun with \x5C \x22 \x07 \x08 \x1B \x0C \x0A \x0D \x09 \x0B \x00 \x20 \xA0 \x85 \u2028 \u2029 A A A"))
        (of_string_exn {|"Fun with \\
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

  ]


let basic_structures = "basic structures" >::: [
    "6.1" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("Not indented",
              `O (
                [("By one space", `String ("By four\n  spaces\n"));
                 ("Flow style",
                  `A (
                    [`String ("By two"); `String ("Also by two");
                     `String ("Still by two")]
                  ))
                ]
              ))
            ]
          ))
        (of_string_exn {|  # Leading comment line spaces are
   # neither content nor indentation.
    
Not indented:
 By one space: |
    By four
      spaces
 Flow style: [    # Leading spaces
   By two,        # in flow style
  Also by two,    # are neither
  	Still by two   # content nor
    ]             # indentation.|})
      )
  ; "6.2" >:: (fun ctxt ->
      warning "example 6.2 won't work b/c ocaml-yaml refuses to parse it (but perl's YAML::PP (1.2-compat) does)" ;
      assert_raises_exn_pattern
        "found character that cannot start any token"
        (fun () ->
      assert_equal ~printer
        (`Null)
        (of_string_exn "? a\n\
                        : -\tb\n\
                       \  -  -\tc\
                       \     - d"))
      )
  ; "6.3" >:: (fun ctxt ->
      warning "example 6.3 won't work b/c ocaml-yaml refuses to parse it (but perl's YAML::PP (1.2-compat) does)" ;
      assert_raises_exn_pattern
        "found character that cannot start any token"
        (fun () ->
      assert_equal ~printer
          (`Null)
        (of_string_exn "- foo:\t bar\n\
                        - - baz\n\
                       \  -\tbaz"))
      )
  ; "6.4" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("plain", `String ("text lines")); ("quoted", `String ("text lines"));
             ("block", `String ("text\n \tlines"))]
          ))
        (of_string_exn {|
plain: text
  lines
quoted: "text
  	lines"
block: |
  text
   	lines|})
      )
  ; "6.5" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("Folding", `String ("Empty line\nas a line feed"));
             ("Chomping", `String ("Clipped empty lines\n"))]
          ))
        (of_string_exn {|
Folding:
  "Empty line
   	
  as a line feed"
Chomping: |
  Clipped empty lines
 |})
      )
  ; "6.6" >:: (fun ctxt ->
      assert_equal ~printer
          (`String ("trimmed\n\n\nas space"))
        (of_string_exn {|>-
  trimmed
  
 

  as
  space|})
      )
  ; "6.7" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("foo \n\n\t bar\n\nbaz\n"))
        (of_string_exn {|>
  foo 
 
  	 bar

  baz
|})
      )
  ; "6.8" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (" foo\nbar\nbaz "))
        (of_string_exn {|"
  foo 
 
  	 bar

  baz
"|})
      )
  ; "6.9" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("key", `String ("value"))]))
        (of_string_exn {|key:    # Comment
  value|})
      )
  ; "6.10" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {|  # Comment
   
|})
      )
  ; "6.11" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("key", `String ("value"))]))
        (of_string_exn {|key:    # Comment
        # lines
  value
|})
      )
  ; "6.12" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "only string keys are supported"
        (fun () -> of_string_exn {|{ first: Sammy, last: Sosa }:
# Statistics:
  hr:  # Home runs
     65
  avg: # Average
   0.278|})
      )
  ; "6.13" >:: (fun ctxt ->
      warning "example 6.13 won't work b/c ocaml-yaml refuses to parse it (but perl's YAML::PP (1.2-compat) does)" ;
      assert_raises_exn_pattern
        "found unknown directive"
        (fun () ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {|%FOO  bar baz # Should be ignored
               # with a warning.
--- "foo"|}))
      )
  ; "6.14" >:: (fun ctxt ->
      warning "example 6.14 won't work b/c ocaml-yaml refuses to parse it (but perl's YAML::PP (1.2-compat) does)" ;
      assert_raises_exn_pattern
        "found incompatible YAML document"
        (fun () ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {|%YAML 1.3 # Attempt parsing
           # with a warning
---
"foo"|}))
      )
  ; "6.15" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "found incompatible YAML document"
        (fun () -> of_string_exn {|%YAML 1.2
%YAML 1.1
foo|})
      )
  ; "6.16" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("foo"))
        (of_string_exn {|%TAG !yaml! tag:yaml.org,2002:
---
!yaml!str "foo"|})
      )
  ; "6.17" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "found duplicate %TAG directive"
        (fun () ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {|%TAG ! !foo
%TAG ! !foo
bar|}))
      )
  ; "6.18" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("bar"))
        (of_string_exn {|# Private
!foo "bar"
...
# Global
%TAG ! tag:example.com,2000:app/
---
!foo "bar"|})
      )
  ; "6.19" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("1 - 3"))
        (of_string_exn {|
%TAG !! tag:example.com,2000:app/
---
!!int 1 - 3 # Interval, not integer|})
      )
  ; "6.20" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("bar"))
        (of_string_exn {|
%TAG !e! tag:example.com,2000:app/
---
!e!foo "bar"|})
      )
  ; "6.21" >:: (fun ctxt ->
      warning "example 6.21 has multiple docs: this isn't implemented right" ;
      assert_equal ~printer
        (`String ("fluorescent"))
        (of_string_exn {|%TAG !m! !my-
--- # Bulb here
!m!light fluorescent
...
%TAG !m! !my-
--- # Color here
!m!light green|})
      )
  ; "6.22" >:: (fun ctxt ->
      assert_equal ~printer
        (`A ([`String ("bar")]))
        (of_string_exn {|%TAG !e! tag:example.com,2000:app/
---
- !e!foo "bar"|})
      )
  ; "6.23" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "Anchors are not supported when serialising to JSON"
        (fun () -> of_string_exn {|!!str &a1 "foo":
  !!str bar
&a2 baz : *a1|})
      )
  ; "6.24" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("foo", `String ("baz"))]))
        (of_string_exn {|!<tag:yaml.org,2002:str> foo :
  !<!bar> baz|})
      )
  ; "6.25" >:: (fun ctxt ->
      warning "example 6.25 has invalid verbatim tags: this isn't implemented right" ;
      assert_equal ~printer
        (`A ([`String ("foo"); `String ("bar")]))
        (of_string_exn {|- !<!> foo
- !<$:?> bar|})
      )
  ; "6.26" >:: (fun ctxt ->
      assert_equal ~printer
        (`A ([`String ("foo"); `String ("bar"); `String ("baz")]))
        (of_string_exn {|%TAG !e! tag:example.com,2000:app/
---
- !local foo
- !!str bar
- !e!tag%21 baz|})
      )
  ; "6.27" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "did not find expected tag URI"
        (fun () ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {|%TAG !e! tag:example,2000:app/
---
- !e! foo
- !h!bar baz|}))
      )
  ; "6.28" >:: (fun ctxt ->
      assert_equal ~printer
        (`A ([`String ("12"); `Float (12.); `Float (12.)]))
        (of_string_exn {|# Assuming conventional resolution:
- "12"
- 12
- ! 12|})
      )
  ; "6.29" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "Anchors are not supported when serialising to JSON"
        (fun () -> of_string_exn {|First occurrence: &anchor Value
Second occurrence: *anchor|})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {||})
      )
  ]

let flow_styles = "flow styles" >::: [
    "7.1" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "Anchors are not supported when serialising to JSON"
        (fun () -> of_string_exn {|First occurrence: &anchor Foo
Second occurrence: *anchor
Override anchor: &anchor Bar
Reuse anchor: *anchor|})
      )
  ; "7.2" >:: (fun ctxt ->
      warning "example 7.2 not accepted by ocaml-yaml (b/c 1.1)" ;
      assert_raises_exn_pattern
        "did not find expected"
        (fun () ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {|{
  foo : !!str,
  !!str : bar,
}|}))
      )
  ; "7.3" >:: (fun ctxt ->
      warning "example 7.2 not accepted by ocaml-yaml (b/c 1.1)" ;
      assert_raises_exn_pattern
        "found unexpected ':' character"
        (fun () ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {|{
  ? foo :,
  : bar,
}|}))
      )
  ; "7.4" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("implicit block key",
              `A ([`O ([("implicit flow key", `String ("value"))])]))]
          ))
        (of_string_exn {|"implicit block key" : [
  "implicit flow key" : value,
 ]|})
      )
  ; "7.5" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("folded to a space,\nto a line feed, or \t \tnon-content"))
        (of_string_exn {|"folded 
to a space,	
 
to a line feed, or 	\
 \ 	non-content"|})
      )
  ; "7.6" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (" 1st non-empty\n2nd non-empty 3rd non-empty "))
        (of_string_exn {|" 1st non-empty

 2nd non-empty 
	3rd non-empty "|})
      )
  ; "7.7" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("here's to \"quotes\""))
        (of_string_exn {| 'here''s to "quotes"'|})
      )
  ; "7.8" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("implicit block key",
              `A ([`O ([("implicit flow key", `String ("value"))])]))]
          ))
        (of_string_exn {|'implicit block key' : [
  'implicit flow key' : value,
 ]|})
      )
  ; "7.9" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (" 1st non-empty\n2nd non-empty 3rd non-empty "))
        (of_string_exn {|
' 1st non-empty

 2nd non-empty 
	3rd non-empty '|})
      )
  ; "7.10" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "did not find expected node content"
        (fun () ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {|# Outside flow collection:
- ::vector
- ": - ()"
- Up, up, and away!
- -123
- http://example.com/foo#bar
# Inside flow collection:
- [ ::vector,
  ": - ()",
  "Up, up and away!",
  -123,
  http://example.com/foo#bar ]
|}))
      )
  ; "7.10-::vector" >:: (fun ctxt ->
      assert_equal ~printer
        (`A ([`String ("::vector"); `A ([`String ("::vector")])]))
        (of_string_exn {|# Outside flow collection:
- ::vector
- [ "::vector" ]
|})
      )
  ; "7.10-::vector-busted" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "did not find expected node content"
        (fun () ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {|# Outside flow collection:
- ::vector
- [ ::vector ]
|}))
      )
  ; "7.11" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("implicit block key",
              `A ([`O ([("implicit flow key", `String ("value"))])]))]
          ))
        (of_string_exn {|
implicit block key : [
  implicit flow key : value,
 ]|})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {||})
      )

  ]
let examples = "examples" >::: [
    "" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {||})
      )
  ; "" >:: (fun ctxt ->
      assert_equal ~printer
          (`Null)
        (of_string_exn {||})
      )

  ]

let tests = "all" >::: [
    preview ; characters ; basic_structures ; flow_styles
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
