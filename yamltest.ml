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
  | `String of string ] [@@deriving show]

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
  ; "prototype" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "prototype" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "prototype" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "prototype" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )
  ; "prototype" >:: (fun ctxt ->
      assert_equal ~printer
          (Ok(`Null))
        (of_string {||})
      )

  ]


let tests = "all" >::: [examples]

if not !Sys.interactive then
  run_test_tt_main tests
;;
