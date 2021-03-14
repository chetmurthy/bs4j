open OUnit2
open OUnitTest
open Yaml

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

  ]


let tests = "all" >::: [examples]

if not !Sys.interactive then
  run_test_tt_main tests
;;
