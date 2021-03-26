open OUnit2
open OUnitTest

let tml_dir = "/home/chet/Hack/Github/yaml/yaml-test-suite/test"

let skiplist = [
  ("BS4K.tml", None)
; ("L94M.tml", Some "``?'' used for key, unsupported syntax")
; ("RR7F.tml", Some "``?'' used for key, unsupported syntax")
; ("V55R.tml", Some "anchors are unsupported")
; ("2AUY.tml", Some "tags are unsupported")
; ("5U3A.tml", Some "supported syntax in BS4J")
; ("ZCZ6.tml", Some "supported syntax in BS4J")
; ("6KGN.tml", None)
; ("ZH7C.tml", None)
; ("3MYT.tml", None)
; ("8XYN.tml", None)
; ("Y2GN.tml", None)
; ("F2C7.tml", None)
; ("A2M4.tml", Some "``?'' used for key, unsupported syntax")
; ("9KBC.tml", Some "supported syntax in BS4J")
; ("JS2J.tml", None)
; ("E76Z.tml", None)
; ("9C9N.tml", None)
; ("ZWK4.tml", None)
; ("BF9H.tml", None)
; ("EB22.tml", None)
; ("GH63.tml", Some "``?'' used for key, unsupported syntax")
; ("SU5Z.tml", None)
; ("U9NS.tml", Some "``:'' is a special char, cannot be used in raw scalars")
; ("X8DW.tml", Some "``?'' used for key, unsupported syntax")
; ("MYW6.tml", Some "raw-string-literals obviate this")
; ("J3BT.tml", None)
; ("753E.tml", Some "raw-string-literals obviate this")
; ("CN3R.tml", None)
; ("87E4.tml", Some "flow style should be JSON")
; ("6H3V.tml", Some "yaml quotations")
; ("Q5MG.tml", Some "<TAB> at margin")
; ("9DXL.tml", Some "only %BS4J is a valid directive, and only at document-start; multiple docs must all be prefixed with ``---''")
; ("P2AD.tml", Some "neither indentation nor chomping are necessary")
; ("BU8L.tml", None)
; ("LE5A.tml", None)
; ("Z67P.tml", None)
; ("2LFX.tml", Some "directives are unsupported")
; ("PRH3.tml", Some "yaml quotations")
; ("4GC6.tml", Some "yaml quotations")
; ("LE5A.tml", None)
; ("BEC7.tml", Some "change filetype to BS4J; filetype matching is strict")
; ("F8F9.tml", Some "chomping/stripping is unnecessary")
; ("26DV.tml", None)
; ("QF4Y.tml", Some "flow style should be JSON")
; ("8UDB.tml", Some "yaml quotations, also multiline scalars forbidden in flow style")
]

let make_test fname =
  let open Tml in
  let t = Tml.from_file fname in
  let base = Fpath.(fname |> v |> basename) in
  match List.assoc base skiplist with
    msg ->
    let msg = match msg with None -> t.name | Some s -> s in
    let name = Fmt.(str "%s (%s)" t.name base) in
    name >:: (fun ctxt ->
        Tml.warning (Fmt.str "%s: Not handled: %s" fname msg)
      )
  | exception Not_found ->
    let name = Fmt.(str "%s (%s)" t.name base) in
    name >:: (fun ctxt ->
        BS4J.exec t
      )

let parse1 n =
  let file =
    if n |>  Fpath.v |> Bos.OS.File.exists |> Rresult.R.get_ok then n
    else Fpath.(to_string (append (v tml_dir) (v n))) in
  Tml.(BS4J.parse_yaml (from_file file))

let exec1 n =
  let file =
    if n |>  Fpath.v |> Bos.OS.File.exists |> Rresult.R.get_ok then n
    else Fpath.(to_string (append (v tml_dir) (v n))) in
  Tml.(BS4J.exec (from_file file))

let tests = "BS4J testsuite" >::: (
    List.map make_test (Tml.files ~override_dir:"bs4j-overrides" tml_dir)
  )

if not !Sys.interactive then
  run_test_tt_main tests
;;
