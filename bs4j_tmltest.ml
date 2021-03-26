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
