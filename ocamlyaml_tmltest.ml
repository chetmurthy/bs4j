open OUnit2
open OUnitTest

let tml_dir = "/home/chet/Hack/Github/yaml/yaml-test-suite/test"

let tml_re = Str.regexp ".*\\.tml$"

let is_tml f =
  Str.string_match tml_re f 0

let tml_files dir =
  let l = Bos.OS.Dir.contents ~rel:false (Fpath.v dir)
          |> Rresult.R.get_ok
          |> List.map Fpath.to_string
  in
  List.filter is_tml l

let make_test fname =
  let open Tml in
  let t = Tml.from_file fname in
  let base = Fpath.(fname |> v |> basename) in
  let name = Fmt.(str "%s (%s)" t.name base) in
  name >:: (fun ctxt ->
      OCamlYAML.exec t
    )

let tests = "OCamlYaml testsuite" >::: (List.map make_test (tml_files tml_dir))

if not !Sys.interactive then
  run_test_tt_main tests
;;
