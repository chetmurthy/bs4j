open OUnit2
open OUnitTest

let testsuite_dir = "/home/chet/Hack/Github/nst/JSONTestSuite"

let skiplist = []

module JSONTestsuite = struct

let printer x = Fmt.(str "%a" Jsontypes.pp_yaml_list x)
let cmp = Jsontypes.equal_yaml_list

let of_string_exn s =
  s
  |> Jsonparse.(parse_string parse_json_stream_eoi)
  |> List.map Jsontypes.json2yaml

let yojson_of_string_exn jsons =
  List.map Jsontypes.json2yaml (Tml.list_of_stream (Yojson.Basic.stream_from_string jsons))

let file_contents fname =
  fname
  |> Fpath.v
  |> Bos.OS.File.read
  |> Rresult.R.get_ok

let exec ok fname =
  let jsons = file_contents fname in
  if ok then
    assert_equal ~printer
      (List.map Jsontypes.canon_yaml (yojson_of_string_exn jsons))
      (List.map Jsontypes.canon_yaml (of_string_exn jsons))
  else
    Tml.assert_raises_exn_pattern ""
      (fun () -> List.map Jsontypes.canon_yaml (of_string_exn jsons))

end

let make_test fname =
  let open Tml in
  let base = Fpath.(fname |> v |> basename) in
  let name = Fmt.(str "[ %s ]" fname) in

  match List.assoc base skiplist with
    msg ->
    let msg = match msg with None -> base | Some s -> s in
    base >::: [
      name >:: (fun ctxt ->
          Tml.warning (Fmt.str "%s: Not handled: %s" fname msg)
        )
    ]
  | exception Not_found -> begin
      match String.get base 0 with
      'y' -> 
        base >::: [
          name >:: (fun ctxt ->
              JSONTestsuite.exec true fname
            )
        ]
      | 'n' ->
        base >::: [
          name >:: (fun ctxt ->
              JSONTestsuite.exec false fname
            )
        ]
      | 'i' ->
        base >::: [
          name >:: (fun ctxt ->
              JSONTestsuite.exec true fname
            )
        ]
      | _ ->
        failwith Fmt.(str "%s: unhandled case in JSONTestsuite.exec" fname)
    end

let exec1 fname =
  JSONTestsuite.exec fname

let tests = "JSONTestsuite" >::: (
    let fl = 
      Fpath.(add_seg (testsuite_dir |> v) "test_parsing")
      |> Bos.OS.Dir.contents
      |> Rresult.R.get_ok
      |> List.map Fpath.to_string in
    List.map make_test fl
  )

if not !Sys.interactive then
  run_test_tt_main tests
;;
