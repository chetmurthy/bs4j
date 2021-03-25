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

let skiplist = [
  ("BS4K.tml", None)
; ("3HFZ.tml", None)
; ("RHX7.tml", None)
; ("QLJ7.tml", None)
; ("CVW2.tml", None)
; ("Y2GN.tml", Some "ocaml-yaml doesn't support anchors")
; ("9KBC.tml", None)
; ("9JBA.tml", None)
; ("4H7K.tml", Some "ocaml-yaml doesn't seem to handle extra junk after flow value")
; ("C2SP.tml", None)
; ("9C9N.tml", Some "Why is this even an error?")
; ("KS4U.tml", None)
; ("S98Z.tml", Some "whitespace madness in ocaml-yaml")
; ("DK3J.tml", Some "indentation madness in ocaml-yaml")
; ("FP8R.tml", Some "indentation madness in ocaml-yaml")
; ("EB22.tml", None)
; ("CXX2.tml", None)
; ("X4QW.tml", None)
; ("9HCY.tml", None)
; ("2CMS.tml", None)
; ("S4JQ.tml", Some "ocaml-yaml doesn't deal with tags well")
; ("U99R.tml", Some "ocaml-yaml, yeah, that's busted")
; ("SU5Z.tml", None)
; ("U99R.tml", None)
; ("Q5MG.tml", None)
; ("9DXL.tml", None)
; ("JHB9.tml", None)
; ("6ZKB.tml", None)
; ("AVM7.tml", None)
; ("PUW8.tml", None)
; ("6XDY.tml", None)
; ("KSS4.tml", None)
; ("35KP.tml", None)
; ("RZT7.tml", None)
; ("9KAX.tml", None)
; ("U9NS.tml", None)
; ("7Z25.tml", None)
; ("QB6E.tml", None)
; ("98YD.tml", None)
; ("LE5A.tml", None)
; ("2LFX.tml", None)
; ("BEC7.tml", None)
; ("26DV.tml", None)
; ("V55R.tml", None)
; ("K858.tml", None)
; ("8G76.tml", None)
; ("4MUZ.tml", None)
; ("6BCT.tml", None)
; ("6KGN.tml", None)
; ("8XYN.tml", None)
; ("4FJ6.tml", None)
; ("CFD4.tml", None)
; ("HWV9.tml", None)
; ("X38W.tml", None)
; ("QT73.tml", None)
; ("A2M4.tml", None)
; ("6BFJ.tml", None)
; ("RZP5.tml", None)
; ("JS2J.tml", None)
; ("UGM3.tml", None)
; ("9MMW.tml", None)
; ("6M2F.tml", None)
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
        OCamlYAML.exec t
      )

let parse1 n =
  Tml.(OCamlYAML.parse_yaml (from_file Fpath.(to_string (append (v tml_dir) (v n)))))

let tests = "OCamlYaml testsuite" >::: (List.map make_test (tml_files tml_dir))

if not !Sys.interactive then
  run_test_tt_main tests
;;
