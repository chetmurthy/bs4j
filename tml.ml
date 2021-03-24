
type t =
  {
    name : string
  ; sections : (string * string list) list
  }

let spec_line = Str.regexp "=== \\(.*\\)"
let sect_line = Str.regexp "--- \\(.*\\)"

let match_extract rex groupl s =
  if not (Str.string_match rex s 0) then
    None
  else Some(List.map (fun n -> Str.matched_group n s) groupl)

let parse_lines l =
  let (specl, sectl1, tl) = match l with
      (specl :: sectl1 :: tl) -> (specl, sectl1, tl)
    | _ -> failwith "Tml.mk: need at least two lines"
  in
  match match_extract spec_line [1] specl with
    None -> failwith "Tml.mk: failed to match spec line"
  | Some [name] ->
    let rec sectrec acc (sectname,sectacc) = function
        [] -> List.rev ((sectname, List.rev sectacc)::acc)
      | h::t -> begin match match_extract sect_line [1] h with
            None -> sectrec acc (sectname, h::sectacc) t
          | Some [name] -> sectrec ((sectname, List.rev sectacc)::acc) (name, [h]) t
          | _ -> assert false
        end
    in begin
      match match_extract sect_line [1] sectl1 with
        None -> failwith "Tml.mk: failed to match first section line"
      | Some [sectname] ->
        { name = name ;
          sections = sectrec [] (sectname, [sectl1]) tl }
      | _ -> assert false
    end
  | _ -> assert false

let from_string s =
  let l = String.split_on_char '\n' s in
  parse_lines l

let read_lines ic =
  let rec rerec acc =
    match Stdlib.input_line ic with
      s -> rerec (s::acc)
    | exception End_of_file -> List.rev acc
  in rerec []

let from_channel ic =
  let l = read_lines ic in
  parse_lines l

let from_file f =
  let l = f |> Fpath.v |> Bos.OS.File.read_lines
          |> Rresult.R.get_ok in
  parse_lines l
