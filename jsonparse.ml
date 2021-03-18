open Jsontoken ;

value positions_to_loc ?{comments=""} (spos, epos) =
  let open Lexing in
  Ploc.make_loc spos.pos_fname spos.pos_lnum spos.pos_bol (spos.pos_cnum, epos.pos_cnum) comments
;

value compatible_lexer lb =
  let (t, pos) = Jsontoken.jsontoken lb in
  let pos = positions_to_loc pos in
  let t = match t with [
    LBRACKET -> ("","[")
  | RBRACKET -> ("","]")
  | LBRACE -> ("","{")
  | RBRACE -> ("","}")
  | COLON -> ("",":")
  | COMMA -> ("",",")
  | KWFALSE -> ("","false")
  | KWTRUE -> ("","true")
  | KWNULL -> ("","null")
  | NUMBER s -> ("NUMBER",s)
  | STRING s -> ("STRING",s)
  | EOF -> ("EOI","")

  ] in
  (t, pos)
;

(* camlp5r *)
(* calc.ml,v *)

value input_file = ref "" ;
value nonws_re = Pcre.regexp "\\S" ;
value has_nonws s = Pcre.pmatch ~{rex=nonws_re} s;

value lexer_func_of_sedlex_located lexfun cs =
  let read1 () =
    try Some (Stream.next cs) with [ Stream.Failure -> None ] in
  let lb = Sedlexing.Latin1.from_gen read1
  in
  let next_token_func () = lexfun lb in
  Plexing.make_stream_and_location next_token_func
;

value lexer = lexer_func_of_sedlex_located compatible_lexer ;
value lexer = {Plexing.tok_func = lexer;
 Plexing.tok_using _ = (); Plexing.tok_removing _ = ();
 Plexing.tok_match = Plexing.default_match;
 Plexing.tok_text = Plexing.lexer_text;
 Plexing.tok_comm = None} ;

type json = [
    Null
  | Bool of bool
  | Number of string
  | String of string
  | Arr of list json
  | Obj of list (string * json)
]
;

value g = Grammar.gcreate lexer;
value json = Grammar.Entry.create g "json";
value json_eoi = Grammar.Entry.create g "json_eoi";

EXTEND
  GLOBAL: json json_eoi ;
  json:
    [ [ "null" -> Null
      | "true" -> Bool True
      | "false" -> Bool False
      | n=NUMBER -> Number n
      | s=STRING -> String s
      | "[" ; l = LIST0 json SEP "," ; "]" -> Arr l
      | "{" ; l = LIST0 [ s=STRING ; ":" ; v=json -> (s,v) ] SEP "," ; "}" -> Obj l
    ] ]
  ;

  json_eoi : [ [ l = json ; EOI -> l ] ] ;
END;

value parse_json = Grammar.Entry.parse json ;
value parse_json_eoi = Grammar.Entry.parse json_eoi ;

value parse_string pf s =
  pf (Stream.of_string s)
;
