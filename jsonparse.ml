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
  | DASH -> ("","-")
  | YAMLSTRING "false" -> ("","false")
  | YAMLSTRING "true" -> ("","true")
  | YAMLSTRING "null" -> ("","null")
  | YAMLSTRING s -> ("YAMLSTRING",s)
  | RAWSTRING s -> ("RAWSTRING",s)
  | INDENT _ _ -> ("INDENT","")
  | DEDENT _ _ -> ("DEDENT","")
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

value lexer_func_of_sedlex_state_located lexfun cs =
  let read1 () =
    try Some (Stream.next cs) with [ Stream.Failure -> None ] in
  let lb = St.mk (Sedlexing.Latin1.from_gen read1)
  in
  let next_token_func () = lexfun lb in
  Plexing.make_stream_and_location next_token_func
;

value lexer = lexer_func_of_sedlex_state_located compatible_lexer ;
value lexer = {Plexing.tok_func = lexer;
 Plexing.tok_using _ = (); Plexing.tok_removing _ = ();
 Plexing.tok_match = Plexing.default_match;
 Plexing.tok_text = Plexing.lexer_text;
 Plexing.tok_comm = None} ;

type _value =
  [= `A of list _value
  | `Bool of bool
  | `Float of (float [@equal fun x y -> 0 = compare x y;])
  | `Null
  | `O of list (string * _value)
  | `String of string ] [@@deriving (show,eq);]
;

value g = Grammar.gcreate lexer;
value (json : Grammar.Entry.e _value) = Grammar.Entry.create g "json";
value json_eoi = Grammar.Entry.create g "json_eoi";

EXTEND
  GLOBAL: json json_eoi ;
  json:
    [ [ "null" -> `Null
      | "true" -> `Bool True
      | "false" -> `Bool False
      | n=NUMBER -> `Float (float_of_string n)
      | s=STRING -> `String s
      | "[" ; l = LIST0 json SEP "," ; "]" -> `A l
      | "{" ; l = LIST0 [ s=STRING ; ":" ; v=json -> (s,v) ] SEP "," ; "}" -> `O l
    ] ]
  ;

  json_eoi : [ [ l = json ; EOI -> l ] ] ;
END;

value parse_json = Grammar.Entry.parse json ;
value parse_json_eoi = Grammar.Entry.parse json_eoi ;

value parse_string pf s =
  pf (Stream.of_string s)
;