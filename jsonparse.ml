open Jsontoken ;
open Jsontypes ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;

type t += [
    Exc of Ploc.t and t[@rebind_to Ploc.Exc;][@name "Ploc.Exc";]
] [@@deriving show;]
;

value print_exn exn = Some (show exn) ;
Printexc.register_printer print_exn ;

value positions_to_loc ?{comments=""} (spos, epos) =
  let open Lexing in
  Ploc.make_loc spos.pos_fname spos.pos_lnum spos.pos_bol (spos.pos_cnum, epos.pos_cnum) comments
;

value compatible_lexer lb =
  let ((tok, pos) as t) = Jsontoken.jsontoken lb in
  let pos = positions_to_loc pos in
  let tok = match tok with [
    BS4J s -> ("BS4J",s)
  | LBRACKET -> ("","[")
  | RBRACKET -> ("","]")
  | LBRACE -> ("","{")
  | RBRACE -> ("","}")
  | COLON -> ("",":")
  | COMMA -> ("",",")
  | DASH -> ("","-")
  | DASHDASHDASH -> ("","---")
  | DOTDOTDOT -> ("","...")
  | BAR -> ("","|")
  | BARDASH -> ("","|-")
  | GT -> ("",">")
  | GTDASH -> ("",">-")
  | PLUS -> ("","+")
  | YAMLSTRING "false" -> ("","false")
  | YAMLSTRING "true" -> ("","true")
  | YAMLSTRING "null" -> ("","null")
  | YAMLSTRING s -> ("YAMLSTRING",s)
  | YAMLSQSTRING s -> ("YAMLSQSTRING",s)
  | YAMLDQSTRING s -> ("YAMLDQSTRING",s)
  | RAWSTRING s ->
    ("RAWSTRING",s)
  | INDENT _ _ -> ("INDENT","")
  | DEDENT _ _ -> ("DEDENT","")
  | DECIMAL s -> ("DECIMAL",s)
  | HEXADECIMAL s -> ("HEXADECIMAL",s)
  | OCTAL s -> ("OCTAL",s)
  | JSONSTRING s -> ("JSONSTRING", s)
  | EOF -> ("EOI","")

  ] in
  (tok, pos)
;

value lex_string s =
  let st = St.mk (Sedlexing.Latin1.from_gen (gen_of_string s)) in
  let rec lexrec acc =
    match compatible_lexer st with [
      (("EOI",_),_) as t -> List.rev [t::acc]
    | t -> lexrec [t::acc]
    ] in lexrec []
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

value g = Grammar.gcreate lexer;
value (json : Grammar.Entry.e json) = Grammar.Entry.create g "json";
value (flow_json : Grammar.Entry.e json) = Grammar.Entry.create g "flow_json";
value (scalar : Grammar.Entry.e json) = Grammar.Entry.create g "scalar";
value (flow_scalar : Grammar.Entry.e json) = Grammar.Entry.create g "flow_scalar";
value json_eoi = Grammar.Entry.create g "json_eoi";
value (doc : Grammar.Entry.e json) = Grammar.Entry.create g "doc";
value (doc_eoi : Grammar.Entry.e json) = Grammar.Entry.create g "doc_eoi";
value (docs : Grammar.Entry.e (list json)) = Grammar.Entry.create g "docs";
value (docs_eoi : Grammar.Entry.e (list json)) = Grammar.Entry.create g "docs_eoi";

value string_of_scalar = fun [
  `String s -> s
| `Float f -> string_of_float f
| `Null -> "null"
| `Bool True -> "true"
| `Bool False -> "false"
| _ -> assert False
]
;

EXTEND
  GLOBAL: flow_json json json_eoi flow_scalar
          doc doc_eoi docs docs_eoi ;
  doc: [ [ v=json -> v
         | "---" ; v=json -> v
         | "---" ; v=json ; "..." -> v
         | v=json ; "..." -> v
    ] ]
  ;
  delim_doc: [ [ "---" ; v=json -> v
               | "---" ; v=json ; OPT "..." -> v

    ] ]
  ;
  docs: [ [ l = LIST1 delim_doc -> l
          | v=json -> [v]
    ] ]
  ;

  flow_json:
    [ [ s = flow_scalar -> s

      | "[" ; l = LIST0 flow_json SEP "," ; "]" -> `List l
      | "{" ; l = LIST0 [ s = flow_scalar ; ":" ; v=flow_json -> (string_of_scalar s,v) ] SEP "," ; "}" -> `Assoc l
    ] ]
  ;

  block_members:
    [ [ s = scalar_nonstring -> s
      | s = scalar_nonstring ; ":" ; v=json ;
         l = LIST0 [ s=key_scalar ; ":" ; v=json -> (string_of_scalar s,v) ]
         -> `Assoc [(string_of_scalar s,v) :: l]

      | (fold,chomp) = must_fold_chomp ; (s,l) = scalar_rawstring0 ->
         let s = unquote_rawstring ~{fold} ~{chomp} l s in
        `String s

      | (fold, chomp) = must_fold_chomp ; (s,l) = scalar_rawstring0 ; ":" ; v=json ;
         rest = LIST0 [ s=key_scalar ; ":" ; v=json -> (string_of_scalar s,v) ] ->
         let s = unquote_rawstring ~{fold} ~{chomp} l s in
         `Assoc [(s,v) :: rest]

      | (s,l) = scalar_rawstring0 ->
         let s = unquote_rawstring ~{fold=False} ~{chomp=False} l s in
        `String s

      | (s,l) = scalar_rawstring0 ; ":" ; v=json ;
         rest = LIST0 [ s=key_scalar ; ":" ; v=json -> (string_of_scalar s,v) ] ->
         let s = unquote_rawstring ~{fold=False} ~{chomp=False} l s in
         `Assoc [(s,v) :: rest]

      | s = YAMLSTRING ; l = LIST0 [ s = YAMLSTRING -> s ] -> `String (foldchomp_yamlstrings ~{fold=True} ~{chomp=True} [s::l])
      | s = YAMLSTRING ; ":" ; v=json ;
         l = LIST0 [ s=key_scalar ; ":" ; v=json -> (string_of_scalar s,v) ]
         -> `Assoc [(s,v) :: l]

      | s = scalar_other_string -> `String s
      | s = scalar_other_string ; ":" ; v=json ;
         l = LIST0 [ s=key_scalar ; ":" ; v=json -> (string_of_scalar s,v) ]
         -> `Assoc [(s,v) :: l]


      | "-" ; v=json ;
         l = LIST0 [ "-" ; v=json -> v ]
         -> `List [v :: l]
      ] ]
    ;

  json:
    [ [ s = block_members -> s

      | INDENT ; v=json ; DEDENT -> v

      | "[" ; l = LIST0 flow_json SEP "," ; "]" -> `List l
      | "{" ; l = LIST0 [ s = flow_scalar ; ":" ; v=flow_json -> (string_of_scalar s,v) ] SEP "," ; "}" -> `Assoc l
    ] ]
  ;

  must_fold_chomp :
    [ [ ">" -> (True, False)
      | "|" -> (False, False)
      | ">-" -> (True, True)
      | "|-" -> (False, True)
      ] ]
    ;

  fold_chomp :
    [ [ fc = must_fold_chomp -> fc
      | -> (False, False)
      ] ]
    ;
  
  scalar_rawstring:
    [ [ fc = fold_chomp ; (s,l) = scalar_rawstring0 -> (fc, s, l)
    ] ]
  ;
  
  scalar_rawstring0:
    [ [ (s,l) = [ s = RAWSTRING -> (s,loc) ] ->
        (s, l)

      | (s,l) = [ INDENT ; s = RAWSTRING ; DEDENT -> (s,loc) ] ->
        (s, l)
    ] ]
  ;

  scalar_other_string:
    [ [ s = JSONSTRING -> (unquote_jsonstring s)
      | s=YAMLSQSTRING -> (unquote_yaml_sqstring s)
      | s=YAMLDQSTRING -> (unquote_yaml_dqstring s)
    ] ]
  ;

  scalar_nonstring:
    [ [ n = DECIMAL -> `Float (if n = ".NaN" then nan
                               else if n = ".inf" then infinity
                               else if n = "-.inf" then neg_infinity
                               else float_of_string n)
      | n = HEXADECIMAL -> `Float (float_of_int (int_of_string n))
      | n = OCTAL -> `Float (float_of_int (int_of_string n))
      | "null" -> `Null
      | "true" -> `Bool True
      | "false" -> `Bool False
    ] ]
  ;

  key_scalar:
    [ [ (fold, chomp) = must_fold_chomp ; (s, l) = scalar_rawstring0 -> `String (unquote_rawstring ~{fold} ~{chomp} l s)
      | (s, l) = scalar_rawstring0 -> `String (unquote_rawstring ~{fold=False} ~{chomp=False} l s)
      | s = YAMLSTRING -> `String s
      | s = scalar_other_string -> `String s
      | s = scalar_nonstring -> s
    ] ]
  ;

  flow_scalar:
    [ [ (fold,chomp) = fold_chomp ; (s,l) = [ s = RAWSTRING -> (s,loc) ] ->
        `String (unquote_rawstring ~{fold=fold} ~{chomp} l s)

      | s = YAMLSTRING -> `String s
      | s = JSONSTRING -> `String (unquote_jsonstring s)
      | s=YAMLSQSTRING -> `String (unquote_yaml_sqstring s)
      | s=YAMLDQSTRING -> `String (unquote_yaml_dqstring s)
      | n = DECIMAL -> `Float (if n = ".NaN" then nan
                               else if n = ".inf" then infinity
                               else if n = "-.inf" then neg_infinity
                               else float_of_string n)
      | n = HEXADECIMAL -> `Float (float_of_int (int_of_string n))
      | n = OCTAL -> `Float (float_of_int (int_of_string n))
      | "null" -> `Null
      | "true" -> `Bool True
      | "false" -> `Bool False
    ] ]
  ;

  json_eoi : [ [ l = json ; EOI -> l ] ] ;
  doc_eoi : [ [ v = OPT BS4J ; l = doc ; EOI -> l ] ] ;
  docs_eoi : [ [ v = OPT BS4J ; l = docs ; EOI -> l ] ] ;
END;

value parse_json = Grammar.Entry.parse json ;
value parse_flow_json = Grammar.Entry.parse flow_json ;
value parse_json_eoi = Grammar.Entry.parse json_eoi ;
value parse_doc = Grammar.Entry.parse doc ;
value parse_doc_eoi = Grammar.Entry.parse doc_eoi ;
value parse_docs = Grammar.Entry.parse docs ;
value parse_docs_eoi = Grammar.Entry.parse docs_eoi ;

value parse_string pf s =
  pf (Stream.of_string s)
;
