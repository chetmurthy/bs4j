(*

   JSON-text = ws value ws

   These are the six structural characters:

      begin-array     = ws %x5B ws  ; [ left square bracket

      begin-object    = ws %x7B ws  ; { left curly bracket

      end-array       = ws %x5D ws  ; ] right square bracket

      end-object      = ws %x7D ws  ; } right curly bracket

      name-separator  = ws %x3A ws  ; : colon

      value-separator = ws %x2C ws  ; , comma

   Insignificant whitespace is allowed before or after any of the six
   structural characters.

      ws = *(
              %x20 /              ; Space
              %x09 /              ; Horizontal tab
              %x0A /              ; Line feed or New line
              %x0D )              ; Carriage return

      false null true

   The literal names MUST be lowercase.  No other literal names are
   allowed.

      value = false / null / true / object / array / number / string

      false = %x66.61.6c.73.65   ; false

      null  = %x6e.75.6c.6c      ; null

      true  = %x74.72.75.65      ; true


      object = begin-object [ member *( value-separator member ) ]
               end-object

      member = string name-separator value

   array = begin-array [ value *( value-separator value ) ] end-array


      number = [ minus ] int [ frac ] [ exp ]

      decimal-point = %x2E       ; .

      digit1-9 = %x31-39         ; 1-9

      e = %x65 / %x45            ; e E

      exp = e [ minus / plus ] 1*DIGIT

      frac = decimal-point 1*DIGIT

      int = zero / ( digit1-9 *DIGIT )

      minus = %x2D               ; -

      plus = %x2B                ; +

      zero = %x30                ; 0

      string = quotation-mark *char quotation-mark

      char = unescaped /
          escape (
              %x22 /          ; "    quotation mark  U+0022
              %x5C /          ; \    reverse solidus U+005C
              %x2F /          ; /    solidus         U+002F
              %x62 /          ; b    backspace       U+0008
              %x66 /          ; f    form feed       U+000C
              %x6E /          ; n    line feed       U+000A
              %x72 /          ; r    carriage return U+000D
              %x74 /          ; t    tab             U+0009
              %x75 4HEXDIG )  ; uXXXX                U+XXXX

      escape = %x5C              ; \

      quotation-mark = %x22      ; "

      unescaped = %x20-21 / %x23-5B / %x5D-10FFFF


*)

let gen_of_string s =
  let pos = ref 0 in
  fun () ->
    if !pos = String.length s then None
    else let c = String.get s !pos in
      pos := !pos + 1 ;
      Some c

let linews = [%sedlex.regexp? ' ' | '\t' | '\r']

let digit = [%sedlex.regexp? '0'..'9']
let int = [%sedlex.regexp? '0' | ( ('1'..'9') , (Star digit) )]
let frac = [%sedlex.regexp? '.' , (Plus digit)]
let exp = [%sedlex.regexp? ('e' | 'E') , (Opt ('-' | '+')) , (Plus digit)]
let number = [%sedlex.regexp?  (Opt '-') , int , (Opt frac) , (Opt exp)]

let letter = [%sedlex.regexp? 'a'..'z'|'A'..'Z']
let ident = [%sedlex.regexp? letter, Star (letter|digit)]

let unescaped = [%sedlex.regexp? 0x20 .. 0x21 | 0x23 .. 0x5B | 0x5D .. 0x10FFFF ]
let hexdigit = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F']
let escaped = [%sedlex.regexp? "\\" , ( 0x22 | 0x5C | 0x2F | 0x62 | 0x66 | 0x6E | 0x72 | 0x74 | (0x75, Rep(hexdigit,4)) ) ]
let char = [%sedlex.regexp? (unescaped | escaped ) ]
let string = [%sedlex.regexp?  '"' , (Star char) , '"']

let yamlscalar_char = [%sedlex.regexp? Compl (Chars "-[]{}:,#\\\"\r\n") ]
let yamlscalar_endchar = [%sedlex.regexp? Sub (yamlscalar_char, linews) ]
let yamlscalar = [%sedlex.regexp?  yamlscalar_endchar, Opt (Star yamlscalar_char, yamlscalar_endchar) ]

let comment = [%sedlex.regexp? '#' , Star(Compl '\n') ]

let unquote_string s =
  let buf = Buffer.create (String.length s) in
  let lb = Sedlexing.Latin1.from_gen (gen_of_string s) in
  let rec unrec0 () =
    match%sedlex lb with
      '"' -> unrec1 ()
    | _ -> failwith "unquote_string: unexpected character"
  and unrec1 () =
    match%sedlex lb with
      Plus unescaped ->
      Buffer.add_string buf (Sedlexing.Latin1.lexeme lb) ;
      unrec1 ()
    | "\\", '"' -> Buffer.add_char buf '"' ; unrec1 ()
    | "\\", '\\' -> Buffer.add_char buf '\\' ; unrec1 ()
    | "\\", '/' -> Buffer.add_char buf '/' ; unrec1 ()
    | "\\", 'b' -> Buffer.add_char buf '\b' ; unrec1 ()
    | "\\", 'f' -> Buffer.add_char buf '\x4c' ; unrec1 ()
    | "\\", 'n' -> Buffer.add_char buf '\n' ; unrec1 ()
    | "\\", 'r' -> Buffer.add_char buf '\r' ; unrec1 ()
    | "\\", 't' -> Buffer.add_char buf '\t' ; unrec1 ()
    | "\\", 'u', Rep(hexdigit,4) ->
      let s = Sedlexing.Latin1.sub_lexeme lb 2 4 in
      let n = int_of_string ("0x"^s) in
      Buffer.add_utf_8_uchar buf (Uchar.of_int n) ; unrec1 ()

    | '"' ->
      Buffer.contents buf

    | _ -> failwith "unquote_string: internal error"
  in unrec0 ()

let indented n s =
  let slen = String.length s in
  if slen <= n then false
  else
    let rec irec ofs =
      if ofs = n then true
      else if String.get s ofs = ' ' then
        irec (ofs+1)
      else false
    in irec 0

let consume_indent n s =
  let slen = String.length s in
  if slen = 0 then ""
  else if indented n s then
    String.sub s n (slen - n)
  else failwith "consume_indent"

let fold_exceptions s =
  if s = "" then "\n"
  else if String.get s 0 = ' ' then s^"\n"
  else s

let fold_lines l =
  let buf = Buffer.create 23 in
  let rec frec = function
      [] -> Buffer.contents buf
    | l1::tl when l1 <> "" && String.get l1 0 = ' ' ->
      Buffer.add_string buf l1 ;
      Buffer.add_string buf "\n" ;
      frec tl

    | ""::l2::tl ->
      Buffer.add_string buf "\n" ;
      frec (l2::tl)

    | l1::l2::tl when l2 <> "" ->
      Buffer.add_string buf l1 ;
      Buffer.add_string buf " " ;
      frec (l2::tl)

    | l1::l2::tl when l2 = "" ->
      Buffer.add_string buf l1 ;
      Buffer.add_string buf "\n" ;
      frec (l2::tl)

    | l1::tl when l1 <> "" ->
      Buffer.add_string buf l1 ;
      frec tl
    | [""] ->
      Buffer.add_string buf "\n" ;
      frec []
  in frec l

let unquote_rawstring ~fold indent s =
  let sofs = (String.index s '(') + 1 in
  let eofs = (String.index s ')') in
  if sofs = eofs then "" else
  let s = String.sub s sofs (eofs-sofs) in
  let l = String.split_on_char '\n' s in
  let l = (List.hd l) :: (List.map (consume_indent indent) (List.tl l)) in
  if fold then
    fold_lines l
  else String.concat "\n" l

type token =
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COLON
  | COMMA
  | DASH
  | DASHDASHDASH
  | DOTDOTDOT
  | BAR
  | GT
  | PLUS
  | NUMBER of string
  | STRING of string
  | RAWSTRING of string
  | YAMLSTRING of string
  | INDENT of int * int
  | DEDENT of int * int
  | NEWLINE (* internal token *)
  | EOF

type style_t =
    BLOCK of int
  | FLOW

let extract_indent_position = function
    (EOF, _) -> 0
  | (_, ({Lexing.pos_bol; pos_cnum; _}, _)) ->
    pos_cnum - pos_bol

module St = struct
type t =
  {
    lexbuf : Sedlexing.lexbuf
  ; mutable style_stack : style_t list
  ; mutable at_bol : bool
  ; mutable pushback : (token * (Lexing.position * Lexing.position)) list
  }
  let mk lexbuf = {
    lexbuf
  ; style_stack = [BLOCK 0]
  ; at_bol = true
  ; pushback = []
  }
  let set_bol st b = st.at_bol <- b
  let pop_flow st =
    match st with
      { style_stack = FLOW :: sst ; _ } -> st.style_stack <- sst
    | _ -> failwith "pop_flow: internal error"

  let push_flow st = st.style_stack <- FLOW::st.style_stack

let rec pop_styles loc rev_pushback = function
    ((BLOCK m)::(BLOCK m')::sst, n) when n < m -> pop_styles loc ((DEDENT(m',m),loc)::rev_pushback) ((BLOCK m')::sst, n)
  | ((BLOCK m)::sst, n) when n < m -> pop_styles loc ((DEDENT(n,m),loc)::rev_pushback) (sst, n)

  | ((BLOCK m)::sst, n) when n = m && m > 0 -> (rev_pushback, (BLOCK m)::sst)

  | ((BLOCK m)::sst, n) when n = m && m = 0 ->
    assert (sst = []) ;
    (rev_pushback, [BLOCK 0])
  | _ -> failwith "pop_styles: dedent did not move back to previous indent position"

let handle_indents_with st ((tok,(spos,epos as loc)) as t) =
  assert (st.pushback = []) ;
  match st.style_stack with
    (BLOCK m)::_ ->
    let n = extract_indent_position t in
    if n = m then begin
      t
    end
    else if n > m then begin
      st.style_stack <- (BLOCK n)::st.style_stack ;
      st.pushback <- [t] ;
      (INDENT(m,n),(spos,spos))
    end
    else (* n < m *) begin
      let (rev_pushback, new_sst) = pop_styles loc [] (st.style_stack, n) in
      let new_pushback = (List.rev rev_pushback)@[t] in
      st.pushback <- List.tl new_pushback ;
      st.style_stack <- new_sst ;
      List.hd new_pushback
    end
end

let indentspaces buf =
  match%sedlex buf with
  | Star ' ' -> String.length (Sedlexing.Latin1.lexeme buf)
  | _ -> failwith "indentspaces: should never happen"

let rec rawstring2 (spos, id, acc) st =
  let open St in
  let pos() = Sedlexing.lexing_positions st.lexbuf in
  let buf = st.lexbuf in
  match%sedlex buf with
  | Star(Compl(')')) ->
    let txt = Sedlexing.Latin1.lexeme buf in
    Buffer.add_string acc txt ;
    rawstring2 (spos, id, acc) st
  | ")" ->
    Buffer.add_string acc ")" ;
    rawstring3 (spos, id, acc) 0 st
  | _ -> failwith "rawstring2: unexpected character"

and rawstring3 (spos, id, acc) ofs st =
  let open St in
  let pos() = Sedlexing.lexing_positions st.lexbuf in
  let buf = st.lexbuf in
  match%sedlex buf with
  | any ->
    let c = Sedlexing.lexeme_char buf 0 in
    Buffer.add_utf_8_uchar acc c ;
    if ofs = Array.length id && c = Uchar.of_char '"' then begin
      let (_, epos) = pos() in
      (RAWSTRING (Buffer.contents acc), (spos, epos))
    end
    else if c = id.(ofs) then begin
      rawstring3 (spos, id, acc) (ofs+1) st
    end
    else
      rawstring2 (spos, id, acc) st
  | _ -> failwith "rawstring3: unexpected character"

let rawstring1 (id,acc) st =
  let open St in
  let pos() = Sedlexing.lexing_positions st.lexbuf in
  let buf = st.lexbuf in
  match%sedlex buf with
  | "(" ->
    let (_, epos) = pos() in
    Buffer.add_string acc "(" ;
    rawstring2 (epos, id, acc) st
  | _ -> failwith "rawstring1: unexpected character"


let rawstring0 st =
  let open St in
  let pos() = Sedlexing.lexing_positions st.lexbuf in
  let buf = st.lexbuf in
  match%sedlex buf with
  | Opt ident ->
    let uni_id = Sedlexing.lexeme buf in
    let id = Sedlexing.Latin1.lexeme buf in
    let acc = Buffer.create 23 in
    Buffer.add_string acc "R\"" ;
    Buffer.add_string acc id ;
    rawstring1 (uni_id,acc) st
  | _ -> failwith "rawstring0: unexpected character"

let rec rawtoken st =
  let open St in
  let pos() = Sedlexing.lexing_positions st.lexbuf in
  let buf = st.lexbuf in
  match%sedlex buf with
  | number -> (NUMBER (Sedlexing.Latin1.lexeme buf),pos())
  | string -> (STRING (Sedlexing.Latin1.lexeme buf),pos())
  | "R\"" ->
    rawstring0 st
  | "[" -> (LBRACKET, pos())
  | "]" -> (RBRACKET, pos())
  | "{" -> (LBRACE,pos())
  | "}" -> (RBRACE,pos())
  | ":" -> (COLON,pos())
  | "|" -> (BAR,pos())
  | ">" -> (GT,pos())
  | "+" -> (PLUS,pos())
  | "," -> (COMMA,pos())
  | "-" -> (DASH,pos())
  | "---" -> (DASHDASHDASH,pos())
  | "..." -> (DOTDOTDOT,pos())
  | Plus linews -> rawtoken st
  | '\n' -> (NEWLINE,pos())
  | yamlscalar -> (YAMLSTRING (Sedlexing.Latin1.lexeme buf), pos())
  | eof -> (EOF,pos())
  | comment -> rawtoken st
  | _ -> failwith "Unexpected character"


let rec jsontoken st =
  let open St in
  match st with
    { pushback = h::t ; _ } ->
    st.pushback <- t ;
    h

  | { pushback = [] ; at_bol = true ; style_stack = (BLOCK _) :: _ ; _ } ->
    ignore(indentspaces st.lexbuf) ;
    St.set_bol st false ;
    jsontoken st

  | { pushback = [] ; at_bol = false ; style_stack = (BLOCK m) :: sst ; _ } -> begin
      match rawtoken st with
        (RBRACKET, _) -> failwith "jsontoken: ']' found in block style"
      | (LBRACKET, _) as t -> St.push_flow st ; t
      | (RBRACE, _) -> failwith "jsontoken: '}' found in block style"
      | (LBRACE, _) as t -> St.push_flow st ; t
      | (COLON, _) as t -> t
      | (RAWSTRING _, _) as t -> t
      | (NEWLINE, _) -> St.set_bol st true ; jsontoken st
      | t -> handle_indents_with st t
  end

  | { pushback = [] ; style_stack = FLOW :: _ ; _ } -> begin
      match rawtoken st with
        (RBRACKET, _) as t -> St.pop_flow st ; t
      | (LBRACKET, _) as t -> St.push_flow st ; t
      | (RBRACE, _) as t -> St. pop_flow st ; t
      | (LBRACE, _) as t -> St.push_flow st ; t
      | (NEWLINE, _) -> St.set_bol st true ; jsontoken st
      | t -> t
    end

let lex_string f s =
  let st = St.mk (Sedlexing.Latin1.from_gen (gen_of_string s)) in
  let rec lexrec acc =
    match jsontoken st with
      (EOF,_) as t -> List.rev (t::acc)
    | t -> lexrec (t::acc)
  in lexrec []
