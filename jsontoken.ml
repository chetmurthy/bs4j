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

let linews = [%sedlex.regexp? ' ' | '\t' | '\r']

let digit = [%sedlex.regexp? '0'..'9']
let int = [%sedlex.regexp? '0' | ( ('1'..'9') , (Star digit) )]
let frac = [%sedlex.regexp? '.' , (Plus digit)]
let exp = [%sedlex.regexp? ('e' | 'E') , (Opt ('-' | '+')) , (Plus digit)]
let number = [%sedlex.regexp?  (Opt '-') , int , (Opt frac) , (Opt exp)]

let unescaped = [%sedlex.regexp? 0x20 .. 0x21 | 0x23 .. 0x50 | 0x50 .. 0x10FFFF ]
let hexdigit = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F']
let escaped = [%sedlex.regexp? "\\" , ( 0x22 | 0x5C | 0x2F | 0x62 | 0x66 | 0x6E | 0x72 | 0x74 | (0x75, Rep(hexdigit,4)) ) ]
let char = [%sedlex.regexp? (unescaped | escaped ) ]
let string = [%sedlex.regexp?  '"' , (Star char) , '"']

type token =
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COLON
  | COMMA
  | KWFALSE
  | KWTRUE
  | KWNULL
  | NUMBER of string
  | STRING of string
  | EOF

let rec jsontoken buf =
  let pos() = Sedlexing.lexing_positions buf in
  match%sedlex buf with
  | "[" -> (LBRACKET, pos())
  | "]" -> (RBRACKET,pos())
  | "{" -> (LBRACE,pos())
  | "}" -> (RBRACE,pos())
  | ":" -> (COLON,pos())
  | "," -> (COMMA,pos())
  | linews -> jsontoken buf
  | '\n' -> jsontoken buf
  | "false" -> (KWFALSE,pos())
  | "true" -> (KWTRUE,pos())
  | "null" -> (KWNULL,pos())
  | number -> (NUMBER (Sedlexing.Latin1.lexeme buf),pos())
  | string -> (STRING (Sedlexing.Latin1.lexeme buf),pos())
  | eof -> (EOF,pos())
  | _ -> failwith "Unexpected character"

let gen_of_string s =
  let pos = ref 0 in
  fun () ->
    if !pos = String.length s then None
    else let c = String.get s !pos in
      pos := !pos + 1 ;
      Some c

let lex_string f s =
  let lexbuf = Sedlexing.Latin1.from_gen (gen_of_string s) in
  let rec lexrec acc =
    match jsontoken lexbuf with
      (EOF,_) as t -> List.rev (t::acc)
    | t -> lexrec (t::acc)
  in lexrec []
