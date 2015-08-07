{
open Mp7common;;

let line_count = ref 1
let char_count = ref 1

let cinc n = char_count := !char_count + n
let linc n = line_count := (char_count := 1; !line_count + n)
}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z' ]
let id_char = numeric | alpha | "'" | "_"

let open_comment = "(*"
let close_comment = "*)"
let whitespace = [' ' '\t' '\n']

rule token = parse
  | [' ' '\t'] { cinc 1; token lexbuf }  (* skip over whitespace *)
  | ['\n'] { linc 1; token lexbuf }  (* skip over whitespace *)
  | eof             { EOF }

(* your rules go here *)
  | "~"         { cinc 1; NEG }
  | "+"     { cinc 1; PLUS  }
  | "-"     { cinc 1; MINUS  }
  | "*"     { cinc 1; TIMES  }
  | "/"     { cinc 1; DIV  }
  | "+."    { cinc 2; DPLUS  }
  | "-."    { cinc 2; DMINUS  }
  | "*."    { cinc 2; DTIMES  }
  | "/."    { cinc 2; DDIV  }
  | "^"     { cinc 1; CARAT  }
  | "<"     { cinc 1; LT  }
  | ">"     { cinc 1; GT  }
  | "<="    { cinc 2; LEQ  }
  | ">="    { cinc 2; GEQ  }
  | "="     { cinc 1; EQUALS  }
  | "<>"        { cinc 2; NEQ }
  | "|"     { cinc 1; PIPE  }
  | "=>"    { cinc 2; ARROW  }
  | "::"    { cinc 2; DCOLON  }
  | "@"     { cinc 1; AT  }
  | "nil"       { cinc 3; NIL }
  | "let"   { cinc 3; LET  }
  | "local" { cinc 5; LOCAL}
  | "val"   { cinc 3; VAL}
  | "rec"   { cinc 3; REC  }
  | "and"   { cinc 3; AND}
  | "end"   { cinc 3; END}
  | "in"    { cinc 2; IN  }
  | "if"    { cinc 2; IF  }
  | "then"  { cinc 4; THEN  }
  | "else"  { cinc 4; ELSE  }
  | "fun"   { cinc 3; FUN  }
  | "fn"    { cinc 2; FN}
  | "op"    { cinc 2; OP}
  | "mod"   { cinc 3; MOD  }
  | "raise" { cinc 5; RAISE }
  | "handle" { cinc 6; HANDLE}
  | "with"  { cinc 4; WITH }
  | "not"   { cinc 3; NOT  }
  | "andalso" { cinc 7; ANDALSO}
  | "orelse"  { cinc 6; ORELSE}
  | "["     { cinc 1; LBRAC  }
  | "]"     { cinc 1; RBRAC  }
  | "("     { cinc 1; LPAREN  }
  | ")"     { cinc 1; RPAREN  }
  | ","     { cinc 1; COMMA  }
  | "_"     { cinc 1; UNDERSCORE }

  | numeric+ as s { cinc (String.length s); INT (int_of_string s) }
  | ((numeric+)'.'(numeric+)) as s       { cinc (String.length s); REAL (float_of_string s) }

  | "true"  { cinc 4; BOOL true }
  | "false" { cinc 5; BOOL false }
  | "()"    { cinc 2; UNIT }

  | (alpha (id_char*)) as s     { cinc (String.length s); IDENT s }

  | open_comment       { cinc 2; comment [({line_num = !line_count; char_num = !char_count - 2}:Mp7common.position)] lexbuf }

  | close_comment      { raise (CloseComm {line_num = !line_count; char_num = !char_count}) }

  | "\""    { cinc 1; string "" lexbuf }
  | ";"    {cinc 1; SEMI}
  | (";;"([^'\n']*)) as s	{ cinc (String.length s); token lexbuf }

and comment open_dimens = parse
   open_comment        { cinc 2; comment ({line_num = !line_count; char_num = !char_count - 2}::open_dimens) lexbuf }
 | close_comment       { cinc 2;
                         match open_dimens with [pos] -> token lexbuf
                         | dim::dimens -> comment dimens lexbuf
                         | [] -> raise (Failure "Solution error") }
 | ['\n']       { linc 1; comment open_dimens lexbuf }
 | eof             { raise (OpenComm (List.hd open_dimens)) }
 | _                   { cinc 1; comment open_dimens lexbuf }

and string start_string = parse

   "\""     { cinc 1; STRING start_string }
 | "\\\\"   { cinc 2; string (start_string ^ "\\") lexbuf }
 | "\\'"   { cinc 2; string (start_string ^ "'") lexbuf }
 | "\\\""   { cinc 2; string (start_string ^ "\"") lexbuf }
 | "\\t"    { cinc 2; string (start_string ^ "\t") lexbuf }
 | "\\n"    { cinc 2; string (start_string ^ "\n") lexbuf }
 | "\\r"    { cinc 2; string (start_string ^ "\r") lexbuf }
 | "\n"     { linc 1; string (start_string ^ "\n") lexbuf }
(* | "\\"         { cinc 1; stringwhitespace start_string lexbuf } *)

 | "\\"(['0' - '1']['0' - '9']['0' - '9'] as ch) { cinc 4; string (start_string ^ (String.make 1 (char_of_int (int_of_string ch)))) lexbuf } 
 | "\\"(['2']['0' - '4']['0' - '9'] as ch) { cinc 4; string (start_string ^ (String.make 1 (char_of_int (int_of_string ch)))) lexbuf } 
 | "\\"(['2']['5']['0' - '5'] as ch) { cinc 4; string (start_string ^ (String.make 1 (char_of_int (int_of_string ch)))) lexbuf } 

 | _ as c   { cinc 1; string (start_string ^ (String.make 1 c)) lexbuf }
(*
and stringwhitespace startstring = parse
    [' ' '\t']  { cinc 1; stringwhitespace startstring lexbuf }
  | ['\n']      { linc 1; stringwhitespace startstring lexbuf }
  | "\\"        { cinc 1; string startstring lexbuf }
*)

(* do not modify this function: *)
{ let lextest s = token (Lexing.from_string s)

let opcom r = OPCOM(r.line_num,r.char_num)
let clcom r = CLCOM(r.line_num,r.char_num)

  let get_all_tokens s =
      let _ = char_count := 1 in
      let _ = line_count := 1 in
      let b = Lexing.from_string (s^"\n") in
      let rec g () =
      match token b with EOF -> []
      | t -> t :: g () in
      g ()

let try_get_all_tokens s =
    try Some (get_all_tokens s) with Failure "unmatched comment" -> None
                                 | OpenComm r -> None
                                 | CloseComm r -> None
let try_comm_get_all_tokens s =
    try Some (get_all_tokens s) with Failure "unmatched comment" -> None
                                 | OpenComm r -> Some ([opcom r])
                                 | CloseComm r -> Some ([clcom r])
 }
