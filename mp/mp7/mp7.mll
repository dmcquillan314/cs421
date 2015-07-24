{

open Mp7common;;

let line_count = ref 1
let char_count = ref 1

let cinc n = char_count := !char_count + n
let linc n = line_count := (char_count := 1; !line_count + n)

let make_ascii_string escaped_ascii = 
    let code_str = String.sub escaped_ascii 1 ((String.length escaped_ascii) - 1) in
    let code = int_of_string code_str in
    if code < 0 || code > 255 then raise( Failure "invalid ascii character" )
    else String.make 1 (char_of_int code);;
}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']
let upper_lower_case = [ 'a' - 'z' 'A' - 'Z' ]
let ident_char = [ 'a' - 'z' 'A' - 'Z' '0' - '9' '_' ''' ]
let letter =['a' - 'z' 'A' - 'Z' '_']
let unit_reg = "()"
let boolean_reg = "true" | "false"

rule token = parse
  (* whitespace *)
  | [' ' '\t'] { cinc 1; token lexbuf }  (* skip over whitespace *)
  | ['\n'] { linc 1; token lexbuf }  (* skip over whitespace *)
  (* end of file *)
  | eof             { EOF } 
  (* p1 *)
  | '~'             { cinc 1; NEG }
  | '+'             { cinc 1; PLUS }
  | '-'             { cinc 1; MINUS }
  | '*'             { cinc 1; TIMES }
  | '/'             { cinc 1; DIV }
  | "+."            { cinc 2; DPLUS }
  | "-."            { cinc 2; DMINUS }
  | "*."            { cinc 2; DTIMES }
  | "/."            { cinc 2; DDIV }
  | '^'             { cinc 1; CARAT }
  | '<'             { cinc 1; LT }
  | '>'             { cinc 1; GT }
  | "<="            { cinc 2; LEQ }
  | ">="            { cinc 2; GEQ }
  | '='             { cinc 1; EQUALS }
  | "<>"            { cinc 2; NEQ }
  | '|'             { cinc 1; PIPE }
  | "=>"            { cinc 2; ARROW }
  | ';'             { cinc 1; SEMI }
  | "::"            { cinc 2; DCOLON }
  | '@'             { cinc 1; AT }
  | "nil"           { cinc 3; NIL }
  | "let"           { cinc 3; LET }
  | "local"         { cinc 5; LOCAL }
  | "val"           { cinc 3; VAL }
  | "rec"           { cinc 3; REC }
  | "and"           { cinc 3; AND }
  | "end"           { cinc 3; END }
  | "in"            { cinc 2; IN }
  | "if"            { cinc 2; IF }
  | "then"          { cinc 4; THEN }
  | "else"          { cinc 4; ELSE }
  | "fun"           { cinc 3; FUN }
  | "fn"            { cinc 2; FN }
  | "op"            { cinc 2; OP }
  | "mod"           { cinc 3; MOD }
  | "raise"         { cinc 5; RAISE }
  | "handle"        { cinc 6; HANDLE }
  | "with"          { cinc 4; WITH }
  | "not"           { cinc 3; NOT }
  | "andalso"       { cinc 7; ANDALSO }
  | "orelse"        { cinc 6; ORELSE }
  | '['             { cinc 1; LBRAC }
  | ']'             { cinc 1; RBRAC }
  | '('             { cinc 1; LPAREN }
  | ')'             { cinc 1; RPAREN }
  | ','             { cinc 1; COMMA }
  | '_'             { cinc 1; UNDERSCORE }
  (* p2 *)
  | numeric+ as integer             { cinc (String.length integer); INT (int_of_string integer) }
  | (numeric+'.'(numeric*)) as real   { cinc (String.length real) ; REAL (float_of_string real) }
  (* p3 *)
  | unit_reg                        { cinc 2; UNIT }
  | boolean_reg as b                { cinc (String.length b); BOOL (bool_of_string b) } 
  (* p4 *)
  | upper_lower_case ident_char* as ident { cinc (String.length ident); IDENT ident }  
  (* p5 *)
  | (";;" [^ '\n' ]*) as lineComment  { cinc (String.length lineComment); token lexbuf }
  | "*)"                            { raise ( CloseComm { line_num = !line_count; char_num = !char_count } ) }
  | "(*"                            { cinc 2; blockComment [{ line_num = !line_count; char_num = ! char_count - 2 }] lexbuf }
  | "\""                             { cinc 1; str "" lexbuf }
  and blockComment open_comments = parse
    | "*)" { cinc 2; 
            match open_comments with [open_comment] -> token lexbuf
            | open_comment::open_comments -> blockComment open_comments lexbuf 
            | [] -> raise (Failure "I shouldn't be here") }
    | "(*" { cinc 2; blockComment ({ line_num = !line_count; char_num = ! char_count - 2 }::open_comments) lexbuf }
    | '\n' { linc 1; blockComment open_comments lexbuf }
    | eof  { raise ( OpenComm (List.hd open_comments) ) }
    | _    { cinc 1; blockComment open_comments lexbuf }
  and str cur_string = parse
    | ('\\' numeric numeric numeric) as code_str { cinc 4; str (cur_string ^ (make_ascii_string code_str) ) lexbuf }  
    | "\\\\"  { cinc 2; str (cur_string ^ (String.make 1 '\\')) lexbuf }
    | "\\'"   { cinc 2; str (cur_string ^ (String.make 1 '\'')) lexbuf }
    | "\\\""  { cinc 2; str (cur_string ^ (String.make 1 '"')) lexbuf }
    | "\\t"   { cinc 2; str (cur_string ^ (String.make 1 '\t')) lexbuf }
    | "\\n"   { cinc 2; str (cur_string ^ (String.make 1 '\n')) lexbuf }
    | "\\r"   { cinc 2; str (cur_string ^ (String.make 1 '\r')) lexbuf }
    | "\""    { cinc 1; STRING cur_string }
    | eof     { raise (Failure "unmatched string") }
    | _  as c { cinc 1; str (cur_string ^ (String.make 1 c)) lexbuf }

{(* do not modify this function: *)
 let lextest s = token (Lexing.from_string s)

let opcom r = OPCOM(r.line_num,r.char_num)
let clcom r = CLCOM(r.line_num,r.char_num)
let sclcom r = SCLCOM(r.line_num,r.char_num)

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
    	     			      	 | SuperCloseComm r -> None
let try_comm_get_all_tokens s =
    try Some (get_all_tokens s) with Failure "unmatched comment" -> None
    	     			      	 | OpenComm r -> Some ([opcom r])
    	     			      	 | CloseComm r -> Some ([clcom r])
    	     			      	 | SuperCloseComm r -> Some ([sclcom r])

 }

