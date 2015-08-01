type token =
  | INT of (int)
  | REAL of (float)
  | BOOL of (bool)
  | STRING of (string)
  | IDENT of (string)
  | OPCOM of ((int*int))
  | CLCOM of ((int*int))
  | NEG
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | DPLUS
  | DMINUS
  | DTIMES
  | DDIV
  | CARAT
  | LT
  | GT
  | LEQ
  | GEQ
  | EQUALS
  | NEQ
  | PIPE
  | ARROW
  | SEMI
  | DCOLON
  | AT
  | NIL
  | LET
  | LOCAL
  | VAL
  | REC
  | AND
  | END
  | IN
  | IF
  | THEN
  | ELSE
  | FUN
  | FN
  | OP
  | MOD
  | RAISE
  | HANDLE
  | WITH
  | NOT
  | ANDALSO
  | ORELSE
  | HD
  | TL
  | FST
  | SND
  | LBRAC
  | RBRAC
  | LPAREN
  | RPAREN
  | COMMA
  | UNDERSCORE
  | UNIT
  | ERROR
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Mp8common.dec
