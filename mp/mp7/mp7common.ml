(* File: mp8common.ml *)

type position = {line_num : int; char_num : int}

exception OpenComm of position
exception CloseComm of position
exception SuperCloseComm of position

type token =
  | INT of (int)
  | REAL of (float)
  | BOOL of (bool)
  | STRING of (string)
  | IDENT of (string)
  | OPCOM of ((int*int))
  | CLCOM of ((int*int))
  | SCLCOM of ((int*int))
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
  | LBRAC
  | RBRAC
  | LPAREN
  | RPAREN
  | COMMA
  | UNDERSCORE
  | UNIT
  | EOF
(*
  | ERROR
  | DBLSEMI
  | EXP
  | OR
*)

