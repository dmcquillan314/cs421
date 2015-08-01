/* Use the expression datatype defined in expressions.ml: */
%{
    open Mp8common
(* add any extra code here *)

%}

/* Define the tokens of the language: */
%token <int> INT
%token <float> REAL
%token <bool> BOOL
%token <string> STRING IDENT
%token <(int*int)> OPCOM CLCOM
%token NEG PLUS MINUS TIMES DIV DPLUS DMINUS DTIMES DDIV CARAT LT GT LEQ GEQ
       EQUALS NEQ PIPE ARROW SEMI DCOLON AT NIL LET LOCAL VAL REC AND END IN
       IF THEN ELSE FUN FN OP MOD RAISE HANDLE WITH NOT ANDALSO ORELSE
       HD TL FST SND
       LBRAC RBRAC LPAREN RPAREN COMMA UNDERSCORE
       UNIT ERROR EOF

/* Define the "goal" nonterminal of the grammar: */
%start main
%type <Mp8common.dec> main

%%

main:
    expression SEMI                             { Val("it", $1) }
  | dec SEMI                                    { $1 }

dec:
    atomic_dec                                  { $1 }
  | dec atomic_dec                              { Seq($1, $2) }

atomic_dec:
    VAL simp_bind                               { Val (fst $2, snd $2) }
    /* You will work here */

simp_bind:                                      {raise (Failure "Not implemented") }
    /* You will work here */

expression:                                     {raise (Failure "Not implemented") }
    /* You will work here */
