/* Use the expression datatype defined in expressions.ml: */ %{
    open Mp8common

    let mon_app mon_op x = MonOpAppExp( mon_op, x )
    let bin_app bin_op x y = BinOpAppExp( bin_op, x, y )
    let cr_orelse x y = IfExp(x, ConstExp(BoolConst true), y) 
    let cr_andalso x y = IfExp(x, y, ConstExp(BoolConst false) )

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
    | VAL REC IDENT rec_var_param_bind          { Rec ($3, fst $4, snd $4) }
    | FUN IDENT rec_var_param_bind              { Rec ($2, fst $3, snd $3) }
    | LOCAL dec IN dec END                      { Local($2, $4) }

rec_var_param_bind:
    IDENT EQUALS expression        { ($1, $3) }
    | IDENT rec_var_param_bind     { ($1, FnExp(fst $2, snd $2) ) }    

simp_bind:                                      
    IDENT EQUALS expression        { ($1, $3) }
    | UNDERSCORE EQUALS expression { ("", $3) }

expression:                                     
    or_else_exp                      { $1 }
    | app_exp                        { $1 }

expression_seq:
    expression                        { $1 }
    | expression SEMI expression_seq  { LetExp(Val("", $1), $3) }

or_else_exp:
    or_else_no_ifr_exp ORELSE and_also_exp         { cr_orelse $1 $3 }
    | and_also_exp                                 { $1 }

or_else_no_ifr_exp:
    or_else_no_ifr_exp ORELSE and_also_no_ifr_exp  { cr_orelse $1 $3 }
    | and_also_no_ifr_exp                          { $1 }

and_also_exp:     
    and_also_no_ifr_exp ANDALSO comp_exp           { cr_andalso $1 $3 }
    | comp_exp                                     { $1 }

and_also_no_ifr_exp:
    and_also_no_ifr_exp ANDALSO comp_no_ifr_exp    { cr_andalso $1 $3 }
    | comp_no_ifr_exp                              { $1 }

comp_exp:
    comp_no_ifr_exp comp_op cons_exp               { $2 $1 $3 }  
    | cons_exp                                     { $1 }

comp_no_ifr_exp:
    comp_no_ifr_exp comp_op cons_no_ifr_exp        { $2 $1 $3 }
    | cons_no_ifr_exp                              { $1 }

cons_exp:
    add_sub_no_ifr_exp DCOLON cons_exp             { bin_app ConsOp $1 $3 } 
    | add_sub_exp                                  { $1 }

cons_no_ifr_exp:
    cons_no_ifr_exp DCOLON add_sub_no_ifr_exp      { bin_app ConsOp $1 $3 }
    | add_sub_no_ifr_exp                           { $1 }

add_sub_exp:
    add_sub_no_ifr_exp add_sub_op mult_div_exp     { $2 $1 $3 }
    | mult_div_exp                                 { $1 }

add_sub_no_ifr_exp:
    add_sub_no_ifr_exp add_sub_op mult_div_no_ifr_exp { $2 $1 $3 }
    | mult_div_no_ifr_exp                          { $1 }

mult_div_exp:
    mult_div_no_ifr_exp div_mult_op non_op_exp     { $2 $1 $3 }
    | non_op_exp                                   { $1 }

mult_div_no_ifr_exp:
    mult_div_no_ifr_exp div_mult_op app_no_ifr_exp { $2 $1 $3 }
    | app_no_ifr_exp                               { $1 }

non_op_exp:
    if_fn_raise_exp                         { $1 }
    | app_exp                               { $1 }

app_exp:
    atomic_mon_exp                            { $1 }
    | mon_op app_exp                          { MonOpAppExp($1, $2) }
    | app_no_ifr_mo_exp non_app_exp           { AppExp($1, $2) }

app_no_ifr_mo_exp:
    atomic_exp                              { $1 }
    | app_no_ifr_mo_exp atomic_mon_exp      { AppExp($1, $2) }

app_no_ifr_exp:
    atomic_mon_exp                            { $1 }
    | mon_op app_no_ifr_exp                   { MonOpAppExp($1, $2) }
    | app_no_ifr_mo_exp atomic_mon_exp        { AppExp($1, $2) }

non_app_exp:
    atomic_mon_exp                            { $1 }
    | if_fn_raise_exp                         { $1 }

if_fn_raise_exp: 
    IF expression THEN expression ELSE or_else_exp  { IfExp($2, $4, $6) }
    | FN IDENT ARROW or_else_exp                    { FnExp($2, $4) }
    | RAISE or_else_exp                             { RaiseExp $2 }

app_exp_mon_exp:
    atomic_exp         { $1 }
    | mon_op_exp       { FnExp("x", $1 (VarExp "x") ) }                       

atomic_mon_exp:
    atomic_exp                     { $1 }
    | mon_op_exp                   { FnExp( "x", $1 (VarExp ("x") ) ) }

atomic_exp:
    const_exp                      { ConstExp $1 }
    | OP bin_op                    { FnExp("x", (FnExp("y", $2 (VarExp "x") (VarExp "y")))) }
    | paren_exp                    { $1 } 
    | list_exp                     { $1 }
    | IDENT                        { VarExp $1 }
    | LET dec IN expression END    { LetExp($2, $4) } 

const_exp:
    INT                            { IntConst $1 }
    | BOOL                         { BoolConst $1 }
    | REAL                         { RealConst $1 }
    | STRING                       { StringConst $1 }
    | LBRAC RBRAC                  { NilConst }
    | NIL                          { NilConst }
    | UNIT                         { UnitConst }
    | LPAREN RPAREN                { UnitConst }

bin_op:
    comp_op                        { $1 }
    | add_sub_op                   { $1 }
    | div_mult_op                  { $1 }
    | DCOLON                       { fun x y -> bin_app ConsOp x y }
    | COMMA                        { fun x y -> bin_app CommaOp x y }

bin_op_exp:
    bin_op                         { fun x -> fun y -> $1 x y }

add_sub_op:
    PLUS                           { fun x y -> bin_app IntPlusOp x y }
    | DPLUS                        { fun x y -> bin_app RealPlusOp x y }
    | MINUS                        { fun x y -> bin_app IntMinusOp x y }
    | DMINUS                       { fun x y -> bin_app RealMinusOp x y }
    | CARAT                        { fun x y -> bin_app ConcatOp x y }

div_mult_op:
    DIV                            { fun x y -> bin_app IntDivOp x y }
    | DDIV                         { fun x y -> bin_app RealDivOp x y }
    | TIMES                        { fun x y -> bin_app IntTimesOp x y }
    | DTIMES                       { fun x y -> bin_app RealTimesOp x y }

mon_op_exp:
    NEG         { fun x -> mon_app IntNegOp x }
    | HD        { fun x -> mon_app HdOp x }
    | TL        { fun x -> mon_app TlOp x }
    | FST       { fun x -> mon_app FstOp x }
    | SND       { fun x -> mon_app SndOp x }

mon_op:
    NEG         { IntNegOp }
    | HD        { HdOp }
    | TL        { TlOp }
    | FST       { FstOp }
    | SND       { SndOp }

comp_op:
    GT               { bin_app GreaterOp }
    | EQUALS         { bin_app EqOp }
    | desug_comp_op  { $1 }

desug_comp_op:
    LT             { fun x y -> bin_app GreaterOp y x } 
    | LEQ          { fun x y -> cr_orelse 
                        (bin_app GreaterOp y x)
                        (bin_app EqOp x y)
                   }
    | GEQ          { fun x y -> cr_orelse 
                        (bin_app GreaterOp x y)
                        (bin_app EqOp x y)
                   }
    | NEQ          { fun x y -> IfExp(
                        bin_app EqOp x y, 
                        ConstExp(BoolConst false),
                        ConstExp(BoolConst true))
                   }


list_exp:
    LBRAC list_inner_exp RBRAC                        { $2 }

list_inner_exp:
    expression                                        { bin_app ConsOp $1 (ConstExp NilConst) } 
    | expression COMMA list_inner_exp                 { bin_app ConsOp $1 $3 }

paren_exp:
    LPAREN expression RPAREN                          { $2 }
    | LPAREN expression SEMI expression_seq RPAREN    { LetExp(Val("", $2), $4) }
    | LPAREN expression COMMA pair_exp RPAREN         { bin_app CommaOp $2 $4 }

pair_exp:
    expression                                  { $1 }
    | expression COMMA pair_exp RPAREN          { LetExp(Val("", $1), $3) }
                  
pat:
    INT          { Some $1 }
    | UNDERSCORE { None }
