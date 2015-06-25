open Mp4common

let rec import_list lst = match lst
    with [] -> ConExp Nil
    | (h :: t) -> BinExp(Cons,ConExp(Int h), import_list t);;

let elem = RecExp("elem", "e",
    FunExp("xs", 
        IfExp( BinExp(Eq, VarExp "xs", ConExp Nil ),
            ConExp( Bool false ),
            IfExp( BinExp(Eq,
                MonExp(Head, VarExp "xs"),
                VarExp "e" ),
                    ConExp( Bool true ),
                        AppExp(
                            AppExp(VarExp
                            "elem", VarExp "e"),
                            MonExp(Tail,
                            VarExp "xs" ) )
                )
            )
    ),
    VarExp "elem"
);;

let rec num_of_consts expression = 
    match expression
        with ConExp(_) -> 1
        | IfExp(exp1,exp2,exp3) -> num_of_consts exp1 + num_of_consts exp2 +
        num_of_consts exp3
        | AppExp(exp1,exp2) -> num_of_consts exp1 + num_of_consts exp2
        | BinExp(_,exp1,exp2) -> num_of_consts exp1 + num_of_consts exp2
        | MonExp(_,exp) -> num_of_consts exp
        | FunExp(_,exp) -> num_of_consts exp
        | LetExp(_,exp1,exp2) -> num_of_consts exp1 + num_of_consts exp2
        | RecExp(_,_,exp1,exp2) -> num_of_consts exp1 + num_of_consts exp2
        | OAppExp(exp1,exp2) -> num_of_consts exp1 + num_of_consts exp2
        | _ -> 0;;

let rec freeVars expression = raise (Failure "Not Implemented")

let rec cps expression cont = raise (Failure "Not Implemented")
