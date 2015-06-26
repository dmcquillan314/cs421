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

let rec freeVars expression = 
    let remove_item cur v = List.filter (fun e -> v <> e) cur
    in
        match expression
        with VarExp(name) -> [ name ] 
            | ConExp(_) -> [] 
            | IfExp(exp1,exp2,exp3) -> freeVars exp1 @ freeVars exp2 @ freeVars exp3
            | MonExp(_,exp) -> freeVars exp
            | BinExp(_,exp1,exp2) -> freeVars exp1 @ freeVars exp2
            | AppExp(exp1,exp2) -> freeVars exp1 @ freeVars exp2
            | FunExp(s, exp) -> remove_item (freeVars exp) s
            | LetExp(s,exp1,exp2) -> freeVars exp1 @ (remove_item (freeVars exp2) s )
            | RecExp(f,x,exp1,exp2) -> (remove_item (remove_item (freeVars exp1) f) x) @ (remove_item (freeVars exp2) f)
            | _ -> [];;

let rec cps expression cont = 
    match expression
        with VarExp(v) -> AppExp(cont, VarExp(v))
        | ConExp(c) -> AppExp(cont, ConExp(c))
        | IfExp( exp1, exp2, exp3 ) -> 
                let v = freshFor (freeVars exp2 @ freeVars exp3 @ freeVars cont )
                    in 
                        AppExp( 
                            FunExp(v, IfExp(VarExp v, 
                                AppExp(cont, exp2),
                                AppExp(cont, exp3) 
                            ) ), 
                            exp1
                        )
        | AppExp( exp1, exp2 ) -> 
                let v_1 = freshFor (freeVars exp2 @ freeVars cont)
                in let v_2 = freshFor (v_1 :: freeVars cont)
                in
                    AppExp(
                        FunExp(v_1, 
                            AppExp(
                                FunExp(v_2, AppExp(AppExp(VarExp v_1, VarExp v_2), cont)),
                                exp2
                            )
                        ),
                        exp1
                    )
        | BinExp(binop, exp1, exp2) ->
                let v_1 = freshFor (freeVars exp1 @ freeVars exp2 @ freeVars cont)
                in let v_2 = freshFor (v_1 :: freeVars exp1 @ freeVars exp2 @ freeVars cont)
                in
                    AppExp(FunExp(v_1, 
                        AppExp(FunExp(v_2, 
                            AppExp(cont, BinExp(binop, VarExp v_1, VarExp v_2))
                        ), exp2)
                    ), exp1 );;    





