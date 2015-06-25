open Mp4common

let rec import_list lst = match lst
    with [] -> ConExp Nil
    | (h :: t) -> BinExp(Cons,ConExp(Int h), import_list t);;

let elem = IfExp( BinExp(Eq, VarExp "xs", ConExp Nil ), 
        ConExp( Bool true ),
        IfExp( BinExp(Eq, MonExp(Head, VarExp "xs"), VarExp "e" ),
            ConExp( Bool false ),
            AppExp( AppExp(VarExp "elem", VarExp "e"), MonExp(Tail, VarExp "xs" ) )    
        ) 
    );;

let rec num_of_consts expression = raise (Failure "Not Implemented")

let rec freeVars expression = raise (Failure "Not Implemented")

let rec cps expression cont = raise (Failure "Not Implemented")
