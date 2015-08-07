open Mp4common

(*Solution 1*)
let import_list lst =
    List.fold_right (fun x l -> BinExp(Cons, ConExp(Int x), l)) lst (ConExp Nil)



(*Solution 2*)

let elem =
    RecExp("elem", "e",
           FunExp("xs",
                 IfExp(BinExp(Eq,VarExp "xs",ConExp Nil),
                       ConExp(Bool false),
                       IfExp(BinExp(Eq,MonExp(Head,VarExp "xs"),VarExp "e"),
                             ConExp(Bool true),
                             AppExp(AppExp(VarExp "elem", VarExp "e"),
                                    MonExp(Tail,VarExp "xs"))))),
           VarExp "elem")



(*Solution 3*)

let rec num_of_consts exp =
    match exp
    with VarExp _ -> 0
       | ConExp _ -> 1
       | IfExp (bexp, thenexp, elseexp) ->
         (num_of_consts bexp) + (num_of_consts thenexp) + (num_of_consts elseexp)
       | AppExp(exp1, exp2) -> (num_of_consts exp1) + (num_of_consts exp2)
       | BinExp(binop, exp1, exp2) -> (num_of_consts exp1) + (num_of_consts exp2)
       | MonExp(monop, exp) -> num_of_consts exp
       | FunExp(x, exp) -> num_of_consts exp
       | LetExp(x, exp1, exp2) -> (num_of_consts exp1) + (num_of_consts exp2)
       | RecExp(f, x, exp1, exp2) -> (num_of_consts exp1) + (num_of_consts exp2)
       | OAppExp(exp1, exp2) -> (num_of_consts exp1) + (num_of_consts exp2)



(*Solution 4*)

let rec freeVars expression =
  match expression with
  | ConExp v -> []
  | VarExp v -> [v]
  | FunExp (i,e) -> List.filter (fun s -> not (s = i)) (freeVars e)
  | IfExp (c,t,f) -> freeVars c @ freeVars t @ freeVars f
  | AppExp (e1,e2) -> freeVars e1 @ freeVars e2
  | MonExp (_,e) -> freeVars e
  | BinExp (_,e1,e2) -> freeVars e1 @ freeVars e2
  | LetExp (i,e1,e2) -> freeVars e1 @ List.filter (fun s -> not (s = i)) (freeVars e2)
  | RecExp (i,a,e1,e2) -> (List.filter (fun s -> not (s = i || s = a)) (freeVars e1))
                        @ (List.filter (fun s -> not (s = i)) (freeVars e2))
  | OAppExp (e1,e2) -> freeVars e1 @ freeVars e2

(*
let rec freeVarsList explist =
    let rec freeVarsList_aux exp bvars fvars= 
        match exp with
               VarExp x ->
                if List.mem x bvars then fvars
                else x::fvars
             | ConExp c -> fvars
             | IfExp (bexp, thenexp, elseexp) ->
                freeVarsList_aux elseexp bvars
                 (freeVarsList_aux thenexp bvars
                   (freeVarsList_aux bexp bvars fvars))
             | AppExp (funexp, argexp) ->
                freeVarsList_aux argexp bvars (freeVarsList_aux funexp bvars fvars)
             | BinExp (binop, exp1, exp2) ->
                freeVarsList_aux exp2 bvars (freeVarsList_aux exp1 bvars fvars)
             | MonExp (monop, exp) -> freeVarsList_aux exp bvars fvars
             | FunExp (x, exp) -> freeVarsList_aux exp (x::bvars) fvars
             | LetExp (x, exp1, exp2) ->
                freeVarsList_aux exp2 (x::bvars) (freeVarsList_aux exp1 bvars fvars)
             | RecExp (f, x, exp1, exp2) ->
                freeVarsList_aux exp2 (f::bvars) (freeVarsList_aux exp1 (f::x::bvars) fvars)
             | OAppExp (funexp, argexp) ->
                freeVarsList_aux argexp bvars (freeVarsList_aux funexp bvars fvars)
in List.fold_left (fun fvars -> fun exp ->  freeVarsList_aux exp [] fvars) [] explist

let freeVars e = freeVarsList [e]
*)



(*Solution 5*)

let rec cps exp cont =
    match exp with
          VarExp x -> AppExp (cont,VarExp x)
        | ConExp c -> AppExp(cont,ConExp c)
        | IfExp(bexp,thenexp,elseexp) ->
          let v = freshFor (freeVars thenexp @ freeVars elseexp @ freeVars cont) in
           cps bexp (FunExp(v,IfExp(VarExp v,cps thenexp cont, cps elseexp cont)))
        | AppExp(funexp, argexp) ->
          let v0 = freshFor (freeVars argexp @ freeVars cont) in
          let v1 = freshFor (v0 :: freeVars cont) in
          cps funexp 
          (FunExp(v0, cps argexp
                      (FunExp(v1, AppExp(AppExp(VarExp v0, VarExp v1),cont)))))
        | BinExp(binop, exp1, exp2) ->
          let v0 = freshFor (freeVars exp2 @ freeVars cont) in
          let v1 = freshFor (v0 :: freeVars cont) in
          cps exp1
          (FunExp(v0, cps exp2
                      (FunExp(v1, AppExp(cont, BinExp(binop, VarExp v0, VarExp v1))))))
        | MonExp (monop, exp) ->
          let v = freshFor (freeVars cont) in
          cps exp (FunExp(v, AppExp(cont, MonExp(monop, VarExp v))))
        | FunExp(x,exp) ->
          let k = freshFor (freeVars exp) in
          AppExp(cont, FunExp(x, (FunExp(k, cps exp (VarExp k)))))
        | LetExp(x,exp1,exp2) -> cps exp1 (FunExp(x, cps exp2 cont))
        | RecExp(f,x,exp1,exp2) ->
          let k = freshFor (f::x::freeVars exp1) in
          RecExp(f,x,FunExp(k,cps exp1 (VarExp k)),cps exp2 cont)
        | OAppExp(funexp, argexp) ->
          let v0 = freshFor (freeVars funexp @ freeVars cont) in
          let v1 = freshFor (v0 :: freeVars cont) in
          cps argexp
          (FunExp(v0, cps funexp
                      (FunExp(v1, AppExp(AppExp(VarExp v1, VarExp v0),cont)))))

(*
let rec cps exp cont =
    match exp with
          VarExp x -> AppExp (cont,VarExp x)
        | ConExp c -> AppExp(cont,ConExp c)
        | IfExp(bexp,thenexp,elseexp) ->
          let v = freshFor (freeVarsList [thenexp; elseexp; cont]) in
           cps bexp
             (FunExp(v,IfExp(VarExp v,cps thenexp cont, cps elseexp cont)))
        | AppExp(funexp, argexp) ->
          let v0 = freshFor (freeVarsList [argexp; cont]) in
          let v1 = freshFor (v0 :: freeVarsList [cont]) in
          cps funexp 
          (FunExp(v0, cps argexp
                      (FunExp(v1, AppExp(AppExp(VarExp v0, VarExp v1),cont)))))
        | BinExp(binop, exp1, exp2) ->
          let v0 = freshFor (freeVarsList [exp2; cont]) in
          let v1 = freshFor (v0 :: freeVarsList [cont]) in
          cps exp1
          (FunExp(v0, cps exp2
                      (FunExp(v1, AppExp(cont, BinExp(binop, VarExp v0, VarExp v1))))))
        | MonExp (monop, exp) ->
          let v = freshFor (freeVarsList [cont]) in
          cps exp (FunExp(v, AppExp(cont, MonExp(monop, VarExp v))))
        | FunExp(x,exp) ->
          let k = freshFor (freeVarsList [exp]) in
          AppExp(cont, FunExp(x, (FunExp(k, cps exp (VarExp k)))))
        | LetExp(x,exp1,exp2) ->
          cps exp1 (FunExp(x, cps exp2 cont))
        | RecExp(f,x,exp1,exp2) ->
          let k = freshFor (f::x::freeVarsList [exp1]) in
          RecExp(f,x,FunExp(k,cps exp1 (VarExp k)),cps exp2 cont)
        | OAppExp(funexp, argexp) ->
          let v0 = freshFor (freeVarsList [funexp; cont]) in
          let v1 = freshFor (v0 :: freeVarsList [cont]) in
          cps argexp
          (FunExp(v0, cps funexp
                      (FunExp(v1, AppExp(AppExp(VarExp v1, VarExp v0),cont)))))
*)





