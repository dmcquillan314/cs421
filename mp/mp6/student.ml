(*
 * File: mp6-skeleton.ml
 *)

open Mp6common

(* Problem 0*)
let asMonoTy1 () = TyConst("->", [
    TyConst("bool", []);
    TyConst("list", [ 
        TyConst("int", [])
    ])
]);; 
let asMonoTy2 () = TyConst("->", [
    TyVar 1;
    TyConst("->", [
        TyVar 2;
        TyConst("->", [
            TyVar 3;
            TyVar 4
        ])
    ])
]);; 
let asMonoTy3 () = TyConst("->", [
    TyVar 1;
    TyConst("list", [
        TyConst("*", [
            TyVar 2;
            TyConst("int", [])
        ])
    ])
]);;
let asMonoTy4 () = TyConst("*", [
    TyConst("string", []);
    TyConst("->", [
        TyConst("list", [ TyVar 2 ]);
        TyVar 1
    ])
]);; 

(* Problem 1*)
let rec subst_fun subst m = 
    match subst with [] -> TyVar(m) 
    | ( (t_var, m_ty) :: t_subs ) -> if t_var = m then m_ty 
                                     else (subst_fun t_subs m);; 

(* Problem 2*)
let rec monoTy_lift_subst subst monoTy =
    (match monoTy with TyVar( ty_var ) -> subst_fun subst ty_var
    | TyConst(s, mono_tys) -> TyConst(s, monoTy_lift_subst_list subst mono_tys)) 
and monoTy_lift_subst_list subst mono_tys = 
    match mono_tys with [] -> []
    | ( mono_ty :: mono_ty_tail ) -> 
        let rest_tys = monoTy_lift_subst_list subst mono_ty_tail in
        monoTy_lift_subst subst mono_ty :: rest_tys;; 

(* Problem 3*)
let rec occurs x ty = 
    (match ty with TyVar( ty_var ) -> ty_var = x
     | TyConst( s, mono_tys ) -> occurs_list x mono_tys)
and occurs_list x ty_list = 
    match ty_list with [] -> false
    | ( ty_head :: ty_list_tail ) -> 
        let head_occurs = occurs x ty_head in
        head_occurs || ( occurs_list x ty_list_tail );;

(* Problem 4*)
let rec unify eqlst = failwith "Not implemented"

(* Problem 5: No Credit *)
let equiv_types ty1 ty2 = failwith "Not implemented"
