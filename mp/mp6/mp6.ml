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
let rec unify eqlst =
    (match eqlst with [] -> Some( [] ) 
    | ((s, t) :: eqlst_t) ->
        if s = t then unify eqlst_t (* delete *)
        else 
            (
                match (s, t)
                    with (TyConst _, TyVar _) -> (* orient *)
                        unify ((t, s) :: eqlst_t) 
                    | (TyConst(n_1, ty_list_1), TyConst(n_2, ty_list_2) ) -> (* decompose *)
                        if n_1 = n_2 && (equal_count ty_list_1 ty_list_2) then 
                            unify (eqlst_t @ (List.combine ty_list_1 ty_list_2))
                        else None (* failure *)
                    | (TyVar i, t ) -> (* eliminate *)
                        if occurs i t then None (* failure *)
                        else
                            (match unify (create_c_2 eqlst_t [(i, t)]) 
                              with None -> None (* failure *)
                                 | Some(phi) -> 
                                    let phi_t = monoTy_lift_subst phi t in
                                    let subst_s_phi_t = (i, phi_t) in
                                    Some(subst_s_phi_t :: phi)
                            )
                    | _ -> None (* failure *)
            )
    )
and equal_count l_1 l_2 =
    (List.length l_1) = (List.length l_2)
and create_c_2 con subst =
    match con with [] -> []
    | ( ( s_i, t ) :: c_t) -> 
        (
            monoTy_lift_subst subst s_i,
            monoTy_lift_subst subst t
        ) :: create_c_2 c_t subst;;

(* Problem 5: No Credit *)
let equiv_types ty1 ty2 = failwith "Not implemented"
