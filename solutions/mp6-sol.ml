open Mp6common

(* Problem 0 *)
let asMonoTy1 () = mk_fun_ty bool_ty (mk_list_ty int_ty);;
let asMonoTy2 () =
    mk_fun_ty (fresh()) (mk_fun_ty (fresh()) (mk_fun_ty (fresh()) (fresh())));;
let asMonoTy3 () = mk_fun_ty (fresh()) (mk_list_ty (mk_pair_ty (fresh()) int_ty));;
let asMonoTy4 () = mk_pair_ty string_ty (mk_fun_ty (mk_list_ty (fresh())) (fresh()));;

(* Problem 1 *)
let rec subst_fun subst m =
    match subst with [] -> TyVar m
    | (n,ty) :: more -> if n = m then ty else subst_fun more m

(* Problem 2 *)
let rec monoTy_lift_subst subst monoTy =
    match monoTy
    with TyVar m -> subst_fun subst m
    | TyConst(c, typelist) ->
      TyConst(c, List.map (monoTy_lift_subst subst) typelist)

(* Problem 3 *)
let rec occurs x ty =
    match ty
    with TyVar n -> x = n
    | TyConst(c, typelist) -> List.exists (occurs x) typelist

(* Problem 4 *)
let rec unify eqlst : substitution option =
  let rec addNewEqs lst1 lst2 acc =
    match lst1,lst2 with
      [],[] -> Some acc
    | t::tl, t'::tl' -> addNewEqs tl tl' ((t,t')::acc)
    | _ -> None
  in
  match eqlst with
    [] -> Some([])
  | (s,t)::eqs ->
    (* Delete *)
    if s = t then unify eqs
    else (match (s,t) 
    (* Decompose *)
          with (TyConst(c, tl), TyConst(c', tl')) ->
            if c=c' then (match (addNewEqs tl tl' eqs) with None -> None | Some l -> unify l)
            else None
    (* Orient *)
          | (TyConst(c, tl), TyVar(m)) -> unify ((TyVar(m), TyConst(c, tl))::eqs)
    (* Eliminate *)
          | (TyVar(n),t) ->
             if (occurs n t)
             then None
             else let eqs' =
                      List.map
                      (fun (t1,t2) ->
                           (monoTy_lift_subst [(n,t)] t1, monoTy_lift_subst [(n,t)] t2))
                      eqs
                   in (match unify eqs'
                       with None -> None
                       | Some(phi) -> Some((n, monoTy_lift_subst phi t):: phi)))

(* Problem 5 *)

let rec first p l =
    match l with [] -> None
    | (x :: xs) -> if p x then Some x else first p xs

let rec canon_type subst m ty =
   match ty
   with TyVar n ->
     (match first (fun p -> fst p = n) subst
      with Some (j,k) -> (subst, m, TyVar k)
      | None -> ((n,m)::subst), m+1, TyVar m)
   | TyConst (c, tys) ->
     (match
       List.fold_left
       (fun (subst, n, tyl) -> fun ty ->
        (match canon_type subst n ty
         with (subst', n', ty') -> (subst', n', ty'::tyl)))
       (subst, m, [])
       tys
      with (new_subst, new_m, new_tys) -> (new_subst, new_m, TyConst(c, List.rev new_tys)))

let canon_type_pair subst m (ty1, ty2) =
    let (subst1, m1, canon_ty1) = canon_type subst m ty1 in
    let (subst2, m2, canon_ty2) = canon_type subst1 m1 ty2 in
        (subst2, m2, (canon_ty1, canon_ty2))

let canon_constraints subst m consts =
    let (subst2, m2, new_consts) =
    (List.fold_left
     (fun (subst, n, consts) -> fun (ty1, ty2) -> 
      let (subst1, m1, new_eq) = canon_type_pair subst m (ty1, ty2) in
       (subst1, m1, new_eq::consts))
     (subst, m, [])
     consts)
     in (subst2, m2, List.rev new_consts)

let canon_by_subst ty =
    match canon_type [] 0 ty with (subst, _ , canon_ty) -> (subst, canon_ty)

let canonicalize ty = let (_, _, c_ty) = canon_type [] 0 ty in c_ty

let equiv_types ty1 ty2 =
   let new_ty1 = canonicalize ty1 in
   let new_ty2 = canonicalize ty2 in new_ty1 = new_ty2;;


