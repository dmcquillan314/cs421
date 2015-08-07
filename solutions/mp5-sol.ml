(* File: mp5-sol.ml *)

open Mp5common

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
(*
    let _ = print_string ("Trying to type "^ string_of_judgment judgment^"\n") in
*)
    let result =
    match exp
    with ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))
    | VarExp x -> 
      (match lookup_env gamma x with None -> None
       | Some tau' ->
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma)))
    | MonOpAppExp (monop, e1) ->
      let tau' = monop_signature monop in
      let tau1 = fresh() in
      (match gather_exp_ty_substitution gamma e1 tau1
       with None -> None
       | Some(pf, sigma) ->
         (match unify[(monoTy_lift_subst sigma (mk_fun_ty tau1 tau),
                       freshInstance tau')]
          with None -> None
          | Some subst ->
            Some(Proof([pf], judgment),
                 subst_compose subst sigma)))
    | BinOpAppExp (binop, e1,e2) ->
      let tau' = binop_signature binop in
      let tau1 = fresh() in
      (match gather_exp_ty_substitution gamma e1 tau1
       with None -> None
       | Some(pf1, sigma1) ->
         let tau2 = fresh() in
         (match gather_exp_ty_substitution (env_lift_subst sigma1 gamma) e2 tau2
          with None -> None
          | Some (pf2, sigma2) ->
            let sigma21 = subst_compose sigma2 sigma1 in
            (match unify[(monoTy_lift_subst sigma21
                          (mk_fun_ty tau1 (mk_fun_ty tau2 tau)),freshInstance tau')]
             with None -> None
             | Some sigma3 -> 
               Some(Proof([pf1;pf2], judgment),subst_compose sigma3 sigma21))))
    | IfExp(e1,e2,e3) ->
      (match gather_exp_ty_substitution gamma e1 bool_ty
       with None -> None
       | Some(pf1, sigma1) ->
         (match gather_exp_ty_substitution
                (env_lift_subst sigma1 gamma) e2 (monoTy_lift_subst sigma1 tau)
          with None -> None
          | Some (pf2, sigma2) ->
            let sigma21 = subst_compose sigma2 sigma1 in
            (match gather_exp_ty_substitution
                   (env_lift_subst sigma21 gamma) e3
                   (monoTy_lift_subst sigma21 tau)
             with  None -> None
             | Some(pf3, sigma3) ->
               Some(Proof([pf1;pf2;pf3], judgment), subst_compose sigma3 sigma21))))
    | FnExp(x,e) ->
      let tau1 = fresh() in
      let tau2 = fresh() in
      (match gather_exp_ty_substitution
             (ins_env gamma x (polyTy_of_monoTy tau1)) e tau2
       with None -> None
       | Some (pf, sigma) ->
         (match unify [(monoTy_lift_subst sigma tau,
                        monoTy_lift_subst sigma (mk_fun_ty tau1 tau2))]
          with None -> None
          | Some sigma1 ->
            Some(Proof([pf],judgment), subst_compose sigma1 sigma)))
    | AppExp(e1,e2) ->
      let tau1 = fresh() in
      (match gather_exp_ty_substitution gamma e1 (mk_fun_ty tau1 tau)
       with None -> None
       | Some(pf1, sigma1) ->
         (match gather_exp_ty_substitution (env_lift_subst sigma1 gamma) e2
                                           (monoTy_lift_subst sigma1 tau1)
          with None -> None
          | Some (pf2, sigma2) ->
            Some(Proof([pf1;pf2], judgment), subst_compose sigma2 sigma1)))
    | LetExp(dec,e) ->
      (match gather_dec_ty_substitution gamma dec
       with None -> None
       | Some (pf1,delta,sigma1) ->
         (match gather_exp_ty_substitution 
                (sum_env delta (env_lift_subst sigma1 gamma))
                e
                (monoTy_lift_subst sigma1 tau)
          with None -> None
          | Some (pf2, sigma2) ->
            Some (Proof([pf1;pf2], judgment), subst_compose sigma2 sigma1)))
    | RaiseExp e ->
      (match gather_exp_ty_substitution gamma e int_ty
       with None -> None
       | Some(pf, sigma) -> Some(Proof([pf],judgment), sigma))
    | HandleExp (e,intopt1,e1, match_list) ->
      (match (gather_exp_ty_substitution gamma e tau)
       with None -> None
       | Some (pf, sigma) ->
         (match
           List.fold_left
           (fun part_result -> fun (intopti, ei) ->
            (match part_result with None -> None
             | Some (rev_pflist, comp_sigmas) ->
               (match gather_exp_ty_substitution
                      (env_lift_subst comp_sigmas gamma) ei
                      (monoTy_lift_subst comp_sigmas tau)
                with None -> None
                | Some (pfi, sigmai) ->
                  Some (pfi :: rev_pflist, subst_compose sigmai comp_sigmas))))
           (Some([pf], sigma))
           ((intopt1,e1):: match_list)
           with None -> None
           | Some (rev_pflist, comp_subst) ->
             Some(Proof(List.rev rev_pflist, judgment), comp_subst)))
in (
(*
    (match result
     with None ->
      print_string ("Failed to type "^string_of_judgment judgment^"\n")
     | Some (_, subst) -> print_string ("Succeeded in typing "^
                               string_of_judgment judgment^"\n"^
"  with substitution "^ string_of_substitution subst ^"\n"));
*)
    result)

and gather_dec_ty_substitution gamma dec =
(*
    let _ = print_string ("Trying to type declaration "^ string_of_dec dec^
                          " in environment\n"^string_of_env gamma^"\n") in
*)
    let result = 
    match dec
    with Val(x,e) ->
      let tau = fresh() in
      (match gather_exp_ty_substitution gamma e tau
       with None -> None
       | Some(pf, sigma) ->
         let delta = make_env x (gen (env_lift_subst sigma gamma) 
                                     (monoTy_lift_subst sigma  tau)) in
         Some(Proof([pf], DecJudgment (gamma, dec, delta)), delta, sigma))
    | Rec(f,x,e) ->
      let tau1 = fresh() in
      let tau2 = fresh() in
      let t1_arrow_t2 = mk_fun_ty tau1 tau2 in
      (match gather_exp_ty_substitution 
             (ins_env (ins_env gamma x (polyTy_of_monoTy tau1)) 
                      f (polyTy_of_monoTy t1_arrow_t2))
             e tau2
       with None -> None
       | Some(pf, sigma) ->
         let delta = make_env f (gen (env_lift_subst sigma gamma) 
                                     (monoTy_lift_subst sigma t1_arrow_t2))
         in Some (Proof([pf], DecJudgment (gamma, dec, delta)), delta, sigma))
    | Seq(dec1,dec2) ->
      (match gather_dec_ty_substitution gamma dec1
       with None -> None
       | Some (pf1,delta1,sigma1) ->
         (match gather_dec_ty_substitution
                (env_lift_subst sigma1 (sum_env delta1 gamma))
                dec2
          with None -> None
          | Some (pf2,delta2,sigma2) ->
            let sigma21 = subst_compose sigma2 sigma1 in
            let delta21 = env_lift_subst sigma21 (sum_env delta2 delta1) in
            Some (Proof([pf1; pf2], DecJudgment (gamma, dec, delta21)), delta21,
                  sigma21)))
    | Local(dec1, dec2) ->
      (match gather_dec_ty_substitution gamma dec1
       with None -> None
       | Some (pf1,delta1,sigma1) ->
         (match gather_dec_ty_substitution
                (env_lift_subst sigma1 (sum_env delta1 gamma))
                dec2
          with None -> None
          | Some (pf2,delta2,sigma2) ->
            let sigma21 = subst_compose sigma2 sigma1 in
            let delta2 = env_lift_subst sigma21 delta2 in
            Some (Proof([pf1; pf2], DecJudgment (gamma, dec, delta2)), delta2,
                  subst_compose sigma2 sigma1)))
(*    | _ -> raise (Failure "Not implemented yet") *)
in
(*
    (match result
     with None ->
      print_string ("Failed to type declaration "^ string_of_dec dec^
                          " in environment\n"^string_of_env gamma^"\n")
     | Some (pf, subst) -> print_string ("Succeeded in typing "^
                               string_of_proof pf^"\n"^
"  with substitution "^ string_of_substitution subst ^"\n"));
*)  
    result
