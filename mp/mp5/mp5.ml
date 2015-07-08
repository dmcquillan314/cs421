open Mp5common

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp
    with ConstExp c ->
         (* T |- c : t | unify{  (t, freshInstance(t')) } *)
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))
    | VarExp x -> 
         (* T |- x : t | unify{  (t, freshInstance(T(x))) } *)
         let tau_x = lookup_env gamma x in
         (match tau_x 
            with None -> None
               | Some t -> (match unify [(tau, freshInstance t)]
                with None        -> None
                   | Some sigma  -> Some(Proof([],judgment), sigma)))
    | BinOpAppExp( b_op, exp_1, exp_2 ) ->
        let tau' = binop_signature b_op
        and t_1  = fresh()
        and t_2  = fresh() in
            let t_1_proof = gather_exp_ty_substitution gamma exp_1 t_1 in
            (match t_1_proof
                with None -> None
                   | Some(proof_1, sigma_1) -> 
                        let gamma_exp_1 = env_lift_subst sigma_1 gamma in
                        let t_2_proof = gather_exp_ty_substitution gamma_exp_1 exp_2 t_2 in
                            match t_2_proof
                            with None -> None
                               | Some(proof_2, sigma_2) ->
                                    let sigma_3 = subst_compose sigma_2 sigma_1 in
                                    let u = unify [(monoTy_lift_subst sigma_3
                                            (mk_fun_ty t_1 (mk_fun_ty t_2 tau)), 
                                            freshInstance tau'
                                        )] in
                                    match u
                                    with None -> None
                                       | Some sigma -> Some(
                                           Proof([proof_1;proof_2], judgment), 
                                           subst_compose sigma sigma_3
                                        ))
    | MonOpAppExp( m_op, exp_1 ) -> 
        let (tau', t_1) = (monop_signature m_op, fresh()) in
        let t_proof = gather_exp_ty_substitution gamma exp_1 t_1 in
        (match t_proof
            with None -> None
               | Some (proof_1, sigma_1) ->
                    let u = unify [(monoTy_lift_subst sigma_1 (mk_fun_ty t_1 tau), freshInstance tau')] in
                    match u
                        with None -> None
                           | Some u_sigma ->
                                Some(Proof([proof_1], judgment), subst_compose u_sigma sigma_1))
    | IfExp(exp_1, exp_2, exp_3) ->
        let t_1_proof = gather_exp_ty_substitution gamma exp_1 bool_ty in
        (match t_1_proof with None -> None
             | Some (bool_proof, sigma_1) ->
                let gamma_e2 = env_lift_subst sigma_1 gamma
                and tau_2 = monoTy_lift_subst sigma_1 tau in
                let t_2_proof = gather_exp_ty_substitution gamma_e2 exp_2 tau_2 in
                match t_2_proof with None -> None
                    | Some(then_proof, sigma_2) ->
                        let sigma_2_1 = subst_compose sigma_2 sigma_1 in
                        let gamma_e3 = env_lift_subst sigma_2_1 gamma 
                            and tau_3 = monoTy_lift_subst sigma_2_1 tau in
                        let t_3_proof = gather_exp_ty_substitution gamma_e3
                                                                   exp_3 
                                                                   tau_3 in
                        match t_3_proof with None -> None 
                            | Some(else_proof, sigma_3) -> 
                                Some(
                                    Proof([bool_proof;then_proof;else_proof], judgment),
                                    subst_compose sigma_3 sigma_2_1))
    | _ -> raise(Failure "Not yet complete")
and gather_dec_ty_substitution gamma dec = 
    raise (Failure "Not implemented yet")
