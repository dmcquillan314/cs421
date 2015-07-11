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
    (*| BinOpAppExp( b_op, exp_1, exp_2 ) ->
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
                                        )) *)
    | BinOpAppExp( b_op, e1, e2 ) ->
        let tau' = binop_signature b_op in
        let tau1 = fresh() in
        let tau2 = fresh() in
        let t_1_proof = gather_exp_ty_substitution gamma e1 tau1 in
        (match t_1_proof with None -> None
             | Some(proof_t1, sigma_1) ->
                let gamma_e2 = env_lift_subst sigma_1 gamma in
                let t_2_proof = gather_exp_ty_substitution gamma_e2 e2 tau2 in
                (match t_2_proof with None -> None
                     | Some(proof_t2, sigma_2) ->
                        let sigma_21 = subst_compose sigma_2 sigma_1 in
                        let u = unify [(monoTy_lift_subst sigma_21
                                                          (mk_fun_ty tau1
                                                                 (mk_fun_ty
                                                                     tau2
                                                                     tau)),
                                                          freshInstance tau')]
                        in (match u with None -> None
                             | Some( sigma_3 ) ->
                                Some(Proof([proof_t1;proof_t2], judgment),
                                     subst_compose sigma_3 sigma_21))))
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
    | FnExp(v, exp_1) ->
        let (t_1,t_2) = (fresh(),fresh()) in
        let t_2_proof = gather_exp_ty_substitution (ins_env gamma v (polyTy_of_monoTy t_1)) 
                                                    exp_1
                                                    t_2 in
            (match t_2_proof with None -> None
                 | Some (proof_t2, sigma) -> 
                     let u = unify [(
                         monoTy_lift_subst sigma tau,
                         monoTy_lift_subst sigma ( mk_fun_ty t_1 t_2 ) )] in
                    (match u with None -> None
                         | Some( u_sigma ) -> Some(
                            Proof([proof_t2], judgment),
                            subst_compose u_sigma sigma
                         )
                    )
            )
    | AppExp(exp_1, exp_2) -> 
        let t_1 = fresh() in
        let t_1_t_proof = gather_exp_ty_substitution gamma 
                                                     exp_1 
                                                     (mk_fun_ty t_1 tau) in
        (match t_1_t_proof with None -> None
             | Some(proof_t_1_t, sigma_1) ->
                     let gamma_e2 = env_lift_subst sigma_1 gamma
                     and t_2 = monoTy_lift_subst sigma_1 t_1 in
                     let t_2_proof = gather_exp_ty_substitution gamma_e2
                                                                exp_2
                                                                t_2 in
                     match t_2_proof with None -> None
                         | Some(proof_t_2, sigma_2) -> 
                                 Some( Proof([proof_t_1_t;proof_t_2], judgment),
                                       subst_compose sigma_2 sigma_1))
    | RaiseExp(exp_1) -> 
        let int_type = int_ty in
        let int_type_proof = gather_exp_ty_substitution gamma exp_1 int_type in
        (match int_type_proof with None -> None
             | Some(proof_int_type, sigma) -> Some( Proof([proof_int_type], judgment),
                                                    sigma ))
    | LetExp(dec_1, exp) ->
        let dec_proof = gather_dec_ty_substitution gamma dec_1 in
        (match dec_proof with None -> None
             | Some(proof_dec,delta,sigma_1) ->
                let gamma_e = sum_env delta (env_lift_subst sigma_1 gamma) in
                let tau' = monoTy_lift_subst sigma_1 tau in
                let e_proof = gather_exp_ty_substitution gamma_e exp tau in
                match e_proof with None -> None
                | Some(proof_e, sigma_2) -> 
                    let sigma_21 = subst_compose sigma_2 sigma_1 in
                    Some( Proof([proof_dec;proof_e], judgment), sigma_21) )
    | HandleExp(exp, o_int_1, e_1, pair_list) -> 
        let exp_proof = gather_exp_ty_substitution gamma exp tau in
        (match exp_proof with None -> None
             | Some(proof_exp, sigma) ->
                 let fold_result = 
                     List.fold_left 
                        (fun proof_sigma_pair -> 
                            fun (o_int_i, e_i) ->
                                (match proof_sigma_pair with None -> None
                                     | Some(rev_proof_list, composed_sigma) ->
                                        let gamma_i = env_lift_subst composed_sigma gamma in
                                        let tau_i = monoTy_lift_subst composed_sigma tau in
                                        let e_i_proof = gather_exp_ty_substitution gamma_i e_i tau_i in 
                                        (match e_i_proof with None -> None
                                             | Some(proof_e_i, sigma_i) ->
                                                Some(
                                                    proof_e_i :: rev_proof_list, 
                                                    subst_compose sigma_i composed_sigma
                                                )
                                        )
                                )
                        )
                        (Some([proof_exp], sigma))
                        ((o_int_1, e_1) :: pair_list) in
                 (match fold_result with None -> None
                      | Some(rev_proof_list, composed_subst) ->
                        let proof_list = List.rev rev_proof_list in
                        Some(Proof(proof_list, judgment), composed_subst)))
and gather_dec_ty_substitution gamma dec = 
    match dec with 
        Val(v, exp) -> 
            let t_1 = fresh() in
            let t_1_proof = gather_exp_ty_substitution gamma exp t_1 in
            (match t_1_proof with None -> None
                | Some(proof_t_1, sigma) ->
                    let gamma_1 = env_lift_subst sigma gamma in
                    let t_2 = monoTy_lift_subst sigma t_1 in
                    let gen_type = gen gamma_1 t_2 in
                    let gamma_v = make_env v gen_type in
                    let judgment = DecJudgment(gamma, dec, gamma_v) in
                    Some( Proof([proof_t_1], judgment), gamma_v, sigma))
        | Rec(f, x, exp) -> 
            let (tau_1,tau_2) = (fresh(),fresh()) in
            let tau_f = mk_fun_ty tau_1 tau_2 in
            let gamma_x = ins_env gamma x (polyTy_of_monoTy tau_1) in 
            let gamma_fx = ins_env gamma_x f (polyTy_of_monoTy tau_f) in
            let fx_proof = gather_exp_ty_substitution gamma_fx exp tau_2 in
            (match fx_proof with None -> None
                 | Some(proof_fx, sigma) ->
                    let gamma_s = env_lift_subst sigma gamma in
                    let tau_s_f = monoTy_lift_subst sigma tau_f in
                    let tau_gen = gen gamma_s tau_s_f in
                    let gamma_s_f = make_env f tau_gen in
                    let judgment = DecJudgment(gamma, dec, gamma_s_f) in
                    Some( Proof([proof_fx], judgment), gamma_s_f, sigma))
        | Seq(dec_1, dec_2) ->
            let dec_1_proof = gather_dec_ty_substitution gamma dec_1 in
            (match dec_1_proof with None -> None
                 | Some(proof_dec_1, delta_1, sigma_1) ->
                    let gamma_dec_2 = env_lift_subst sigma_1 (sum_env delta_1 gamma) in
                    let dec_2_proof = gather_dec_ty_substitution gamma_dec_2 dec_2 in
                    (match dec_2_proof with None -> None
                         | Some(proof_dec_2, delta_2, sigma_2) ->
                            let gamma_dec_21 = sum_env delta_2 delta_1 in
                            let sigma_21 = subst_compose sigma_2 sigma_1 in
                            let gamma_s_21_d_21 = env_lift_subst sigma_21 gamma_dec_21 in
                            let judgment = DecJudgment(gamma, dec, gamma_s_21_d_21) in
                            Some( Proof([proof_dec_1;proof_dec_2], judgment),
                                  gamma_s_21_d_21,
                                  sigma_21
                            )))
        | Local(dec_1, dec_2) -> 
            let dec_1_proof = gather_dec_ty_substitution gamma dec_1 in
            (match dec_1_proof with None -> None
                 | Some(proof_dec_1, delta_1, sigma_1) ->
                    let gamma_2 = env_lift_subst sigma_1 (sum_env delta_1 gamma) in
                    let dec_2_proof = gather_dec_ty_substitution gamma_2 dec_2 in
                    (match dec_2_proof with None -> None
                         | Some( proof_dec_2, delta_2, sigma_2 ) ->
                            let sigma_21 = subst_compose sigma_2 sigma_1 in
                            let gamma' = env_lift_subst sigma_21 delta_2 in
                            let judgment = DecJudgment(gamma, dec, gamma') in
                            Some( Proof([proof_dec_1;proof_dec_2], judgment),
                                  gamma',
                                  sigma_21 )))
        | _ -> raise (Failure "Not implemented yet")
