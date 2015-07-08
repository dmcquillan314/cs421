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
             | _ -> raise (Failure "Not implemented yet")

and gather_dec_ty_substitution gamma dec = 
    raise (Failure "Not implemented yet")
