(*
 * File: mp6-skeleton.ml
 *)

open Mp6common

(* Problem 0*)
let asMonoTy1 () = mk_fun_ty bool_ty (mk_list_ty int_ty);; 
let asMonoTy2 () = let (d,c,b,a) = (fresh(),fresh(),fresh(),fresh()) in
                   mk_fun_ty d (mk_fun_ty c (mk_fun_ty b a ) );;
let asMonoTy3 () = let (f,e) = (fresh(),fresh()) in 
                   let t_list = mk_list_ty (mk_pair_ty e int_ty) in
                   mk_fun_ty f t_list;;
let asMonoTy4 () = mk_pair_ty string_ty (mk_fun_ty (mk_list_ty (fresh())) (fresh()));; 

(* Problem 1*)
let rec subst_fun subst m = failwith "Not implemented"

(* Problem 2*)
let rec monoTy_lift_subst subst monoTy = failwith "Not implemented"

(* Problem 3*)
let rec occurs x ty = failwith "Not implemented"

(* Problem 4*)
let rec unify eqlst = failwith "Not implemented"

(* Problem 5: No Credit *)
let equiv_types ty1 ty2 = failwith "Not implemented"
