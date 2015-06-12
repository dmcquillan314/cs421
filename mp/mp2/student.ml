(* CS421 - Summer 2015
 * MP2
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open Mp2common

(* Problem 1 *)
let rev_apply f (x,y) = (f y, f x);; 

(* Problem 2 *)
let rec s n = 
    if n <= 1 then 1
    else if (n mod 2) = 0 then 3 * s(n / 2)
    else 2 + s(n - 1);; 

(* Problem 3 *)
let rec rle lst = raise (Failure "Function not implemented yet.") 

(* Problem 4 *)
let rec merge l1 l2 = raise (Failure "Function not implemented yet.") 

(* Problem 5 *)
let rec separate l = raise (Failure "Function not implemented yet.") 

(* Problem 6 *)
let rec maxsumseq l = raise (Failure "Function not implemented yet.") 

(* Problem 7 *)
let check_adj adj_list (a,b) = raise (Failure "Function not implemented yet.") 

(* Problem 8 *)
let cumsum l = raise (Failure "Function not implemented yet.") 
