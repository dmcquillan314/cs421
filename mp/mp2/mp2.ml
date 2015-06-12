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
let rec merge l1 l2 = match (l1, l2)
    with ([], []) -> []
    | ( l1 :: l1s, [] ) -> l1 :: merge l1s []
    | ( [], l2 :: l2s ) -> l2 :: merge [] l2s
    | ( l1h :: l1r, l2h :: l2r ) -> 
        if l1h > l2h then 
            l2h :: merge l1 l2r 
        else 
            l1h :: merge l1r l2;;

(* Problem 5 *)
let rec separate l = raise (Failure "Function not implemented yet.") 

(* Problem 6 *)
let rec maxsumseq l = raise (Failure "Function not implemented yet.") 

(* Problem 7 *)
let check_adj adj_list (a,b) = raise (Failure "Function not implemented yet.") 

(* Problem 8 *)
let cumsum l = raise (Failure "Function not implemented yet.") 
