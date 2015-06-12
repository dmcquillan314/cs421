(* CS421 - Summer 2015
 * MP1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open Mp1common

(* Problem 1 *)
let title = "MP 1 -- Basic OCaml"

(* Problem 2 *)
let e = 2.71828

(* Problem 3 *)
let firstFun n = (n * 2) + 5;; 

(* Problem 4 *)
let divide_e_by x = e /. x;; 

(* Problem 5 *)
let diff_square_9 m = (if m >= -3 && m <= 3 then (m * m) - 9 else 9 - (m * m));;   

(* Problem 6 *)
let dist_double s n = let text = "I guess it's double or nothing!\n" in
print_string (s ^ ", "  ^ text); n * 2;;  

(* Problem 7 *)
let swizzle (w,x,y,z) = (z, y, w, x);; 

