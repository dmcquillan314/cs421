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
    else if n mod 2 == 0 then (3 * s (n/2))
    else (2 + s (n-1));;

(* Problem 3 *)
let rec rle lst = match lst with
        [] -> []
    |   (x :: xs) -> (match rle xs with
            [] -> [(x, 1)]
        |   ((y,n)::rest) -> if x = y then ((y, (n+1))::rest) else (x,1)::(y,n)::rest);;

(* Problem 4 *)
let rec merge l1 l2 = match l1 with
        [] -> l2
    |   x1::xs1 -> (match l2 with
            [] -> l1
        |   x2::xs2 -> (if x1 < x2 then (x1:: merge xs1 l2) else (x2 :: merge l1 xs2)));;

(* Problem 5 *)
let rec separate l = match l with 
        [] -> ([],[])
    |   x::xs -> (match separate xs with
            (odd, even) -> if (x mod 2 = 0) then (odd, x::even) else (x::odd, even));;

(* Problem 6 *)
let rec maxsumseq l =
    let rec aux l c m = match l with
            [] -> m
        |   x::xs -> (let s = x + c in
            if s > 0 then aux xs s (if s > m then s else m) else aux xs 0 m)
    in (aux l 0 0);;

(* Problem 7 *)
let rec outnode_list l n = match l with
        [] -> []
    |   (x::xs) -> if n < 0 then []
                   else if n = 0 then x else outnode_list xs (n-1);;

let rec elt x l = match l with
        [] -> false
    |   (y::ys) -> (x = y) || elt x ys

let check_adj adj_list (a,b) =
    a >= 0 && b >= 0 && elt b (outnode_list adj_list a);;

(* Problem 8 *)
let cumsum l =
    let rec aux lst sum curr = match lst with
        [] -> sum
    |   (x::xs) -> (aux xs (sum@[(x+curr)]) (x+curr))
    in aux l [] 0;;
