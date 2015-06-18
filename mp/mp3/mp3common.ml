(* File: mp3common.ml *)

let output_str = ref "";;

let print_string str = 
  (output_str := !output_str ^ str;
  Pervasives.print_string (str))

let print_int n = print_string (string_of_int n)
let print_float x = print_string (string_of_float x)
let print_newline () = print_string "\n";;
let print_endline s = print_string (s^"\n");;

let rep_int x =
   print_string "Result: ";
   print_int x;
   print_newline();;
let rep_bool x =
   print_string "Result: ";
   if x then (print_string "true") else (print_string "false");
   print_newline();;
let rep_float x =
   print_string "Result: ";
   print_float x;
   print_newline();;

let addk x y k = k (x + y)
let subk x y k = k (x - y)
let mulk x y k = k (x * y)
let divk x y k = k (x / y)
let float_addk x y k = k(x +. y)
let float_subk x y k = k(x -. y)
let float_mulk x y k = k(x *. y)
let float_divk x y k = k(x /. y)
let pairk x y k = k(x,y)
let catk s1 s2 k = k (s1^s2)
let consk x l k = k (x::l)
let geqk x y k = k (x >= y)
let leqk x y k = k (x <= y)
let eqk x y k = k (x = y)
let modk x y k = k (x mod y)

let print_intk x k = (print_int x; k x)
