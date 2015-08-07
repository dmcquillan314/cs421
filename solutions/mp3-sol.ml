open Mp3common

(* Problem 1 *)
let minmax lst = match lst with
	[] -> (0,0)
    |   x::xs -> List.fold_left (fun (min,max) y -> (if y < min then (y,max) else if max < y then (min,y) else (min,max))) (x,x) xs;;
let minmax_list lst =  List.map minmax lst;;

(* Problem 2 *)
let cumlist_step x lst = [x] :: (List.map (fun l -> x::l) lst);;
let cumlist lst =  List.fold_right cumlist_step lst [];;

(* Problem 3 *)
let revsplit_step f (tr,fl) x = if (f x) then (x::tr,fl) else (tr,x::fl);;
let revsplit f lst =   List.fold_left (revsplit_step f) ([],[]) lst;;

(* Problem 4 *)
let andk a b k = k (a && b);;
let ork a b k = k (a || b);;
let landk a b k = k (a land b);;
let lork a b k = k (a lor b);;
let lxork a b k = k (a lxor b);;
let expk a k = k (exp a);;
let logk a k = k (log a);;
let powk a b k = k (a ** b);;

(* Problem 5 *)
let modaddk a b n k = addk a b (fun ab -> modk ab n k);;
let modsubk a b n k = subk a b (fun ab -> modk ab n k);;
let modmulk a b n k = mulk a b (fun ab -> modk ab n k);;
let modeqk a b n k = subk a b (fun ab -> modk ab n (fun d -> eqk d 0 k));;

(* Problem 6 *)
let rec power a n = if n <= 0 then 1 else a * (power a (n - 1));;
let rec powerk a n k =
	leqk n 0 (fun b -> (if b then (k 1) else (subk n 1
		      		(fun s -> powerk a s (fun m -> mulk a m k)))));;

(* Problem 7 *)
let rec dup_alt lst =
	match lst with [] -> []
	| x::[] -> [x]
	| x::y::xs -> x::y::y:: dup_alt xs;;
(*with continuation*)
let rec concatk l1 l2 k = 
	match l1 with [] -> k l2
	| x::xs -> concatk xs l2 (fun l -> consk x l k);;
let rec dup_altk lst k =
	match lst with [] -> k []
	| x::[] -> k [x] 
	| x::y::xs -> dup_altk xs (fun d -> concatk [x] [y] (fun xy -> concatk xy [y] (fun xyy -> concatk xyy d k)));;


(* Problem 8 *)
let rec rev_map f l =
  match l with
  | [] -> []
  | h :: t -> let t = rev_map f t in (f h) :: t;;

let rec rev_mapk f l k =
  match l with
  | [] -> k []
  | h :: t -> rev_mapk f t
      (fun t1 -> f h
        (fun t2 -> consk t2 t1 k));;

(* Problem 9 *)
let rec partition l p =
  match l with
  | [] -> ([], [])
  | h :: t -> match (partition t p) with
    | (l1, l2) -> if (p h) then (h :: l1, l2) else (l1, h :: l2);;

let rec partitionk l p k = 
  match l with
  | [] -> k ([], [])
  | h :: t -> partitionk t p
      (fun t -> match t with
        | (l1, l2) -> p h
            (fun t2 -> if t2 then (consk h l1 (fun t3 -> k (t3, l2)))
                             else (consk h l2 (fun t3 -> k (l1, t3)))));;




