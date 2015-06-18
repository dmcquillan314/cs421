open Mp3common

(* Problem 1 *)
let minmax lst =
    let accum minMax x = match minMax with (min, max) -> (
        (if min > x then x else min),
        (if max < x then x else max)
    ) in 
        match lst
        with [] -> (0,0)
        | (x :: xs) -> List.fold_left accum (x,x) lst;;

let minmax_list lst = List.map minmax lst;;

(* Problem 2 *)
let cumlist_step x y = [ [ x ] ] @ List.map (fun l -> x :: l) y;;  
let cumlist lst = List.fold_right cumlist_step lst [];;

(* Problem 3 *)
let revsplit_step f x y = match x
    with (truthy,falsy) -> 
        if f(y) then 
            (y :: truthy, falsy) 
        else (truthy, y :: falsy);;
let revsplit f lst = List.fold_left (revsplit_step f) ([],[]) lst;;

(* Problem 4 *)
let andk a b k = k( a && b );;
let ork a b k = k( a || b );;
let landk a b k = k( a land b );;
let lork a b k = k( a lor b );;
let lxork a b k = k( a lxor b );;
let expk a k = k( exp a );;
let logk a k = k( log a );;
let powk a b k = k( a ** b );;

(* Problem 5 *)
let modaddk a b n k = addk a b (fun ab -> modk ab n k);;
let modsubk a b n k = subk a b (fun ab -> modk ab n k);;
let modmulk a b n k = mulk a b (fun ab -> modk ab n k);;
let modeqk a b n k = modk a n (fun an -> modk b n (fun bn -> eqk an bn k));;

(* Problem 6 *)
let rec power a n = if n <= 0 then 1 else a * power a (n - 1);;
let rec powerk a n k = leqk n 0 (fun b -> 
    if b then k 1
    else subk n 1 (fun s -> powerk a s (fun p -> mulk a p k)));;


(* Problem 7 *)
let rec dup_alt l = match l
    with [] -> []
    | (x :: xs) -> x :: match xs
        with [] -> dup_alt xs 
        | (y :: ys) -> y :: y :: dup_alt ys;;

let rec dup_altk l k = match l
    with [] -> k( [] )
    | (x :: xs) -> match xs
        with [] -> consk x [] k
        | (y :: ys) -> dup_altk ys 
            (fun lst -> consk y lst 
                (fun ylst -> consk y ylst
                    (fun yylst -> consk x yylst k)));;

(* Problem 8 *)
let rec rev_map f l = match l
    with [] -> []
    | (x :: xs) -> f(x) :: rev_map f xs;;
let rec rev_mapk f l k = match l
    with [] -> k( [] )
    | (x :: xs) -> rev_mapk f xs 
        (fun lst -> f x 
            (fun fx -> consk fx lst k));;

(* Problem 9 *)
let rec partition l p = match l
    with [] -> ([], [])
    | (x :: xs) -> match partition xs p
        with (t, f) -> if p(x) then (x :: t, f) else (t, x :: f);;
let rec partitionk l p k = match l
    with [] -> pairk [] [] k
    | (x :: xs) -> partitionk xs p
        (fun pair -> match pair
            with (t, f) -> p x
                (fun b -> 
                    if b then consk x t (fun c -> pairk c f k) 
                    else consk x f (fun c -> pairk t c k)));;
