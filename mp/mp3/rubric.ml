(*
 * grader for mp3
 * This file will be preprocessed to generate the actual OCaml file.
 *)
let rubric_version = "1.0"
let rubric_title = "CS421 Fall 2011 MP3"

open Grader
open Test
open Mp3common

(*
 * use a timeout of 4 seconds
 *)

let outputOk () =
  try (
  let len = String.length !output_str
  in let half1 = String.sub !output_str 0 (len / 2)
     and half2 = String.sub !output_str (len / 2) (len / 2)
     in half1=half2
  ) with e -> false

let isEq i j =
  (i = j) && (let res = outputOk() in output_str := ""; res)

let mptest weight pair = compare isEq 4 weight pair


(**************************************************************************
 * You can add new test cases by adding new elements to the following lists
 * Format is:
 * TEST<X>ARG(<weight>, <function_name>, <arg1>, <arg2>, ..., <argX>)
 *
 * <X> is the number of argument that the function being tested takes.
 **************************************************************************)
open Mp3common

let id = fun x -> x;;

(* This list is for regular problems *)
let rubric =
[
  "minmax"^" "^"[1;-2;4;5;-8]", mptest 1 (ss_pair1 Solution.minmax Student.minmax ([1;-2;4;5;-8]));
  "minmax_list"^" "^"[[1];[-1;2];[1;3;4];[]]", mptest 1 (ss_pair1 Solution.minmax_list Student.minmax_list ([[1];[-1;2];[1;3;4];[]]));
  "cumlist"^" "^"[1;2;3]", mptest 1 (ss_pair1 Solution.cumlist Student.cumlist ([1;2;3]));
  "revsplit"^" "^"(fun x -> x < 0)"^" "^"[-1; 4; -2; 3; 0; 1]", mptest 1 (ss_pair2 Solution.revsplit Student.revsplit ((fun x -> x < 0)) ([-1; 4; -2; 3; 0; 1]));
  "andk"^" "^"true"^" "^"false"^" "^"id", mptest 1 (ss_pair3 Solution.andk Student.andk (true) (false) (id));
  "ork"^" "^"true"^" "^"false"^" "^"id", mptest 1 (ss_pair3 Solution.ork Student.ork (true) (false) (id));
  "landk"^" "^"0"^" "^"1"^" "^"id", mptest 1 (ss_pair3 Solution.landk Student.landk (0) (1) (id));
  "lork"^" "^"0"^" "^"1"^" "^"id", mptest 1 (ss_pair3 Solution.lork Student.lork (0) (1) (id));
  "lxork"^" "^"0"^" "^"1"^" "^"id", mptest 1 (ss_pair3 Solution.lxork Student.lxork (0) (1) (id));
  "expk"^" "^"1.0"^" "^"id", mptest 1 (ss_pair2 Solution.expk Student.expk (1.0) (id));
  "logk"^" "^"2.0"^" "^"id", mptest 1 (ss_pair2 Solution.logk Student.logk (2.0) (id));
  "powk"^" "^"2.0"^" "^"3.0"^" "^"id", mptest 1 (ss_pair3 Solution.powk Student.powk (2.0) (3.0) (id));
  "modaddk"^" "^"5"^" "^"7"^" "^"10"^" "^"id", mptest 1 (ss_pair4 Solution.modaddk Student.modaddk (5) (7) (10) (id));
  "modsubk"^" "^"12"^" "^"4"^" "^"7"^" "^"id", mptest 1 (ss_pair4 Solution.modsubk Student.modsubk (12) (4) (7) (id));
  "modmulk"^" "^"3"^" "^"4"^" "^"8"^" "^"id", mptest 1 (ss_pair4 Solution.modmulk Student.modmulk (3) (4) (8) (id));
  "modeqk"^" "^"6"^" "^"5"^" "^"11"^" "^"id", mptest 1 (ss_pair4 Solution.modeqk Student.modeqk (6) (5) (11) (id));
  "power"^" "^"2"^" "^"3", mptest 1 (ss_pair2 Solution.power Student.power (2) (3));
  "powerk"^" "^"2"^" "^"3"^" "^"id", mptest 1 (ss_pair3 Solution.powerk Student.powerk (2) (3) (id));
  "dup_alt"^" "^"[1;2;3;4]", mptest 1 (ss_pair1 Solution.dup_alt Student.dup_alt ([1;2;3;4]));
  "dup_altk"^" "^"[1;2;3;4]"^" "^"id", mptest 1 (ss_pair2 Solution.dup_altk Student.dup_altk ([1;2;3;4]) (id));
  "rev_map"^" "^"(fun x -> print_int x; x + 1)"^" "^"[1;2;3;4;5]", mptest 1 (ss_pair2 Solution.rev_map Student.rev_map ((fun x -> print_int x; x + 1)) ([1;2;3;4;5]));
  "rev_mapk"^" "^"(fun x -> fun k -> print_intk x (fun t -> addk x 1 k))"^" "^"[1;2;3;4;5]"^" "^"(fun x -> x)", mptest 1 (ss_pair3 Solution.rev_mapk Student.rev_mapk ((fun x -> fun k -> print_intk x (fun t -> addk x 1 k))) ([1;2;3;4;5]) ((fun x -> x)));
  "partition"^" "^"[1;2;3;4;5]"^" "^"(fun x -> x >= 3)", mptest 1 (ss_pair2 Solution.partition Student.partition ([1;2;3;4;5]) ((fun x -> x >= 3)));
  "partitionk"^" "^"[1;2;3;4;5]"^" "^"(fun x -> fun k -> geqk x 3 k)"^" "^"(fun x -> x)", mptest 1 (ss_pair3 Solution.partitionk Student.partitionk ([1;2;3;4;5]) ((fun x -> fun k -> geqk x 3 k)) ((fun x -> x)));
]
(*




*)

(* Note: the last entry should not be followed by a semicolon. *)

let extra_rubric = [
]


let _ = Main.main rubric extra_rubric rubric_title rubric_version
