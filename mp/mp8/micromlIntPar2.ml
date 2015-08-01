(*
  interactive-parser.ml - DO NOT EDIT
*)

open Mp8common
open Solution

let _ =
  print_endline "\nWelcome to the Solution parser \n";
  let rec loop gamma =
  try
      let lexbuf = Lexing.from_channel stdin in
        print_string "> "; flush stdout;
        (try
          let dec = main (fun lb -> match Micromllex2.token lb
                         with EOF -> raise Micromllex2.EndInput
                     | r -> r)
                    lexbuf in
          match infer_dec gather_dec_ty_substitution gamma dec with
             None          -> (print_string "\ndoes not type check\n"; loop gamma)
           | Some (env_inc,p) ->
              let gamma' = sum_env env_inc gamma
              in
                        (
			      print_string (string_of_inc_env env_inc);print_newline();
                              print_string "\n\nfinal environment:\n\n";
                              print_string (string_of_type_env gamma');  print_string "\n\nproof:";
                              print_string(string_of_proof p); print_newline(); loop gamma')
        with Failure s -> (print_newline(); print_endline s; print_newline(); loop gamma)
           | Parsing.Parse_error -> print_string "\ndoes not parse\n"); loop gamma;
  with Micromllex2.EndInput -> exit 0
(* in loop init_type_env *)
  in loop empty_env
