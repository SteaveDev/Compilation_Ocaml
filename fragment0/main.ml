(*
  Lecture fichier -->
  Source : https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/23456034
*)
let read_file filename =
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines
;;

let rec convert_list s l =
match l with
| [] -> s
| e::ll -> convert_list (s^e) ll
;;


open AST
let _ =
  try
    let lexbuf = Lexing.from_string ((convert_list "" (read_file "code.js"))^"\n") in

      let ast = Parseur.programme Lexeur.token lexbuf and file_out = open_out "code.jsm" in

      AST.print_AST_programme Format.std_formatter ast;
      Format.print_newline();
      flush stdout;

      output_string file_out ((AST.print_prog ast)^"Halt\n");
      Format.print_newline();
      flush stdout;

  with
    | Lexeur.Eof          ->  exit 0
    | Lexeur.TokenInconu 
    | Parsing.Parse_error ->  Printf.printf ("Ceci n'est pas une expression arithmétique\n")
 
