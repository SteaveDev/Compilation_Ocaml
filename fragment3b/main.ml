(*
  Lecture fichier -->
  Source : https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/23456034
*)
let read_file filename =
let lines = ref [] in
let chan = open_in filename in
try
    while true; do
        let line = input_line chan in
        if String.contains line '/' then
            begin
                let indexLcote = (String.rindex line '/') in
                if indexLcote>0 && line.[indexLcote-1]='/'
                    then lines :=  (String.sub line 0 (indexLcote-1)) :: !lines
                else lines :=  line :: !lines
            end
        else lines :=  line :: !lines
    done; !lines
with End_of_file ->
    close_in chan;
    List.rev !lines
;;

let rec convert_list s l =
match l with
| [] -> print_string "\n";s
| e::ll -> print_string e;convert_list (s^e) ll
;;

let count_substring str sub =
    let sub_len = String.length sub in
    let len_diff = (String.length str) - sub_len
    and reg = Str.regexp_string sub in
    let rec aux i n =
        if i > len_diff then n else
            try
                let pos = Str.search_forward reg str i in
                aux (pos + sub_len) (succ n)
            with Not_found -> n
    in
    aux 0 0
;;

let cpt = ref 0
;;
let rec getnb s ss =
    let c = s.[0] in match c with
        | '0' .. '9'    -> getnb (String.sub s 1 ((String.length s)-1)) (ss^(Char.escaped c))
        | _             -> string_of_int (!cpt+(int_of_string ss))

let rec removeNb s =
    let c = s.[0] in match c with
        | '0' .. '9'    -> removeNb (String.sub s 1 ((String.length s)-1))
        | _             -> s

let rec print_list l s = match l with
    | []            -> "";
    | [e]           -> (s^e);
    | e :: a :: ll  -> cpt := !cpt+ (count_substring e "\n");print_list ((removeNb a)::ll) (s^e^(getnb a ""))

;;

open AST
let _ =
  try
    let lexbuf = Lexing.from_string ((convert_list "" (read_file "data.js"))^"\n") in

        let ast = Parseur.programme Lexeur.token lexbuf and file_out = open_out "code.jsm" in

        AST.print_AST_prog Format.std_formatter ast;
        Format.print_newline();
        flush stdout;

        let code = ((AST.print_preProg ast (AST.getTailleGlobal ast))^(AST.print_prog ast)^"Halt\n") in

        let code_List = Str.split (Str.regexp_string "$") code in
        output_string file_out (print_list code_List "" ) ;
        Format.print_newline();
        flush stdout;
        
  with
    | Lexeur.Eof          ->  exit 0
    | Lexeur.TokenInconu 
    | Parsing.Parse_error ->  Printf.printf ("Ceci n'est pas un programme JS\n")
