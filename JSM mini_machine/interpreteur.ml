open AST


module type Interne_type = sig
  exception Type_attendu of string
  exception Null_Exception 
  type valeur
  type context
  type objet
  val init_code : instruction_a array -> unit
  val stack : valeur array
  val pc : int ref
  val sp : int ref
  val cc : context ref
  val undefined : valeur
  val nulObj : objet
  val emptyObj : unit->objet
  val print_valeur : Format.formatter -> valeur -> unit
  val print_machine : Format.formatter -> unit
  val next : unit -> instruction_a
  val declare : (string * valeur) -> context -> unit
  val set : (string * valeur) -> context -> unit
  val set_obj : (string * valeur) -> objet-> unit
  val get : string -> context -> valeur
  val get_obj : string -> objet -> valeur
  val copy_cont :  context -> context
  val prototyper : objet -> objet 
  val get_prototype : objet -> objet 
  val push : valeur -> unit
  val pop : unit -> valeur
  val pull : unit -> valeur
  val pop_bool : unit -> bool
  val pop_reel : unit -> float
  val pop_string : unit -> string
  val pop_entier : unit -> int
  val pop_cloture : unit -> (int * context * (string list))
  (*val pop_continuation :  unit -> (bool * int * context) *)
  val pop_objet :  unit -> objet
  val pull_objet :  unit -> objet
  val pull_type : unit -> int
  val push_bool : bool -> unit
  val push_reel : float -> unit
  val push_string : string -> unit
  val push_entier : int -> unit
  val push_cloture : (int * context * (string list)) -> unit
  val push_continuation : (bool * int * context) -> unit
  val push_undefined : unit -> unit
  val push_objet : objet -> unit
  val reel_of_bool : bool -> float
  (* val bool_of_reel : float -> bool *)
  val return_aux : unit -> unit
  val return_opt : unit -> unit
  val throw_aux : unit -> unit
  val set_stack : (int * valeur) -> unit
  val get_stack : int -> valeur

end




module Interne  : Interne_type =
struct
  exception Type_attendu of string
  exception Null_Exception 
      
  type valeur =
   | Bool of bool
   | Reel of float
   | String of string
   | Cloture of int * context * (string list)
   | Continuation of bool * int * context
   | Objet of objet
   | Undefined
   
and context = (string,valeur) Hashtbl.t list
and objet   = (string,valeur) Hashtbl.t list

let undefined = Undefined
let nulObj = []
let emptyObj () = [Hashtbl.create 100]

let stack = Array.make 5000 Undefined
let pc = ref (-1)
let sp = ref (-1)
let cc = ref [Hashtbl.create 1000]
let code = ref (Array.make 0 Noop)

let init_code c = code := c 

(* Affichage *)
    
let print_weak_ass_wcont form (var,v,c) = Format.fprintf form "%s,%t" var  c
let rec print_weak_ht form ht = Hashtbl.fold (fun var v c f -> print_weak_ass_wcont f (var,v,c))
                                        ht
		 	                (fun f -> Format.fprintf f "")
		           form
let rec print_weak_cont form = function
    []   -> Format.fprintf form ""
  | m::c -> Format.fprintf form "%a%a" print_weak_ht m print_weak_cont c

let rec print_valeur form = function
   | Bool b                -> Format.fprintf form (if b then "True" else "False")
   | Reel x                -> Format.fprintf form "%g" x
   | String s              -> Format.fprintf form "\"%s\"" s
   | Cloture (l,c,vs)      -> Format.fprintf form "Cloture(%i,<%a>,[%a])" l print_weak_cont c print_list_str vs 
   | Continuation (b,l,c)  -> Format.fprintf form "Continuation%s(%i,<%a>)" (if b then "" else "Err") l print_weak_cont c
   | Objet o               -> Format.fprintf form "Objet(%a)" print_weak_cont o
   | Undefined             -> Format.fprintf form "Undefined"
   
and print_list_str form = function
   | []     -> Format.fprintf form ""  
   | [b]    -> Format.fprintf form "%s " b
   | (b::l) -> Format.fprintf form " %s , %a" b print_list_str l 

and print_list_valeur form = function
   | []     -> Format.fprintf form ""  
   | [b]    -> Format.fprintf form "%a " print_valeur b
   | (b::l) -> Format.fprintf form " %a , %a" print_valeur b print_list_valeur l 


let stack_to_list () =
   let res = ref [] in
   for i=0 to !sp do res := (stack.(i)):: !res done;
   !res

let print_stack form () = Format.fprintf form "%s@[<2>@%a@ %s@]"
                      "Pile : [" print_list_valeur (stack_to_list()) "]" 


let print_ass_wcont form (var,v,c) = Format.fprintf form "( %s => %a ),  %t" var  print_valeur v  c
      

let rec print_ht form ht = Hashtbl.fold (fun var v c f -> print_ass_wcont f (var,v,c))
                                        ht
		 	                (fun f -> Format.fprintf f "")
		    form


let rec print_cont form = function
    []   -> Format.fprintf form ""
  | m::c -> Format.fprintf form "%a|%a" print_ht m print_cont c
              

let print_cc form = Format.fprintf form "@[<2>@%s %a@]"
                  "Contexte : " print_cont !cc 

let print_machine form =  Format.fprintf form "%s%i@.%s%a%s@.%s%a@.%s%a\n"
                     "PC : " (!pc+1)  
		     "Pile : [" print_list_valeur (stack_to_list()) "]"
		     "Contexte : " print_cont !cc
		     "Instruction : " print_ast (!code).(!pc+1) 
		     


(* fonction sur PC *)

let next () = pc:=!pc+1; (!code).(!pc) 



(* fonctions sur le contexte *)

let rec declare (var,v) = function
  | m::_     -> Hashtbl.replace m var v
  | []       -> failwith "impossible"

let rec set (var,v) = function
  | [m]  -> Hashtbl.replace m var v
  | m::c -> if Hashtbl.mem m var
            then Hashtbl.replace m var v
            else set (var,v) c
  | []   -> failwith "impossible"

let rec set_obj (var,v) = function
  | m::_     -> Hashtbl.replace m var v;
  | []       -> raise Null_Exception


let rec get var = function
  | []   -> Undefined
  | m::c -> try Hashtbl.find m var
            with (Not_found ) -> get var c

let rec get_obj = get

let rec copy_cont c = (Hashtbl.create 1000)::c

let rec prototyper c =
  let ht=Hashtbl.create 100 in
  Hashtbl.add ht "__proto__" (Objet c);
  ht :: c

let get_prototype = List.tl



(* fonctions sur la pile *)

let push x  = sp := !sp+1 ; stack.(!sp) <- x   
let pop ()  = sp := !sp-1 ; stack.(!sp+1)
let pull () = stack.(!sp)

(* spécifications sur la tête de pile *)

let pop_bool () = match pop() with
     Bool x     -> x
   | Reel x     -> x<>0.
   | String x   -> x<>""
   | Objet c    -> c<>[]
   | Undefined  -> false
   | _          -> raise (Type_attendu "Bool")

let pop_reel () = match pop() with
     Reel k       -> k
   | _            -> raise (Type_attendu "Reel")

let pop_string () = match pop() with
     String k     -> k
   | _            -> raise (Type_attendu "String")

let pop_entier () = match pop() with
     Reel k       -> int_of_float k
   | _            -> raise (Type_attendu "Reel")

let pop_cloture () = match pop() with
     Cloture (x,y,z) -> (x,y,z)
   | _               -> raise (Type_attendu "Cloture")

(*
let pop_continuation () = match pop() with
     Continuation (f,x,y) -> (f,x,y)
   | _                    -> raise (Type_attendu "Continuation")
*)

let pop_objet () = match pop() with
     Objet x       -> x
   | _             -> raise (Type_attendu "Objet")
   
let pull_objet () = match pull() with
     Objet x       -> x
   | _             -> raise (Type_attendu "Objet")

let pull_type() = match pull() with 
   | Bool _          -> 0
   | Reel _          -> 1
   | String _        -> 2
   | Undefined       -> 3
   | Objet _         -> 4
   | Cloture _       -> 5
   | Continuation _  -> 6

let push_bool x = push (Bool x)
let push_reel x = push (Reel x) 
let push_string x = push (String x) 
let push_entier x = push (Reel (float_of_int x)) 
let push_cloture (x,y,z) = push (Cloture (x,y,z))
let push_objet x = push (Objet x)
let push_continuation (f,x,y) = push (Continuation (f,x,y))
let push_undefined () = push Undefined


(* casts *)

let reel_of_bool b = if b then 1. else 0.
(* let bool_of_reel x = x<>0. *)

(* fonctions auxiliaires de depilement *)

let rec return_aux () = match pop() with
  | Continuation(true,p,c)  -> pc := p; cc := c
  | Continuation(false,p,c) -> return_aux ()
  | _ -> return_aux ()
           
let rec return_opt () = match pop() with
  | Continuation(_,p,c)  -> pc := p; cc := c
  | _ -> raise (Type_attendu "Continuation de retour")

let rec throw_aux () = match pop() with
  | Continuation(false,p,c)  -> pc := p; cc := c
  | _                        -> throw_aux ()


let set_stack (i,v) = stack.(!sp-i) <- v

let get_stack i = stack.(!sp-i)

end










module MiniJSMachine   :
sig
  exception Type_attendu of string
  exception Erreur_programme of string
  exception Fin_programme
  exception Null_Exception 
  val print_machine : Format.formatter -> unit
  val step : unit -> unit
  val init_code : instruction_a array -> unit
end  =
struct
  exception Erreur_programme of string
  exception Fin_programme

  include Interne

  (* execution d'une instruction *)

  let step () = match next() with
    | AddiRe            -> push_reel(pop_reel() +. pop_reel())
    | SubsRe            -> push_reel(pop_reel() -. pop_reel()) 
    | MultRe            -> push_reel(pop_reel() *. pop_reel())
    | DiviRe            -> push_reel(pop_reel() /. pop_reel())
    | NegaRe            -> push_reel(-. pop_reel())
    | Modulo            -> (pop_entier() mod pop_entier())  |>  push_entier 
    | Not               -> pop_bool() |> not |> push_bool
    | BoToRe            -> pop_bool() |> reel_of_bool |> push_reel
    | ReToBo            -> push_bool(pop_bool())
    | Equal             -> push_bool(pop() = pop())
    | NotEq             -> push_bool(pop() <> pop())
    | LowEqR            -> push_bool(pop() <= pop())
    | GreEqR            -> push_bool(pop() >= pop())
    | LowStR            -> push_bool(pop() < pop())
    | GreStR            -> push_bool(pop() > pop())
    | Jump off          -> pc := !pc + off
    | ConJmp off        -> if not (pop_bool()) then pc := !pc + off
    | Case q            -> pc := !pc + q*pop_entier() 
    | CstRe c           -> push_reel(c) 
    | CstBo c           -> push_bool(c) 
    | CstUn             -> push_undefined() 
    | Copy              -> pull() |> push
    | Swap              -> let (x,y) = (pop(),pop()) in push y; push x
    | Drop              -> sp := !sp-1
    | Noop              -> ()
    | TypeOf            -> pull_type() |> push_entier
    | Halt              -> raise Fin_programme
    | Error err         -> raise (Erreur_programme err)
    | Assert err        -> if not (pop_bool()) then raise (Erreur_programme err)
    | Print             -> Format.fprintf  Format.std_formatter "%a\n\n"  print_valeur (pop())
    | Call              -> let (p,c,l) = pop_cloture() in
      List.iter (fun var -> declare (var,undefined) c) l;
      push_continuation(true,!pc,!cc);
      pc := p;
      cc := c;
    | Return            ->
      let res = pop() in
      return_aux ();
      push res
    | Lambda off        -> push_cloture(!pc+off , !cc , [])
    | DclArg var        ->
      let (p,c,l) = pop_cloture() in
      push_cloture (p,c,var::l)
    | Throw             ->
      let res = pop() in
      throw_aux ();
      push res
    | Continue (pos,b)  -> push_continuation (b,pos,!cc)
    | Catch off         -> push_continuation (false,!pc+off,!cc)
    | Finally off       -> push_continuation (true,!pc+off,!cc)
    | Concat            -> push_string(pop_string() ^ pop_string())
    | CstStr str        -> push_string(str)
    | LowEqS            -> push_bool(pop() <= pop())
    | GreEqS            -> push_bool(pop() >= pop())
    | LowStS            -> push_bool(pop() < pop())
    | GreStS            -> push_bool(pop() > pop())
    | StToRe            -> pop_string() |> float_of_string |> push_reel
    | ReToSt            -> pop_reel() |> string_of_float |> push_string
    | Null              -> push_objet(nulObj)
    | NewObj            -> emptyObj() |> push_objet
    | IsEmpt            -> (pop_objet() = nulObj) |> push_bool
    | GetVar var        -> get var !cc |> push
    | Point var         -> pop_objet() |> get_obj var |> push
    | DclVar var        -> declare (var,undefined) !cc
    | SetVar var        -> set (var,pop()) !cc 
    | StCall            -> let (p,c,l) = pop_cloture() in push_cloture(p,copy_cont c,l) 
    | SetArg            -> let v = pop() in
      ( match  pop_cloture() with
     	| (p,c,[])      -> push_cloture(p,c,[])
	| (p,c,var::l') -> 
          declare (var,v) c ;
	  push_cloture(p,c,l')
      )
    | SetObj var        -> let v = pop() in
      pull_objet() |> set_obj (var,v)
    | Protot            -> pop_objet() |>  prototyper |> push_objet 
    | GetPrt            -> pop_objet() |> get_prototype |> push_objet 
    | TlCall            -> let (p,c,l) = pop_cloture() in
      List.iter (fun var -> declare (var,undefined) c) l;
      pc := p;
      cc := c
    | RetOpt            -> 
      let res = pop() in
      return_opt ();
      push res
    | RetZer            -> return_aux ();
    | RetZOp            -> return_opt ();
    | SetVD var         -> set (var,pull()) !cc 
    | SetVS i           -> set_stack (i,pull())
    | GetVS i           -> i |> get_stack |> push
end                      


module type Executable = sig
  val execution : unit -> unit
  val init_code : instruction_a array -> unit
end

(* execution de toutes les instructions *)
module Std_Exe  : Executable =
struct
  include MiniJSMachine
  
  let execution () =
    print_machine Format.std_formatter;
    try
      while true do step(); print_machine Format.std_formatter  done
    with 
    | Type_attendu st       -> Printf.printf "Crash : type entré imprévu\n"
    | Erreur_programme st   -> Printf.printf "Exception : st"
    | Fin_programme         -> Printf.printf "Programme exécuté avec succes \n"
    | Invalid_argument st   -> Printf.printf "%s ;\n raison possible : la pile est vidée, la pile est pleine (stack overflow), ou bien vous allez au delà du programme (avez-vous bien mis un Halt ?)\n" st
    | Null_Exception        -> Printf.printf "Null n'a pas de propriété"
end


(* Main *)

let _ =
  if (Array.length Sys.argv > 1)
  then (
    try
      Lexing.from_channel (open_in Sys.argv.(1)) 
      |> Parseur.main Lexeur.token
      |> Array.of_list 
      |> Std_Exe.init_code;
      Std_Exe.execution ();
    with 
    | Lexeur.CharInconu st  -> Printf.printf "\"%c\" n'est pas un tocken d'assembleur abstrait (erreur lexical)\n" st
    | Lexeur.TokenInconu st  -> Printf.printf "\"%s\" n'est pas un tocken d'assembleur abstrait (erreur lexical)\n" st
    | Parsing.Parse_error -> Printf.printf "Instruction incohérente (erreur syntaxique)\n"
  ) else (
    Printf.printf "Veuillez indiquer le fichier assembleur à interpréter en argment\n"
  )
 
