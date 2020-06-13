type arguments_a =
      Id    of string * int
    | Args  of string * arguments_a * int
and expressions_a =
      Eexp  of expression_a * int
    | Exps  of expression_a * expressions_a * int
and expression_a =
    | Plus  of expression_a * expression_a * int
    | Moins of expression_a * expression_a * int
    | Mult  of expression_a * expression_a * int
    | Div   of expression_a * expression_a * int
    | DivE  of expression_a * expression_a * int
    | Egale of expression_a * expression_a * int
    | Negal of expression_a * expression_a * int
    | Supp  of expression_a * expression_a * int
    | Et    of expression_a * expression_a * int
    | Ou    of expression_a * expression_a * int
    | Tern  of expression_a * expression_a  * expression_a * int
    | Neg   of expression_a * int
    | Non   of expression_a * int
    | To    of expression_a * int
    | Num   of float * int
    | Ident of string * int
    | Bool  of bool * int
    | Strin of string * int
    | Aff   of string * expression_a * int
    | CallA  of string * expressions_a * int
    | CallV  of string * int
and affect_expr_a =
      AEexp of expression_a * int
    | VarA  of string * expression_a * int
    | Var   of string * int
and commande_a =
      Nil   of int
    | Com   of affect_expr_a * int
    | Cprog of programme_a * int
    | SiSin of expression_a * commande_a * commande_a * int
    | Tq    of expression_a * commande_a * int
    | Faire of commande_a * expression_a * int
    | Pour  of affect_expr_a * expression_a * expression_a * commande_a * int
    | Ecrir of expression_a * int
    | FuncA of string * arguments_a * programme_a * int
    | FuncV of string * programme_a * int
    | Retur of expression_a * int
and programme_a =
      Nill of int
    | Last of commande_a * int
    | Prog of commande_a * programme_a * int
;;


let getTaille_expr expression = match expression with
    | Plus  (g,d,t)         -> t
    | Moins (g,d,t)         -> t
    | Mult  (g,d,t)         -> t
    | Div   (g,d,t)         -> t
    | DivE  (g,d,t)         -> t
    | Egale (g,d,t)         -> t
    | Negal (g,d,t)         -> t
    | Supp  (g,d,t)         -> t
    | Et    (g,d,t)         -> t
    | Ou    (g,d,t)         -> t
    | Tern  (g,c,d,t)       -> t
    | Neg   (e,t)           -> t
    | Non   (e,t)           -> t
    | To    (e,t)           -> t
    | Num   (n,t)           -> t
    | Ident (i,t)           -> t
    | Bool  (b,t)           -> t
    | Strin (s,t)           -> t
    | Aff   (i,e,t)         -> t
    | CallA (i,es,t)        -> t
    | CallV (i,t)           -> t
and getTaille_com commande = match commande with
    | Nil  t                -> t
    | Com   (e,t)           -> t
    | Cprog (e,t)           -> t
    | SiSin (e,c1,c2,t)     -> t
    | Tq    (e,c,t)         -> t
    | Faire (c,e,t)         -> t
    | Pour  (ae,e2,e3,c,t)  -> t
    | Ecrir (e,t)           -> t
    | FuncA (i,a,p,t)       -> t
    | FuncV (i,p,t)         -> t
    | Retur (e,t)           -> t
and getTailleAE affect_expr = match affect_expr with
    | AEexp (e,t)           -> t
    | VarA  (i,e,t)         -> t
    | Var   (i,t)           -> t
and getTailleArgs arguments = match arguments with
    | Id    (i,t)           -> t
    | Args  (i,a,t)         -> t
and getTaille_exprs expressions = match expressions with
    | Eexp  (e,t)           -> t
    | Exps  (e,es,t)        -> t
;;


let rec print_binaire form s g d t = Format.fprintf form "@[<2>%i%s%s@ %a%s@ %a%s@]" t s "(" print_AST_exp g " ," print_AST_exp d " )"
and print_AST_exp form = let open Format in function
    | Plus  (g,d,t)         -> print_binaire form "Plus" g d t
    | Moins (g,d,t)         -> print_binaire form "Moins" g d t
    | Mult  (g,d,t)         -> print_binaire form "Mult" g d t
    | Div   (g,d,t)         -> print_binaire form "Div" g d t
    | DivE  (g,d,t)         -> print_binaire form "DivE" g d t
    | Egale (g,d,t)         -> print_binaire form "Egale" g d t
    | Negal (g,d,t)         -> print_binaire form "Negal" g d t
    | Supp  (g,d,t)         -> print_binaire form "Supp" g d t
    | Et    (g,d,t)         -> print_binaire form "Et" g d t
    | Ou    (g,d,t)         -> print_binaire form "Ou" g d t
    | Tern  (g,c,d,t)       -> fprintf form "@[<2>%i%s@ %a%s@ %a%s@ %a%s@]"         t "Tern(" print_AST_exp g " ," print_AST_exp c " ," print_AST_exp d ")"
    | Neg    (e,t)          -> fprintf form "@[<2>%i%s@ %a%s@]"                     t "Neg(" print_AST_exp e ")"
    | Non   (e,t)           -> fprintf form "@[<2>%i%s@ %a%s@]"                     t "Non(" print_AST_exp e ")"
    | To    (e,t)           -> fprintf form "@[<2>%i%s@ %a%s@]"                     t "To(" print_AST_exp e ")"
    | Num    (n, t)         -> fprintf form "@[<2>%i%s@ %g@]"                       t "Num" n
    | Ident  (i,t)          -> fprintf form "@[<2>%i%s@ %s@]"                       t "Ident" i
    | Bool  (b,t)           -> fprintf form "@[<2>%i%s@ %b@]"                       t "Bool" b
    | Strin (s,t)           -> fprintf form "@[<2>%i%s@ %s@]"                       t "String" s
    | Aff   (i,e,t)         -> fprintf form "@[<2>%i%s@ %s%s@ %a%s@]"               t "Aff(" i " ," print_AST_exp e ")"
    | CallA  (i,es,t)       -> fprintf form "@[<2>%i%s@ %s%s@ %a%s@]"               t "CallavArg(" i " ," print_AST_exps es ")"
    | CallV  (i,t)          -> fprintf form "@[<2>%i%s@ %s%s@]"                     t "CallssArg(" i ")"
and print_AST_com form = let open Format in function
    | Nil  t                -> ()
    | Com   (e,t)           -> fprintf form "@[<2>%i%s@ %a%s@]"                     t "Com(" print_AST_ae e ")"
    | Cprog (p,t)           -> fprintf form "@[<2>%i%s@ %a%s@]"                     t "ComP(" print_AST_prog p ")"
    | SiSin (e,c1,c2,t)     -> fprintf form "@[<2>%i%s@ %a%s@ %a%s@ %a%s@]"         t "SiSin(" print_AST_exp e " ," print_AST_com c1 " ," print_AST_com c2 ")"
    | Tq    (e,c,t)         -> fprintf form "@[<2>%i%s@ %a%s@ %a%s@]"               t "TantQue(" print_AST_exp e " ," print_AST_com c ")"
    | Faire (c,e,t)         -> fprintf form "@[<2>%i%s@ %a%s@ %a%s@]"               t "Faire(" print_AST_com c " ," print_AST_exp e ")"
    | Pour  (e1,e2,e3,c,t)  -> fprintf form "@[<2>%i%s@ %a%s@ %a%s@ %a%s@ %a%s@]"   t "Pour(" print_AST_ae e1 " ," print_AST_exp e2 " ," print_AST_exp e3 " ,"  print_AST_com c ")"
    | Ecrir (e,t)           -> fprintf form "@[<2>%i%s@ %a%s@]"                     t "Ecrire(" print_AST_exp e ")"
    | FuncA (i,a,p,t)       -> fprintf form "@[<2>%i%s@ %s%s@ %a%s@ %a%s@]"         t "FuncA(" i " ," print_AST_arg a " ," print_AST_prog p")"
    | FuncV (i,p,t)         -> fprintf form "@[<2>%i%s@ %s%s@ %a%s@]"               t "FuncV(" i " ," print_AST_prog p")"
    | Retur (e,t)           -> fprintf form "@[<2>%i%s@ %a%s@]"                     t "Return(" print_AST_exp e ")"
and print_AST_prog form = let open Format in function
    | Nill t                -> ()
    | Last (l,t)            -> fprintf form "@[<2>%i%s@ %a%s@]"                     t "Last(" print_AST_com l ")"
    | Prog (c,p,t)          -> fprintf form "@[<2>%i%s@ %a%s@ %a%s@]"               t "Prog(" print_AST_com c " ," print_AST_prog p ")"
and print_AST_ae form = let open Format in function
    | AEexp (e,t)           -> fprintf form "@[<2>%i%s@ %a%s@]"                     t "AEexp(" print_AST_exp e ")"
    | VarA  (i,e,t)         -> fprintf form "@[<2>%i%s@ %s%s@ %a%s@]"               t "VarA(" i " ," print_AST_exp e ")"
    | Var   (i,t)           -> fprintf form "@[<2>%i%s@ %s%s@]"                     t "Var(" i ")"
and print_AST_exps form = let open Format in function
    | Eexp  (e,t)           -> fprintf form "@[<2>%i%s@ %a%s@]"                     t "Eexp(" print_AST_exp e ")"
    | Exps  (e,es,t)        -> fprintf form "@[<2>%i%s@ %a%s@ %a%s@]"               t "Exps(" print_AST_exp e " ," print_AST_exps es ")"
and print_AST_arg form = let open Format in function
    | Id    (i,t)           -> fprintf form "@[<2>%i%s@ %s%s@]"                     t "Arg(" i ")"
    | Args  (i,a,t)         -> fprintf form "@[<2>%i%s@ %s%s@ %a%s@]"               t "Args(" i "," print_AST_arg a ")"
;;


let rec print_expr_nb3 g c d = (print_expr_nb1 g)^
                            "\nConJmp "^string_of_int ((getTaille_expr c)+1)^
                            "\n"^(print_expr_nb1 c)^
                            "\nJump "^string_of_int (getTaille_expr d)^
                            "\n"^(print_expr_nb1 d)
and print_expr_nb2_ou g d = (print_expr_nb1 g)^
                            "\nConJmp "^string_of_int 1^
                            "\nJump"^string_of_int ((getTaille_expr d)+1)^
                            "\n"^(print_expr_nb1 d)

and print_expr_nb2_et g d = (print_expr_nb1 g)^
                            "\nConJmp "^string_of_int ((getTaille_expr d)+1)^
                            "\n"^(print_expr_nb1 d)
and print_expr_nb2 s g d = (print_expr_nb1 g)^"\n"^(print_expr_nb1 d)^"\n"^s
and print_expr_nb1  = function
    | Plus  (g,d,t)         -> print_expr_nb2 "AddiRe" g d
    | Moins (g,d,t)         -> print_expr_nb2 "SubsRe" g d
    | Mult  (g,d,t)         -> print_expr_nb2 "MultRe" g d
    | Div   (g,d,t)         -> print_expr_nb2 "DiviRe" g d
    | DivE  (g,d,t)         -> print_expr_nb2 "Modulo" g d
    | Egale (g,d,t)         -> print_expr_nb2 "Equal" g d
    | Negal (g,d,t)         -> print_expr_nb2 "NotEq" g d
    | Supp  (g,d,t)         -> print_expr_nb2 "GreStR" g d
    | Et    (g,d,t)         -> print_expr_nb2_et g d
    | Ou    (g,d,t)         -> print_expr_nb2_ou g d
    | Tern  (g,c,d,t)       -> print_expr_nb3 g c d
    | Neg   (e,t)           -> (print_expr_nb1 e)^"\nNegaRe"
    | Non   (e,t)           -> (print_expr_nb1 e)^"\nNot"
    | To    (e,t)           -> (print_expr_nb1 e)^"\nTypeOf"
    | Num   (n, t)          -> "CstRe "^(string_of_float n)
    | Ident (i,t)           -> "GetVar "^ i
    | Bool  (b,t)           -> "CstRe "^(string_of_bool b)
    | Aff   (i,e,t)         -> (print_expr_nb1 e)^"\nSetVar " ^ i
    | Strin (s,t)           -> "CstStr"^s
    | CallA (i,es,t)        -> "GetVar " ^ i ^ "\nStCall\n" ^(print_expr es) ^"\nSetArg\nCall"
    | CallV (i,t)           -> "GetVar " ^ i ^ "\nStCall\nCall"


and print_com_sisin g c d = (print_expr_nb1 g)^
                            "\nConJmp "^string_of_int ((getTaille_com c)+1)^
                            "\n"^(print_com c)^
                            "\nJump "^string_of_int (getTaille_com d)^
                            "\n"^(print_com d)
and print_com_tq e c = (print_expr_nb1 e)^
                            "\nConJmp "^string_of_int ((getTaille_com c)+1)^
                            "\n"^(print_com c)^
                            "\nJump "^string_of_int (((getTaille_com c)+(getTaille_expr e)+1)*(-1))
and print_com_faire c e = (print_com c)^
                            "\n"^(print_expr_nb1 e)^
                            "\nConJmp 1\nJump "^string_of_int (((getTaille_com c)+(getTaille_expr e)+1)*(-1))
and print_com_pour e1 e2 e3 c = (print_ae e1)^
                            "\n"^(print_expr_nb1 e2)^
                            "\nConJmp "^string_of_int ((getTaille_expr e3)+(getTaille_com c)+1)^
                            "\n"^(print_com c)^
                            "\n"^(print_expr_nb1 e3)^
                            "\nJump "^string_of_int (((getTaille_com c)+(getTaille_expr e3)+(getTaille_expr e2)+2)*(-1))
and print_com = function
    | Nil   t               -> ""
    | Com   (ex,t)          -> print_ae ex
    | Cprog (p,t)           -> print_prog p
    | SiSin (e,c1,c2,t)     -> print_com_sisin e c1 c2
    | Tq    (e,c,t)         -> print_com_tq e c
    | Faire (c,e,t)         -> print_com_faire c e
    | Pour  (e1,e2,e3,c,t)  -> print_com_pour e1 e2 e3 c
    | Ecrir (e,t)           -> (print_expr_nb1 e)^"\nPrint"
    | FuncA (i,a,p,t)       -> "Jump "^string_of_int (t)^"\n"^print_prog p
    | FuncV (i,p,t)         -> "Jump "^string_of_int (t)^"\n"^print_prog p
    | Retur (e,t)           -> (print_expr_nb1 e)^"\nReturn "

and print_ae = function
    | AEexp (e,t)           -> print_expr_nb1 e
    | VarA  (i,e,t)         -> "DclVar "^i^"\n"^(print_expr_nb1 e)^"\nSetVar "^i
    | Var   (i,t)           -> "DclVar "^i ^"\n"
and print_expr = function
    | Eexp  (e,t)           -> print_expr_nb1 e
    | Exps  (e,es,t)        -> print_expr_nb1 e ^ print_expr es
and print_prog = function
    | Nill  t               -> ""
    | Last  (l,t)           -> print_com l
    | Prog  (c,p,t)         -> (print_com c)^"\n"^(print_prog p)
;;


let rec taille_expr_nb3 x y z = 2 + (taille_expr_nb1 x) + (taille_expr_nb1 y) + (taille_expr_nb1 z)
and taille_expr_nb2_ou x y = 2 + (taille_expr_nb1 x) + (taille_expr_nb1 y)
and taille_expr_nb2 x y = 1 + (taille_expr_nb1 x) + (taille_expr_nb1 y)
and taille_expr_nb1 w = match w with
    | Plus  (g,d,t)         -> taille_expr_nb2 g d
    | Moins (g,d,t)         -> taille_expr_nb2 g d
    | Mult  (g,d,t)         -> taille_expr_nb2 g d
    | Div   (g,d,t)         -> taille_expr_nb2 g d
    | DivE  (g,d,t)         -> taille_expr_nb2 g d
    | Egale (g,d,t)         -> taille_expr_nb2 g d
    | Negal (g,d,t)         -> taille_expr_nb2 g d
    | Supp  (g,d,t)         -> taille_expr_nb2 g d
    | Et    (g,d,t)         -> taille_expr_nb2 g d
    | Ou    (g,d,t)         -> taille_expr_nb2_ou g d
    | Tern  (g,c,d,t)       -> taille_expr_nb3 g c d
    | Neg   (e ,t)          -> 1 + (taille_expr_nb1 e)
    | Non   (e,t)           -> 1 + (taille_expr_nb1 e)
    | To    (e,t)           -> 1 + (taille_expr_nb1 e)
    | Num   (n,t)           -> t
    | Ident (i,t)           -> t
    | Bool  (b,t)           -> t
    | Strin (s,t)           -> t
    | Aff   (i,e,t)         -> 1 + (taille_expr_nb1 e)
    | CallA (i,es,t)        -> 4 + (taille_exps es)
    | CallV (i,t)           -> t
and taille_com_nb4 e1 e2 e3 c = 2 + taille_affect_expr e1 + taille_expr_nb1 e2 + taille_expr_nb1 e3 + taille_com_nb1 c
and taille_com_nb3 e c1 c2 = 2 + taille_expr_nb1 e + taille_com_nb1 c1 + taille_com_nb1 c2
and taille_com_nb2 e c = 2 +  taille_expr_nb1 e + taille_com_nb1 c
and taille_com_nb1 c = match c with
    | Nil   t               -> t
    | Com   (ex,t)          -> taille_affect_expr ex
    | Cprog (p,t)           -> taille_prog_nb1 p
    | SiSin (e,c1,c2,t)     -> taille_com_nb3 e c1 c2
    | Tq    (e,c,t)         -> taille_com_nb2 e c
    | Faire (c,e,t)         -> taille_com_nb2 e c
    | Pour  (e1,e2,e3,c,t)  -> taille_com_nb4 e1 e2 e3 c
    | Ecrir (e,t)           -> 1 + taille_expr_nb1 e
    | FuncA (i,a,p,t)       -> taille_prog_nb1 p
    | FuncV (i,p,t)         -> taille_prog_nb1 p
    | Retur (e,t)           -> 1 + (taille_expr_nb1 e)
and taille_affect_expr ae = match ae with
    | AEexp (e,t)           -> taille_expr_nb1 e
    | VarA  (i,e,t)         -> 2 + (taille_expr_nb1 e)
    | Var   (i,t)           -> t
and taille_exps es = match es with
    | Eexp  (e,t)           -> taille_expr_nb1 e
    | Exps  (e,es,t)        -> (taille_expr_nb1 e) + (taille_exps es)
and taille_prog_nb2 c p = (taille_com_nb1 c) + (taille_prog_nb1 p)
and taille_prog_nb1 p = match p with
    | Nill t               -> t
    | Last (l,t)           -> taille_com_nb1 l
    | Prog (c,p,t)         -> taille_prog_nb2 c p
;;


let rec print_preCommande commande tailleGlobal = match commande with
    | Nil   t               -> ""
    | Com   (e,t)           -> ""
    | Cprog (p,t)           -> print_preProg p tailleGlobal
    | SiSin (e,c1,c2,t)     -> print_preCommande c1 tailleGlobal^ print_preCommande c2 tailleGlobal
    | Tq    (e,c,t)         -> print_preCommande c tailleGlobal
    | Faire (c,e,t)         -> print_preCommande c tailleGlobal
    | Pour  (ae,e2,e3,c,t)  -> print_preCommande c tailleGlobal
    | Ecrir (e,t)           -> ""
    | FuncA (i,a,p,t)       -> "Lambda " ^string_of_int (tailleGlobal-t+(nbArguments a)*2+1) ^ print_preArgs a tailleGlobal ^ "\nSetVar " ^ i ^ "\n"
    | FuncV (i,p,t)         -> "Lambda " ^string_of_int (tailleGlobal-t) ^"\nSetVar " ^ i ^ "\n"
    | Retur (e,t)           -> ""
and print_preArgs arguments tailleGlobal = match arguments with
    | Id    (i,t)           -> "\nDclArg " ^ i
    | Args  (i,a,t)         -> "\nDclArg " ^ i ^ print_preArgs a tailleGlobal
and print_preProg programme tailleGlobal = match programme with
    | Nill  t               -> ""
    | Last  (l,t)           -> print_preCommande l tailleGlobal
    | Prog  (c,p,t)         -> (print_preCommande c tailleGlobal)^(print_preProg p tailleGlobal)
and nbArguments a = match a with
    | Id    (i,t)           -> 1
    | Args  (i,a,t)         -> 1 + nbArguments a
;;


let getTailleGlobal p = match p with
    | Nill  t               -> t
    | Last  (l,t)           -> t
    | Prog  (c,p,t)         -> t
;;