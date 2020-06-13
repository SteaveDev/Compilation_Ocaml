type expression_a =
	| Plus  	of expression_a * expression_a * int
	| Moins 	of expression_a * expression_a * int
	| Mult  	of expression_a * expression_a * int
	| Div   	of expression_a * expression_a * int
	| DivE  	of expression_a * expression_a * int
	| Egale 	of expression_a * expression_a * int
	| Tern  	of expression_a * expression_a * expression_a * int
	| Neg   	of expression_a * int
	| Num   	of float * int
	| Ident 	of string * int
;;



type commande_a =
	| Nil 		of int
	| Com 		of expression_a * int
	| Aff 		of string * expression_a * int
;;



type programme_a =
	| Nill 		of int
	| Last 		of commande_a * int
	| Prog 		of commande_a * programme_a * int
;;



let getTaille x = match x with
	| Plus  	(g,d,t)		-> t
	| Moins 	(g,d,t)		-> t
	| Mult  	(g,d,t)		-> t
	| Div   	(g,d,t)		-> t
	| DivE  	(g,d,t)		-> t
	| Egale 	(g,d,t)		-> t
	| Tern  	(g,c,d,t)	-> t
	| Neg    	(e,t) 		-> t
	| Num    	(n, t)		-> t
	| Ident  	(i,t) 		-> t
;;



let rec print_binaire form s g d t = Format.fprintf form "@[<2>%i%s%s@ %a%s@ %a%s@]" t s "(" print_AST_expression g " ," print_AST_expression d " )"
and print_AST_expression form = let open Format in function
    | Plus  (g,d,t)     	-> print_binaire form "Plus" 	g d t
    | Moins (g,d,t)     	-> print_binaire form "Moins" 	g d t
    | Mult  (g,d,t)     	-> print_binaire form "Mult" 	g d t
    | Div   (g,d,t)     	-> print_binaire form "Div" 	g d t
    | DivE  (g,d,t)     	-> print_binaire form "DivE" 	g d t
    | Egale (g,d,t)     	-> print_binaire form "Egale" 	g d t
    | Tern  (g,c,d,t)   	-> fprintf form "@[<2>%i%s@ %a%s@ %a%s@ %a%s@]" 	t "Tern(" print_AST_expression g " ," print_AST_expression c " ," print_AST_expression d ")"
    | Neg   (e,t)       	-> fprintf form "@[<2>%i%s@ %a@]" 					t "Neg" print_AST_expression e
    | Num   (n, t)      	-> fprintf form "@[<2>%i%s@ %g@]" 					t "Num" n
    | Ident (i,t)       	-> fprintf form "@[<2>%i%s@ %s@]" 					t "Ident" i
and print_AST_commande form = let open Format in function
    | Nil  t            	-> ()
    | Com  (e,t)        	-> fprintf form "@[<2>%i%s@ %a%s@]" 				t "Com(" print_AST_expression e ")"
    | Aff  (i,e,t)      	-> fprintf form "@[<2>%i%s@ %s%s@ %a%s@]" 			t "Aff (" i " ," print_AST_expression e ")"
and print_AST_programme form = let open Format in function
     | Nill t           	-> ()
     | Last (l,t)       	-> fprintf form "@[<2>%i%s@ %a%s@]" 				t "Last(" print_AST_commande l ")"
     | Prog (c,p,t)     	-> fprintf form "@[<2>%i%s@ %a%s@ %a%s@]" 			t "Prog(" print_AST_commande c " ," print_AST_programme p ")"
;;



let rec print_expression_arg_nb3 g c d = (print_expression_arg_nb1 g)^
                                        "\nConJmp "^string_of_int ((getTaille c)+1)^
                                        "\n"^(print_expression_arg_nb1 c)^
                                        "\nJump "^string_of_int (getTaille d)^
                                        "\n"^(print_expression_arg_nb1 d)

and print_expression_arg_nb2 s g d = (print_expression_arg_nb1 g)^"\n"^(print_expression_arg_nb1 d)^"\n"^s 
and print_expression_arg_nb1  = function
    | Plus  (g,d,t) 		-> print_expression_arg_nb2 "AddiRe" g d
    | Moins (g,d,t) 		-> print_expression_arg_nb2 "SubsRe" g d
    | Mult  (g,d,t) 		-> print_expression_arg_nb2 "MultRe" g d
    | Div   (g,d,t) 		-> print_expression_arg_nb2 "DiviRe" g d
    | DivE  (g,d,t) 		-> print_expression_arg_nb2 "Modulo" g d
    | Egale (g,d,t)			-> print_expression_arg_nb2 "Equal" g d
    | Tern  (g,c,d,t)		-> print_expression_arg_nb3 g c d
    | Neg    (e,t)			-> (print_expression_arg_nb1 e)^"\nNegaRe"
    | Num    (n, t)			-> "CstRe "^(string_of_float n)
    | Ident  (i,t)   		-> "GetVar "^ i
and print_com = function
    | Nil t 				-> ""
    | Com (ex,t) 			-> print_expression_arg_nb1 ex
    | Aff (i,e,t) 			-> (print_expression_arg_nb1 e)^"\nSetVar "^i
and print_prog = function
    | Nill t        		-> ""
    | Last (l,t)    		-> print_com l
    | Prog (c,p,t)  		-> (print_com c)^"\n"^(print_prog p)
;;



let rec taille_expression_arg_nb3 x y z = 2 + (taille_expression_arg_nb1 x) + (taille_expression_arg_nb1 y) + (taille_expression_arg_nb1 z)
and taille_expression_arg_nb2 x y = 1 + (taille_expression_arg_nb1 x) + (taille_expression_arg_nb1 y)
and taille_expression_arg_nb1 w = match w with
         | Plus  (g,d,t) 	-> taille_expression_arg_nb2 g d
         | Moins (g,d,t) 	-> taille_expression_arg_nb2 g d
         | Mult  (g,d,t) 	-> taille_expression_arg_nb2 g d
         | Div   (g,d,t) 	-> taille_expression_arg_nb2 g d
         | DivE  (g,d,t) 	-> taille_expression_arg_nb2 g d
         | Egale (g,d,t) 	-> taille_expression_arg_nb2 g d
         | Tern  (g,c,d,t) 	-> taille_expression_arg_nb3 g c d
         | Neg   (e ,t) 	-> 1 + (taille_expression_arg_nb1 e)
         | Num    (n, t) 	-> t
         | Ident  (i,t) 	-> t
and taille_com c = match c with
         | Nil t 			-> t
         | Com (ex,t) 		-> taille_expression_arg_nb1 ex
         | Aff (i,e,t) 		-> 1 + taille_expression_arg_nb1 e
and taille_programme_arg_nb2 c p = (taille_com c) + (taille_programme_arg_nb1 p)
and taille_programme_arg_nb1 p = match p with
         | Nill t 			-> t
         | Last (l,t) 		-> taille_com l
         | Prog (c,p,t) 	-> taille_programme_arg_nb2 c p
;;