
%{
open AST
%}

%token <float> NOMBRE
%token <bool> BOOLEEN
%token <string> IDENT
%token PLUS MOINS FOIS DIV DIVE
%token GPAREN DPAREN GACCOL DACCOL
%token EGALE NEGALE SUPP TYPEOF
%token INTERRO POINT SI SINON
%token AFFECT
%token TQ FAIRE POUR
%token EOL POINTVIR
%token NON ET OU
%token ECRIRE
%token COMMENT

%right AFFECT
%right INTERRO POINT
%left OU
%left ET
%left EGALE NEGALE
%left SUPP
%left PLUS MOINS
%left FOIS DIV DIVE
%nonassoc UMOINS NON TYPEOF

%type <AST.programme_a> programme
%type <AST.commande_a> commande
%type <AST.expression_a> expression

%start programme

%%
programme:
      EOL                                                                              { Nill (0) }
    | COMMENT programme                                                                { $2 }
    | commande                                                                         { Last ($1,(AST.taille_com_nb1 $1))}
    | commande programme                                                               { Prog ($1,$2,(AST.taille_prog_nb2 $1 $2)) }
;
commande:
      POINTVIR                                                                         { Nil (0) }
    | GACCOL programme DACCOL                                                          { Cprog ($2,(AST.taille_prog_nb1 $2)) }
    | expression POINTVIR                                                              { Com ($1,(AST.taille_expr_nb1 $1)) }
    | SI GPAREN expression DPAREN commande SINON commande                              { SiSin ($3,$5,$7,(AST.taille_com_nb3 $3 $5 $7)) }
    | TQ GPAREN expression DPAREN commande                                             { Tq ($3,$5,(AST.taille_com_nb2 $3 $5)) }
    | FAIRE commande TQ GPAREN expression DPAREN                                       { Faire ($2,$5,(AST.taille_com_nb2 $5 $2)) }
    | POUR GPAREN expression POINTVIR expression POINTVIR expression DPAREN commande   { Pour ($3,$5,$7,$9,(AST.taille_com_nb4 $3 $5 $7 $9)) }
    | ECRIRE GPAREN expression DPAREN POINTVIR                                         { Ecrir ($3,1+(AST.taille_expr_nb1 $3)) }
 ;
expression:
      expression PLUS expression                                                       { Plus ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression MOINS expression                                                      { Moins ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression FOIS expression                                                       { Mult ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression DIV expression                                                        { Div ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression DIVE expression                                                       { DivE ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression EGALE expression                                                      { Egale ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression SUPP expression                                                       { Supp ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression NEGALE expression                                                     { Negal ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression ET expression                                                         { Et ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression OU expression                                                         { Ou ($1,$3,(AST.taille_expr_nb2_ou $1 $3)) }

    | IDENT AFFECT expression                                                          { Aff ($1,$3,1+(AST.taille_expr_nb1 $3)) }

    | MOINS expression %prec UMOINS                                                    { Neg ($2,(1+(AST.taille_expr_nb1 $2))) }
    | NON expression                                                                   { Non ($2,(1+(AST.taille_expr_nb1 $2))) }
    | TYPEOF expression                                                                { To ($2,(1+(AST.taille_expr_nb1 $2))) }

    | expression INTERRO expression POINT expression                                   { Tern  ($1,$3,$5, (AST.taille_expr_nb3 $1 $3 $5)) }
    | GPAREN expression DPAREN                                                         { $2 }
    | NOMBRE                                                                           { Num ($1,1) }
    | IDENT                                                                            { Ident ($1,1) }
    | BOOLEEN                                                                          { Bool ($1,1) }
 ;
