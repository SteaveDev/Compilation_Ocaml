%{
    open AST
%}

%token <float> NOMBRE
%token <bool> BOOLEEN
%token <string> IDENT STRING
%token PLUS MOINS FOIS DIVI MODUL
%token GPAREN DPAREN GACCOL DACCOL
%token T_EGALE NEGALE SUPP TYPEOF COMPARE
%token INTERRO POINT SI SINON
%token AFFECT
%token TQ FAIRE POUR
%token FONCT RETURN
%token EOL POINTVIR VIRGULE
%token NON ET OU
%token ECRIRE VAR
%token TRY CATCH THROW

%left VIRGULE
%right AFFECT
%right INTERRO POINT
%left OU
%left ET
%left T_EGALE NEGALE COMPARE
%left SUPP
%left PLUS MOINS
%left FOIS DIVI MODUL
%nonassoc UMOINS TYPEOF NON

%type <AST.programme_a> programme
%type <AST.commande_a> commande
%type <AST.affect_expr_a> affect_expr
%type <AST.expression_a> expression
%type <AST.expressions_a> expressions
%type <AST.arguments_a> arguments
%start programme

%%
programme:
      EOL                                                                              { Nill (0) }
    | commande                                                                         { Last ($1,(AST.taille_com_nb1 $1))}
    | commande programme                                                               { Prog ($1,$2,(AST.taille_prog_nb2 $1 $2)) }
;
commande:
      POINTVIR                                                                         { Nil (0) }
    | GACCOL programme DACCOL                                                          { Cprog ($2,(AST.taille_prog_nb1 $2)) }
    | affect_expr POINTVIR                                                             { Com ($1,(AST.taille_affect_expr $1)) }
    | SI GPAREN expression DPAREN commande SINON commande                              { SiSin ($3,$5,$7,(AST.taille_com_nb3 $3 $5 $7)) }
    | TQ GPAREN expression DPAREN commande                                             { Tq ($3,$5,(AST.taille_com_nb2 $3 $5)) }
    | FAIRE commande TQ GPAREN expression DPAREN                                       { Faire ($2,$5,(AST.taille_com_nb2 $5 $2)) }
    | POUR GPAREN affect_expr POINTVIR expression POINTVIR expression DPAREN commande  { Pour ($3,$5,$7,$9,(AST.taille_com_nb4 $3 $5 $7 $9)) }
    | ECRIRE GPAREN expression DPAREN POINTVIR                                         { Ecrir ($3,1+(AST.taille_expr_nb1 $3)) }

    | FONCT IDENT GPAREN arguments DPAREN GACCOL programme DACCOL                      { FuncA ($2,$4,$7,AST.taille_prog_nb1 $7) }
    | FONCT IDENT GPAREN DPAREN GACCOL programme DACCOL                                { FuncV ($2,$6,AST.taille_prog_nb1 $6) }
    | RETURN GPAREN expression DPAREN POINTVIR                                         { Retur ($3,1 + (AST.taille_expr_nb1 $3)) }

    | TRY commande CATCH GPAREN IDENT DPAREN commande                                  { Try ($2,$5,$7,4 + (AST.taille_com_nb1 $2) + (AST.taille_com_nb1 $7) ) }
    | THROW expression POINTVIR                                                        { Throw ($2,1 + (AST.taille_expr_nb1 $2)) }
 ;
affect_expr:
      expression                                                                       { AEexp ($1,(AST.taille_expr_nb1 $1)) }
    | VAR IDENT AFFECT expression                                                      { VarA ($2,$4,2+(AST.taille_expr_nb1 $4))}
    | VAR IDENT                                                                        { Var ($2,1)}
 ;
expression:
      expression PLUS expression                                                       { Plus ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression MOINS expression                                                      { Moins ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression FOIS expression                                                       { Mult ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression DIVI expression                                                       { Divi ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression MODUL expression                                                      { Modul ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression T_EGALE expression                                                    { T_Egale ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression SUPP expression                                                       { Supp ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression NEGALE expression                                                     { Negal ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression ET expression                                                         { Et ($1,$3,(AST.taille_expr_nb2 $1 $3)) }
    | expression OU expression                                                         { Ou ($1,$3,(AST.taille_expr_nb2_ou $1 $3)) }

    | IDENT AFFECT expression                                                          { Aff ($1,$3,1+(AST.taille_expr_nb1 $3)) }
    | IDENT GPAREN expressions DPAREN                                                  { CallA ($1,$3,4+(AST.taille_exps $3)) }
    | IDENT GPAREN DPAREN                                                              { CallV ($1,3)}

    | MOINS expression %prec UMOINS                                                    { Neg ($2,(1+(AST.taille_expr_nb1 $2))) }
    | NON expression                                                                   { Non ($2,(1+(AST.taille_expr_nb1 $2))) }
    | TYPEOF expression                                                                { To ($2,(1+(AST.taille_expr_nb1 $2))) }

    | expression INTERRO expression POINT expression                                   { Tern  ($1,$3,$5, (AST.taille_expr_nb3 $1 $3 $5)) }
    | GPAREN expression DPAREN                                                         { $2 }
    | NOMBRE                                                                           { Num ($1,1) }
    | IDENT                                                                            { Ident ($1,1) }
    | BOOLEEN                                                                          { Bool ($1,1) }
    | STRING                                                                           { Strin ($1,1) }
 ;
expressions:
      expression VIRGULE expressions                                                   { Exps ($1,$3,((AST.taille_expr_nb1 $1)+(AST.taille_exps $3))) }
    | expression                                                                       { Eexp ($1,(AST.taille_expr_nb1 $1)) }
 ;
arguments:
      IDENT                                                                            { Id ($1,0) }
    | IDENT VIRGULE arguments                                                          { Args ($1,$3,0) }
 ;





