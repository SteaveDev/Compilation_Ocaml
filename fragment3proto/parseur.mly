
%{
open AST
%}

%token <float> NOMBRE
%token <bool> BOOLEEN
%token <string> IDENT STRING
%token PLUS MOINS FOIS DIV DIVE
%token GPAREN DPAREN GACCOL DACCOL
%token EGALE NEGALE SUPP TYPEOF COMPARE
%token INTERRO DPOINT SI SINON
%token AFFECT
%token TQ FAIRE POUR
%token FONCT RETURN
%token EOL POINTVIR VIRGULE
%token NON ET OU
%token ECRIRE VAR
%token COMMENT
%token TRY CATCH THROW
%token SWITCH CAS
%token POINT NUL
%token LAMBDA

%left VIRGULE
%right AFFECT LAMBDA
%right INTERRO DPOINT
%left OU
%left ET
%left EGALE NEGALE COMPARE
%left SUPP
%left PLUS MOINS
%left FOIS DIV DIVE
%left POINT
%nonassoc UMOINS

%type <AST.programme_a> programme
%type <AST.commande_a> commande
%type <AST.affect_expr_a> affect_expr
%type <ASt.cont_objet_a> cont_objet
%type <AST.expression_a> expression
%type <AST.cas_switch_a> cas_switch
%type <AST.expressions_a> expressions
%type <AST.arguments_a> arguments
%start programme

%%
programme:
    EOL                                                                                         { Nill (0) }
 |  COMMENT  programme                                                                          { $2 }
 |  commande                                                                                    { Last ($1,(AST.taille_com $1))}
 |  commande programme                                                                          { Prog ($1,$2,(AST.taille_prog2 $1 $2)) }
;
commande:
   POINTVIR                                                                                     { Nil (0) }
 | GACCOL programme DACCOL                                                                      { Cprog ($2,(AST.taille_prog $2)) }
 | affect_expr POINTVIR                                                                         { Com ($1,(AST.taille_affect_expr $1)) }
 | SI GPAREN expression DPAREN commande SINON commande                                          { SiSin ($3,$5,$7,(AST.taille_com3 $3 $5 $7)) }
 | TQ GPAREN expression DPAREN commande                                                         { Tq ($3,$5,(AST.taille_com2 $3 $5)) }
 | FAIRE commande TQ GPAREN expression DPAREN                                                   { Faire ($2,$5,(AST.taille_com2 $5 $2)) }
 | POUR GPAREN affect_expr POINTVIR expression POINTVIR expression DPAREN commande              { Pour ($3,$5,$7,$9,(AST.taille_com4 $3 $5 $7 $9)) }
 | ECRIRE GPAREN expression DPAREN POINTVIR                                                     { Ecrir ($3,1+(AST.taille_exp $3)) }

 | FONCT IDENT GPAREN arguments DPAREN GACCOL programme DACCOL                                  { FuncA ($2,$4,$7,AST.taille_prog $7) }
 | FONCT IDENT GPAREN DPAREN GACCOL programme DACCOL                                            { FuncV ($2,$6,AST.taille_prog $6) }
 | RETURN GPAREN expression DPAREN POINTVIR                                                     { Retur ($3,1 + (AST.taille_exp $3)) }

 | TRY commande CATCH GPAREN IDENT DPAREN commande                                              { Try ($2,$5,$7,4 + (AST.taille_com $2) + (AST.taille_com $7) ) }
 | THROW expression POINTVIR                                                                    { Throw ($2,1 + (AST.taille_exp $2)) }

 | SWITCH GPAREN expression DPAREN GACCOL cas_switch DACCOL                                     { Swt ($3,$6,1)}
 | SWITCH GPAREN expression DPAREN GACCOL DACCOL                                                { SwtV ($3,1)}
 ;
affect_expr:
   expression                                       { AEexp ($1,(AST.taille_exp $1)) }
 | VAR IDENT AFFECT expression                      { VarA ($2,$4,2+(AST.taille_exp $4))}
 | VAR IDENT                                        { Var ($2,1)}
 ;
cont_objet:
   IDENT DPOINT expressions                         { Obj ($1,$3,1)}
 | IDENT DPOINT expressions VIRGULE cont_objet      { Objs ($1,$3,$5,1) }
 ;
expression:
   expression PLUS expression                       { Plus ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression MOINS expression                      { Moins ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression FOIS expression                       { Mult ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression DIV expression                        { Div ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression DIVE expression                       { DivE ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression EGALE expression                      { Egale ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression SUPP expression                       { Supp ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression NEGALE expression                     { Negal ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression ET expression                         { Et ($1,$3,(AST.taille_exp2 $1 $3)) }
 | expression OU expression                         { Ou ($1,$3,(AST.taille_exp2_ou $1 $3)) }

 | IDENT AFFECT expression                          { Aff ($1,$3,1+(AST.taille_exp $3)) }
 | IDENT GPAREN expressions DPAREN                  { CallA ($1,$3,4+(AST.taille_exps $3)) }
 | IDENT GPAREN DPAREN                              { CallV ($1,3)}

 | MOINS expression %prec UMOINS                    { Neg ($2,(1+(AST.taille_exp $2))) }
 | NON expression                                   { Non ($2,(1+(AST.taille_exp $2))) }
 | TYPEOF expression                                { To ($2,(1+(AST.taille_exp $2))) }

 | expression POINT IDENT                           { CallO ($1,$3,1) }
 | GACCOL cont_objet DACCOL                         { EObj ($2,1) }
 | NUL                                              { Nul (1) }
 | GPAREN arguments DPAREN LAMBDA expression       { Lmbd ($2,$5,1) }

 | expression INTERRO expression DPOINT expression  { Tern  ($1,$3,$5, (AST.taille_exp3 $1 $3 $5)) }
 | GPAREN expression DPAREN                         { $2 }
 | NOMBRE                                           { Num ($1,1) }
 | IDENT                                            { Ident ($1,1) }
 | BOOLEEN                                          { Bool ($1,1) }
 | STRING                                           { Strin ($1,1) }
 ;
cas_switch:
   CAS expression DPOINT programme cas_switch       { Cswt ($2,$4,$5,1) }
;
expressions:
   expression VIRGULE expressions                   { Exps ($1,$3,((AST.taille_exp $1)+(AST.taille_exps $3))) }
 | expression                                       { Eexp ($1,(AST.taille_exp $1)) }
 ;
arguments:
    IDENT                                           { Id ($1,0) }
  | IDENT VIRGULE arguments                         { Args ($1,$3,0) }
 ;





