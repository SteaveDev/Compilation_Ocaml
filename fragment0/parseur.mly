
%{
open AST
%}

%token <float> NOMBRE
%token <string> IDENT
%token PLUS MOINS FOIS DIV DIVE GPAREN DPAREN EGALE INTERRO POINT POINTVIR AFFECT EOL

%right AFFECT
%right INTERRO POINT
%left EGALE
%left PLUS MOINS
%left FOIS DIV DIVE
%nonassoc UMOINS

%type <AST.programme_a> programme
%type <AST.commande_a> commande
%type <AST.expression_a>  expression

%start programme

%%
programme:
    EOL                                             { Nill (0) }
 |  commande                                        { Last ($1,(AST.taille_com $1))}
 |  commande programme                              { Prog ($1,$2,(AST.taille_programme_arg_nb2 $1 $2)) }
 ;
commande:
   POINTVIR                                         { Nil (0) }
 | expression POINTVIR                              { Com ($1,(AST.taille_expression_arg_nb1 $1)) }
 | IDENT AFFECT expression POINTVIR                 { Aff ($1,$3,(1+(AST.taille_expression_arg_nb1 $3))) }
 ;
expression:
   expression PLUS expression                       { Plus  ($1,$3,(AST.taille_expression_arg_nb2 $1 $3)) }
 | expression MOINS expression                      { Moins ($1,$3,(AST.taille_expression_arg_nb2 $1 $3)) }
 | expression FOIS expression                       { Mult  ($1,$3,(AST.taille_expression_arg_nb2 $1 $3)) }
 | expression DIV expression                        { Div   ($1,$3,(AST.taille_expression_arg_nb2 $1 $3)) }
 | expression DIVE expression                       { DivE  ($1,$3,(AST.taille_expression_arg_nb2 $1 $3)) }
 | expression EGALE expression                      { Egale ($1,$3,(AST.taille_expression_arg_nb2 $1 $3)) }
 | expression INTERRO expression POINT expression   { Tern  ($1,$3,$5, (AST.taille_expression_arg_nb3 $1 $3 $5)) }
 | GPAREN expression DPAREN                         { $2 }
 | MOINS expression %prec UMOINS                    { Neg ($2,(1+(AST.taille_expression_arg_nb1 $2))) }
 | NOMBRE                                           { Num ($1,1) }
 | IDENT                                            { Ident ($1,1) }
 ;
