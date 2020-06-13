{
     open Parseur     
     exception Eof
     exception TokenInconu
}
rule token = parse
       [' ' '\t'] | ("/*" ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* "*/")  { token lexbuf } 
     | ['\n' ]              { EOL }
     | ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]*           as lexem { IDENT(lexem) }
     | ['0'-'9']+ ('.' ['0'-'9']*)?                        as lexem { NOMBRE(float_of_string lexem) }
     | '"' ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* '"'   as lexem { STRING(String.sub lexem 1 ((String.length lexem )-2)) }
     | "Vrai"               { BOOLEEN(true) }
     | "Faux"               { BOOLEEN(false) }
     | '+'                  { PLUS }
     | '-'                  { MOINS }
     | '*'                  { FOIS }
     | '/'                  { DIVI }
     | '%'                  { MODUL }
     | "==="                { T_EGALE }
     | "!=="                { NEGALE }
     | "&&"                 { ET }
     | "||"                 { OU }
     | '>'                  { SUPP }
     | "Typeof"             { TYPEOF }
     | '!'                  { NON }
     | '?'                  { INTERRO }
     | ':'                  { POINT }
     | "Si"                 { SI }
     | "Sinon"              { SINON }
     | "TantQue"            { TQ }
     | "Faire"              { FAIRE }
     | "Pour"               { POUR }
     | "Ecrire"             { ECRIRE }
     | '='                  { AFFECT }
     | '{'                  { GACCOL }
     | '}'                  { DACCOL }
     | '('                  { GPAREN }
     | ')'                  { DPAREN }
     | ';'                  { POINTVIR }
     | "Fonction"           { FONCT }
     | "Retourner"          { RETURN }
     | "Var"                { VAR }
     | ','                  { VIRGULE }
     | "Essayer"            { TRY }
     | "Rattraper"          { CATCH }
     | "Lancer"             { THROW }
     | eof                  { raise Eof }
     | _                    { raise TokenInconu }

