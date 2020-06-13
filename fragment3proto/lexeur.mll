

{
  open Parseur     
  exception Eof
  exception TokenInconu
}
rule token = parse
            [' ' '\t']           { token lexbuf } 
          | ['\n' ]              { EOL }
          | ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]*           as lexem { IDENT(lexem) }
          | ['0'-'9']+ ('.' ['0'-'9']*)?                        as lexem { NOMBRE(float_of_string lexem) }
          | '"' ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* '"'   as lexem { STRING(String.sub lexem 1 ((String.length lexem )-2)) }
          | "/*" ['a'-'z' 'A'-'Z' '0'-'9' '_' ' ']* "*/"           { COMMENT }
          | "Vrai"               { BOOLEEN(true) }
          | "Faux"               { BOOLEEN(false) }
          | '+'                  { PLUS }
          | '-'                  { MOINS }
          | '*'                  { FOIS }
          | '/'                  { DIV }
          | '%'                  { DIVE }
          | "==="                { EGALE }
          | "!=="                { NEGALE }
          | "&&"                 { ET }
          | "||"                 { OU }
          | '>'                  { SUPP }
          | "Typeof"             { TYPEOF }
          | '!'                  { NON }
          | '?'                  { INTERRO }
          | ':'                  { DPOINT }
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
          | "Switch"             { SWITCH }
          | "Cas"                { CAS }
          | '.'                  { POINT }
          | "Nul"                { NUL }
          | "=>"                 { LAMBDA }
          | eof                  { raise Eof }
	      | _                    { raise TokenInconu }

