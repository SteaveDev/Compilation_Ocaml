{
  open Parseur     
  exception Eof
  exception TokenInconu
}

rule token = parse
	  [' ' '\t'] | ("/*" ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* "*/")  { token lexbuf } 
	| ['\n' ]              { EOL }
	| ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]*           as lexem { IDENT(lexem) }
	| ['0'-'9']* ('.' ['0'-'9']+)?                        as lexem { NOMBRE(float_of_string lexem) }
	| "vrai"               { BOOLEEN(true) }
	| "faux"               { BOOLEEN(false) }
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
	| eof                  { raise Eof }
	| _                    { raise TokenInconu }