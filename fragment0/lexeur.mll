{
  open Parseur     
  exception Eof
  exception TokenInconu
}

rule token = parse
    [' ' '\t']           { token lexbuf }
  | ['\n' ]              { EOL }
  | ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*    as lexem { IDENT(lexem) }
  | ['0'-'9']* ('.' ['0'-'9']+)?                    as lexem { NOMBRE(float_of_string lexem) }
  | '+'                  { PLUS }
  | '-'                  { MOINS }
  | '*'                  { FOIS }
  | '/'                  { DIV }
  | '%'                  { DIVE }
  | '?'                  { INTERRO }
  | ':'                  { POINT }
  | "==="                { EGALE }
  | "="                  { AFFECT }
  | '('                  { GPAREN }
  | ')'                  { DPAREN }
  | ';'                  { POINTVIR }
  | eof                  { raise Eof }
  | _                    { raise TokenInconu }

