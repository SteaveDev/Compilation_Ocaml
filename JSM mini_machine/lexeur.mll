
{
  open Parseur     
  exception Eof
  exception TokenInconu of string
  exception CharInconu of char
}
rule token = parse
          | [' ' '\t']           {token lexbuf } 
          | ('#'[' '-'\255' '\t']*)?'\n'([' ' '\t']*('#'[' '-'\255' '\t']*)?'\n')*              { EOL }		  
          | '-'?['0'-'9' ]+                    as lexem {NOMBRE(int_of_string lexem) }		  
          | '-'?['0'-'9' ]*'.'['0'-'9']*       as lexem {DOUBLE(float_of_string lexem) }
          | ['T' 't']"rue"                              {BOOL(true)}
          | ['F' 'f']"alse"                             {BOOL(false)}
          | "Ad"['a'-'z']*"R"['a'-'z']*                 {AddiRe}
          | "Sub"['a'-'z']*'R'['a'-'z']*                {SubsRe} 
          | 'M'['a'-'z']*'R'['a'-'z']*                  {MultRe}
          | 'D'['a'-'z']*'R'['a'-'z']*                  {DiviRe} 
          | "Neg"['a'-'z']*'R'['a'-'z']*                {NegaRe} 
          | "Mod"['a'-'z']*                             {Modulo} 
          | "Not"['a'-'z']*                             {Not}   
          | 'B'['a'-'z']*'T'['a'-'z']*'R'['a'-'z']*     {BoToRe} 
          | 'R'['a'-'z']*'T'['a'-'z']*'B'['a'-'z']*     {ReToBo} 
          | "Equal"['a'-'z']*                           {Equal}
          | 'N'['a'-'z']*'E'['a'-'z']*                  {NotEq} 
          | 'L'['a'-'z']*'E'['a'-'z']*'R'['a'-'z']*     {LowEqR}  
          | 'G'['a'-'z']*'E'['a'-'z']*'R'['a'-'z']*     {GreEqR} 
          | 'L'['a'-'z']*'S'['a'-'z']*'R'['a'-'z']*     {LowStR} 
          | 'G'['a'-'z']*'S'['a'-'z']*'R'['a'-'z']*     {GreStR}
          | "Jump"['a'-'z']*                            {Jump}    
          | 'C'['a'-'z']*'J'['a'-'z']*                  {ConJmp}  
          | "Case"['a'-'z']*                            {Case}
          | "Cst"'R'?['a'-'z']*                         {CstRe}   
          | "CstB"['a'-'z']*                            {CstBo}  
          | "CstU"['a'-'z']*                            {CstUn}
          | "Copy"['a'-'z']*                            {Copy}  
          | "Swap"['a'-'z']*                            {Swap} 
          | "Drop"['a'-'z']*                            {Drop}
          | "Noop"['a'-'z']*                            {Noop}
          | "Type""O"?['a'-'z']*                        {TypeOf}
          | "Halt"['a'-'z']*                            {Halt} 
          | "Error"['a'-'z']*                           {Error}  
          | "Assert"['a'-'z']*                          {Assert} 
          | "Print"['a'-'z']*                           {Print}  
          | "SetV"['a'-'z']*                            {SetVar} 
          | "GetV"['a'-'z']*                            {GetVar} 
          | "SetA"['a'-'z']*                            {SetArg}  
          | 'S'['a'-'z']"Call"['a'-'z']*           {StCall}    
          | "Call"['a'-'z']*                            {Call}  
          | "Ret"['a'-'z']*                             {Return}  
          | 'D'['a'-'z']*'V'['a'-'z']*                  {DclVar} 
          | "Lamb"['a'-'z']*                            {Lambda}  
          | 'D'['a'-'z']*'A'['a'-'z']*                  {DclArg} 
          | "Throw"['a'-'z']*                           {Throw}
          | "Cont"['a'-'z']*                            {Continue}
          | "Conc"['a'-'z']*                            {Concat}
          | "CstS"['a'-'z']*                            {CstStr} 
          | 'L'['a'-'z']*'E'['a'-'z']*'S'['a'-'z']*     {LowEqS}  
          | 'G'['a'-'z']*'E'['a'-'z']*'S'['a'-'z']*     {GreEqS} 
          | 'L'['a'-'z']*'S'['a'-'z']*'S'['a'-'z']*     {LowStS} 
          | 'G'['a'-'z']*'S'['a'-'z']*'S'['a'-'z']*     {GreStS}
          | 'S'['a'-'z']*'T'['a'-'z']*'R'['a'-'z']*     {StToRe}
          | 'R'['a'-'z']*'T'['a'-'z']*'S'['a'-'z']*     {ReToSt}
          | 'I'['a'-'z']*'E'['a'-'z']*                  {IsEmpt}
          | "Point"['a'-'z']*                           {Point}  
          | "Null"['a'-'z']*                            {Null}
          | "NewO"['a'-'z']*                            {NewObj}
          | "SetO"['a'-'z']*                            {SetObj} 
          | "Prot"['a'-'z']*                            {Protot}
          | "GetP"['a'-'z']*                            {GetPrt}            
          | 'T'['a'-'z']*"Call"['a'-'z']*               {TlCall}
          | "Ret"['a'-'z']*'O'['a'-'z']*                {RetOpt} 
          | "Ret"['a'-'z']*'Z'['a'-'z']*                {RetZer} 
          | "Ret"['a'-'z']*'Z'['a'-'z']*'O'['a'-'z']*   {RetZOp} 
          | "SetV"['a'-'z']*'2'['a'-'z']*               {SetVD}
          | "SetV"['a'-'z']*'S'['a'-'z']*               {SetVS}
          | "GetV"['a'-'z']*'S'['a'-'z']*               {GetVS}
          | "Catch"['a'-'z']*                           {Catch}
          | "Final"['a'-'z']*                           {Finally}
          | eof                                         { EOF }
          | ['a'-'z']['A'-'z''0'-'z''-''_''0'-'9''\'']*  as lexem { VAR(lexem) }
          | '"'['0'-'z' ' ']*'"'                    as lexem { STRING(String.sub lexem 1 (String.length lexem - 2)) }
	  | ['A'-'z''0'-'z''-''_''0'-'9''\'']+      as lexem      { raise (TokenInconu lexem)}
	  | _      as lexem      { raise (CharInconu lexem)}




